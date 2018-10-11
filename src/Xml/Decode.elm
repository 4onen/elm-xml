module Xml.Decode exposing
    ( Decoder
    , ParseError(..)
    , andThen
    , errorToString
    , fail
    , float
    , int
    , list
    , map
    , map2
    , map3
    , map4
    , map5
    , map6
    , maybeTag
    , nonEmptyList
    , oneOf
    , presenceTag
    , string
    , succeed
    , tag
    , tagPresence
    , value
    , xmlString
    )

import Dict
import Parser.Advanced as Parser
import Xml.Decode.ErrorParsing as EEP
import Xml.Decode.Internal as Internal


type ParseError
    = BadValue String
    | MissingValue String
    | BadRoot String String
    | InContext String ParseError
    | OneOfFail (List ParseError)
    | CustomFail String
    | InternalFail (List Internal.DeadEnd)


type alias Decoder a =
    Internal.Tag -> Result ParseError a


xmlString : Decoder val -> String -> Result ParseError val
xmlString dec str =
    case Parser.run Internal.xml str of
        Result.Ok xmlValue ->
            xmlValue
                |> dec
                |> Result.mapError (InContext "From the root of your string...\n")

        Result.Err deadEnds ->
            Result.Err <| InternalFail deadEnds


map : (a -> val) -> Decoder a -> Decoder val
map f dec xmlValue =
    Result.map f (dec xmlValue)


map2 : (a -> b -> val) -> Decoder a -> Decoder b -> Decoder val
map2 f decA decB xmlValue =
    case ( decA xmlValue, decB xmlValue ) of
        ( Result.Ok a, Result.Ok b ) ->
            Result.Ok (f a b)

        ( Result.Err s, _ ) ->
            Result.Err s

        ( _, Result.Err s ) ->
            Result.Err s


map3 : (a -> b -> c -> val) -> Decoder a -> Decoder b -> Decoder c -> Decoder val
map3 f decA decB decC xmlValue =
    case ( map2 Tuple.pair decA decB xmlValue, decC xmlValue ) of
        ( Result.Ok ( a, b ), Result.Ok c ) ->
            Result.Ok (f a b c)

        ( Result.Err s, _ ) ->
            Result.Err s

        ( _, Result.Err s ) ->
            Result.Err s


map4 : (a -> b -> c -> d -> val) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder val
map4 f decA decB decC decD xmlValue =
    case ( map2 Tuple.pair decA decB xmlValue, map2 Tuple.pair decC decD xmlValue ) of
        ( Result.Ok ( a, b ), Result.Ok ( c, d ) ) ->
            Result.Ok (f a b c d)

        ( Result.Err s, _ ) ->
            Result.Err s

        ( _, Result.Err s ) ->
            Result.Err s


tripleTuple : a -> b -> c -> ( a, b, c )
tripleTuple a b c =
    ( a, b, c )


map5 : (a -> b -> c -> d -> e -> val) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder val
map5 f decA decB decC decD decE xmlValue =
    case ( map3 tripleTuple decA decB decC xmlValue, map2 Tuple.pair decD decE xmlValue ) of
        ( Result.Ok ( a, b, c ), Result.Ok ( d, e ) ) ->
            Result.Ok (f a b c d e)

        ( Result.Err s, _ ) ->
            Result.Err s

        ( _, Result.Err s ) ->
            Result.Err s


map6 :
    (a -> b -> c -> d -> e -> f -> val)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder val
map6 func decA decB decC decD decE decF xmlValue =
    case ( map3 tripleTuple decA decB decC xmlValue, map3 tripleTuple decD decE decF xmlValue ) of
        ( Result.Ok ( a, b, c ), Result.Ok ( d, e, f ) ) ->
            Result.Ok (func a b c d e f)

        ( Result.Err s, _ ) ->
            Result.Err s

        ( _, Result.Err s ) ->
            Result.Err s


maybeTag : String -> Decoder a -> Decoder (Maybe a)
maybeTag str dec ( _, { content, attributes } ) =
    case content of
        Internal.Children dict ->
            case Dict.get str dict of
                Just xcv ->
                    case xcv of
                        [] ->
                            Result.Ok Maybe.Nothing

                        x :: [] ->
                            dec ( str, x )
                                |> Result.map Maybe.Just
                                |> Result.mapError (InContext <| "Inside the maybe tag <" ++ str ++ ">...\n")

                        x :: xs ->
                            Result.Err <| BadValue <| "You were expecting to find no more than one of the tag <" ++ str ++ ">, but I found more than one of it!"

                Nothing ->
                    Result.Ok Maybe.Nothing

        Internal.NoContent ->
            Result.Ok Maybe.Nothing

        Internal.Unstructured s ->
            Result.Err <| BadValue <| "You were expecting to find the tag <" ++ str ++ ">, but the tag you looked in was a string!\nRemember, this library doesn't support unstructured XML data!"


tag : String -> Decoder a -> Decoder a
tag str dec thisTag =
    case maybeTag str dec thisTag of
        Result.Ok (Just val) ->
            Result.Ok val

        Result.Ok Nothing ->
            Result.Err <| MissingValue <| "You were expecting to find the tag <" ++ str ++ ">, but I couldn't find it!"

        Result.Err (InContext _ e) ->
            Result.Err (InContext ("Inside the required tag <" ++ str ++ ">...\n") e)

        Result.Err e ->
            Result.Err e


nonEmptyList : String -> Decoder a -> Decoder (List a)
nonEmptyList str dec (( _, { content, attributes } ) as thisTag) =
    case list str dec thisTag of
        Result.Ok (l :: ls) ->
            Result.Ok (l :: ls)

        Result.Ok [] ->
            Result.Err <| BadValue <| "You were expecting a non-empty list of <" ++ str ++ ">s, but I didn't find any!"

        Result.Err (InContext _ e) ->
            Result.Err (InContext ("Searching the non-empty list of <" ++ str ++ ">s...\n") e)

        Result.Err e ->
            Result.Err e


list : String -> Decoder a -> Decoder (List a)
list str dec ( _, { content, attributes } ) =
    case content of
        Internal.Children dict ->
            case Dict.get str dict of
                Just l ->
                    let
                        decodedL =
                            List.map (Tuple.pair str >> dec) l

                        ( firstError, succeedingL ) =
                            List.foldl
                                (\entry ( pastError, newL ) ->
                                    case ( pastError, entry ) of
                                        ( Just error, _ ) ->
                                            ( pastError, [] )

                                        ( Nothing, Result.Err error ) ->
                                            ( Just error, [] )

                                        ( Nothing, Result.Ok ok ) ->
                                            ( Nothing, ok :: newL )
                                )
                                ( Nothing, [] )
                                decodedL
                    in
                    case firstError of
                        Just error ->
                            error
                                |> InContext ("Searching the list of <" ++ str ++ ">s...\n")
                                |> Result.Err

                        Nothing ->
                            Result.Ok succeedingL

                _ ->
                    Result.Ok []

        Internal.NoContent ->
            Result.Ok []

        Internal.Unstructured _ ->
            Result.Err <| BadValue <| "You were expecting to find a list of <" ++ str ++ ">s, but you looked in a string tag!"


tagPresence : Decoder Bool
tagPresence ( _, _ ) =
    Result.Ok True


presenceTag : Decoder Bool
presenceTag ( _, { content, attributes } ) =
    case content of
        Internal.NoContent ->
            Result.Ok True

        Internal.Unstructured _ ->
            Result.Err <| BadValue <| "This presence tag had string content! Did you mean to test for \"tagPresence\"?"

        Internal.Children d ->
            case Dict.isEmpty d of
                True ->
                    Result.Ok True

                False ->
                    Result.Err <| BadValue <| "This presence tag had children! Do you want to change your datamodel to support these?"


string : Decoder String
string =
    expectingStringContent "a string"


int : Decoder Int
int =
    expectingStringContent "an integer"
        >> Result.andThen
            (\s ->
                if String.length s < 100 then
                    String.toInt s
                        |> Result.fromMaybe
                            (BadValue <| "You were expecting an integer, but only found this string! \n\"" ++ s ++ "\"")

                else
                    String.toInt s
                        |> Result.fromMaybe
                            (BadValue "You were expecting an integer, but only found a really long string!")
            )


float : Decoder Float
float =
    expectingStringContent "a floating-point number"
        >> Result.andThen
            (\s ->
                if String.length s < 100 then
                    String.toFloat s
                        |> Result.fromMaybe
                            (BadValue <| "You were expecting a floating-point number, but only found this string! \n\"" ++ s ++ "\"")

                else
                    String.toFloat s
                        |> Result.fromMaybe
                            (BadValue "You were expecting a floating-point number, but only found a really long string!")
            )


expectingStringContent : String -> Decoder String
expectingStringContent expect ( _, { content, attributes } ) =
    case content of
        Internal.Unstructured str ->
            Result.Ok str

        Internal.Children _ ->
            Result.Err <| BadValue <| "You were expecting " ++ expect ++ ", but found more XML children!"

        Internal.NoContent ->
            Result.Err <| MissingValue <| "You were expecting " ++ expect ++ ", but found an empty tag!"


value : Decoder Internal.Tag
value =
    Result.Ok


oneOf : List (Decoder a) -> Decoder a
oneOf decs xmlContentValue =
    case decs of
        [] ->
            Result.Err (OneOfFail [])

        dec :: [] ->
            case dec xmlContentValue of
                Result.Ok val ->
                    Result.Ok val

                Result.Err e ->
                    Result.Err <| OneOfFail [ e ]

        dec :: moreDecs ->
            case dec xmlContentValue of
                Result.Ok val ->
                    Result.Ok val

                Result.Err e ->
                    case oneOf moreDecs xmlContentValue of
                        Result.Ok val ->
                            Result.Ok val

                        Result.Err (OneOfFail listOfFails) ->
                            Result.Err (OneOfFail <| e :: listOfFails)

                        Result.Err e2 ->
                            Result.Err e2


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen makeDecB decA xmlContentValue =
    case decA xmlContentValue of
        Result.Ok a ->
            makeDecB a xmlContentValue

        Result.Err e ->
            Result.Err e


succeed : a -> Decoder a
succeed a _ =
    Result.Ok a


fail : String -> Decoder a
fail msg _ =
    Result.Err <| CustomFail msg


errorToString : ParseError -> String
errorToString err =
    case err of
        BadValue s ->
            s

        MissingValue s ->
            s

        BadRoot expected root ->
            "The expected root tag <"
                ++ expected
                ++ "> did not match the xml's real root tag <"
                ++ root
                ++ ">.\n"
                ++ "Are you absolutely sure this is the data you wanted?"

        InContext s e ->
            s ++ errorToString e

        OneOfFail listOfFails ->
            case listOfFails of
                [] ->
                    "You tried to use a \"oneOf\" decoder, but gave it no decoders!"

                e :: [] ->
                    "The one decoder passed to this oneOf failed with error:\n" ++ errorToString e

                es ->
                    es
                        |> List.map (\e -> "[" ++ errorToString e ++ "]")
                        |> String.concat
                        |> (++) "You tried a few different decoders, but they all failed. See the [bracketed] errors below:\n"

        CustomFail s ->
            "Your decoder threw the custom error below:\n" ++ s

        InternalFail listDeadEnds ->
            listDeadEnds
                |> List.map EEP.deadEndToString
                |> List.foldl (++) ""
                |> (++) "I ran into a syntax error while parsing this Xml!\n"



{--Old internal error-to-string implementation
decodeParserDeadEnd : Parser.DeadEnd -> String
decodeParserDeadEnd de =
    let
        endInfo row col =
            " at row " ++ String.fromInt row ++ " and column " ++ String.fromInt col ++ ".\n"

        expectation str row col =
            "I was expecting " ++ str ++ endInfo row col
    in
    case de.problem of
        Expecting str ->
            expectation ("\"" ++ str ++ "\"") de.row de.col

        ExpectingInt ->
            expectation "an integer" de.row de.col

        ExpectingHex ->
            expectation "a hexadecimal" de.row de.col

        ExpectingOctal ->
            expectation "an octal number" de.row de.col

        ExpectingBinary ->
            expectation "a binary number" de.row de.col

        ExpectingFloat ->
            expectation "a floating-point number" de.row de.col

        ExpectingNumber ->
            expectation "a number" de.row de.col

        ExpectingVariable ->
            expectation "a valid xml name" de.row de.col

        ExpectingSymbol str ->
            expectation ("the xml symbol \"" ++ str ++ "\"") de.row de.col

        ExpectingKeyword str ->
            expectation ("the keyword \"" ++ str ++ "\"") de.row de.col

        ExpectingEnd ->
            expectation "the end of the file" de.row de.col

        UnexpectedChar ->
            "I ran into an unexpected character, but I lost it" ++ endInfo de.row de.col

        Problem str ->
            "I ran into a problem that can be best described \"" ++ str ++ "\"" ++ endInfo de.row de.col

        BadRepeat ->
            "While processing this input, I began to recurse infinitely! :( Let my programmer know! It happened" ++ endInfo de.row de.col
--}
