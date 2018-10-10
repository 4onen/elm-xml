module Xml.Decode exposing
    ( XmlContentDecoder
    , XmlDecoder
    , XmlParseError(..)
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
    , rootTag
    , string
    , succeed
    , tag
    , tagPresence
    , value
    , xmlFileString
    , xmlString
    )

import Dict
import Parser exposing ((|.), (|=), Problem(..))
import Xml.Decode.Internal exposing (XmlContentValue(..), XmlValue)


type XmlParseError
    = SyntaxError String
    | BadValue String
    | MissingValue String
    | BadRoot String String
    | InContext String XmlParseError
    | OneOfFail (List XmlParseError)
    | CustomFail String


type alias XmlDecoder a =
    XmlValue -> Result XmlParseError a


type alias XmlContentDecoder a =
    XmlContentValue -> Result XmlParseError a


xmlFileString : XmlDecoder val -> String -> Result XmlParseError val
xmlFileString dec str =
    case Parser.run Xml.Decode.Internal.xmlFile str of
        Result.Ok xmlValue ->
            xmlValue
                |> dec
                |> Result.mapError (InContext "From the root of your file...\n")

        Result.Err deadEnds ->
            deadEnds
                |> List.map decodeParserDeadEnd
                |> List.foldl (++) ""
                |> (++) "I ran into an error while parsing this xml file!\n\n"
                |> SyntaxError
                |> Result.Err


xmlString : XmlDecoder val -> String -> Result XmlParseError val
xmlString dec str =
    case Parser.run (Parser.succeed identity |. Parser.spaces |= Xml.Decode.Internal.xml) str of
        Result.Ok xmlValue ->
            xmlValue
                |> dec
                |> Result.mapError (InContext "From the root of your string...\n")

        Result.Err deadEnds ->
            deadEnds
                |> List.map decodeParserDeadEnd
                |> List.foldl (++) ""
                |> (++) "I ran into an error while parsing this xml string!\n\n"
                |> SyntaxError
                |> Result.Err


map : (a -> val) -> XmlContentDecoder a -> XmlContentDecoder val
map f dec xmlValue =
    Result.map f (dec xmlValue)


map2 : (a -> b -> val) -> XmlContentDecoder a -> XmlContentDecoder b -> XmlContentDecoder val
map2 f decA decB xmlValue =
    case ( decA xmlValue, decB xmlValue ) of
        ( Result.Ok a, Result.Ok b ) ->
            Result.Ok (f a b)

        ( Result.Err s, _ ) ->
            Result.Err s

        ( _, Result.Err s ) ->
            Result.Err s


map3 : (a -> b -> c -> val) -> XmlContentDecoder a -> XmlContentDecoder b -> XmlContentDecoder c -> XmlContentDecoder val
map3 f decA decB decC xmlValue =
    case ( map2 Tuple.pair decA decB xmlValue, decC xmlValue ) of
        ( Result.Ok ( a, b ), Result.Ok c ) ->
            Result.Ok (f a b c)

        ( Result.Err s, _ ) ->
            Result.Err s

        ( _, Result.Err s ) ->
            Result.Err s


map4 : (a -> b -> c -> d -> val) -> XmlContentDecoder a -> XmlContentDecoder b -> XmlContentDecoder c -> XmlContentDecoder d -> XmlContentDecoder val
map4 f decA decB decC decD xmlValue =
    case ( map2 Tuple.pair decA decB xmlValue, map2 Tuple.pair decC decD xmlValue ) of
        ( Result.Ok ( a, b ), Result.Ok ( c, d ) ) ->
            Result.Ok (f a b c d)

        ( Result.Err s, _ ) ->
            Result.Err s

        ( _, Result.Err s ) ->
            Result.Err s


map5 : (a -> b -> c -> d -> e -> val) -> XmlContentDecoder a -> XmlContentDecoder b -> XmlContentDecoder c -> XmlContentDecoder d -> XmlContentDecoder e -> XmlContentDecoder val
map5 f decA decB decC decD decE xmlValue =
    case ( map2 Tuple.pair decA decB xmlValue, map2 Tuple.pair decC decD xmlValue, decE xmlValue ) of
        ( Result.Ok ( a, b ), Result.Ok ( c, d ), Result.Ok e ) ->
            Result.Ok (f a b c d e)

        ( Result.Err s, _, _ ) ->
            Result.Err s

        ( _, Result.Err s, _ ) ->
            Result.Err s

        ( _, _, Result.Err s ) ->
            Result.Err s


map6 :
    (a -> b -> c -> d -> e -> f -> val)
    -> XmlContentDecoder a
    -> XmlContentDecoder b
    -> XmlContentDecoder c
    -> XmlContentDecoder d
    -> XmlContentDecoder e
    -> XmlContentDecoder f
    -> XmlContentDecoder val
map6 func decA decB decC decD decE decF xmlValue =
    case ( map2 Tuple.pair decA decB xmlValue, map2 Tuple.pair decC decD xmlValue, map2 Tuple.pair decE decF xmlValue ) of
        ( Result.Ok ( a, b ), Result.Ok ( c, d ), Result.Ok ( e, f ) ) ->
            Result.Ok (func a b c d e f)

        ( Result.Err s, _, _ ) ->
            Result.Err s

        ( _, Result.Err s, _ ) ->
            Result.Err s

        ( _, _, Result.Err s ) ->
            Result.Err s


rootTag : String -> XmlContentDecoder a -> XmlDecoder a
rootTag str dec ( rootTagStr, xmlContentValue ) =
    if not (str == rootTagStr) then
        Result.Err <| BadRoot str rootTagStr

    else
        dec xmlContentValue
            |> Result.mapError (InContext <| "Inside the root tag <" ++ str ++ ">...\n")


maybeTag : String -> XmlContentDecoder a -> XmlContentDecoder (Maybe a)
maybeTag str dec xmlContentValue =
    case xmlContentValue of
        SubTags dict ->
            case Dict.get str dict of
                Just xcv ->
                    case xcv of
                        [] ->
                            Result.Ok Maybe.Nothing

                        x :: [] ->
                            dec x
                                |> Result.map Maybe.Just
                                |> Result.mapError (InContext <| "Inside the maybe tag <" ++ str ++ ">...\n")

                        x :: xs ->
                            Result.Err <| BadValue <| "You were expecting to find no more than one of the tag <" ++ str ++ ">, but I found more than one of it!"

                Nothing ->
                    Result.Ok Maybe.Nothing

        PresenceTag ->
            Result.Ok Maybe.Nothing

        XmlString s ->
            Result.Err <| BadValue <| "You were expecting to find the tag <" ++ str ++ ">, but the tag you looked in was a string!\nRemember, this library doesn't support unstructured XML data!"


tag : String -> XmlContentDecoder a -> XmlContentDecoder a
tag str dec xmlContentValue =
    case maybeTag str dec xmlContentValue of
        Result.Ok (Just val) ->
            Result.Ok val

        Result.Ok Nothing ->
            Result.Err <| MissingValue <| "You were expecting to find the tag <" ++ str ++ ">, but I couldn't find it!"

        Result.Err (InContext _ e) ->
            Result.Err (InContext ("Inside the required tag <" ++ str ++ ">...\n") e)

        Result.Err e ->
            Result.Err e


nonEmptyList : String -> XmlContentDecoder a -> XmlContentDecoder (List a)
nonEmptyList str dec xmlContentValue =
    case list str dec xmlContentValue of
        Result.Ok (l :: ls) ->
            Result.Ok (l :: ls)

        Result.Ok [] ->
            Result.Err <| BadValue <| "You were expecting a non-empty list of <" ++ str ++ ">s, but I didn't find any!"

        Result.Err (InContext _ e) ->
            Result.Err (InContext ("Searching the non-empty list of <" ++ str ++ ">s...\n") e)

        Result.Err e ->
            Result.Err e


list : String -> XmlContentDecoder a -> XmlContentDecoder (List a)
list str dec xmlContentValue =
    case xmlContentValue of
        SubTags dict ->
            case Dict.get str dict of
                Just l ->
                    let
                        decodedL =
                            List.map dec l

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

        PresenceTag ->
            Result.Ok []

        XmlString _ ->
            Result.Err <| BadValue <| "You were expecting to find a list of <" ++ str ++ ">s, but you looked in a string tag!"


tagPresence : XmlContentDecoder Bool
tagPresence xmlContentValue =
    Result.Ok True


presenceTag : XmlContentDecoder Bool
presenceTag xmlContentValue =
    case xmlContentValue of
        PresenceTag ->
            Result.Ok True

        XmlString _ ->
            Result.Err <| BadValue <| "This presence tag had string content! Did you mean to test for \"tagPresence\"?"

        SubTags d ->
            case Dict.isEmpty d of
                True ->
                    Result.Ok True

                False ->
                    Result.Err <| BadValue <| "This presence tag had children! Do you want to change your datamodel to support these?"


string : XmlContentDecoder String
string =
    expectingStringContent "a string"


int : XmlContentDecoder Int
int xmlContentValue =
    expectingStringContent "an integer" xmlContentValue
        |> Result.andThen
            (\s ->
                if String.length s < 100 then
                    Result.fromMaybe
                        (BadValue <| "You were expecting an integer, but only found this string! \n\"" ++ s ++ "\"")
                        (String.toInt s)

                else
                    Result.fromMaybe
                        (BadValue "You were expecting an integer, but only found a really long string!")
                        (String.toInt s)
            )


float : XmlContentDecoder Float
float xmlContentValue =
    expectingStringContent "a floating-point number" xmlContentValue
        |> Result.andThen
            (\s ->
                if String.length s < 100 then
                    Result.fromMaybe
                        (BadValue <| "You were expecting a floating-point number, but only found this string! \n\"" ++ s ++ "\"")
                        (String.toFloat s)

                else
                    Result.fromMaybe
                        (BadValue "You were expecting a floating-point number, but only found a really long string!")
                        (String.toFloat s)
            )


expectingStringContent : String -> XmlContentDecoder String
expectingStringContent expect xmlContentValue =
    case xmlContentValue of
        XmlString str ->
            Result.Ok str

        SubTags d ->
            Result.Err <| BadValue <| "You were expecting " ++ expect ++ ", but found more XML children!"

        PresenceTag ->
            Result.Err <| MissingValue <| "You were expecting " ++ expect ++ ", but found an empty tag!"


value : XmlContentDecoder XmlContentValue
value xmlContentValue =
    Result.Ok xmlContentValue


oneOf : List (XmlContentDecoder a) -> XmlContentDecoder a
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


andThen : (a -> XmlContentDecoder b) -> XmlContentDecoder a -> XmlContentDecoder b
andThen makeDecB decA xmlContentValue =
    case decA xmlContentValue of
        Result.Ok a ->
            makeDecB a xmlContentValue

        Result.Err e ->
            Result.Err e


succeed : a -> XmlContentDecoder a
succeed a _ =
    Result.Ok a


fail : String -> XmlContentDecoder a
fail msg _ =
    Result.Err <| CustomFail msg


errorToString : XmlParseError -> String
errorToString err =
    case err of
        SyntaxError s ->
            s

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



--TODO: Maybe switch to Parser.Advanced for custom errors?


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
