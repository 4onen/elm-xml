module Encode exposing
    ( children
    , encode
    , float
    , int
    , list
    , string
    , tag
    )

import Dict
import Xml.Decode.Internal as Internal


encode : Int -> Internal.Tag -> String
encode indent rootTag =
    let
        clampedIndent =
            if indent < 0 then
                0

            else
                indent

        indentBlock =
            String.repeat clampedIndent " "
    in
    "I don't know how to do anything yet! But I've got " ++ String.fromInt clampedIndent ++ " indent!"


float : Float -> Internal.Element
float =
    String.fromFloat
        >> Internal.Unstructured
        >> attributeless


int : Int -> Internal.Element
int =
    String.fromInt
        >> Internal.Unstructured
        >> attributeless


string : String -> Internal.Element
string =
    Internal.Unstructured
        >> attributeless


empty : Internal.Element
empty =
    Internal.NoContent
        |> attributeless


attributeless : Internal.Content -> Internal.Element
attributeless c =
    { content = c, attributes = Nothing }


tag : String -> Internal.Element -> Internal.Tag
tag tagName el =
    ( tagName, el )


list : String -> List Internal.Element -> List Internal.Tag
list tagName =
    List.map (\el -> Tuple.pair tagName el)


children : List Internal.Tag -> Internal.Element
children =
    attributeless
        << Internal.Children
        << List.foldl
            childrenFold
            Dict.empty


childrenFold : Internal.Tag -> Internal.Children -> Internal.Children
childrenFold ( tagName, tagEl ) =
    Dict.update tagName
        (Maybe.withDefault []
            >> (::) tagEl
            >> Just
        )
