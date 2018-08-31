module Xml.Advanced exposing (XmlTag(..),SubTagDict,xmlFile,xml,possibleComments)

import Parser exposing (Parser,(|.),(|=),spaces,succeed,symbol,keyword, andThen)
import Dict exposing (Dict)
import Set exposing (Set)

type XmlTag
    = SubTags SubTagDict
    | PresenceTag
    | XmlInt Int
    | XmlFloat Float
    | XmlString String

type alias SubTagDict = Dict String (List XmlTag)

xmlFile : Parser XmlTag
xmlFile =
    succeed identity
        |. Parser.symbol "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
        |. mySpaces
        |. possibleComments
        |. mySpaces
        |= (xml |> Parser.map Tuple.second)
        |. mySpaces
        |. Parser.end

possibleComments : Parser ()
possibleComments =
    symbol "<!--"
        |. Parser.chompUntil "-->"
        |. Parser.symbol "-->"

xml : Parser (String, XmlTag)
xml =
    xmlTag 
        |> andThen 
            (\tag -> Parser.oneOf
                [ succeed (tag,PresenceTag)
                    |. symbol "/>"
                , succeed identity
                    |. symbol ">"
                    |. mySpaces
                    |= parseXmlHelp tag
                ]
            )

xmlTag : Parser String
xmlTag = 
    succeed identity
        |. symbol "<"
        |= Parser.variable
            { start = Char.isAlpha
            , inner = Char.isAlphaNum
            , reserved = Set.empty
            }
        |. discardAttributeList

parseXmlHelp : String -> Parser (String, XmlTag)
parseXmlHelp tag =
    Parser.oneOf
        [ succeed (tag,PresenceTag)
            |. symbol ("</"++tag++">")
        , succeed (\x -> (tag,x))
            |= subTagContent tag
        , succeed (\x -> (tag,x))
            |= simpleContent
            |. symbol ("</"++tag++">")
        , Parser.problem 
            "This super simple XML library doesn't support semi-structured XML. Please decide between either child tags or text inside any particular tag, not both!"
        ]

simpleContent : Parser XmlTag
simpleContent =
    Parser.getChompedString (Parser.chompUntil "</")
    |> andThen 
        (\s -> succeed
            ( case String.toInt s of
                Just i ->
                    XmlInt i
                Nothing ->
                    case String.toFloat s of
                        Just f ->
                            XmlFloat f
                        Nothing ->
                            XmlString s
            )
        )

subTagContent : String -> Parser XmlTag
subTagContent tag =
    Parser.loop Dict.empty 
        (subTagContentHelp tag)

subTagContentHelp : String -> SubTagDict -> Parser (Parser.Step SubTagDict XmlTag)
subTagContentHelp tag dict = 
    Parser.oneOf
        [ succeed (Parser.Loop dict)
            |. possibleComments
        , succeed (Parser.Done (SubTags dict))
            |. symbol ("</"++tag++">")
        , succeed (\(t,x) -> Parser.Loop (dict |> insertSubTag t x))
            |= xml
        ]
        |. mySpaces

insertSubTag : String -> XmlTag -> SubTagDict -> SubTagDict
insertSubTag tag xmltag =
    Dict.update tag 
        (\m ->
            case m of
                Just l ->
                    Just (xmltag :: l)
                Nothing ->
                    Just (List.singleton xmltag)
        )


discardAttributeList : Parser ()
discardAttributeList =
    (Parser.loop () <| always
        ( succeed identity 
            |. spaces 
            |= Parser.oneOf
                [ succeed (Parser.Loop ())
                    |. Parser.variable
                        { start = Char.isAlpha
                        , inner = \c -> Char.isAlphaNum c || c == ':'
                        , reserved = Set.empty
                        }
                    |. symbol "=\""
                    |. Parser.chompUntil "\""
                    |. symbol "\""
                , succeed (Parser.Done ())
                ]
        )
    ) |. spaces

mySpaces : Parser ()
mySpaces =
    Parser.chompWhile
        (\c ->
            Set.fromList [' ','\n','\r','\t']
                |> Set.member c
        )