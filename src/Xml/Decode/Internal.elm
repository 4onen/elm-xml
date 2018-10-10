module Xml.Decode.Internal exposing (SubTagDict, XmlContentValue(..), XmlValue, discardAttributeList, insertSubTag, mySpaces, parseXmlHelp, possibleComments, simpleContent, subTagContent, subTagContentHelp, xml, xmlFile, xmlTag)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, andThen, keyword, spaces, succeed, symbol)
import Set exposing (Set)


type alias XmlValue =
    ( String, XmlContentValue )


type XmlContentValue
    = SubTags SubTagDict
    | PresenceTag
    | XmlString String


type alias SubTagDict =
    Dict String (List XmlContentValue)


xmlFile : Parser XmlValue
xmlFile =
    succeed identity
        |. Parser.symbol "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
        |. mySpaces
        |. possibleComments
        |. mySpaces
        |= xml
        |. mySpaces
        |. Parser.end


possibleComments : Parser ()
possibleComments =
    symbol "<!--"
        |. Parser.chompUntil "-->"
        |. Parser.symbol "-->"


xml : Parser XmlValue
xml =
    xmlTag
        |> andThen
            (\tag ->
                Parser.oneOf
                    [ succeed ( tag, PresenceTag )
                        |. symbol "/>"
                    , succeed identity
                        |. symbol ">"
                        |= parseXmlHelp tag
                    ]
            )



{--| Detect a valid xml tag opener followed by a valid xml tag name, then discard the xml attribute list.

Does not consume the tag close, as that may help define whether the tag has any content.

See XML Naming Rules on https://www.w3schools.com/xml/xml_elements.asp to help understand valid tag names.--}


xmlTag : Parser String
xmlTag =
    succeed identity
        |. symbol "<"
        |= Parser.variable
            { start = \c -> Char.isAlpha c || c == '_'
            , inner = \c -> Char.isAlphaNum c || Set.member c (Set.fromList [ '-', '_', '.' ])
            , reserved = Set.singleton "xml"
            }
        |. discardAttributeList


parseXmlHelp : String -> Parser XmlValue
parseXmlHelp tag =
    Parser.oneOf
        [ succeed ( tag, PresenceTag )
            |. Parser.backtrackable mySpaces
            |. symbol ("</" ++ tag ++ ">")
        , succeed (\x -> ( tag, x ))
            |. Parser.backtrackable mySpaces
            |= subTagContent tag
        , succeed (\x -> ( tag, x ))
            |= simpleContent
            |. symbol ("</" ++ tag ++ ">")
        , Parser.problem
            "This super simple XML library doesn't support semi-structured XML. Please decide between either child tags or text inside any particular tag, not both!"
        ]


simpleContent : Parser XmlContentValue
simpleContent =
    Parser.getChompedString (Parser.chompUntil "</")
        |> Parser.map XmlString


subTagContent : String -> Parser XmlContentValue
subTagContent tag =
    Parser.loop Dict.empty
        (subTagContentHelp tag)


subTagContentHelp : String -> SubTagDict -> Parser (Parser.Step SubTagDict XmlContentValue)
subTagContentHelp tag dict =
    Parser.oneOf
        [ succeed (Parser.Loop dict)
            |. possibleComments
        , succeed (Parser.Done (SubTags dict))
            |. symbol ("</" ++ tag ++ ">")
        , succeed (\( t, x ) -> Parser.Loop (dict |> insertSubTag t x))
            |= xml
        ]
        |. mySpaces


insertSubTag : String -> XmlContentValue -> SubTagDict -> SubTagDict
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
    (Parser.loop () <|
        always
            (succeed identity
                |. spaces
                |= Parser.oneOf
                    [ succeed (Parser.Loop ())
                        |. Parser.variable
                            { start = Char.isAlpha
                            , inner = \c -> Char.isAlphaNum c || c == ':'
                            , reserved = Set.empty
                            }
                        |. Parser.oneOf
                            [ symbol "=\""
                                |. Parser.chompUntil "\""
                                |. symbol "\""
                            , symbol "='"
                                |. Parser.chompUntil "'"
                                |. symbol "'"
                            ]
                    , succeed (Parser.Done ())
                    ]
            )
    )
        |. spaces


mySpaces : Parser ()
mySpaces =
    Parser.chompWhile
        (\c ->
            Set.member c <| Set.fromList [ ' ', '\n', '\u{000D}', '\t' ]
        )
