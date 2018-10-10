module Xml.Decode.Internal exposing
    ( Attributes
    , Children
    , Content(..)
    , Context(..)
    , Element
    , Problem(..)
    , Tag
    , XmlParser
    , xml
    )

import Dict exposing (Dict)
import Parser.Advanced as Parser exposing (..)
import Set exposing (Set)


type alias Tag =
    ( String, Element )


type alias Element =
    { content : Content
    , attributes : Maybe Attributes
    }


type alias Attributes =
    Dict String String


type Content
    = Unstructured String
    | NoContent
    | Children Children


type alias Children =
    Dict String Element


type Context
    = BeforeRoot
    | BetweenTags
    | Tag String
    | EmptyTag String
    | StringTag String
    | BetweenAttributes
    | Attribute String


type Problem
    = ExpectingCommentStart
    | ExpectingCommentEnd
    | ExpectingDirectiveStart
    | ExpectingDirectiveEnd
    | ExpectingEOF
    | ExpectingQuotationMark Char
    | ExpectingTagOpening
    | ExpectingValidTagName
    | ExpectingOnlyAttrTagEnd String
    | ExpectingOpeningTagClose String
    | ExpectingClosingTagOpening String
    | ExpectingClosingTag String
    | ExpectingClosingTagEnd String
    | ExpectingValidAttrName
    | ExpectingAttrEq
    | ExpectingAttrValue


type alias XmlParser a =
    Parser Context Problem a


xml : XmlParser Tag
xml =
    inContext BeforeRoot <|
        Parser.succeed identity
            |. ignoreCommentsAndDirectives
            |= tag
            |. ignoreCommentsAndDirectives
            |. end ExpectingEOF


tag : XmlParser Tag
tag =
    let
        tagOpening : XmlParser ( String, Maybe Attributes )
        tagOpening =
            inContext BetweenTags <|
                succeed Tuple.pair
                    |. spaces
                    |. symbol (Token "<" ExpectingTagOpening)
                    |= variable
                        { start = \c -> Char.isAlpha c || c == '_'
                        , inner = \c -> Char.isAlphaNum c || Set.member c (Set.fromList [ '-', '_', '.', ':' ])
                        , reserved = Set.singleton "xml"
                        , expecting = ExpectingValidTagName
                        }
                    |= maybeAttributes

        tagOpeningEndToken =
            ">"
    in
    tagOpening
        |> andThen
            (\( tagName, attrs ) ->
                succeed (\content -> ( tagName, Element content attrs ))
                    |. spaces
                    |= (inContext (Tag tagName) <|
                            oneOf
                                [ emptyTagRemainder tagName
                                , tagChildren tagName
                                , stringTagRemainder tagName
                                ]
                       )
            )


openingTagEnd : String -> XmlParser ()
openingTagEnd tagName =
    symbol <| Token ">" <| ExpectingOpeningTagClose tagName


closingTag : String -> XmlParser ()
closingTag tagName =
    let
        closingTagOpening =
            symbol <| Token "</" <| ExpectingClosingTagOpening tagName

        closingTagName =
            keyword <| Token tagName <| ExpectingClosingTag tagName

        closingTagEnd =
            symbol <| Token ">" <| ExpectingClosingTagEnd tagName
    in
    closingTagOpening
        |. spaces
        |. closingTagName
        |. spaces
        |. closingTagEnd


emptyTagRemainder : String -> XmlParser Content
emptyTagRemainder tagName =
    let
        attributeOnlyTagEnd =
            symbol <| Token "/>" <| ExpectingOnlyAttrTagEnd tagName
    in
    inContext (EmptyTag tagName) <|
        map (\() -> NoContent) <|
            oneOf
                [ attributeOnlyTagEnd
                , openingTagEnd tagName
                    |. spaces
                    |. closingTag tagName
                ]


tagChildren : String -> XmlParser Content
tagChildren tagName =
    succeed (Children << Dict.fromList)
        |. openingTagEnd tagName
        |. spaces
        |= loop []
            (\listSoFar ->
                oneOf
                    [ map (\newTag -> Loop (newTag :: listSoFar)) tag
                    , map (\() -> Done listSoFar) (closingTag tagName)
                    ]
            )


stringTagRemainder : String -> XmlParser Content
stringTagRemainder tagName =
    inContext (StringTag tagName) <|
        map Unstructured <|
            getChompedString <|
                loop ()
                    (\() ->
                        let
                            closingTagName =
                                keyword <| Token tagName <| ExpectingClosingTag tagName

                            closingTagEnd =
                                symbol <| Token ">" <| ExpectingClosingTagEnd tagName
                        in
                        oneOf
                            [ map Loop <|
                                chompUntil (Token "</" (ExpectingClosingTagOpening tagName))
                                    |. symbol (Token "</" (ExpectingClosingTagOpening tagName))
                            , map Done
                                (closingTagName
                                    |. closingTagEnd
                                )
                            ]
                    )


maybeAttributes : XmlParser (Maybe Attributes)
maybeAttributes =
    succeed identity
        |. spaces
        |= oneOf
            [ map (Dict.fromList >> Just)
                (attribute
                    |> andThen
                        (\first ->
                            succeed ((::) first)
                                |= attributeList
                        )
                )
            , succeed Nothing
            ]


attributeList : XmlParser (List ( String, String ))
attributeList =
    loop []
        (\listSoFar ->
            oneOf
                [ map (\newAttr -> Loop (newAttr :: listSoFar))
                    attribute
                , succeed (Done listSoFar)
                ]
        )


attribute : XmlParser ( String, String )
attribute =
    let
        attributeName =
            variable
                { start = \c -> Char.isAlpha c || c == '_'
                , inner = \c -> Char.isAlphaNum c || Set.member c (Set.fromList [ '-', '_', '.', ':' ])
                , reserved = Set.singleton "xml"
                , expecting = ExpectingValidAttrName
                }
    in
    attributeName
        |> andThen
            (\attrName ->
                inContext (Attribute attrName) <|
                    succeed (Tuple.pair attrName)
                        |. spaces
                        |. symbol (Token "=" ExpectingAttrEq)
                        |. spaces
                        |= oneOf
                            [ quotedString '"' --" --Ignore this comment; VSCode code rendering bug fix.
                            , quotedString '\''
                            ]
            )


quotedString : Char -> XmlParser String
quotedString quote =
    let
        quoteToken =
            Token (String.fromChar quote) (ExpectingQuotationMark quote)
    in
    succeed identity
        |. symbol quoteToken
        |= getChompedString (chompUntil quoteToken)
        |. symbol quoteToken


ignoreCommentsAndDirectives : XmlParser ()
ignoreCommentsAndDirectives =
    let
        commentStart =
            Token "<!--" <| ExpectingCommentStart

        commentEnd =
            Token "-->" <| ExpectingCommentEnd

        directiveStart =
            Token "<?" <| ExpectingDirectiveStart

        directiveEnd =
            Token "?>" <| ExpectingDirectiveEnd
    in
    Parser.succeed ()
        |. spaces
        |. loop ()
            (\() ->
                oneOf
                    [ map (\_ -> Loop ()) <|
                        multiComment commentStart commentStart NotNestable
                    , map (\_ -> Loop ()) <|
                        multiComment directiveStart directiveEnd NotNestable
                    , succeed (Done ())
                    ]
            )
