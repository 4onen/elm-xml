module Main exposing (main)

import Browser
import Html
import Html.Attributes as Html
import Html.Events as Html
import Parser.Advanced
import Xml.Decode as XD
import Xml.Decode.ErrorParsing as XDEP
import Xml.Decode.Internal as XDI
import Xml.Encode as XE


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init =
    Nothing


type alias Model =
    Maybe (Result (List String) XDI.Tag)


type Msg
    = ParseGoodData
    | ParseBadData


update : Msg -> Model -> Model
update msg model =
    case msg of
        ParseGoodData ->
            goodData
                |> Parser.Advanced.run XDI.xml
                |> Result.mapError (List.map XDEP.deadEndToString)
                |> Just

        ParseBadData ->
            badData
                |> Parser.Advanced.run XDI.xml
                |> Result.mapError (List.map XDEP.deadEndToString)
                |> Just


view model =
    Html.div []
        [ Html.div []
            [ viewButton "ParseGoodData" ParseGoodData
            , viewButton "ParseBadData" ParseBadData
            ]
        , Html.div []
            [ Html.text <| Debug.toString model
            ]
        ]


viewButton name msg =
    Html.input
        [ Html.type_ "button"
        , Html.value name
        , Html.onClick msg
        ]
        []


goodData =
    """
    <tag>
        <text>D'aww. I've been put in a labeled box!</text>
        <text>I can still have multiple children</text>
        <text>Ooh, they can have the same tag!</text>
        <text>But I lose fancy stuff like bolding this text.</text>
        <text>Maybe I could add *Markdown* and parse that later?</text>
        <ul>
            <li>I'm not sure why I stole these ul and li tags from html</li>
            <tagname>Tag names can still be anything!</tagname>
            <li>
                <value>0</value>
            </li>
            <li>Oh, right, tags with no content are a thing!</li>
            <li>Check this out:</li>
            <iamatagtoo />
            <yippee></yippee>
        </ul>
        <somenumbers>
            <n>864</n>
            <n>999</n>
        </somenumbers>
        <text>I think you get the point.</text>
    </tag>
"""


badData =
    """
    <tag>
        I'm some unstructured <b>text</b> with tags in the middle!
        <ul>
            <li>This ul is an unstructured child of tag 'tag'</li>
            <li>but each of these list items is a structured--</li>
            whoops!
            <li>Now all these list items are unstructured children of ul!</li>
        </ul>
    </tag>
"""
