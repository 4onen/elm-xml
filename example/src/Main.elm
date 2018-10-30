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


type alias ParseResult =
    Result (List XDI.DeadEnd) XDI.Tag


type Model
    = NoParse
    | ParseDat String ParseResult


type Msg
    = GoParse String


init =
    NoParse


update : Msg -> Model -> Model
update msg model =
    case msg of
        GoParse data ->
            data
                |> Parser.Advanced.run XDI.xml
                |> ParseDat data


view : Model -> Html.Html Msg
view model =
    let
        renderBlock =
            case model of
                NoParse ->
                    Html.text "No parse yet."

                ParseDat dat result ->
                    Html.div []
                        [ sometext <| Debug.toString result
                        , sometext <| Debug.toString <| Result.mapError (List.map XDEP.deadEndToString) result
                        , sometext <| Debug.toString dat
                        ]

        buttonBlock =
            Html.div []
                [ viewButton "ParseGood" (GoParse goodData)
                , viewButton "ParseBad" (GoParse badData)
                , viewButton "ParseThree" (GoParse threeData)
                ]
    in
    Html.div []
        [ buttonBlock
        , renderBlock
        ]


viewButton name msg =
    Html.input
        [ Html.type_ "button"
        , Html.value name
        , Html.onClick msg
        ]
        []


sometext : String -> Html.Html Msg
sometext text =
    text
        |> String.split "\\n"
        |> List.map (\s -> Html.p [] [ Html.text s ])
        |> Html.div []


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


threeData =
    "<myroot><sub></sub><sub></sub><str>s</str></myroot>"
