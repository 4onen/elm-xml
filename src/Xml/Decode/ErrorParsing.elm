module Xml.Decode.ErrorParsing exposing (deadEndToString)

import Xml.Decode.Internal exposing (..)


deadEndToString : DeadEnd -> String
deadEndToString { row, col, problem, contextStack } =
    let
        header =
            problemHeader problem

        desc =
            problemDesc problem

        rowColDesc =
            "Take a look near line " ++ String.fromInt row ++ ", about " ++ String.fromInt col ++ " characters in."

        betterTodo =
            "If you need better error messages, bug the package maintainer!\n"
                ++ "He's already wired up the information in the parser,\n"
                ++ "He's just too lazy to write all the messages!\n"
    in
    header ++ "\n\n" ++ desc ++ "\n" ++ rowColDesc ++ "\n" ++ betterTodo ++ "\n\n"


problemDesc problem =
    case problem of
        ExpectingCommentStart ->
            "I was expecting a comment here, but couldn't find it!\n"
                ++ maintainerNote

        ExpectingCommentEnd ->
            "I was expecting the end of a comment before here, but couldn't find it!\n"

        ExpectingDirectiveStart ->
            "I was expecting a XML processing directive (the <? ?> things) about here, but I never found it!\n"
                ++ "I don't actually understand these, so I treat them just like comments.\n"
                ++ maintainerNote

        ExpectingDirectiveEnd ->
            "I wax expecting the end of an XML directive (the <? ?> things) about here, but I never found it!\n"
                ++ "I don't actually understand these, so I treat them just like comments.\n"
                ++ "If that's a problem for your use case, tell the package maintainer how they work!\n"

        ExpectingEOF ->
            "I was expecting the XML file to be over about here, but it wasn't!\n"
                ++ "Are you sure this XML closed the root tag?\n"
                ++ "If it isn't the root tag, I have no idea how you got here.\n"

        ExpectingQuotationMark c ->
            "I was looking for an XML quotation mark (specifically, the "
                ++ String.fromChar c
                ++ " character)\n"
                ++ "    but I couldn't find it!\n"

        ExpectingTagOpening ->
            "I was looking for the start of an XML tag (a < character) but couldn't find it!\n"
                ++ "Do you close every XML tag in your document?\n"

        ExpectingValidTagName ->
            "I was looking for a valid tag name, but what I found didn't work!\n"
                ++ "Maybe a name got typo'd with a special character?\n"

        ExpectingOnlyAttrTagEnd tagName ->
            "I was looking for the end of an attribute-only tag (like <this kind=\"of\" thing=\"here\"/>)\n"
                ++ "    but I couldn't find the /> at the end!\n"
                ++ "I was looking at a tag I think was named \""
                ++ tagName
                ++ "\" at the time.\n"

        ExpectingOpeningTagClose tagName ->
            "I was looking for the end of an opening tag (like <this>)\n"
                ++ "    but I couldn't find the > at the end!\n"
                ++ "It was a tag I think was named \""
                ++ tagName
                ++ "\"\n"

        ExpectingClosingTagOpening tagName ->
            "I was looking for the end of a closing tag (like </this>)\n"
                ++ "    but I couldn't find the </ at the start!\n"
                ++ "It was supposed to be a closing tag to <"
                ++ tagName
                ++ ">,\n"
                ++ "    something like \"</"
                ++ tagName
                ++ ">\"\n"

        ExpectingClosingTag tagName ->
            "I was looking for a closing tag (like </this>)\n"
                ++ "    but I couldn't find the it!\n"
                ++ "It was supposed to be a closing tag to <"
                ++ tagName
                ++ ">,\n"
                ++ "    something like \"</"
                ++ tagName
                ++ ">\"\n"

        ExpectingClosingTagEnd tagName ->
            "I was looking for the end of a closing tag (like </this>)\n"
                ++ "    but I couldn't find the </ at the start!\n"
                ++ "It was supposed to be a closing tag to <"
                ++ tagName
                ++ ">,\n"
                ++ "    something like \"</"
                ++ tagName
                ++ ">\"\n"

        ExpectingValidAttrName ->
            "I was looking for a valid XML attribute name, but couldn't understand it!\n"
                ++ "I wish I could tell you more, but attributes are pretty hard to understand.\n"
                ++ "If you get this a lot, bug the package maintainer about making me smarter!\n"

        ExpectingAttrEq attrName ->
            "I was looking for the equals sign (the = character) that XML requires\n"
                ++ "    after every attribute name, but I couldn't find it!\n"
                ++ "My guess at the attribute name was \""
                ++ attrName
                ++ "\"\n"


maintainerNote : String
maintainerNote =
    "This should never happen, so let the package maintainer know!"


problemHeader problem =
    let
        headerTitle =
            "EXPECTING "
                ++ (case problem of
                        ExpectingCommentStart ->
                            "COMMENT START"

                        ExpectingCommentEnd ->
                            "COMMENT END"

                        ExpectingDirectiveStart ->
                            "DIRECTIVE START"

                        ExpectingDirectiveEnd ->
                            "DIRECTIVE END"

                        ExpectingEOF ->
                            "END OF FILE"

                        ExpectingQuotationMark _ ->
                            "QUOTATION MARK"

                        ExpectingTagOpening ->
                            "TAG START"

                        ExpectingValidTagName ->
                            "VALID TAG NAME"

                        ExpectingOnlyAttrTagEnd _ ->
                            "ATTRIBUTE TAG ENDING"

                        ExpectingOpeningTagClose _ ->
                            "TAG ENDING"

                        ExpectingClosingTagOpening _ ->
                            "CLOSING TAG START"

                        ExpectingClosingTag _ ->
                            "CLOSING TAG"

                        ExpectingClosingTagEnd _ ->
                            "CLOSING TAG END"

                        ExpectingValidAttrName ->
                            "VALID ATTRIBUTE NAME"

                        ExpectingAttrEq _ ->
                            "EQUALS"
                   )

        dashLengthTarget =
            42 - 4

        followingDashCount =
            dashLengthTarget - String.length headerTitle
    in
    "-- " ++ headerTitle ++ " " ++ String.repeat followingDashCount "-"
