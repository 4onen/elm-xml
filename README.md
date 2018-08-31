## Structured XML decoding for Elm

Since the move to **Elm 0.19**, a number of dependencies I had for my development work on a neat toy project broke. Most of these dependencies were somewhat not-Elm-ish, and I shouldn't have been using them anyway. However, one of these dependencies was [ymtszw/elm-xml-decode](https://github.com/ymtszw/elm-xml-decode/tree/1.0.1). For a number of reasons, I want to decode a bunch of structured XML using Elm, because conversion to another data format is hard. I made this library to help with that.

### What's 'structured' XML?

Structured XML is XML that doesn't look like this:

    <tag>
        I'm some unstructured <b>text</b> with tags in the middle!
        <ul>
            <li>This ul is an unstructured child of tag "tag"</li>
            <li>but each of these list items is a structured--</li>
            whoops!
            <li>Now all these list items are unstructured children of ul!</li>
        </ul>
    </tag>

Instead, it has to look like this:

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

Perfectly structured, as all things should be.

### You didn't use any attributes there.

Oh, right! Sorry. For the moment, I discard any attribute lists in a tag. Trying to parse it has given me a number of backtracking-related headaches and isn't actually required by my use case. I'm open to PRs from more experienced devs, if any would like to contribute!

To be clear, the package can successfully parse XML containing attributes. The limitation is that it simply discards any attributes it comes across.