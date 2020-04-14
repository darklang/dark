# Writing docstrings that will appear in the editor

## Tagging your string
To highlight key points a user may be looking for as they skim the text, we are allowing Dark functions' docstrings to be rich text.

### Tags

You can mark your text with tags, which will pretty print upon render. For example `this function takes in <type Option>`.
The word Option will be transformed into `this function takes in <span class="type">Option</span>`.

Tags follow the form of: `<TYPE CONTENT>`. The tag types are a single word, after which everything else will considered to be content of the tag.

Currently valid tag types are:

| Tag     | Example                     |
|---------|-----------------------------|
| param   | `<param input>`             |
| fn      | `<fn String::split>`        |
| var     | `<var val>`                 |
| type    | `<type String>`             |
| return  | `<return Returns>`          |
| err     | `<err Type Error>`          |
| cmd     | `<cmd take-off-error-rail>` |

If you want to add a new tag type, see guidelines below.
We try to keep the tag types short and simple to make it easy for us to remember and type.

Since most of our tags are rendered as color-coded-text, and more importantly because of the limitations of our Regex parser, **tags cannot contain other tags**

### Code block

To mark a code block, simply wrap the text in `{{ }}`. 

Code blocks can contain tags, such as `{{Just <var value>}}`. But a code block cannot contain other code blocks.

### Validating Syntax

Once you updated your function's description save the file and re-compile. Run the integration test **function_docstrings_are_valid** to verify it passes the syntax checker. `./integration-tests/run.sh --pattern=function_docstrings_are_valid`

Even though the syntax checker catches some typos and mistakes. It's always best to check it visually on the client to make sure it renders exactly how you want it to look.


## Rendering

How the tags will be styled are defined in [the spec](https://www.notion.so/darklang/Rich-Text-Docstrings-1358553ec49d4ba0b52d04995014152c)

To get a string to render as rich text use PrettyDocs.convert, which takes a `docstring`, and returns a list of DOM elements from the transformed string. If the string just plain-text it returns `[Html.text docstring]`.

## New Tag types

If you really feel the need to add a new tag. It needs to be a single word with only a-z characters, and remember to:
1. Update client/styles/_docs.scss with the styling for your new tag
2. In client/src/ViewUtils.md, in the `PrettyDocs` module there's a `validTags` definition list. Add your new tag to that list.