# Writing docstrings that will appear in the editor

## Tagging your string
To highlight key points a user may be looking for as they skim the text, we are allowing Dark functions' docstrings to be rich text.

### Tags

You can mark your text with tags, which will pretty print upon render. For example `this function takes in <type Option>`.
The word Option will be transformed into `this function takes in <span class="type">Option</span>`.

Tags follow the form of: `<{TYPE} {CONTENT}>`. The tag types are a single word, after which everything else will considered to be content of the tag.

Currently valid tag types are: *param*, *fn*, *var*, *type*, *return*, *err*, *cmd*. If you want to add a new tag type, see guidelines below.
We try to keep the tag types short and simple to make it easy for us to remember and type.

Since most of our tags are rendered as color-coded-text, and more importantly because of the limitations of our Regex parser, **tags cannot contain other tags**

### Code block

To mark a code block, simply wrap the text in `{ }`. 

Code blocks can contain tags, such as `{Just <var value>}`. But a code block cannot contain other code blocks.

## Rendering

How the tags will be styled are defined in [the spec](https://www.notion.so/darklang/Rich-Text-Docstrings-1358553ec49d4ba0b52d04995014152c)

To get a string to render as rich text use PrettyDocs.convert, which takes a `docstring`, and returns a list of DOM elements from the transformed string. If the string just plain-text it returns `[Html.text docstring]`.

## New Tag types

If you really feel the need to add a new tag. It needs to be a single word with only a-z characters, and remember to update \_docs.scss with the styling for your new tag.
