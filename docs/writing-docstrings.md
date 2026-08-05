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
| err     | `<err Type Error>`          |
| cmd     | `<cmd take-off-error-rail>` |

If you want to add a new tag type, see guidelines below.
We try to keep the tag types short and simple to make it easy for us to remember and type.

Since most of our tags are rendered as color-coded-text, and more importantly because of the limitations of our Regex parser, **tags cannot contain other tags**

### Code block

To mark a code block, simply wrap the text in `{{ }}`. 

Code blocks can contain tags, such as `{{Some <var value>}}`. But a code block cannot contain other code blocks.

### Link tag

If you want to include a link, use the same delimiters as markdown. `[LINK NAME](LINK URL)`.

Link tags cannot contain code blocks or other tags.

## Rendering

Nothing renders these at the moment. The renderer was `PrettyDocs.convert` in the
old client, which turned a docstring into DOM elements; that client is gone and
nothing has replaced it. The styling spec lived in Notion, which is also gone.

The syntax above is still what docstrings in `packages/` are written in, so it's
worth following. Just don't expect to see it rendered yet.

## New tag types

A tag has to be a single word of a-z characters. Adding one used to mean editing
`client/styles/_docs.scss` and the `validTags` list in `client/src/ViewUtils.md`;
with no renderer there is currently nowhere to register it.