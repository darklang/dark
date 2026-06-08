# Outliner: Next Steps

## Architecture

The outliner is composed of reusable components following a nested TEA pattern:

- **core.dark** — shared types (Outline, Document) and pure outline operations
- **text-editor.dark** — reusable inline text editing (cursor, insert, backspace)
- **list-picker.dark** — reusable list selection (up/down/enter/escape)
- **outline-editor.dark** — tree navigation/editing, delegates to TextEditor
- **main.dark** — compositor: Screen enum, document management, persistence, rendering
- **app.dark** — CLI boundary (SubApp wrapping)
- **export.dark** / **markdown.dark** — import/export

Each component has its own State and Result types. The compositor holds the
active child's state in a Screen enum and delegates keys/rendering to it.

Layout negotiation (`Darklang.Cli.UI.Layout`) handles space distribution:
components declare size requests, the parent distributes rows proportionally
via `vstack`, and each child renders into its assigned Region.

See `read-me/outliner-app-architecture.md` for the full design.


## Next: Features

### Undo/Redo
- Store a stack of outline snapshots per document
- `u` to undo, `ctrl+r` or `U` to redo in navigate mode
- Cap the stack at some reasonable depth (20-50)

### Checkboxes / Todo Items
- Node type: plain text vs todo (unchecked / checked)
- Toggle with `x` in navigate mode
- Render: `[ ]` / `[x]` prefix instead of bullet

### Search / Filter
- `f` in navigate mode to enter search
- Filter the visible tree to matching nodes (plus their ancestors for context)

### Navigation
- Home/End to jump to first/last visible item
- Jump to parent with left arrow at depth 0


## Next: Data & Persistence

### OPML Import
- Round-trip: export to OPML and import back

### One File Per Document
- `~/.darklang/outliner/<title>.json` instead of single monolithic file


## Next: Rendering & UX

### Word Wrap
- Long node text currently extends off screen

### Tree Drawing Characters
- Option to use `├──` / `└──` instead of bullet characters

### Node Types / Rich Content
- Headings, notes/annotations, links, tags


## Next: Layout System (`Darklang.Cli.UI.Layout`)

### HStack
- Horizontal stacking for side-by-side views (e.g. doc picker + editor)
- Column distribution analogous to row distribution in vstack

### View Data
- Components return view data instead of printing directly
- Enables testing, composition, and split views
- See architecture doc "Later: multi-view composition" section
