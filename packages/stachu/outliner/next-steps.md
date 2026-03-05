# Outliner: Next Steps

## Done

These items from the original plan have been implemented:

- **Flat model refactor** — `NodeContent` + `TreeStructure` dicts instead of recursive `Node`. All tree ops are now simple list manipulations on sibling ID lists.
- **Multi-document support** — `Document` wrapper with title, `State` with doc list, `activeDocId`, `nextDocId`.
- **Document picker** — `DocPicker` mode with up/down/enter/n/d/r, plus `DocRenaming` mode for inline rename.
- **Persistence** — JSON file at `~/.darklang/outliner.json`, auto-saves on every mutation.
- **Import/export** — Markdown and OPML export per document, Markdown import.
- **Two default documents** — "My Notes" (getting started guide) and "Tasks" (sample todo structure).
- **30 unit tests** covering all core operations.


## Next: Features

### Undo/Redo
- Store a stack of state snapshots (or outline snapshots per document)
- `u` to undo, `ctrl+r` or `U` to redo in navigate mode
- Cap the stack at some reasonable depth (20-50)
- This would add a lot of day-to-day confidence — currently there's no way to recover from accidental deletes or moves

### Checkboxes / Todo Items
- Node type: plain text vs todo (unchecked / checked)
- Toggle with `x` in navigate mode
- Render: `[ ]` / `[x]` prefix instead of bullet
- Track completion counts on parent nodes ("3/5 done")
- Could filter to show only incomplete items

### Search / Filter
- `f` in navigate mode to enter search
- Filter the visible tree to matching nodes (plus their ancestors for context)
- Highlight matches
- Enter to jump to selected match, Escape to clear filter

### Bulk Operations
- Shift+Up/Down to extend selection
- Bulk indent/outdent/delete/move on selection
- Visual indicator for selected range

### Navigation
- Home/End to jump to first/last visible item
- `gg` / `G` vim-style (if we want to go that route)
- Jump to parent with `p` or left arrow at depth 0


## Next: Data & Persistence

### OPML Import
- Round-trip: export to OPML and import back
- Currently only markdown import is implemented

### Export All Documents
- Single JSON or zip file with all docs
- Or a directory of markdown/OPML files, one per doc

### Backup / Versioning
- Keep last N versions of the JSON file (outliner.json.1, .2, etc.)
- Or timestamp-based backups on save

### One File Per Document
- `~/.darklang/outliner/<title>.json` instead of single monolithic file
- Better for version control, sharing individual docs
- More filesystem management though


## Next: Rendering & UX

### Word Wrap
- Long node text currently extends off screen
- Wrap at terminal width, respecting indentation

### Tree Drawing Characters
- Option to use `├──` / `└──` instead of bullet characters
- Makes the hierarchy more visually obvious

### Color Themes
- Currently hardcoded ANSI colors
- Allow theme selection (dark, light, minimal, colorful)

### Node Types / Rich Content
- Headings (bold, larger conceptual weight)
- Notes/annotations (dimmed, below the node text)
- Links (clickable in supporting terminals)
- Tags (`#tag` syntax, colored inline)


## Next: Architecture / Extensibility

The outliner is structured as three files:
- **core.dark** — data model, types, pure functions for tree ops
- **render.dark** — terminal rendering
- **keys.dark** — input handling, CLI integration

This separation is good but everything is hardcoded to the outliner. There's an opportunity to extract reusable TUI infrastructure.

### Extract Shared UI Components

These are currently in render.dark but could live in `Darklang.Cli.TuiHelpers`:

1. **Title bar** — colored background with text + mode indicator
2. **Help bar** — bottom bar with key hints, full-width background
3. **Scrollable list** — viewport windowing with scroll offset calculation
4. **Selection highlight** — applying selection color to current item
5. **Editable text field** — inline cursor rendering (used in node editing and doc renaming)
6. **Modal picker** — list selection with up/down/enter/escape (the doc picker pattern)

### Standardize the App Integration Pattern

Each interactive CLI app should provide:
- `execute` — enter alt screen, load state, set page
- `display` — render current state to terminal
- `handleKey` — process input, return updated app state
- `help` — print usage info
- `complete` — tab completion

### Share the Persistence Pattern

A helper that takes a serialize/deserialize pair and a filename, providing:
- `save : 'state -> Unit`
- `load : Unit -> Option<'state>`
- Auto-creates the data directory

### Darklang-Specific Constraints

- No higher-kinded types or traits — can't do a generic `AppConfig<'state>` with constraints
- No dynamic dispatch — everything is statically resolved
- The "framework" would be conventions + helper functions, not true inversion of control
- Each app in its own package namespace, shared pieces in `Darklang.Cli.TuiHelpers`

### Validate with a Second App

Build something small (kanban board? simple notes? bookmark manager?) to prove the pattern works before over-abstracting. The outliner is the reference implementation.

### What NOT to Abstract

- Tree-specific operations (indent, outdent, collapse) — domain logic
- Mode definitions — each app has different modes
- Key bindings — different apps need different keys

The goal: "I want to build a new interactive CLI tool. I reuse the UI chrome and persistence plumbing, and just define my state, renderer, and key handler."
