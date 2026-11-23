# Editors

`dark lsp` is Dark's language server. An editor starts it and talks to it over stdin and
stdout; it stops when the editor closes stdin. It logs to stderr, which an editor shows as the
server's output, and it writes no files of its own.

The server is written in Dark (`Darklang.LanguageTools.Lsp`).

## VS Code

The extension is in `vscode-extension/`. It starts `dark lsp` (or the `dark` that the
`darklang.path` setting names) and stops it with VS Code.

```
cd vscode-extension
npm ci
npm run build      # type-checks and bundles to out/extension.js
npm run package    # darklang-<version>.vsix
```

"Run the VS Code extension" in `.vscode/launch.json` opens a development window with it.

## Other editors

Any editor with a language client can start `dark lsp`.

Neovim (0.11+):

```lua
vim.filetype.add({ extension = { dark = "darklang" } })
vim.lsp.config("dark", { cmd = { "dark", "lsp" }, filetypes = { "darklang" }, root_markers = { ".git" } })
vim.lsp.enable("dark")
```

Helix (`languages.toml`):

```toml
[language-server.dark]
command = "dark"
args = ["lsp"]

[[language]]
name = "darklang"
scope = "source.darklang"
file-types = ["dark"]
comment-token = "//"
indent = { tab-width = 2, unit = "  " }
language-servers = ["dark"]
```

Store documents (below) need an editor that can read a custom URI scheme through the server, so
outside VS Code, go-to-definition into the store lands on a `darkfs:` URI the editor may not
open.

## Store documents

Package items open as documents read from the store: `darkfs:/Darklang/Stdlib/List/map.fn.dark`
for one item, `darkfs:/Darklang/Stdlib/List.dark` for every declaration directly in a module.
`dark/fs/read` serves their text as `dark view --raw` prints it, and "Darklang: Open the Package
Store as a Folder" mounts the tree in the explorer. Saving one applies it the way
`dark edit` (an item) or `dark module` (a module) does, with the same checks; a save they refuse
fails with their message. The Packages view lists the tree (`dark/tree`), and the Changes view and
the status bar show the branch and what the draft changes (`dark/status`).

## What it does

| Feature | Where it comes from |
|---|---|
| Diagnostics | the parser, the name resolver and the at-rest type checker, once typing pauses |
| Hover | the item's signature, its doc comment and its effects (`dark effects`) |
| Definition, references, call hierarchy, implementations | the store and its dependency edges (`dark deps usedby`); locals by scope |
| Outline, folding, expand selection, highlights | the parsed document |
| Completion, signature help | module members with their signatures, locals in scope, enum cases |
| Semantic tokens, formatting | `LanguageTools.SemanticTokens`; `dark format` |
| Inlay hints | the type the checker inferred for each `let` and lambda parameter; live values: each call's value in the function's newest recorded run, replayed against the code as it is now (`docs/live.md`) |
| Code lenses | run (for a function without parameters), its runs and their timing, its effects, its tests |
| Quick fixes | qualify an unresolved name; add the case a match misses |
| Rename | a local in its document; a package item store-wide (`dark rename`) |
| Links | `<fn X>` and friends in doc comments |

The server follows the store. When another process lands an op, the open documents it touches are
analysed again, the editor is asked to refresh its hints, lenses and tokens, and
`dark/storeChanged` tells it which names moved, so it reloads the store documents it shows. A new
recorded run or a `dark switch` in a terminal reaches it the same way, within two seconds.

Read-only requests are answered beside the loop that reads stdin, so a slow one holds up neither
typing nor the others. `$/cancelRequest` stops one; its reply says it was cancelled.

`dark lsp --live` answers each read-only request with the request handler the store has now,
staying on the last version that type-checks. Working on the server in a dev build, an edit
shows in the editor after `scripts/dev/build`, without a restart. The store's version runs under
the instance policy, like any stored code; what the policy refuses, the built-in version answers.

## Tests

`scripts/testing/gates lsp` drives the server over stdio the way an editor does
(`scripts/testing/_lsp-gate.py`). The pure parts have package tests in
`packages/darklang/tests/languageTools/lsp.dark`. `scripts/testing/gates vscode-extension` builds
and packs the extension.
