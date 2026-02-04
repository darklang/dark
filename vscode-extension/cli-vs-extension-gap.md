# CLI vs VS Code Extension — Feature Gap Analysis

## Fully Covered in VS Code

| CLI Command | VS Code Equivalent |
|---|---|
| `status` / `wip` / `changes` | Tree view + `scm.status` command |
| `log` / `history` / `commits` | Tree view (with ancestor commits) + `scm.log` |
| `commit` | `scm.commit` + inline button |
| `discard` / `reset` | `scm.discard` + inline button |
| `show <commitId>` | Click commit in tree / inline button |
| `branch list` | Tree view shows current; switch quick-pick lists all |
| `branch create` | `branch.create` + inline button |
| `branch switch` | `branch.switch` + click branch node |
| `branch rename` | `branch.rename` + context menu |
| `branch delete` | `branch.delete` + context menu |
| `rebase` | `branch.rebase` + context menu |
| `merge` | `branch.merge` + context menu |

## Not Applicable to GUI

| CLI Command | Reason |
|---|---|
| `clear` / `cls` | No terminal to clear |
| `quit` / `exit` | No session to exit |
| `install` / `uninstall` / `update` | VS Code marketplace handles this |

## Missing from VS Code

### Tier 1 — High Value, Reasonable Effort

**`docs`** (aliases: `doc`, `man`)
- CLI has 14 documentation topics: syntax, types, records, enums, pattern-matching, functions, operators, stdlib, cli, errors, http-server, packages, scm, for-ai
- Could render in a WebView panel (HomepagePanel exists as template)
- Content already exists in Dark files
- Effort: Medium

**`eval`** — Evaluate Dark expressions inline
- Example: `eval 1L + 2L`, `eval [1L; 2L] |> List.length`
- Would need an LSP request; backend has `Cli.Run.executeFunction`
- Could use an input box or a dedicated REPL panel
- Effort: Medium

**`search`** — Search packages/types/functions/values
- Supports `--type`, `--fn`, `--val`, `--exact`, `--shallow` flags
- Packages tree exists but has no search/filter
- Could be a quick-pick with filter or a search input in the tree view
- Effort: Medium

**`deps`** (alias: `dependencies`) — Show dependencies and dependents
- F# layer already has `getDependents`/`getDependencies` in Queries.fs
- Could show as a tree view or hover info
- Effort: Medium

**`builtins`** — List all builtin functions grouped by category
- Already implemented in Dark as `Cli.Builtins`
- Could show in a WebView or tree view
- Effort: Medium

**`view`** — View details of a module/type/value/function
- Complements the tree view with drill-down into definitions
- Effort: Medium

### Tier 2 — Important but Harder

**`fn` / `type` / `val`** — Inline code creation
- CLI: `fn myFunc (x: Int64): Int64 = x * 2L`
- Needs parser integration for creation from command palette
- Effort: Hard

**`run @function`** — Run arbitrary functions by reference
- Extension can run scripts but not arbitrary functions
- Effort: Medium-Hard

**`scripts`** — Script management (list, view, add, edit, delete, run)
- Full CRUD lifecycle for stored Dark scripts
- Effort: Medium-Hard

**`find-values`** — Find all values of a given type
- Type-based discovery query
- Effort: Medium

**`config`** — Manage CLI/extension configuration
- Keys: sync.default_instance, sync.interval_seconds, sync.auto_start
- Could integrate with VS Code settings
- Effort: Medium

### Tier 3 — Lower Priority

| Feature | Notes |
|---|---|
| `back` | Navigation history in tree view |
| `help` / `commands` | Command reference display |
| `version` / `install-status` | Info display |
| `http-server` | Server lifecycle management — hard |
| `nav` / `ls` / `tree` | Partially covered by packages tree view |

## Recommended Priority Order

1. **`docs`** — Easiest high-impact win. Content exists, WebView infrastructure exists.
2. **`search`** — Major usability improvement for package discovery.
3. **`eval`** — Core developer workflow for testing expressions.
4. **`deps`** — Backend support exists, just needs UI.
5. **`builtins`** / **`view`** — Reference material in the editor.
6. **`run @function`** — Enable function testing without CLI.
7. **`fn` / `type` / `val`** — Inline code creation (biggest gap, hardest to implement).
