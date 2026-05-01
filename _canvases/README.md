# `_canvases/` — legacy canvas snapshots, not loaded

These `.dark` files are restored copies of `canvases/dark-editor/main.dark`
and `canvases/dark-packages/main.dark` from before phase 9, for
reference + future porting.

**They are not loaded by anything.** The disk → SQL canvas-loading
flow (`LocalExec.Canvas.loadFromDisk` and the `reload-canvases` /
`reloadCanvas` calls inside `reloadPackages`) was deleted in phase 9
of `notes/http-cli-parity-plan.md`.

**TODO: port to Dark properly.**

The two files have different blockers:

- `dark-packages.dark` (355 LOC): has **no** UserDB references. Could be
  ported now — define each handler as a Dark fn under e.g.
  `Darklang.Templates.DarkPackages.*` and assemble a router as a
  package value. Once ported, this file can be deleted.

- `dark-editor.dark` (136 LOC): uses `Stdlib.DB.{getAll, set, delete, keys}`
  with a `PinnedItemsDB`. Blocked on phase 17's UserDB de-canvas-scoping
  work — once UserDBs no longer require an enclosing canvas, this can
  be ported similarly.

When both are ported (or otherwise replaced), delete `_canvases/`
entirely.
