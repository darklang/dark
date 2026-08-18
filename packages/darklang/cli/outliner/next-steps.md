# Outliner: what's next

The app is a compositor over `Stdlib.Cli.UI` widgets: `Editor` (the tree, a widget), `Picker` (documents,
export formats), `TextField` (renaming, export paths, editing a line), `Confirm` (deletes), `Chrome` (the
frame). `Store` owns the file; `Commands` is the same core without a terminal. See
`notes/composed-cli-design-2026-08-18.md` for the shape and `notes/outliner-review-2026-08-17.md` for what
the rebuild fixed.

Everything the old version of this file asked for is done: the flat node map, multi-document support,
persistence, undo/redo, tasks, search, Home/End and jump-to-parent, word wrap, OPML round-tripping, and an
HStack (`UI.Layout.hstack`, which was already there).

---

## Worth doing next

**Notes under a line.** A line is one string. Outliners of any use let a line carry a paragraph underneath
it that folds independently of its children. `UI.Editor` is already a multiline buffer, so the editing side
exists; the work is in the model (a `note: String` on `NodeContent`), the renderer (fold state per note),
and both exporters.

**Sync.** Documents live in `~/.darklang/outliner.json`, outside the op log, so they don't travel between
instances and have no history. "A note written on the laptop is on the reMarkable within seconds" is the
thing we keep writing down; this is the app that would show it. Blocked on nothing in the outliner itself.

**An event log instead of snapshots.** `notes/app-substrate.md` section 9, step 3 names the outliner for
this. Undo already keeps snapshots, which is the cheap version. The honest version is a per-app `Evt` +
`playback`, with the model as the fold - and the outliner is a good size to try it on, now that its
persistence is versioned and every mutation funnels through one place.

**Side-by-side.** The document list and the editor at once, via `UI.SplitPane`. Cheap now; it was the
"HStack" item on the old list.

**Filter by task state.** `x` cycles a line through todo and done; there's no way to ask for "everything
unfinished". A filter that isn't a text match wants a slightly wider `filter` type.

---

## Smaller things

- Undo is per-editing-session: switching documents drops the stack. Per-document history would be better,
  and is a `Dict<Int, List<Outline>>` away.
- The document picker has no search. Fine at four documents, not at forty.
- Export always writes the whole document. A subtree export ("just this branch") is a natural `e` on a
  selected line.
- `Commands.add` appends to the top level only. `--under <line>` would make it a real capture tool.
- Import always creates a new document; merging into an existing one has no path.
