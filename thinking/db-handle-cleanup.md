---
status: tangential cleanup, not scoped to blobs/streams
---

# DDB / TDB cleanup

Extracted from [blobs-and-streams.md](./blobs-and-streams.md) because the
work is independent and deserves its own pass.

## Current state (verified against main)

- `DDB of name: string` — `RuntimeTypes.fs:609`. The name is a runtime
  lookup key into `exeState.program.dbs`. No type info on the handle.
  Comment at `Interpreter.fs:272` flags this as wrong:
  > "References to DBs and Secrets should be resolved at parse-time,
  > not runtime."
- `TDB of TypeReference` — `ProgramTypes.fs:342`. Parsed but dormant.
  You can write the type; nothing consumes a TDB-typed value.
  `DvalReprInternalQueryable.fs:337` and `Json.fs:672` both explicitly
  error on TDB.
- User-facing DB creation: doesn't exist. `RuntimeTypes.fs:1154`
  defines `module DB = { tlid, name, ... }` but no constructor is
  exposed. DBs are canvas/program artifacts defined outside the value
  system.

## What the shape should probably be

- **`DDB of id: DBID * elemType: ValueType`** — stable UUID, not a
  name. Name lookup happens at parse time; runtime carries the
  resolved identity. Same shape as content-addressed references
  elsewhere in Dark.
- **`TDB of TypeReference`** — already takes the row type, so the
  "generic" part is structurally there. Leave alone unless we find a
  reason to change.
- **Val persistability guards** — today there are none.
  `val x = (fun y -> y)` and `val x = someDB` both parse, typecheck,
  serialize, and round-trip. Adding a `Dval.isPersistable` check at
  `LibPackageManager/Inserts.fs` would cover `DApplicable`, `DDB`,
  and any future non-persistable Dvals (streams) in one place.

## Why this is worth doing

Three payoffs:

1. **Precedent for reference-shaped Dvals.** Blobs (and potentially
   other future "handle" types — sockets, processes, workers) all
   follow the same pattern: a stable ID inside a Dval, content lives
   elsewhere. Fixing DDB sets the pattern cleanly.
2. **Refactor-safety.** Renaming a DB today silently breaks every
   existing `DDB "oldname"` reference. IDs make rename a metadata
   change, not a data-integrity event.
3. **Parse-time resolution.** The TODO comment has been sitting there
   for a while. Resolving DB references at parse time matches how
   every other name in Dark is resolved.

## What it involves

- Change `DDB of string` to `DDB of DBID * ValueType` in
  `RuntimeTypes.fs`.
- Update the creation site in `Interpreter.fs:280` (variable
  resolution falling through to DB lookup) — either move resolution to
  the parser, or keep the fallback but stash the ID.
- Update `DB.fs` builtins (~50 consumers of `DDB dbname`) to take
  `DDB(dbid, _)`.
- Update serializers (`RT/Dval.fs` tag `23uy`) — writes ID + ValueType
  instead of name.
- Update error sites that mention DB names — need to look up name from
  ID for error messages.
- Migration concerns: if any existing serialized DDBs live in `rt_dval`
  BLOBs, we need a one-shot conversion. Probably none in practice
  (DDBs are ephemeral; they shouldn't be in `rt_dval` at rest), but
  worth grep-confirming.

## Estimated scope

~200 LOC across ~8 files. Has its own test matrix:
- DB rename preserves DDB references.
- Serializer roundtrip.
- Parser resolves `myDbName` in a value context to `DDB id` at parse
  time.

## Recommendation

Ship after the Blob work lands, reusing the reference-Dval pattern
established there. Keep them unbundled — each has its own risk surface.
