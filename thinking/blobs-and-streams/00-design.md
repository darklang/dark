# Design

Read before starting execution. Commits to choices; no open options.

## Problem

- **Bytes are `List<UInt8>`.** Every byte is a boxed `DUInt8` in a
  persistent list node. A 38 MB file reaches ~49 GB RSS before OOM-kill
  (see [bug-fileread-oom.md](../bug-fileread-oom.md)). Representation
  bug, not a tuning issue.
- **Streams live outside the type system.**
  `BuiltinExecution/Libs/StreamingHttpClient.fs` uses F#-side
  callbacks, materialising each chunk as a `DList` before handing it
  to user code. There's no `Stream` value.
- **Only existing reference Dval is `DDB of name: string`.** No
  precedent for "Dval holding an identity, data elsewhere" besides
  that one case.

## Choice 1 — Blob: hybrid ephemeral/persistent (Option D)

```fsharp
type BlobRef =
  | Ephemeral of uuid                         // VM byte-store
  | Persistent of hash: string * length: int64 // package_blobs

and Dval =
  // ... existing cases ...
  | DBlob of BlobRef
```

- IO builtins mint `Ephemeral`. Bytes live in a per-ExecutionState
  `Map<uuid, byte[]>`.
- Serializing the Dval (commit to val, User DB write, trace capture)
  **promotes** to `Persistent`: hashes bytes, writes to
  `package_blobs`, swaps the ref.
- `DBlob(Persistent(hash, length))` is small (~50 bytes total). Bytes
  are fetched from SQLite on demand by builtins that need them.
- User code never distinguishes Ephemeral vs Persistent at the
  language level.

**Why D.** Most blobs in real programs are short-lived (read a body,
parse it, discard). Persisting them all is wasteful. But some are
worth keeping (static assets, committed `val`s, blobs a User DB row
references). D makes both cases natural.

### Identity: SHA-256

- Dark already uses SHA-256 for package hashes. Second hash family
  would be churn.
- Hardware-accelerated; ~1.5–3 GB/s per core. 10 MB = ~5 ms. Typical
  HTTP bodies are a rounding error.
- Only hashed on *first* promotion. Ephemeral-to-ephemeral copies
  don't hash. Serialize-to-same-serializer twice doesn't re-hash.
- BLAKE3 is faster; defer as an optimization behind a profile.

### Storage — `package_blobs` table

```sql
CREATE TABLE IF NOT EXISTS package_blobs (
  hash TEXT PRIMARY KEY,
  length INTEGER NOT NULL,
  bytes BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);
```

Same SQLite DB as the rest of the tree. Content-addressed — dedup is
automatic. Reuses the PackageManager path pattern (types/values/fns
already have `pt_def BLOB` columns; this is just bulkier content).

### Blobs in User DBs (KV DBs)

User DBs store Dvals as JSON via
`backend/src/LibCloud/DvalReprInternalQueryable.fs`. A `DBlob` inside
a User DB value serializes as a hash reference, not inline bytes:

```json
{ "type": "blob", "hash": "a1b2c3…", "length": 1048576 }
```

Actual bytes stay in `package_blobs`; the User DB row holds the
pointer. Storing into a User DB **forces promotion** if the blob was
ephemeral. Structural queries over blob contents aren't supported —
matches existing KV DB constraints for most structural work.

## Choice 2 — Stream: generic lazy composable (Option E)

```fsharp
type StreamImpl =
  | FromIO of next: (unit -> Ply<option<Dval>>) * elemType: ValueType
  | Mapped of src: StreamImpl * fn: Applicable * elemType: ValueType
  | Filtered of src: StreamImpl * pred: Applicable
  | Take of src: StreamImpl * n: int64
  | Concat of StreamImpl list
  // ...

and Dval =
  // ... existing cases ...
  | DStream of StreamImpl
```

**Lazy by construction.** `Stream.map f s` doesn't iterate — it
builds a `Mapped(s, f, _)` node. Chains of `map |> filter |> take`
cost nothing at build time. Work happens at drain
(`Stream.toList`, `Stream.toBlob`, `Stream.fold`).

**Generic over element type.** `Stream<UInt8>` for bytes,
`Stream<String>` for log lines, `Stream<Event>` for SSE. Parametric
falls out of the shape.

**Why E, not a simpler pull-iterator.** The only historical reason to
avoid Option E was the lambda-cache footgun for user-supplied
transformations. PR 5640 (commit `3ec7b5fce`) landed on `main` and
moved lambda + package-fn instruction caches from VMState onto
ExecutionState, which makes `Stream.map (fun x -> ...) s` safe across
VM boundaries. The blocker is gone; there's no reason to ship a
bytes-only strict iterator first and throw it away later.

### Consumption semantics — single-consumer, locked internally

- `next` guarded by a monitor on an internal lock. Concurrent calls
  serialize. User code sees single-threaded pull semantics; locking
  is not user-surface. No `Stream.lock` / `Stream.acquire`.
- First caller to see `Done` or `Error` marks the stream consumed;
  subsequent `next` returns `Error "consumed"`.
- No teeing. If two consumers needed, drain to Blob first
  (`let b = Stream.toBlob s in Bytes.toStream b |> ...`). Teeing a
  live IO source requires unbounded buffering; not worth it.

### Disposal

- `StreamImpl.FromIO` can hold IDisposable F# resources
  (`HttpResponseMessage`, `FileStream`). Disposal runs via .NET
  finalizer when the stream is GC'd. Explicit `Stream.close` can
  exist as a hint but is not required — users should not have to
  call it in normal code.
- Deterministic disposal is a Phase 3 concern (scope-based lifetime).

### Non-persistable: first Dval with a hard "raises on serialize" rule

- `DStream` serialization raises. Caller sees "cannot store a stream
  — drain to a blob first."
- Places this rule lives:
  - `LibSerialization/Binary/Serializers/RT/Dval.fs` (binary)
  - `LibCloud/DvalReprInternalRoundtrippable.fs` (rt_dval BLOB col)
  - `LibCloud/DvalReprInternalQueryable.fs` (User DB JSON)
  - Trace capture

### Options explicitly rejected

Mentioned inline in `BuiltinExecution/Libs/Stream.fs` as a
`// CLEANUP` for future revisit:

- Channels (Go-style with separate reader/writer ends and explicit
  buffering).
- Actor-mailbox (Erlang-style, with a scheduler).

Neither is needed now; either could be layered on top of Option E
later if a real use-case appears.

## Existing surface to reuse, not recreate

- `packages/darklang/stdlib/bytes.dark` — already a Dark-side module
  (currently 1 function, `hexEncode`). Expand it, don't replace.
- `backend/src/BuiltinExecution/Libs/Bytes.fs` — already exists with
  `bytesHexEncode`. Expand the `fns()` list.
- `backend/src/BuiltinExecution/Libs/Base64.fs` — already handles
  byte list encoding/decoding. Retarget params to `Blob` rather than
  creating a parallel module.
- `backend/src/BuiltinExecution/Libs/Crypto.fs` — similar; takes
  `TList TUInt8` today, becomes `TBlob`.
- `backend/src/BuiltinExecution/Libs/String.fs:353` — returns
  `TList TUInt8`; becomes `TBlob`.

## Call sites taking `List<UInt8>` today

~20 files across `backend/src/` and `packages/darklang/`. All
migrate to `Blob` in Phase 1. List lives in
[20-phase-1.md](./20-phase-1.md) chunk 1.8–1.10.

## Serialization and debug rendering

| Case                         | Binary                         | Debug repr                                    |
| ---------------------------- | ------------------------------ | --------------------------------------------- |
| `DBlob(Ephemeral uuid)`      | Promote; then Persistent tag   | `<blob ephemeral 4.2MB>`                      |
| `DBlob(Persistent(hash, n))` | `tag ++ hash ++ length`        | `<blob 4.2MB sha256:a1b2c3…>`                 |
| `DStream _`                  | **Raises** with clear message  | `<stream of UInt8: fresh|partial|done>`       |

## Impact outside the core runtime

- **F# parser** (`backend/src/LibParser/*.fs`): new type-reference
  cases for `TBlob`, `TStream`.
- **Tree-sitter grammar** (`tree-sitter-darklang/grammar.js`
  line ~984): add `Blob`, `Stream` to `builtin_type` choice. Rebuild
  generated parser.
- **Dark-side parser** (`packages/darklang/languageTools/parser/typeReference.dark`):
  add `Blob`/`Stream` branches alongside `Unit`/`Bool`/`String`.
- **Dark-side pretty-printer** (`packages/darklang/prettyPrinter/programTypes.dark`
  and `runtimeTypes.dark`): add print cases for new types/values.
- **.dark stdlib files**: every caller using `List<UInt8>` in a
  signature or body updates to `Blob` / `Bytes` (phase 1.10 call
  sites list).
- **.dark tests in `backend/testfiles/execution/`**: bytes and
  streaming tests added; existing `file.tests`, `httpclient.tests`
  updated to new shapes.
- **CLI UX**: `val x = Builtin.File.read "..."` Just Works — the user
  sees `DBlob(Persistent(...))` after commit. `view` / `tree`
  render blobs as `<blob NKB sha256:...>`. `debug` same.
- **VS Code extension**: explicitly ignored for now.

## Tracing — promotion on capture

Traces persist past the VM that produced them (trace data in
`trace_data` table). Capturing a `DBlob(Ephemeral _)` into a trace
forces promotion: hash the bytes, write to `package_blobs`, store
the persistent ref in the trace. Without this, the trace would hold
a dangling UUID pointing at a byte-store that no longer exists.

This is the main "promotion happens under the hood" site to flag in
review. Debug-only workflows could fill `package_blobs` with
trace-only content, but:

- Content-addressing dedupes naturally. A handler reading the same
  file on 1000 requests writes one row.
- `trace_data` retention aging can drive blob GC indirectly
  (Phase 3 orphan sweeper).
- No separate `trace_blobs` table needed; one store with one GC
  pass is simpler.

## GC strategy (details live in 40-later.md)

Phase 1 and 2 don't require GC work. Phase 3 introduces:

- **Scope-based ephemeral-blob lifetime** for long-running VMs
  (http-server). ExecutionState gets a scope stack; ephemeral handles
  created inside a scope drop at scope exit.
- **Orphan blob sweeper.** Periodic scan of `package_blobs` rows
  unreferenced by any val/location/trace/User-DB-row.
- **Val persistability guard** (`Dval.isPersistable` at `Inserts.fs`).
  Rejects `DStream`, optionally `DApplicable`, optionally `DDB` —
  covers the pre-existing "lambda in a val" weirdness too.

Until Phase 3, ephemeral handles live as long as the ExecutionState.
Acceptable for CLI; *not* acceptable long-term for http-server. This
is an explicit TODO the phase docs flag at chunk boundaries.
