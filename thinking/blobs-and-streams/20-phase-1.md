# Phase 1 — Blob (Option D)

End state: `fileRead` no longer OOMs. `Bytes` stdlib is the real
surface for binary data. `val`s, User DBs, and traces can all carry
blobs. Streams not yet introduced (Phase 2).

## Scope principles

- `DBlob` carries a `BlobRef = Ephemeral of uuid | Persistent of hash * length`.
- Ephemeral handles live in a per-ExecutionState map, GC'd with the
  ExecutionState (scope work is Phase 3).
- Promotion to Persistent happens transparently on serialize: writes
  bytes into `package_blobs`, swaps the ref.
- SHA-256 for identity. Hashed only on first promotion.
- Pattern matching on blobs is opaque — go through builtins.

## Existing surface to **modify**, not replace

- `backend/src/BuiltinExecution/Libs/Bytes.fs` — already exists with
  `bytesHexEncode`. Expand the `fns()` list.
- `packages/darklang/stdlib/bytes.dark` — already a one-function
  module. Extend.
- `backend/src/BuiltinExecution/Libs/Base64.fs`,
  `backend/src/BuiltinExecution/Libs/Crypto.fs`,
  `backend/src/BuiltinExecution/Libs/String.fs` (line 353 returns
  `TList TUInt8`), `backend/src/BuiltinCli/Libs/File.fs`,
  `backend/src/BuiltinCli/Libs/Posix.fs` — all migrate signatures.

## Call sites for `List<UInt8>` migration (chunk 1.10)

Identified by `grep -rn "TList TUInt8\|List<UInt8>" backend/src/ packages/darklang/`:

- `backend/src/BuiltinExecution/Libs/Crypto.fs`
- `backend/src/BuiltinExecution/Libs/String.fs`
- `backend/src/BuiltinExecution/Libs/Base64.fs`
- `backend/src/BuiltinExecution/Libs/Bytes.fs`
- `backend/src/BuiltinExecution/Libs/HttpClient.fs`
- `backend/src/BuiltinExecution/Libs/StreamingHttpClient.fs` (touched
  here; fully replaced in Phase 2)
- `backend/src/BuiltinCli/Libs/File.fs`
- `backend/src/BuiltinCli/Libs/Posix.fs`
- `packages/darklang/wip/ai/openai/audio.dark`
- `packages/darklang/stdlib/base64.dark`
- `packages/darklang/stdlib/streamingHttpClient.dark`
- `packages/darklang/stdlib/uint8.dark`
- `packages/darklang/stdlib/string.dark`
- `packages/darklang/stdlib/httpclient.dark`
- `packages/darklang/stdlib/crypto.dark`
- `packages/darklang/stdlib/bytes.dark`
- `packages/darklang/stdlib/cli/file.dark`
- `packages/darklang/stdlib/cli/posix.dark`
- `packages/darklang/stdlib/cli/fileSystem.dark`
- `packages/darklang/stdlib/http.dark`

Keep `List<UInt8>` as an escape hatch via `Bytes.toList` /
`Bytes.fromList` — signatures migrate to `Blob` throughout.

## Manual testing for Phase 1

After 1.14:
- `./scripts/run-cli eval 'Builtin.File.read "/some/large/file"'` and
  confirm it does not OOM (use the 38 MB bug repro if available).
- `val myFile = Builtin.File.read "..."; commit "test"`; run `view`
  on the val and confirm the rendering is
  `<blob N sha256:...>`, not a huge byte list.
- Store a blob into a User DB row and re-read — confirm
  roundtrip through the JSON path.

## Chunks

### 1.1 — wire TBlob and KTBlob through PT/RT/ValueType

Files:
- `backend/src/LibExecution/ProgramTypes.fs` (~line 343): add `TBlob`
- `backend/src/LibExecution/RuntimeTypes.fs` (~line 221): add `TBlob`;
  `KTBlob` in KnownType (~line 183); handling in `Dval.toValueType`
- `backend/src/LibExecution/ProgramTypesToRuntimeTypes.fs`: route
  `TBlob`
- `backend/src/LibExecution/TypeChecker.fs`: unify rules for `TBlob`
- `backend/src/LibExecution/Interpreter.fs`: match-arm coverage

No Dval yet (that's 1.2). No serialization yet (that's 1.3).

**Done when:** backend builds and all existing tests still pass. New
type is visible but inert.

**Tests:** lean on existing build + test pass; add nothing yet.

**Commit:** `blob: wire TBlob and KTBlob through pt/rt/valuetype`

### 1.2 — add DBlob with BlobRef ephemeral/persistent variants

Files:
- `backend/src/LibExecution/RuntimeTypes.fs`: add `BlobRef` DU and
  `DBlob of BlobRef` case. Add the ephemeral byte-store
  (`Map<uuid, byte[]>`) to `ExecutionState` (not VMState — matches
  PR 5640's placement of lambda caches).
- `backend/src/LibExecution/Dval.fs`: helpers for creating ephemeral
  blobs (`newEphemeralBlob : ExecutionState -> byte[] -> Dval`);
  reading bytes (`readBlobBytes : ExecutionState -> BlobRef -> Ply<byte[]>`).
- `backend/src/LibExecution/Execution.fs`: initialise the ephemeral
  store when ExecutionState is built.

**Done when:** backend builds. No Dval creation paths yet use
`DBlob`, but F# unit tests can construct one and read bytes back.

**Tests:**
- F# (`backend/tests/Tests/Dval.Tests.fs`): create an ephemeral blob
  from a `byte[]`, read back, compare; create two ephemerals, read
  both.

**Commit:** `blob: add DBlob dval with BlobRef ephemeral/persistent`

### 1.3 — add binary serializers for TBlob/KTBlob/DBlob

Files:
- `backend/src/LibSerialization/Binary/Serializers/PT/TypeReference.fs`:
  new tag for `TBlob`
- `backend/src/LibSerialization/Binary/Serializers/RT/TypeReference.fs`:
  mirror
- `backend/src/LibSerialization/Binary/Serializers/RT/ValueType.fs`:
  new tag for `KTBlob`
- `backend/src/LibSerialization/Binary/Serializers/RT/Dval.fs`:
  `DBlob` read/write. `Ephemeral` in the writer **triggers
  promotion** — but promotion path lands in 1.6; for this chunk,
  writer raises on `Ephemeral` with a `TODO: promotion (chunk 1.6)`
  message. `Persistent` writes `tag ++ hash ++ length`.

**Done when:** F# roundtrip test round-trips a `DBlob(Persistent(h,n))`
through write/read and gets the same value.

**Tests:**
- F#: roundtrip test in `Dval.Tests.fs`.

**Commit:** `blob: add binary serializers for tblob/ktblob/dblob`

### 1.4 — add PT↔Dark and RT↔Dark bridges for blob types

Files:
- `backend/src/LibExecution/ProgramTypesToDarkTypes.fs` (~1900 LOC —
  biggest risk): add `TBlob` case to the `TypeReference` toDT/fromDT.
- `backend/src/LibExecution/RuntimeTypesToDarkTypes.fs`: mirror, plus
  `DBlob` handling (`DBlob(Persistent(h, n))` → Dark record;
  `DBlob(Ephemeral _)` → same shape after forcing promotion — but
  promotion path is 1.6, so until then this path raises).

**Done when:** LSP / reflection paths that call these bridges
continue to work. Add one F# test that round-trips `TBlob` through
PT↔Dark.

**Tests:**
- F# bridge roundtrip tests. Existing `ProgramTypesToDarkTypes.Tests.fs`
  style if it exists; otherwise inline in `Dval.Tests.fs`.

**Commit:** `blob: add pt↔dark and rt↔dark bridges for blob`

### 1.5 — add package_blobs migration and PM blobs lookup

Files:
- `backend/migrations/20260423_000000_package_blobs.sql` (new):
  ```sql
  CREATE TABLE IF NOT EXISTS package_blobs (
    hash TEXT PRIMARY KEY,
    length INTEGER NOT NULL,
    bytes BLOB NOT NULL,
    created_at TEXT NOT NULL DEFAULT (datetime('now'))
  );
  ```
- `backend/src/LibPackageManager/PackageManager.fs`: add
  `blobs : hash -> Ply<option<byte[]>>` lookup alongside
  types/values/functions. Takes branchId the same way.
- `backend/src/LibPackageManager/Inserts.fs`: add `insertBlob : hash -> byte[] -> Ply<unit>` with `INSERT OR IGNORE` semantics.

**Done when:** backend builds. F# test inserts a blob, reads it back
by hash, gets the right bytes.

**Tests:**
- F# (`backend/tests/Tests/PackageManager.Tests.fs` or new
  `PackageBlobs.Tests.fs`): insert-then-lookup, insert-dedup (same
  hash twice is a no-op).

**Commit:** `blob: add package_blobs migration and pm blobs lookup`

### 1.6 — implement ephemeral→persistent promotion on serialize

Files:
- `backend/src/LibSerialization/Binary/Serializers/RT/Dval.fs`: the
  `Ephemeral` case in writer now hashes bytes, calls
  `insertBlob`, swaps the ref in the writer's output. This means
  the writer needs ExecutionState access — plumb it through if not
  already present.
- `backend/src/LibCloud/DvalReprInternalRoundtrippable.fs`: mirror
  promotion on the rt_dval BLOB column path.
- `backend/src/LibCloud/DvalReprInternalQueryable.fs`: mirror for User
  DB JSON (writes `{"type":"blob","hash":"...","length":N}`).

**Done when:** F# test creates a `DBlob(Ephemeral uuid)`, serializes
it via rt_dval round-trip, asserts the round-tripped Dval is
`DBlob(Persistent(..., ...))` and `package_blobs` has the row.

**Tests:**
- F# promotion roundtrip test.
- Test that serializing the same ephemeral twice writes to
  `package_blobs` once (dedup by hash).

**TODO:** traces capture path should also call the promotion path.
Phase 3 formalises; here, add a `// TODO blob-promote-on-trace
(chunk L.1)` comment at the capture site.

**Commit:** `blob: promote ephemeral to persistent on serialize`

### 1.7 — expand Bytes builtin module to full API

File: `backend/src/BuiltinExecution/Libs/Bytes.fs`

Add to `fns()`:
- `length : Blob -> Int64`
- `concat : List<Blob> -> Blob`
- `slice : Blob -> Int64 -> Int64 -> Blob`
- `toHex : Blob -> String` (rename or wrap existing `bytesHexEncode`)
- `fromHex : String -> Result<Blob, String>`
- `toBase64 : Blob -> String`
- `fromBase64 : String -> Result<Blob, String>`
- `toString : Blob -> Result<String, String>` (UTF-8)
- `fromString : String -> Blob`
- `toList : Blob -> List<UInt8>` (escape hatch)
- `fromList : List<UInt8> -> Blob` (escape hatch)

Mark `bytesHexEncode 0` as deprecated via the deprecation system if
it had external callers; otherwise remove.

`toBase64` / `fromBase64` can delegate to `Base64.fs` with adjusted
signatures; don't duplicate.

**Done when:** `Builtin.Bytes.length (Builtin.Bytes.fromString "hi") = 2L` evaluates.

**Tests:**
- .dark: `backend/testfiles/execution/stdlib/bytes.tests` (new) —
  one case per builtin.

**Commit:** `blob: expand bytes builtin module to full api`

### 1.8 — migrate fileRead/fileWrite to Blob

Files:
- `backend/src/BuiltinCli/Libs/File.fs`: `fileRead` returns
  `Result<Blob, String>`; `fileWrite` takes `Blob`. Use
  `Dval.newEphemeralBlob`.
- `backend/src/BuiltinCli/Libs/Posix.fs`: mirror any byte-list reads.

**Done when:** fileRead on a 10 MB file finishes under the memory
bound test in 1.12.

**Tests:**
- F# memory test (added in 1.12).
- .dark: `backend/testfiles/execution/cli/file.tests` updated shape.

**Commit:** `blob: migrate fileRead and fileWrite to blob`

### 1.9 — migrate Crypto and Base64 to Blob

Files:
- `backend/src/BuiltinExecution/Libs/Crypto.fs`: parameters/returns
  switch from `TList TUInt8` to `TBlob`.
- `backend/src/BuiltinExecution/Libs/Base64.fs`: same.
- `backend/src/BuiltinExecution/Libs/String.fs` line 353 (returns
  `TList TUInt8`): switch to `TBlob`.

**Done when:** existing crypto/base64/string tests updated to new
shape, still pass.

**Tests:** update existing .dark tests in
`backend/testfiles/execution/stdlib/` for these modules.

**Commit:** `blob: migrate crypto base64 and string byte APIs to blob`

### 1.10 — update Dark-side stdlib and dependent .dark files

Files (list from the top of this doc):
- `packages/darklang/stdlib/bytes.dark`
- `packages/darklang/stdlib/base64.dark`
- `packages/darklang/stdlib/crypto.dark`
- `packages/darklang/stdlib/uint8.dark`
- `packages/darklang/stdlib/string.dark`
- `packages/darklang/stdlib/httpclient.dark` (partial; full Phase 2)
- `packages/darklang/stdlib/streamingHttpClient.dark` (partial; full
  Phase 2)
- `packages/darklang/stdlib/http.dark`
- `packages/darklang/stdlib/cli/file.dark`
- `packages/darklang/stdlib/cli/posix.dark`
- `packages/darklang/stdlib/cli/fileSystem.dark`
- `packages/darklang/wip/ai/openai/audio.dark`

Change `List<UInt8>` to `Blob` in signatures and bodies; keep
`Bytes.toList` / `fromList` as escape hatches for code not worth
migrating yet.

**Done when:** `./scripts/run-cli docs for-ai` still loads cleanly,
and .dark tests updated to new shapes pass.

**Tests:** existing .dark tests across these modules.

**Commit:** `blob: migrate dark-side stdlib to blob`

### 1.11 — update tree-sitter grammar + Dark-side parser/pretty-printer

Files:
- `tree-sitter-darklang/grammar.js` (~line 1001 `builtin_type`): add
  `/Blob/` to the choice. Rebuild generated parser
  (`tree-sitter generate`); commit `src/` regenerated files.
- `packages/darklang/languageTools/parser/typeReference.dark`: add
  `"Blob" -> TBuiltin.TBlob` branch alongside existing primitives.
- `packages/darklang/prettyPrinter/programTypes.dark`: add
  `| TBlob -> "Blob"` to the type-reference printer.
- `packages/darklang/prettyPrinter/runtimeTypes.dark`: same for RT.
- Dval printer: render `DBlob(Ephemeral _)` as
  `<blob ephemeral <size>>`, `DBlob(Persistent(h, n))` as
  `<blob <size> sha256:<short>>`.

**Done when:** `val x: Blob = Builtin.File.read "..."` parses end-to-end;
a committed val re-prints as `Blob`.

**Tests:** existing parser/pretty-printer tests; add one .dark test
that round-trips a `Blob`-typed signature.

**Commit:** `blob: update tree-sitter grammar and dark parser/printer`

### 1.12 — add F# roundtrip + promotion + memory tests

Files:
- `backend/tests/Tests/Blob.Tests.fs` (new) — consolidates blob tests:
  - `DBlob(Persistent _)` binary roundtrip
  - `DBlob(Ephemeral _)` promotes on serialize, dedup on second write
  - `fileRead` memory bound: `GC.GetTotalAllocatedBytes` delta < 3×
    file size for a 10 MB file
  - User DB round-trip via queryable JSON path
  - Ephemeral UUID accessible before promotion; after promotion, the
    persistent hash resolves (see open-question 5 — UUID lookup after
    promotion is not required but included as a transient convenience:
    ephemeral store retains `uuid -> hash` forwarding for the
    ExecutionState's lifetime)

**Done when:** all tests pass.

**Tests:** the file itself.

**Commit:** `blob: add roundtrip promotion and memory tests`

### 1.13 — add .dark bytes tests

File: `backend/testfiles/execution/stdlib/bytes.tests` (new).

One case per builtin in 1.7. Include:
- hex roundtrip
- base64 roundtrip
- `Bytes.length (Bytes.fromString "hi") = 2L`
- `Bytes.toString (Bytes.fromString "ok") = Ok "ok"`
- `Bytes.slice b 0 1 |> Bytes.toHex` for a known input

**Commit:** `blob: add .dark bytes tests`

### 1.14 — capture before/after diff into phase-1-results.md

Re-run the Phase 0 harness (`Measurement.fs` scenarios 1, 2, 4, 5)
and produce `thinking/blobs-and-streams/phase-1-results.md` with
before/after tables. Scenario 3 (streaming) stays on the old code
path until Phase 2.

**Done when:** `phase-1-results.md` committed with clear pass/fail
commentary.

**Tests:** none new; rerun existing harness.

**Manual test:** run the fileRead 38 MB repro from
bug-fileread-oom.md and paste the RSS number into the results file.

**Commit:** `blob: write phase-1 results with before/after diff`

## Phase 1 exit criteria

- All Phase 0 scenarios rerun with improved numbers
  (specifically: fileRead 38 MB must not OOM; must allocate <100 MB).
- No .dark or F# tests regress.
- Build is green and formatted.
- `phase-1-results.md` exists and tells a legible story.

## Phase 1 open TODOs to carry into later phases

- Trace capture of ephemeral blobs forces promotion — placeholder
  comment added in 1.6; full scope-lifetime handling is L.1.
- Val persistability guard — deferred to L.2 (streams join that
  guard in Phase 2).
