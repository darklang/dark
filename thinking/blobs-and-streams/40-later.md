# Later — After Phase 2

Work that doesn't block shipping Phases 1 and 2 but matters for
long-term correctness and ergonomics. Each item is a chunk the loop
can pick up if time remains.

## L.1 — Scope-based ephemeral-blob lifetime

**Problem.** Phase 1 stores ephemeral blobs in a per-ExecutionState
map that lives as long as the ExecutionState does. CLI is fine; an
http-server handling 10k requests accumulates handles.

**Approach.** ExecutionState gains a scope stack. Each entry is a
`Set<uuid>` of blobs created inside that scope. Push on handler
entry, pop on exit, drop the set's entries from the byte-store.

Files:
- `backend/src/LibExecution/RuntimeTypes.fs` (ExecutionState): add
  `blobScopes : Stack<Set<uuid>>`; helpers to push/pop.
- `backend/src/LibExecution/Dval.fs`: `newEphemeralBlob` records in
  the top-of-stack scope.
- `backend/src/BuiltinHttpServer/Libs/HttpServer.fs`: push scope
  before calling the handler, pop after (with finally).
- Blobs promoted to Persistent before scope-pop stay resolvable via
  the usual `package_blobs` path — we only drop the in-memory
  byte-store entry.

**Done when:** http-server stress test with a handler that creates
many ephemeral blobs per request shows flat memory over time.

**Tests:**
- F#: `HttpServer.Tests.fs` scope test — construct ExecutionState,
  push scope, create blobs, pop, assert byte-store shrunk.

**Commit:** `blob: scope-based ephemeral lifetime for long-lived VMs`

## L.2 — Dval.isPersistable guard in Inserts.fs

**Problem.** Today `val x = (fun y -> y)`, `val x = someDB`, and
(post-Phase-2) `val x = someStream` all succeed at commit with
varying degrees of brokenness. No guard exists at
`LibPackageManager/Inserts.fs`.

**Approach.** Add `Dval.isPersistable : Dval -> bool` in
`LibExecution/Dval.fs`. Returns `false` for `DStream`, returns
`false` for `DApplicable` (with a CLEANUP TODO noting some
applicables might be OK if we serialize instructions fully), and
`false` for `DDB(Ephemeral _)` style cases. Check at val commit;
raise a clear user-facing error otherwise.

Files:
- `backend/src/LibExecution/Dval.fs`: new predicate.
- `backend/src/LibPackageManager/Inserts.fs`: check in the insert
  path.
- New Dark-level error case.

**Done when:** committing a stream or lambda as a val produces
"cannot store this kind of value in a val" rather than silently
corrupting.

**Tests:**
- .dark: `backend/testfiles/execution/errors/valGuards.tests` —
  each banned shape rejected.

**Commit:** `pm: val persistability guard rejects streams/lambdas`

## L.3 — orphan package_blobs sweeper

**Problem.** `package_blobs` rows accumulate. Content-addressing
dedupes most of it, but deprecated branches / deleted vals / aged-out
traces can leave unreferenced rows.

**Approach.** Periodic scan:
```sql
DELETE FROM package_blobs
WHERE hash NOT IN (SELECT blob_hash FROM ... -- all referencing tables
);
```
Referencing tables: `package_values.rt_dval` (structured, need to
parse), `locations`, User DB rows (need to parse JSON),
`trace_data`. Parsing for references is the expensive part; likely
incremental via a `package_blob_refs` reverse-index table maintained
on insert.

Files:
- `backend/migrations/<new>_package_blob_refs.sql`: reverse index
  table.
- `backend/src/LibPackageManager/Inserts.fs`: maintain reverse
  index on serialize.
- New sweeper command in CLI (`pm-sweep-blobs`?) that runs the
  cleanup; schedule on the server side lives outside this work.

**Done when:** a test creates orphan blobs, runs sweep, observes
row count drop.

**Commit:** `blob: orphan package_blobs sweeper`

## L.4 — blob equality semantics

**Problem.** Equal-by-content for blobs is ambiguous:
- `Persistent == Persistent`: compare hashes. Cheap.
- `Ephemeral == Persistent`: must hash the ephemeral side, which
  means promoting (or at least hashing in place).
- `Ephemeral == Ephemeral`: either hash both (expensive) or treat
  as not-equal unless same UUID (surprising).

**Chosen rule (doc now, ship later):**
- Ephemeral-Ephemeral with same UUID: equal (trivial).
- Ephemeral-Ephemeral different UUID: hash both, compare. Side
  effect: both get promoted.
- Ephemeral-Persistent: hash the ephemeral, compare. Ephemeral gets
  promoted.
- Persistent-Persistent: hash compare only.

**Files:**
- `backend/src/LibExecution/Dval.fs`: `equals` (or whatever drives
  `=` in the interpreter) gets the blob-specific path.

**Tests:**
- F#: each of the four cases.
- .dark: `(Bytes.fromString "hi") = (Bytes.fromString "hi")` → true.

**Commit:** `blob: equality forces hash-compare across ephemeral/persistent`

## L.5 — sub-blob slicing (BEAM-style sharing)

**Problem.** `Bytes.slice b 100 200` currently copies (Phase 1
default). Fine for small blobs; wasteful for 100 MB chunked
processing.

**Approach.** Extend `BlobRef` with a `Subblob` variant:
```fsharp
| Subblob of parent: BlobRef * offset: int64 * length: int64
```
`readBlobBytes` walks to the root, copies the slice on actual read.
Promotion of a subblob promotes its parent (or just the slice — open
question; default to promoting the slice only so we don't pull
100 MB in to promote a 1KB window).

Files:
- `backend/src/LibExecution/RuntimeTypes.fs`: extend `BlobRef`.
- `backend/src/LibExecution/Dval.fs`: `readBlobBytes` walks.
- `backend/src/BuiltinExecution/Libs/Bytes.fs`: `slice` returns a
  `Subblob` rather than copying.

**Tests:**
- F#: slice of slice; slice across promotion boundary; slice roundtrip
  through binary serializer.
- .dark: slice correctness on known inputs.

**Skip unless** phase-1 or phase-2 results show slice-copy is hot.

**Commit:** `blob: sub-blob slicing with shared parent bytes`

## L.6 — retype Stdlib.Crypto / Base64 / String to Blob

**Context.** Chunk 1.10 migrated the Builtins (Crypto/Base64/String)
to take and return `Blob`, but kept the Dark-side Stdlib wrappers on
`List<UInt8>` with internal `bytesFromList`/`bytesToList` bridges.
The reason: the .dark test corpus assumes byte-list semantics and
would take ~90 edits to migrate. Deferred rather than bundled.

**What it involves.**

Call sites to update, grouped by test file:

- `backend/testfiles/execution/language/big.dark` — uses
  `Stdlib.Bytes.hexEncode_v0`, `Stdlib.List.length` on a byte list,
  `Stdlib.Base64.urlEncode_v0`.
- `backend/testfiles/execution/stdlib/base64.dark` — ~40 tests
  comparing `Result.Ok (Stdlib.String.toBytes "...")`.
- `backend/testfiles/execution/stdlib/crypto.dark` — ~9 tests nesting
  `Stdlib.Bytes.hexEncode_v0 (Stdlib.Crypto.sha256_v0 (...))`.
- `backend/testfiles/execution/stdlib/bytes.dark` — 4 legacy
  `hexEncode_v0` tests on top of the 22 new-API tests from 1.7/1.13.
- `backend/testfiles/execution/stdlib/http.dark` — body assembly.
- `backend/testfiles/execution/stdlib/string.dark` — bytes bridges.

Strategy:
1. Retype each wrapper signature in `packages/darklang/stdlib/*.dark`
   from `List<UInt8>` → `Blob`.
2. Update the corresponding .dark tests. Many will reduce to using
   `Stdlib.Bytes.fromString` / `toHex` / `toString` directly instead
   of the bridged chain through `toBytes_v0`.
3. Keep `Stdlib.Bytes.hexEncode_v0` on the `List<UInt8>` shape (old
   API) for one release cycle, marked `DeprecatedBecause`.
4. Rerun `./scripts/run-backend-tests`. Expected LOC delta: ~150
   lines of test churn, ~30 of wrapper cleanup.

**Why it's deferred.** Phase 1's goal was "fix the OOM"; that's done.
Public-API churn in stdlib wrappers is a tidy-up that can ship on its
own cadence, doesn't block Phase 2, and has its own test-matrix
concerns (each migrated test file is independently verifiable).

**Commit:** `blob: retype stdlib crypto/base64/string public signatures to blob`
