# PR description draft

Paste this into the GitHub PR body once you open it.

---

## Blobs and Streams

Replaces `List<UInt8>` with two purpose-built types — `Blob`
(immutable byte sequence, content-addressed when persisted) and
`Stream<'a>` (lazy, single-consumer, non-persistable) — and
migrates the byte-handling code paths to use them.

### Why

Byte sequences in Dark were boxed one Dval per byte plus a
list-cons cell per element. A 10 MB file read materialised
~10,000,001 Dvals and allocated ~2.2 GB; a 38 MB read OOM-killed
at ~49 GB RSS. Every byte-shaped IO path (file IO, HTTP bodies,
hex/base64 encoding, crypto digests, SSE parsing) inherited the
cost.

### Headlines

| scenario   | input  | before          | after         |
| ---------- | ------ | --------------- | ------------- |
| fileRead   | 10 MB  | 2.24 GB / 5.2s  | 10 MB / 4 ms  |
| fileRead   | 38 MB  | OOM @ ~49GB RSS | 38 MB / 19 ms |
| httpBody   | 10 MB  | 2.31 GB / 5.5s  | 20 MB / 9 ms  |
| hexEncode  | 1 MB   | 654 MB / 1.75s  | 4 MB / 4 ms   |
| dval nodes | 10 MB  | 10,000,001      | 1             |

Full benchmark table: `backend/benchmarks/results-summary.md`.

### Highlights

**Blob:** new type `Blob` plus `BlobRef = Ephemeral uuid |
Persistent(hash, length)`. Ephemeral bytes live in a per-VM
`blobStore`; persistent bytes in a content-addressed
`package_blobs` table. Auto-promotion at every persistence
boundary. Scope-based GC for long-running VMs (push scope per
http handler request, pop drops ephemerals).

**Stream:** new type `Stream<'a>` with composable transforms
(`map` / `filter` / `take` / `concat` / `unfold`). Lazy by
construction. Single-consumer. GC finalizer + explicit
`streamClose`. `streamUnfold` is the user-facing escape hatch
for custom lazy generators. Chunked-bytes fast path for
`streamToBlob` / SSE.

**Migrated:** file IO, all of `Stdlib.Crypto`, `Stdlib.Base64`,
`HttpClient` request + response bodies, `Stdlib.Http` (server)
request + response bodies, multipart construction. SSE parser
moved into Dark via `streamUnfold` (was previously F# callback
plumbing).

**Tooling:** `pm-sweep-blobs` CLI command for orphan reclaim,
`./scripts/run-local-exec bench` for allocation/timing
benchmarks (results in `backend/benchmarks/results/latest.json`,
viewer at `backend/benchmarks/viewer.html`).

### What's NOT in this PR

- **Trace path migration.** `Tracing.fs` doesn't promote
  ephemerals before serialising. Captured traces holding
  ephemerals deserialise but can't be used. Latent bug;
  fix is a multi-file refactor through the tracing API. See
  `backend/benchmarks/follow-ups.md` item 1.
- **Soak test.** Benchmarks measure single-shot allocation. No
  long-running-canvas validation. See follow-ups item 2.
- **Ply replacement.** Per-element streaming has ~6x overhead
  from Ply continuation costs. Fixing this is a separate
  multi-week effort. See follow-ups item 5.
- **HTTP server `Request.body`** was migrated; `currentLine` in
  the SSE parser stays `List<UInt8>` (byte-by-byte access
  pattern argues for a list). `Stdlib.UInt8.sum` stays
  `List<UInt8>` (genuinely about a list of numbers).

### Migration impact

`CloudExe.executeHandler`'s signature changed to support state
being created before request-Dval construction (so the request
body can be a real ephemeral blob):

```fsharp
// before
inputVars : Map<string, RT.Dval>
InitialExecution of HandlerDesc * varname : string * RT.Dval

// after
inputVars : RT.ExecutionState -> Map<string, RT.Dval>
InitialExecution of HandlerDesc * varname : string *
                    buildInputVar : (RT.ExecutionState -> RT.Dval)
```

If any out-of-tree branches call `executeHandler`, they'll need
to pass a builder closure instead of a pre-built Dval map.

### Reading the diff

86 commits, 270 files. Suggested review approach:

1. **Start with the design doc:** `docs/blobs-and-streams.md`
   covers the types, runtime mechanics, migration scope, and
   known limits.
2. **Look at the type definitions:**
   `backend/src/LibExecution/RuntimeTypes.fs` (DBlob, BlobRef,
   DStream, StreamImpl).
3. **Then the runtime helpers:**
   `backend/src/LibExecution/Dval.fs` —
   `newEphemeralBlob`, `readBlobBytes`, `promoteBlobs`, the
   blob-scope helpers, the stream pull helpers.
4. **Then the user-facing surface:**
   `packages/darklang/stdlib/blob.dark`,
   `packages/darklang/stdlib/httpclient.dark`.
5. **Then a builtin or two for shape:**
   `backend/src/BuiltinExecution/Libs/Bytes.fs`,
   `backend/src/BuiltinExecution/Libs/HttpClient.fs`.
6. **Then the migration commits** — the 1.x / 2.x / L.x prefixes
   in the commit log walk through the phases. The "PR cleanup
   pass" commits are mechanical (comment cleanup, dedup,
   renames) and are mostly skim-able.

### Risks / open questions

- **The `Stdlib.Blob.empty` parser coincidence** — bare
  `Builtin.blobEmpty` resolves only because of an
  ERecordFieldAccess fallback in `WrittenTypesToProgramTypes`. If
  someone cleans up that fallback, the val silently produces
  DUnit. Documented inline. Probably worth a follow-up to add
  Blob literal syntax or a `val =` form that accepts any
  expression.
- **Microsoft.Data.Sqlite zero-length BLOB quirk:** `read.bytes`
  returns None for an empty BLOB column, even when the row
  exists. We work around this with a hard-coded empty-hash
  short-circuit in `readBlobBytes`. Documented; only affects the
  empty blob.
- **The `thinking/blobs-and-streams/` directory** is gitignored
  and contains the original phase plans, baselines, and
  progress log. Worth a look if you want the blow-by-blow.
  Useful context but not in the PR.

### Tests

10,197 backend tests pass. Down from 10,206 because 9
measurement tests moved out of the suite into
`./scripts/run-local-exec bench` (they're benchmarks, not
correctness tests).
