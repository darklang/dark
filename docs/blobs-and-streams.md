# Blobs and Streams

This document covers the `Blob` and `Stream<'a>` types, why they
exist, the runtime mechanics behind them, and the migration of
existing byte-handling code paths to use them.

## Motivation

Before this work, byte sequences in Dark were `List<UInt8>` —
boxed one Dval per byte plus a list-cons cell per element.
Concretely:

- A 10 MB file read into Dark allocated ~2.2 GB and produced
  ~10,000,001 Dvals.
- A 38 MB file read OOM-killed at ~49 GB RSS.
- HTTP response bodies, hex/base64 encoding, and any byte-shaped
  IO inherited the same cost.

The architectural problem was that Dark's runtime treated bytes as
a degenerate case of "list of small numbers". Every byte paid
full Dval boxing overhead. There was no way to read a file
without paying it.

## Types

### `Blob`

`Blob` is an immutable byte sequence. The Dval (`DBlob`) holds a
**reference**, not bytes. Two reference shapes:

- `Ephemeral uuid` — bytes live in `ExecutionState.blobStore`, a
  `ConcurrentDictionary<Guid, byte[]>`. Lifetime is bound to the
  VM (or to a "blob scope" — see below).
- `Persistent(hash, length)` — bytes live in `package_blobs`,
  keyed by their SHA-256. Content-addressed: identical bytes
  dedupe automatically.

The `Persistent` form is what serialises to disk. The `Ephemeral`
form is the construction-time shape; promotion to `Persistent`
happens at every persistence boundary.

### `Stream<'a>`

`Stream<'a>` is a lazy, single-consumer, non-persistable sequence.
The Dval (`DStream`) wraps a `StreamImpl` tree:

- `FromIO` — leaf, pull-driven, optionally with a chunked-bytes
  fast path
- `Mapped` / `Filtered` — transforms over a source
- `Take n` — bounded prefix
- `Concat` — chained streams

Streams flow lazily — no buffering before the first pull. The
chunked-bytes fast path (`nextChunk : int -> Ply<Option<byte[]>>`)
lets byte streams avoid per-byte Dval/Ply continuation cost on
bulk drains (`streamToBlob`, SSE byte accumulation).

## Runtime mechanics

### Blob lifetime — promotion

`DBlob(Ephemeral _)` is the construction-time shape. It can't be
serialised — every byte writer (binary, JSON queryable,
roundtrippable) raises on the ephemeral case. To persist, callers
go through `Dval.promoteBlobs`:

```fsharp
let! dval = Dval.promoteBlobs state pm.persistBlob originalDval
```

Walks the dval tree, hashes any ephemeral blob bytes via SHA-256,
inserts into `package_blobs` (idempotent via `INSERT OR IGNORE`),
and rebuilds the dval with the ephemeral refs swapped for
persistent ones. Most call paths run this implicitly:

- `Inserts.fs` (val commit) auto-promotes via `Seed.fs:promoteBlobs`
  before the persistability guard runs.
- The `=` builtin uses `promoteBlobs` with a no-op insert to
  hash-compare ephemerals without writing.

### Blob lifetime — scope-based GC

Long-running VMs (the http-server) push a fresh blob scope per
request:

```fsharp
Dval.pushBlobScope state
try
  // ... handle request ...
finally
  Dval.popBlobScope state  // drops every ephemeral UUID minted in this scope
```

The scope tracks `HashSet<Guid>` of UUIDs minted under it; pop
removes them from `blobStore`. CLI / test runs don't push a scope
— blobs live as long as the VM, which is fine for short-lived
processes.

Ephemerals promoted to Persistent inside a scope survive — promotion
writes to `package_blobs` (out of `blobStore`) and the ref swap
moves the dval out of the scope's tracked UUIDs.

### Stream lifetime — finalizer + explicit close

`DStream` carries a `disposed: bool ref` flag and a
`StreamFinalizer` object that doubles as the lock target:

- Drain-to-end flips `disposed` and runs the disposer chain.
- `Builtin.streamClose` does the same explicitly.
- If the DStream becomes unreachable, the GC finalises the
  `StreamFinalizer` which runs the same disposer chain.

The disposer is what releases the underlying IO source —
`HttpResponseMessage`, `FileStream`, etc. Idempotent across all
three paths.

### Single-consumer

`DStream` is single-consumer by convention but not by type. The
runtime has no lock on `next` because we removed the per-pull
Monitor (Monitor across Ply continuation awaits throws
`SynchronizationLockException`). Two callers pulling the same
stream race silently. Today, no code shares stream references
across two consumers — this is a footgun rather than an active
bug.

## Migration scope

### Migrated to Blob

- `Builtin.fileRead` / `Builtin.fileWrite` / `Posix.fdReadAll` /
  `Posix.fdWrite` — file IO
- `Stdlib.Crypto.*` (SHA-256/384, MD5, HMACs) — input + output
- `Stdlib.Base64.encode` / `decode` / `urlEncode`
- `Stdlib.Blob.toHex` / `fromHex`
- `Stdlib.HttpClient.Request.body` / `Response.body`
- `Stdlib.Http.Request.body` / `Response.body` (server-side)
- `MultipartRequest.body` (in `wip/ai/openai/audio.dark`)
- `Stdlib.String.toBlob` / `fromBlob` / `fromBlobWithReplacement`
  (added; the `*Bytes` versions still exist for callers that want
  `List<UInt8>`)

### Intentionally still `List<UInt8>`

- `Stdlib.HttpClient.Sse.ParseState.currentLine` — internal
  byte-by-byte SSE-line accumulator. Could be a `Blob` with
  concat, but the byte-by-byte access pattern argues for a list.
- `Stdlib.UInt8.sum` — operates on a list of UInt8 *numbers*, not
  a *sequence of bytes*. Stays.
- `Stdlib.String.toBytes` / `fromBytes` /
  `fromBytesWithReplacement` — explicit list-shaped variants kept
  for callers who want that form (e.g. byte-level transforms).
- `Builtin.bytesToList` / `Builtin.bytesFromList` — the explicit
  bridges between Blob and List<UInt8>.
- `Builtin.bytesHexEncode` (deprecated) — takes List<UInt8>; the
  deprecation note points at `Blob.toHex`.

## Known limits

Recorded here in addition to the inline doc comments at the type
definitions.

### Ephemeral blob heap

- Within one scope, ephemeral bytes accumulate until pop. A handler
  that pulls many large files in a single request can balloon one
  request's footprint. **No per-scope byte budget exists today.**
- Promoted-to-Persistent blobs survive scope pop because they live
  in `package_blobs`, not the in-memory ephemeral cache.
- `pm-sweep-blobs` (CLI) walks `package_values.rt_dval` for
  reachable Persistent hashes and deletes orphans. Today
  `package_values` is the only blob-holding table. Any future
  table holding blob refs needs explicit wiring into the sweep.

### Streams

- Single-consumer is unenforced (see above).
- No backpressure: a producer faster than a consumer fills memory.
  Network-bound for HTTP; unbounded for in-process producers.
- `Mapped`/`Filtered` closures hold `ExecutionState` references.
  Today doesn't manifest because streams aren't persistable, but
  if a Stream ever outlives its originating execution it's a
  stale-state hazard.

### Trace capture

`Tracing.fs:storeTraceInput` and `storeFnResult` serialise dvals
**without** promoting ephemerals first. A trace whose dvals contain
`DBlob(Ephemeral _)` deserialises into a fresh VM where the UUIDs
no longer resolve. Latent bug; the fix needs threading state into
the tracing API.

### `Stdlib.Blob.empty` parser dependency

The Dark stdlib's `let empty = Builtin.blobEmpty` works because
the F#-side parser reads it as record-field access on a `Builtin`
variable, and `WrittenTypesToProgramTypes.ERecordFieldAccess` has
a fallback that tries value-name resolution. Wrapping in `()`
breaks that path. Documented inline in `blob.dark`. If the
fallback ever gets cleaned up, the val silently produces DUnit.

## Performance

See `backend/benchmarks/results-summary.md` for the full table.
Headlines:

| scenario     | input    | before          | after        |
| ------------ | -------- | --------------- | ------------ |
| fileRead     | 10 MB    | 2.24 GB / 5.2s  | 10 MB / 4ms  |
| fileRead     | 38 MB    | OOM @ ~49GB RSS | 38 MB / 19ms |
| httpBody     | 10 MB    | 2.31 GB / 5.5s  | 20 MB / 9ms  |
| hexEncode    | 1 MB     | 654 MB / 1.75s  | 4 MB / 4ms   |
| dval nodes   | 10 MB    | 10,000,001      | 1            |

The streaming-HTTP path has ~6x overhead on byte streams from
per-Ply continuation cost — see `backend/benchmarks/critique.md`
and `backend/benchmarks/follow-ups.md`.

## Where things live

- **F# types** — `backend/src/LibExecution/RuntimeTypes.fs`
  (DBlob, BlobRef, DStream, StreamImpl)
- **Blob runtime helpers** —
  `backend/src/LibExecution/Dval.fs:newEphemeralBlob`,
  `readBlobBytes`, `promoteBlobs`, `pushBlobScope`,
  `popBlobScope`, `disposeStreamImpl`, `wrapStreamImpl`,
  `newStream`, `newStreamChunked`, `readStreamNext`,
  `readStreamChunk`
- **Builtins** —
  `backend/src/BuiltinExecution/Libs/Bytes.fs` (Blob ops),
  `Stream.fs` (Stream ops)
- **Stdlib** — `packages/darklang/stdlib/blob.dark`,
  `packages/darklang/stdlib/httpclient.dark` (includes the
  `Sse` submodule)
- **Migration** —
  `backend/migrations/20260424_000000_package_blobs.sql`
- **Sweeper** — `LibPackageManager.RuntimeTypes.Blob.sweepOrphans`,
  exposed via `./scripts/run-local-exec pm-sweep-blobs`
- **Benchmarks** — `backend/benchmarks/`,
  `backend/src/LocalExec/Benchmarks.fs`, run via
  `./scripts/run-local-exec bench`

## Future-direction docs

These cover items deferred from the migration:

- `backend/benchmarks/critique.md` — implementation review with
  tagged items (🔧 / ⚠️ / 🐛). Marked one-off; can delete after
  the relevant follow-ups land.
- `backend/benchmarks/follow-ups.md` — concrete next-round work
  with cost estimates. Same disposition.
- `backend/benchmarks/results-summary.md` — bench tables with
  before/after numbers and inline commentary.
