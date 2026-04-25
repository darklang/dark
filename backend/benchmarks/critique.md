# Blob & Stream — implementation critique

One-off review. Bullet-pointed, not exhaustive. Tag at the end of each
item: 🔧 fixable in a small follow-up; ⚠️ design tension worth knowing; 🐛
latent bug.

## Benchmark findings

The numbers (`results/latest.json`) are good in absolute terms — fileRead
1.01x, httpBody 2.01x, hexEncode 4.07x — but the bench surface is thin:

- All scenarios are single-shot, single-blob. The architecture's
  weakest path is **many small blobs in one VM scope** (e.g. a handler
  that loops 1000× reading small files into ephemerals). That's the case
  where `blobStore`'s ConcurrentDictionary churn + GUID gen + Dval
  wrapper cost dominate, and we don't measure it.
- Streaming isn't benched here at all — the worst-of-the-bunch
  numbers (~6x) live in the saved `rundir/measurements/phase-2`
  scratch and aren't tracked in the same place. Either bench
  streamingHttp here or be honest and admit it's not measured.
- No real-world soak test. We never ran a cloud canvas under load for
  long enough to catch lifetime/leak issues. The blobScopes /
  StreamFinalizer / promotion paths could all hide leaks that don't
  show up in 50ms benchmarks.
- `httpBody` uses a fake `HttpMessageHandler` returning a single
  ByteArrayContent. That's the easy path. Real responses arrive in
  multiple network reads, get chunked transfer encoding, etc. The
  measured 2.01x is best-case.

## Blob — design observations

1. **`emptyBlobHash` short-circuit in `Dval.readBlobBytes` is a
   workaround.** We hard-code SHA-256 of `[||]` to avoid needing a
   `package_blobs` row for `Stdlib.Blob.empty`. Works for one constant.
   The next time someone wants a small constant Blob (a 4-byte magic
   number, an embedded asset), they'll hit the same wall. Proper fix:
   seed common blobs into `package_blobs` at migration time, or extend
   BuiltInValue to carry `byte[]` directly. 🔧

2. **Equality forces promotion.** `NoModule.equals` calls `promoteBlobs`
   on both sides before comparing. That writes to `package_blobs` for
   every `==` over ephemeral Blobs. A handler doing `if blob1 == blob2`
   on two 10MB ephemerals does two 10MB DB writes per comparison. Should
   hash-and-compare without persisting. ⚠️

3. **`blobStore` has no size cap.** Within one scope (one HTTP request),
   ephemerals accumulate until `popBlobScope`. A handler that loads many
   large files in a single request can balloon. We documented this in
   `RuntimeTypes.fs:1668` — but documentation isn't a fix. ⚠️

4. **`promoteBlobs` walks the full Dval tree on every save.** Most
   dvals can't contain blobs (primitives, strings, etc). Could
   short-circuit at the type level. Probably not hot, but it's
   potentially wasted work. 🔧

5. **`pm-sweep-blobs` only scans `package_values.rt_dval`.** Doesn't
   touch `trace_data` or User DB rows. The original L.3 spec called out
   "any new referencing table needs wiring into the sweep" — but
   nothing enforces this. The next person to add a blob-holding table
   will silently break the sweep. ⚠️

6. **Trace capture doesn't promote.** `Dval.fs:439` has a TODO: "trace
   capture path should also call this." Today, traces holding
   ephemerals don't fail at capture (we use `DStreamStub` /
   `DBlobEphemeral` placeholders in the roundtrippable serializer) but
   they can't actually round-trip back to working values. 🐛

7. **No Blob literal syntax.** Users can write list literals, hex
   strings via `Blob.fromHex`, or UTF-8 via `Blob.fromString`. There's
   no `\x00\x01\x02` byte-literal form. Limits the
   "small-binary-data-in-source" path. ⚠️

8. **Two-namespace footgun.** `Stdlib.String.toBytes` (List<UInt8>) and
   `Stdlib.String.toBlob` (Blob) coexist; `Stdlib.Blob.fromList` and
   `Builtin.bytesFromList` are both bridges. Every byte-handling
   decision now requires "which world am I in?" Most callers are now
   in Blob-world but the legacy List<UInt8> still exists for HTTP
   server request bodies (intentional, per the migration scope) and
   the `Stdlib.UInt8.sum` family. The longer the dual namespace lasts
   the more confusing it gets. ⚠️

## Stream — design observations

1. **Single-consumer invariant is unenforced.** We removed the
   per-pull Monitor in `2.12` because of the SynchronizationLockException
   hazard across Ply awaits. The `disposed` flag is the only safety
   net, and it doesn't catch concurrent `next()` calls. Two callers
   pulling from the same `DStream` will interleave or race silently. 🐛

2. **`Mapped`/`Filtered` closures hold an ExecutionState reference.**
   The user-fn invocation goes through `Exe.executeApplicable` against
   the closing-over `ExecutionState`. If a Stream outlives its
   originating execution — long-lived in a debug pause, returned from
   a handler, etc — the closure holds stale state. We work around
   this by saying "streams are non-persistable" and `DStream` raises
   on serialize, but the in-memory leak path is real. ⚠️

3. **`Concat` is mutable.** Drain pops exhausted heads from a
   `streams : StreamImpl list ref`. If two callers ever got a
   reference to the same `Concat` impl (shouldn't happen given the
   single-consumer story, but the type system doesn't enforce it),
   they'd see torn views. ⚠️

4. **No backpressure.** A producer faster than the consumer fills
   memory. For HTTP this is network-bounded; for in-process producers
   (`streamFromList` over a huge list, `streamUnfold` with no
   termination condition), unbounded. ⚠️

5. **Per-element Ply overhead.** The known issue from phase 2 — every
   `next` is a Ply continuation, which for a 10MB byte stream means
   millions of state-machine allocations. The `nextChunk` fast path
   helps for `streamToBlob` and the SSE parser, but anything
   element-wise (`streamMap`, `streamFilter`) on byte streams pays the
   full cost. The Ply-question doc proposes ripping Ply out; that's a
   cross-cutting change none of the blob/stream work touches. ⚠️

6. **`StreamImpl.elemType` walks the transform tree on every call.**
   Could be cached at construction time. Hot for type-checking
   `streamMap` results. 🔧

7. **`DStreamStub` is a one-way bridge.** `RuntimeTypesToDarkTypes`
   renders a stream as a stub case for LSP/reflection; `fromDT`
   raises. Any Dark code that introspects a Dval and rebuilds it
   silently breaks for streams. The asymmetry isn't documented in the
   public API; surprise factor for someone writing their own pretty-
   printer or trace viewer. ⚠️

## Cross-cutting

1. **`Stdlib.Blob.empty` only works because of a parser
   coincidence.** Top-level `let empty = Builtin.blobEmpty` parses (in
   F# syntax) as a record-field-access expression on a `Builtin`
   variable. It happens to resolve correctly because
   `WrittenTypesToProgramTypes.ERecordFieldAccess` has a fallback that
   tries value-name resolution. If anyone ever cleans up that
   fallback, the val silently breaks. The right fix is either: (a) a
   `val` declaration that accepts more than `simple_expression`, or
   (b) Builtin literal syntax in Dark. ⚠️

2. **`Builtin.blobEmpty` is the only BuiltInValue we have.** The
   pattern (BuiltInValue body = static Dval) is general — we could
   imagine `Builtin.intMaxInt64`, `Builtin.uuidNil`, etc. Right now
   there's exactly one example and we shouldn't assume the
   surrounding machinery is well-exercised. ⚠️

3. **The migration left two intentional `List<UInt8>` survivors:**
   - `Stdlib.Http.Request.body`: kept after a brief detour. Wait — we
     migrated this to `Blob` in the latest commit. Update this if it
     stays.
   - `HttpClient.Sse.ParseState.currentLine: List<UInt8>` — internal
     SSE-line accumulator, byte-by-byte. Could be a `Blob` with
     concat, but the byte-by-byte access pattern argues for a list
     here. Worth revisiting if anyone ever profiles the SSE parser.
   - `Stdlib.UInt8.sum (List<UInt8>) -> UInt8` — semantically about
     numbers in a list, not a sequence-of-bytes. Correct as-is.

4. **The Ply-replace question looms.** Per-element streaming
   overhead, `promoteBlobs` rebuild-Dval-tree allocations, the
   Monitor-across-Ply-await hazard — all rooted in Ply semantics.
   Until that's resolved, every fix here is dancing around the
   underlying issue. The thinking doc (`thinking/ply-question.md` if
   it still exists) frames the trade-off; we deferred it. Worth
   revisiting once a soak test surfaces the cost in production-shape
   workload. ⚠️

5. **The benchmark suite is the right shape but the wrong size.**
   Three scenarios, single-shot, no streaming, no equality, no soak.
   Easy to extend (add scenarios in `Benchmarks.fs`), nothing
   load-bearing here yet. The risk: CI never runs this, the numbers
   drift across PRs, the diff in `latest.json` becomes the only
   record, and a future regression hides in the noise. Worth a
   follow-up to wire it into a workflow_dispatch (not per-PR; too
   noisy) and post the diff to a PR comment. ⚠️

## Summary

The architecture is sound — Blob/Stream are the right concepts, the
bench numbers are competitive with Go/Rust/.NET on the paths that
matter, and the bugs we hit (DUnit-from-stale-evaluation, F# parser
ERecordFieldAccess-as-value, `()` invocation form) were all language
seams, not core-architecture problems.

Top three follow-ups in priority order:

1. **Trace path migration** — currently a latent bug in roundtripping
   any captured trace that holds an ephemeral blob.
2. **Soak test** — get one real cloud canvas running this code under
   sustained load for a week. The leaks (if any) live there, not in
   the benchmark.
3. **Equality without write** — drop the promote-on-equals pattern;
   hash-compare ephemerals in-memory.

Everything else is incremental.
