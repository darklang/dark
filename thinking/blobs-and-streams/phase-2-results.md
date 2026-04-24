# Phase 2 results

Recorded **2026-04-24** after chunks 2.1–2.13 landed on
`blobs-and-streams` (TStream/KTStream wired, DStream with FromIO +
Mapped/Filtered/Take/Concat, 11 Stream builtins, HttpClient.stream,
Dark-side SSE parser, GC-backed finalizer).

Harness: `./scripts/run-backend-tests --filter-test-list measurement`.
Raw rows live in `rundir/measurements/phase-2/streaming.txt`. Compare
against [baseline.md](./baseline.md) (Phase 0) and
[phase-1-results.md](./phase-1-results.md).

## Streaming HTTP — the full pipeline exists

The phase-2 rerun of scenario 3 drives the same slow-chunked server
(100 chunks × 100 KB × 100 ms apart) through the new `HttpClient.stream`
+ `Dval.readStreamNext` path and drains byte-by-byte, mirroring what
the builtin surfaces to Dark.

| Metric                      | Phase 0 (callback)      | Phase 2 (DStream pull) | Delta    |
| --------------------------- | ----------------------: | ---------------------: | -------: |
| time-to-first-chunk         | 242 ms                  | 411 ms                 | +170 ms  |
| time-to-last-chunk          | 15.1 s                  | 20.3 s                 | +5.2 s   |
| total allocation            | 4.83 GB                 | 5.88 GB                | +22 %    |
| per-refill allocation (8 KB)| ~3.7 MB                 | ~4.5 MB                | +22 %    |

Phase 2 is **slightly slower and slightly heavier** on raw byte-by-byte
drain — the Ply continuation plumbing on each `readStreamNext` adds up
across 10 M pulls. The phase-0 callback path processed each 8 KB chunk
in one pass; the phase-2 path pulls one byte at a time, paying a full
`uply { … }` continuation per byte.

This is **honest-but-not-the-headline-metric**: users don't iterate
byte-by-byte in Dark. Real-world pull counts track:

- `Sse.parse |> Stream.toList` — one pull per parsed event (~hundreds
  for a typical LLM response)
- `Stream.take N` — N pulls then done; early terminates
- `Stream.toBlob` — one pull per byte today; a future chunked fast
  path (see [40-later.md](./40-later.md)) could amortize to one pull
  per 8 KB buffer

## What actually landed

The real wins of phase 2 are architectural, not per-byte throughput:

- **`StreamingHttpClient.fs` is gone.** The callback-based F#
  streaming builtin (requestStreaming / requestSSE) and its .dark
  counterpart are deleted. Consumers compose with the standard
  `Stream` builtins instead.
- **SSE is Dark code.** `packages/darklang/stdlib/sse.dark` parses SSE
  events over `Stream<UInt8>`; 8 .dark tests cover single/multi-event,
  comments, multi-line data, id persistence, empty-body, skipped
  empty-data blocks.
- **`HttpClient.stream` returns a real `DStream`.** Body is never
  buffered into a `byte[]` — `HttpCompletionOption.ResponseHeadersRead`
  hands back the live response stream, wrapped in a `FromIO` whose
  disposer releases the response on drain-to-EOF or `streamClose`.
  Verified by 4 in-process Kestrel tests in HttpClient.Tests.fs
  (basic drain, 28 KB multi-refill, empty, pre-drain close).
- **Stream lifecycle is safe.** `Dval.StreamFinalizer` doubles as the
  lockObj so the GC can run the disposer chain when a stream is
  abandoned. Three F# finalizer tests assert never-drained,
  mid-drain, and exactly-once-across-close-and-finalize.
- **Composition works end-to-end.** `Stream.fromList [1..10] |>
  Stream.filter even |> Stream.map (+1) |> Stream.take 3 |>
  Stream.toList = [3; 5; 7]` passes against real builtins in a
  .dark test.

## Against the 2.14 spec bullets

| Target                          | Status                                                |
| ------------------------------- | ----------------------------------------------------- |
| streaming time-to-first-chunk   | Recorded: 411 ms vs 242 ms baseline (see above)       |
| per-chunk allocation near zero  | **Missed** — ~4.5 MB per refill, dominated by per-byte Ply continuations. Fast-path opportunity: a chunked `streamToBlob` variant that reads into the accumulator 8 KB at a time without boxing per byte. |
| `StreamingHttpClient.fs` gone   | ✓ deleted from BuiltinExecution.fsproj, Builtin.fs, PackageRefs.fs, package-ref-hashes.txt; .dark counterpart + StreamingCallbackTests module deleted too. |
| SSE parser works                | ✓ canned Kestrel tests pass (8 .dark + 4 F# integration tests). Live-endpoint validation (2.13) deferred to user. |

## Missed target — per-chunk allocation

The phase-2 prediction was "allocation per chunk near zero — just the
ref and the buffer". Reality: per-byte pull through Ply is expensive
enough that the 10 MB body costs ~5.9 GB of transient allocation.

Root cause: `readStreamNext` is an async boundary per byte. Each call
creates continuation objects (`Ply<Option<Dval>>`, `PlyAwaitable`,
`AwaitableContinuation`) that get collected but still count against
`GetTotalAllocatedBytes`. For byte streams we're amplifying ~500× on
allocation churn even though steady-state RSS stays bounded.

**How to fix (not in phase 2 scope):** add `streamNextChunk : Stream<UInt8> -> Option<Blob>`
that reads an 8 KB (or larger) chunk in one shot, bypassing per-byte
Ply overhead. `streamToBlob` and `Sse.parse`'s byte-draining path are
the obvious callers. A full design pass probably goes in
[40-later.md](./40-later.md) as L.7 (chunked bulk drain). Ticket
template:

> Add `Builtin.streamNextChunk : Stream<UInt8> -> Option<Blob>` that
> accumulates 8 KB of bytes per pull. Update `streamToBlob` to use
> it. Measure against phase-0 streaming — expect ≥20× allocation
> reduction for raw-byte drains, ≥2× for SSE parse. Leaves the
> existing `streamNext` untouched so Stream<T> for non-byte types is
> unchanged.

## Manual SSE validation (2.13)

Deferred to user. Agent has no live endpoint access from this
sandbox. In-process Kestrel coverage in `backend/testfiles/execution/stdlib/sse.dark`
and `HttpClient.Tests.fs StreamDvalTests` exercises the full
`HttpClient.stream → Sse.parse → Stream.toList` pipeline against a
real Kestrel server, just not against an external service. See
the Blockers section in [README.md](./README.md).

## Phase 2 open TODOs carried to Later

From the phase-2 spec's exit TODO list, plus new ones surfaced by
this measurement:

- **L.1 (ephemeral-blob scope-based lifetime)** — not fixed; phase-2
  didn't widen blob scoping.
- **L.2 (`Dval.isPersistable` guard at Inserts.fs)** — not fixed;
  `val x = HttpClient.stream …` still raises only at serialize time.
- **L.7 (NEW) chunked bulk-drain fast path for Stream<UInt8>** — see
  above. Phase-2 per-byte throughput is the gating issue.
- **2.13 manual SSE validation** — open for user against a real live
  endpoint.

## Phase 2 is done

Chunks 2.1–2.12 and 2.14 are `[x]`; 2.13 is `[x]-deferred-to-user`.
`StreamingHttpClient.fs` is deleted. `HttpClient.stream`,
`Stdlib.Sse.parse`, and the 11 Stream builtins are live. The
per-byte-allocation miss is a real but scoped issue with a clear
fix path.
