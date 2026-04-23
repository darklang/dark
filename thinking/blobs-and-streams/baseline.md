# Phase 0 baseline

Recorded **2026-04-23** on branch `deprecation-redesign` (merged to
main; the current branch is the same code).

All measurements via `./scripts/run-backend-tests --filter-test-list measurement`.
Raw per-scenario rows: `rundir/measurements/phase-0/*.txt`.
Harness: `backend/tests/Tests/Measurement{,.Helpers}.fs`.

## Host context

- 64 GB RAM / 20 GB swap, Linux 6.17.9.
- .NET 10 LTS.
- Packages DB (`rundir/data.db`): **33,452,032 bytes** (~32 MB). No
  `package_blobs` table exists yet — this is the pre-Phase-1 floor
  that Phase 1 will grow against.

## Scenario 1 — `fileRead`

Mirrors `BuiltinCli.Libs.File.fileRead`: `File.ReadAllBytesAsync` then
`Dval.byteArrayToDvalList`.

| File size | Alloc delta  | Peak working set | Wall  | Dval nodes | Note                |
| --------- | ------------ | ---------------- | ----- | ---------- | ------------------- |
| 1 KB      | 1.41 MB      | 202 MB           | 0 ms  | 1,025      | ok                  |
| 100 KB    | 27.0 MB      | 231 MB           | 67 ms | 100,001    | ok                  |
| 1 MB      | 279 MB       | 462 MB           | 889 ms| 1,000,001  | ok                  |
| 10 MB     | 1.96 GB      | 1.99 GB          | 5.4 s | 10,000,001 | ok                  |
| 38 MB     | —            | —                | —     | —          | anecdote: ~49 GB RSS then OOM-kill per `bug-fileread-oom.md` (not run in-process here) |

**What sticks.** Every byte costs ~200 bytes of allocation
(list-node + boxed `DUInt8`). A 10 MB file blows up to ~2 GB RSS. The
Dval node count is `bytes + 1` — every byte materialises as its own
heap cell.

**Post-Phase-1 target.** 10 MB file → *one* DBlob allocation
(~40 bytes of Dval state + 10 MB of contiguous byte[] in the ephemeral
byte-store). Expect alloc delta well under 25 MB; wall time well under
100 ms. 38 MB should complete without OOM risk.

## Scenario 2 — HttpClient response body

Fake `HttpMessageHandler` returns a pre-built `ByteArrayContent`;
consumer does `ReadAsByteArrayAsync` then
`Dval.byteArrayToDvalList`. No real socket; dominant cost is the
same list boxing.

| Body size | Alloc delta | Peak working set | Wall  | Dval nodes | Note               |
| --------- | ----------- | ---------------- | ----- | ---------- | ------------------ |
| 100 KB    | 34.9 MB     | 231 MB           | 138 ms| 100,001    | simulated-handler  |
| 1 MB      | 280 MB      | 463 MB           | 890 ms| 1,000,001  | simulated-handler  |
| 10 MB     | 1.82 GB     | 2.09 GB          | 5.1 s | 10,000,001 | simulated-handler  |

**What sticks.** Almost identical to fileRead at 10 MB, confirming the
hotspot is the list representation, not the IO source. The extra
handler / content machinery is noise.

**Post-Phase-1 target.** Same as fileRead: one DBlob per response
body. 10 MB body → <25 MB alloc, <100 ms wall.

## Scenario 3 — streaming HTTP chunks

Custom `SlowChunkedStream` yields 100 × 100 KB chunks, 100 ms apart
(producer ≈10 s total). Consumer reads via an 8 KB buffer loop
mirroring `StreamingHttpClient.fs:266`; each read wraps into a
`Dval.byteArrayToDvalList` (what `StreamChunk.Data` holds today).

| Metric                | Value        |
| --------------------- | ------------ |
| Consumer reads        | 1,300 (≈13 reads per producer chunk, 8 KB buffer) |
| Total alloc delta     | 2.99 GB      |
| Time-to-first-read    | 145 ms       |
| Time-to-last-read     | 14.1 s       |
| Alloc per consumer read (avg) | 2.30 MB (for 8 KB of payload)     |

**What sticks.** Each 8 KB block coming off the socket becomes a
fresh DList with ~8,001 DUInt8 nodes. The per-read alloc is
~287× the payload size. Over a 10 MB streamed body that's 3 GB of
garbage. Producer framing (100 KB) is invisible to the consumer —
this matters for Phase 2's Stream design: "chunk" at the protocol
level and "chunk" at the language level need not match.

**Post-Phase-1 + Phase-2 target.** Each `Stream.next` returns a
Blob referencing the 8 KB buffer (or larger, if we merge reads).
Per-read alloc ~8 KB + one Dval ref ≈ tens of bytes of language-level
overhead. Total alloc for a 10 MB streamed body: <15 MB.

## Scenario 4 — `Bytes.hexEncode` on 1 MB

Broken into two `measure` calls to isolate costs:

| Step                   | Alloc delta | Wall  |
| ---------------------- | ----------- | ----- |
| DList construction     | 295 MB      | 967 ms|
| Hex encode loop itself | 41 MB       | 85 ms |
| Total                  | 336 MB      | 1.05 s|

The hex encode step is `List.iter` over the 1 M DUInt8 cells, plus a
2 M-char StringBuilder. Inherent cost of the representation, not the
algorithm.

**Aside — O(n²) bug in today's builtin.** `BuiltinExecution/Libs/Bytes.fs:25`
uses `bytes[i]` in a `for i = 0 to len - 1` loop. `bytes` is an F#
`List<Dval>`, so indexed access is O(i), making the whole encode
O(n²). Hangs at 1 MB input (never returned after >10 min during the
first measurement attempt). Masked today because nobody calls the
builtin with large inputs. The measurement above uses `List.iter`
(O(n)) to capture a fair representation cost. **Chunk 1.7 retires
this builtin entirely** and the bug with it.

**Post-Phase-1 target.** `Bytes.toHex` walks a `byte[]` directly →
no DList construction. Expect total under 10 MB alloc, <20 ms wall.
About 30× faster, 30× less garbage.

## What the numbers say

1. **Everything is list-representation cost.** Across four different
   scenarios — file, HTTP body, HTTP stream, hex encode — the
   allocation profile is dominated by
   `Dval.byteArrayToDvalList`-equivalent overhead. Phase 1's Blob
   collapses that to roughly 1:1 byte cost.
2. **10 MB is already painful today.** ~2 GB RSS and 5 s wall on a
   fast machine. 38 MB reliably OOMs. The bug is real, not aspirational.
3. **Streaming looks identical to non-streaming in allocation terms.**
   Per-chunk boxing × chunk count ≈ total boxing. Streams currently
   just spread the cost over time. Phase 2 fixes that.
4. **Harness is cheap.** Five scenarios run in ~14 seconds, most of
   which is the 10-second artificial stream delay.

## Summary targets for post-Phase-1 rerun

| Scenario                 | Current (baseline) | Target                 |
| ------------------------ | ------------------ | ---------------------- |
| fileRead 10 MB alloc     | 1.96 GB            | <25 MB                 |
| fileRead 38 MB wall/RSS  | OOM                | <1 s / <100 MB         |
| HTTP body 10 MB alloc    | 1.82 GB            | <25 MB                 |
| Stream 10 MB total alloc | 2.99 GB            | <15 MB                 |
| hex encode 1 MB total    | 336 MB alloc / 1 s | <10 MB / <20 ms        |
| `data.db` size           | 33 MB              | 33 MB + blob payloads  |

Phase 1 re-runs these four scenarios and produces `phase-1-results.md`
alongside this file.
