# Benchmark results

Snapshot at the latest commit. Run via `./scripts/run-local-exec bench`.
Raw JSON: `results/latest.json`. `overhead = allocBytes / inputBytes`.

## fileRead

Read a binary file off disk into a Blob.

**Now:**

| input bytes | alloc bytes | overhead | ms | dval nodes |
|------------:|------------:|---------:|---:|-----------:|
|       1,024 |      88,408 |   86.34x |  0 |          1 |
|     100,000 |     179,320 |    1.79x |  0 |          1 |
|   1,000,000 |   1,075,024 |    1.08x |  0 |          1 |
|  10,000,000 |  10,081,768 |    1.01x |  4 |          1 |
|  38,000,000 |  38,073,824 |    1.00x | 19 |          1 |

**Before (List<UInt8>):**

| input bytes |   alloc bytes | overhead |    ms | dval nodes |
|------------:|--------------:|---------:|------:|-----------:|
|       1,024 |     4,524,088 |   ~4400x |    21 |      1,025 |
|     100,000 |    67,703,344 |    ~677x |   483 |    100,001 |
|   1,000,000 |   326,688,464 |    ~327x |   909 |  1,000,001 |
|  10,000,000 | 2,238,099,880 |    ~224x | 5,182 | 10,000,001 |
|  38,000,000 |          OOM* |     OOM* |  OOM* |       OOM* |

*38MB OOM-killed at ~49GB RSS. **Every byte was a boxed `DUInt8`**;
10MB → 10,000,001 Dvals. Now: 1.

## httpBody

Simulated HTTP response, body read into a Blob.

| input bytes | alloc bytes | overhead | ms | dval nodes |
|------------:|------------:|---------:|---:|-----------:|
|     100,000 |     339,192 |    3.39x |  5 |          1 |
|   1,000,000 |   2,077,600 |    2.08x |  0 |          1 |
|  10,000,000 |  20,082,048 |    2.01x |  9 |          1 |

Pre-Blob baseline: 10MB body = 2.31GB alloc, 5.5s, 10,000,001 Dvals.

## hexEncode (1MB)

| alloc bytes | overhead | ms |
|------------:|---------:|---:|
|   4,067,936 |    4.07x |  4 |

The 4x is structurally optimal for UTF-16 strings (2 bytes per hex
char × 2 chars per input byte). Pre-Blob: 654MB / 1.75s; the encode
loop also had an O(n²) list-index quirk in the old code path.

## base64Encode (1MB)

| alloc bytes | overhead | ms |
|------------:|---------:|---:|
|   2,744,248 |    2.74x |  2 |

`4*input/3` chars × 2 bytes/char ≈ 2.67MB output + ~80KB overhead. Floor.

## manyBlobs

Many small ephemeral blobs in one scope, each with distinct bytes. The
*allocBytes* here is what newEphemeralBlob adds *on top* of the pre-
allocated payloads, so the alloc/input ratio is misleading; the
useful number is per-blob overhead.

| count × size | total bytes | alloc bytes | per-blob overhead | ms |
|-------------:|------------:|------------:|------------------:|---:|
|   100 × 1024 |    102,400  |     102,408 |   ~1,024 bytes\*  |  0 |
|  1000 × 1024 |  1,024,000  |     452,264 |     ~452 bytes    |  1 |
| 10000 ×  256 |  2,560,000  |   3,138,960 |     ~314 bytes    | 13 |

\*The 1024-byte/blob figure for 100×1024B is dominated by
ConcurrentDictionary capacity expansion. At 1k+ blobs it amortises to
~300-450 bytes — roughly the cost of a Guid + a dict entry +
the DBlob wrapper.

## blobEqualityEphemeral

Two independently-built ephemeral blobs with the same bytes, compared
via the `=` builtin (which goes through `promoteBlobs` with a no-op
insert + sha256 hash). No DB writes.

| input bytes | alloc bytes | overhead | ms |
|------------:|------------:|---------:|---:|
|     100,000 |      90,200 |    0.90x |  9 |
|   1,000,000 |      76,776 |    0.08x |  2 |
|  10,000,000 |      73,544 |    0.01x |  9 |

Allocation is essentially constant (~75-90KB regardless of size) — the
SHA-256 stream doesn't allocate, only the wrapper Dvals. CPU is
another story (10MB hashed twice).

## streamToBlob

Drain a chunked Stream<UInt8> into a Blob (the L.7 fast path).

| input bytes | alloc bytes | overhead | ms |
|------------:|------------:|---------:|---:|
|     100,000 |     282,048 |    2.82x |  3 |
|   1,000,000 |   2,082,048 |    2.08x |  0 |
|  10,000,000 |  20,082,048 |    2.01x |  9 |

Matches httpBody — same MemoryStream-grow-and-resize cost. Pre-L.7
byte-by-byte drain was ~600x.

## multipart

Build a Blob by concatenating many parts (MultipartRequest pattern in
audio.dark).

| parts × size | total bytes | alloc bytes | overhead | ms |
|-------------:|------------:|------------:|---------:|---:|
|    10 × 1024 |      10,240 |     130,608 |   12.75x |  0 |
|   100 × 10KB |   1,024,000 |   3,725,616 |    3.64x |  0 |
|   50 × 100KB |   5,000,000 |  17,782,192 |    3.56x |  5 |

Each part allocates an ephemeral blob; the final concatenated blob
allocates a fresh byte[]. So expected overhead = 1.0 (parts) + 1.0
(final) + 1.0 (MemoryStream growth) = ~3x. Small inputs are
fixed-cost-dominated.

## Headlines

- fileRead 1.01x is essentially the floor for any runtime.
- httpBody/streamToBlob 2.01x matches the universal "buffered body"
  shape (Go/Rust/Node all sit there).
- hexEncode 4.07x is structurally optimal for .NET (UTF-16); 2x in
  UTF-8 runtimes, same byte count.
- Equality is alloc-cheap (~75KB independent of size).
- Per-blob overhead in tight loops: ~300-450 bytes.
- Pre-Blob List<UInt8> path was 200-650x worse — the migration paid
  for itself orders of magnitude over.
- Streaming HTTP element-wise still has ~6x overhead from per-Ply
  continuation cost — not addressed here, lives in the Ply-question
  follow-on.
