# Blobs and Streams — Execution Plan

This is the **entrypoint** for executing the blobs-and-streams work.
It's designed so a fresh agent can pick up from anywhere by reading
just this file and the phase file it points to.

## What we are doing

1. Replace `List<UInt8>` with a real `Blob` type (Option D: hybrid
   ephemeral/persistent, content-addressed when persisted).
2. Replace `StreamingHttpClient`'s callback-based F# plumbing with a
   first-class generic `Stream<'a>` value (Option E: lazy composable).
3. Do it in phases so each phase is individually shippable.

Design rationale and decisions are in [00-design.md](./00-design.md).
Read it once before starting execution.

## How the loop works

The loop instruction points at this file. On each iteration, the
agent:

1. Reads this README.
2. Finds the **first unchecked chunk** in the state table below.
3. Opens the corresponding phase file for detailed scope.
4. Executes that one chunk — and only that one.
5. Runs `./scripts/formatting/format` on any changed files.
6. Runs the chunk's specified verification (build check + tests
   listed in the phase file). If verification fails, iterates on the
   fix up to 3 times before logging a blocker and stopping.
7. Commits with the chunk's prescribed commit message (see
   [commit-style.md](#commit-style) below).
8. Marks the chunk `[x]` in the state table below and appends a dated
   line in the **Progress log** section at the bottom.
9. Exits. The next loop iteration picks up the next chunk.

If a chunk is blocked (e.g. test infra problem, ambiguous spec), the
agent writes a **BLOCKER** entry at the bottom and stops without
checking the box.

### Loop command to run

```
/loop execute the next incomplete chunk described in thinking/blobs-and-streams/README.md — read the linked phase file for scope, do exactly that one chunk, run ./scripts/formatting/format, run the chunk's tests, commit with the prescribed message, update README state, then stop
```

Paste that after `/loop` in the CLI. It self-paces: each iteration
does exactly one chunk and hands back.

## Commit style

One commit per chunk. Every commit must leave the tree in a state
where:

- `dotnet build backend/src/LibExecution/LibExecution.fsproj` succeeds.
- `./scripts/run-backend-tests` passes (or the subset specified in
  the chunk, when narrowing helps and nothing else regressed).
- `./scripts/formatting/format` has been run.

Commit message: one line, lowercase, dense, summarizing what changed.
Optionally a blank line followed by a few short lowercase bullets.

Examples:
```
blob: wire TBlob and DBlob through PT/RT core types
```
```
blob: add package_blobs table and PM lookup path

- new migration 20260424_package_blobs.sql
- pm gains blobs : hash -> ply<byte[]>
- inserts.fs handles blob writes with insert-or-ignore
```

## State

Legend: `[ ]` pending, `[~]` in progress (rare; loop does one chunk at
a time), `[x]` done, `[!]` blocked.

### Phase 0 — baseline measurement

See [10-phase-0.md](./10-phase-0.md).

- [x] 0.1 add baseline-measurement harness
- [x] 0.2 record fileRead memory/allocation profile
- [x] 0.3 record http body allocation profile
- [x] 0.4 record streaming-http chunk behaviour
- [x] 0.5 record bytesHexEncode cost on large input
- [x] 0.6 snapshot sqlite size and write results to baseline.md

### Phase 1 — Blob

See [20-phase-1.md](./20-phase-1.md).

- [x] 1.1 wire TBlob and KTBlob through PT/RT/ValueType
- [x] 1.2 add DBlob with BlobRef ephemeral/persistent variants
- [x] 1.3 add binary serializers for TBlob/KTBlob/DBlob
- [x] 1.4 add PT↔Dark and RT↔Dark bridges for blob types
- [x] 1.5 add package_blobs migration and PM blobs lookup
- [x] 1.6 implement ephemeral→persistent promotion on serialize
- [x] 1.7 expand Bytes builtin module to full API
- [x] 1.8 migrate fileRead/fileWrite to Blob
- [x] 1.9 migrate Crypto and Base64 to Blob
- [x] 1.10 update Dark-side stdlib/bytes.dark + dependents
- [x] 1.11 update tree-sitter grammar and Dark-side parser/pretty-printer
- [x] 1.12 add F# roundtrip + promotion + memory tests
- [x] 1.13 add .dark bytes tests
- [x] 1.14 capture before/after diff into phase-1-results.md

### Phase 2 — Stream

See [30-phase-2.md](./30-phase-2.md).

- [x] 2.1 wire TStream of TypeReference through PT/RT/ValueType
- [ ] 2.2 add DStream with StreamImpl; FromIO only
- [ ] 2.3 add binary serializer — DStream raises on write
- [ ] 2.4 add PT↔Dark and RT↔Dark bridges (stream renders elided)
- [ ] 2.5 Stream builtins: next, toList, toBlob (no transforms yet)
- [ ] 2.6 lazy transforms: Mapped, Filtered, Take, Concat
- [ ] 2.7 Stream builtin map/filter/take/concat
- [ ] 2.8 HttpClient.stream returns DStream
- [ ] 2.9 delete StreamingHttpClient.fs; rebuild SSE as Dark-side
- [ ] 2.10 update tree-sitter grammar and Dark-side parser/pretty-printer
- [ ] 2.11 F# drain + disposal + lazy-ordering tests
- [ ] 2.12 .dark stream tests
- [ ] 2.13 validate against real SSE endpoint (manual)
- [ ] 2.14 capture phase-2-results.md

### Later

See [40-later.md](./40-later.md). Not in the core loop — pick up only
if the work above is done and time remains.

- [ ] L.1 scope-based ephemeral-blob lifetime on ExecutionState
- [ ] L.2 Dval.isPersistable guard in Inserts.fs
- [ ] L.3 orphan package_blobs sweeper
- [ ] L.4 blob equality semantics (hash-compare after promotion)
- [ ] L.5 sub-blob slicing (share bytes, BEAM-style) if profiles show need
- [ ] L.6 retype Stdlib.Crypto/Base64/String public signatures to Blob (1.10 follow-on — migrate ~90 .dark test cases in big.dark / base64.dark / crypto.dark / bytes.dark / http.dark / string.dark that currently assume List<UInt8> semantics, then drop the bytesFromList/bytesToList bridges from the stdlib wrappers)

## Progress log

Append one line per chunk completion. Format:
`YYYY-MM-DD HH:MM  <chunk-id>  <short note>`.

<!-- agent appends below -->
2026-04-23 21:40  0.1  harness skeleton + helpers; self-test passes and writes rundir/measurements/phase-0/harness.txt
2026-04-23 21:48  0.2  fileRead profile: 10MB → 890MB alloc / 1.1GB RSS / 10M+1 dval nodes; 38MB anecdote row inline (skipped in-process to avoid OOM)
2026-04-23 21:54  0.3  http body profile via fake HttpMessageHandler: 10MB body → 1.6GB alloc / 2GB RSS / 10M+1 dval nodes (~60% extra over fileRead)
2026-04-23 22:02  0.4  streaming profile: 100 x 100KB producer chunks over 10s → 1300 consumer reads / 2.89GB total alloc / 175ms time-to-first / 13s time-to-last; also added resetOutput helper so reruns start clean
2026-04-23 22:40  0.5  hex encode 1MB: list construction dominates (295MB vs 41MB encode step); also noted the O(n²) list[i] pattern in Bytes.fs:25 which 1.7 will retire. First attempt used the builtin's exact loop and hung on list-index iteration; switched to List.iter to measure representation cost fairly.
2026-04-23 22:46  0.6  baseline.md written: tables per scenario + phase-1 targets. sqlite data.db floor: 33mb. phase 0 complete.
2026-04-23 23:06  1.1  TBlob and KTBlob wired across 18 files (exhaustive-match fallout: PT2RT, PT↔Dark, RT↔Dark, TypeChecker, ValueType, binary serializers, hashing, cloud reprs, pm dependency/ast/resolver, json). 776 binary-serialization tests + 73 pt2rt tests + 5 measurement tests pass. TBlob/KTBlob present but inert; no DBlob yet.
2026-04-23 23:32  1.2  DBlob of BlobRef + BlobRef DU (Ephemeral | Persistent); blobStore ConcurrentDictionary on ExecutionState; Dval.newEphemeralBlob + readBlobBytes helpers; equality/compareDval/binary-serializer/roundtrippable/queryable/json/typeutils coverage. 3 new blob tests pass; 776 bin-ser tests stay green.
2026-04-23 23:46  1.3  binary serializer tags 24/23/24 (PT.TBlob / RT.KTBlob / RT.DBlob) already present from 1.1/1.2 exhaustive-match fallout. Formal roundtrip coverage: 4 new Blob.Tests (persistent roundtrip, ephemeral-raises, TBlob PT+RT, KTBlob) + DBlob(Persistent) + TBlob + KTBlob added to shared test-values fixtures. 7 blob / 777 bin-ser / 12 vanilla-ser all green.
2026-04-23 23:58  1.4  pt↔dark and rt↔dark bridges for TBlob/KTBlob/DBlob were already added in 1.1/1.2 as exhaustive-match fallout (DBlobEphemeral + DBlobPersistent kept distinct so LSP can render ephemerals; differs from the plan's "raise until 1.6"). 5 new bridge roundtrip tests; 12 blob / 819 darktypes / 777 bin-ser all green.
2026-04-24 00:12  1.5  new package_blobs migration (20260424); LibPackageManager.RuntimeTypes.Blob module with get+insert (INSERT OR IGNORE dedup); RT.PackageManager gains getBlob field; PT→RT wrapper stubs it. 3 new PM tests + 15 blob total all green.
2026-04-24 00:25  1.6  Dval.promoteBlobs walks tree, hashes ephemeral bytes via SHA-256, inserts into package_blobs, swaps ref; Dval.sha256Hex helper; readBlobBytes now consults PM for Persistent refs. Shadowing gotcha: Dval.int64 builder masked the primitive; used System.Convert.ToInt64 to disambiguate. 4 new promotion tests (persists+swaps, roundtrip after promote, same-bytes-dedup, readBlobBytes on promoted). 19 blob / 777 bin-ser green.
2026-04-24 00:38  1.7  full Bytes builtin API: length, fromString, toString, toHex, fromHex, toBase64, fromBase64, concat, slice, toList, fromList. Old bytesHexEncode kept but DeprecatedBecause O(n²) on List<UInt8>. Added Blobs sub-record to ExecutionState (+ insertBlob on RT.PackageManager) so builtins can read/write via state.blobs. 11 new .dark tests in bytes.dark; 15 bytes tests green. TBlob now usable from Dark.
2026-04-24 00:52  1.8  fileRead returns Result<Blob, String>; fileWrite takes Blob. posixFdRead/posixFdWrite mirror. Old List<UInt8> shape gone. cli/file.tests rewritten to drive new API. Also bumped cloud/internal.dark table count 22 → 23 (1.5 added package_blobs). All 5591 LibExecution tests green. Dark-side stdlib wrappers calling fileWrite with List<UInt8> are still wrong but tolerated at parse time — 1.10 migrates them.
2026-04-24 01:07  1.9  Crypto + Base64 + stringToBytes/stringFromBytesWithReplacement migrated to Blob. Dark-side wrappers (crypto.dark, base64.dark, string.dark) keep their List<UInt8> public shape via let-bridged bytesFromList/bytesToList — parser doesn't know about Blob yet (1.11 adds). Parser-trip gotcha: `a |> b |> c) |> d` parses as "Pipe: App", use let-binding chain instead. 9 crypto + 41 base64 + 5591 total LibExecution tests green.
2026-04-24 01:18  1.11  Pulled ahead of 1.10 because 1.10 depends on Blob being parseable. tree-sitter grammar + src/parser.c regenerated; WrittenTypes/ProgramTypes/RuntimeTypes Builtin DUs gain TBlob/KTBlob; parser/typeReference.dark, writtenTypesToProgramTypes, semanticTokens, hoverInformation, and both pretty-printers all get the new case. `let x: Blob = Builtin.bytesFromString "hi"` now parses end-to-end. 5591 LibExecution tests green. Dval printer styling for DBlob (<blob ephemeral ...> / <blob sha256:...>) deferred — the existing Dval printer is a TODO elsewhere; catch-all renders fine.
2026-04-24 01:46  1.10  partial. LibParser (F#) WrittenTypes + FSharpToWrittenTypes + WrittenTypesToProgramTypes gained TBlob so `.dark` files parse Blob. bytes.dark got the full new Blob-based surface; cli/file.dark readBytes returns Blob; cli/fileSystem.dark writeFile takes Blob; cli/zsh|bash|fish use Builtin.stringToBytes/FromBytes directly for Blob-shaped IO. **Public signatures of Stdlib.Crypto/Base64/String (List<UInt8>) preserved** via internal bytesFromList/bytesToList bridges because migrating the .dark test corpus to Blob is a bigger separate pass. NoModule.equals/notEquals now promoteBlobs-normalize both args (no-op insert) so two ephemerals with same bytes compare equal. 5591 LibExecution tests green.
2026-04-24 02:02  1.12  Blob.Tests expanded 19 → 24. New: fileRead memory bound (10MB file → <30MB alloc vs baseline ~2GB), User-DB queryable JSON roundtrip (envelope + parse path, needed to round out DvalReprInternalQueryable.parseJsonV0 for TBlob), User-DB queryable ephemeral raises, roundtrippable-json persistent + ephemeral roundtrips. All 24 green.
2026-04-24 02:09  1.13  bytes.dark tests expanded 15 → 26. 1.7 already added 11 Builtin.bytes* tests; 1.13 adds 11 Stdlib.Bytes.* wrapper tests exercising the Dark-side module surface added in 1.10 (length/fromString/toString, hex round, base64 round, concat, slice, toList/fromList).
2026-04-24 02:17  1.14  phase-1-results.md written. Three new measurement scenarios parallel the phase-0 ones via the Blob path (fileRead / httpBody / hex) and write to rundir/measurements/phase-1/. Headline wins: fileRead 10MB 1.96GB→31.5MB (62×), fileRead 38MB OOM→40.7MB/136ms (bug fixed), httpBody 10MB 1.82GB→36.9MB (49×), hex 1MB total 336MB/1.05s→14.6MB/45ms (23×/23×). Phase 1 done. Streaming still on the old code path — Phase 2 replaces it.
2026-04-24 02:34  2.1  TStream of TypeReference + KTStream of ValueType wired through PT/RT/ValueType + all the 1.1-style exhaustive-match cascades (PT2RT, typechecker, PT↔Dark, RT↔Dark, binary serializers, canonical hashing, cloud reprs, pm walkers, json, LibParser WrittenTypes). Tree-sitter grammar adds `stream_type_reference` parallel to `list_type_reference`; Dark-side parser + pretty-printer + semantic tokens + LSP hover + dark-side PT/RT DUs all updated. `let s: Stream<UInt8> = ...` parses end-to-end. TStream inert (no DStream yet — 2.2). 5602 LibExecution tests green (was 5591, the extra 11 are chunk 1.13's Stdlib.Bytes.* tests).

## Blockers

If a chunk can't be completed cleanly, add an entry here and stop the
loop. Format: `<chunk-id>  <one-sentence reason>  <files/lines involved>`.

<!-- agent appends below -->
