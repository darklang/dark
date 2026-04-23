# Phase 2 — Stream (Option E, generic lazy)

End state: `StreamingHttpClient.fs` deleted. `HttpClient.stream`
returns a first-class `DStream`. Users can `Stream.map`, `filter`,
`take`, `concat`, drain to `Blob` or `List<a>`. Generic over element
type: `Stream<UInt8>`, `Stream<String>`, `Stream<Event>`, etc.

Depends on Phase 1 (Blob) being complete — drain-to-blob is the
primary reification path.

## Scope principles

- Streams are pull-based and single-consumer. Locking internal;
  users see a single-threaded pull API.
- Lazy transformation: `Stream.map f s` builds a `Mapped` node; no
  work until drain.
- Disposal via .NET finalizer; explicit `Stream.close` exists as a
  hint only. Users should not need to call it in normal code.
- Non-persistable. Attempting to serialize a `DStream` raises —
  first Dval with a hard raise-on-write rule.
- Generic. `TStream of TypeReference` in PT, `KTStream of ValueType`
  in RT, `DStream of StreamImpl` in Dval.

## Runtime shape

```fsharp
// LibExecution/RuntimeTypes.fs

and StreamImpl =
  | FromIO of next: (unit -> Ply<option<Dval>>) * elemType: ValueType * disposer: (unit -> unit) option
  | Mapped of src: StreamImpl * fn: Applicable * elemType: ValueType
  | Filtered of src: StreamImpl * pred: Applicable
  | Take of src: StreamImpl * n: int64 * remaining: int64 ref
  | Concat of streams: StreamImpl list ref

and Dval =
  // ... existing cases ...
  | DStream of StreamImpl
```

Each drain operation (`Stream.next`, `toList`, `toBlob`, `fold`)
walks the tree. The internal monitor lives on the root `StreamImpl`
only — nested `Mapped`/`Filtered` don't take their own lock.

## Manual testing for Phase 2

After 2.13:
- Point `HttpClient.stream` at a real SSE endpoint (e.g. Anthropic
  messages streaming API). Drain and confirm events parse correctly.
- Hit `/robots.txt` on a real site with `HttpClient.stream`, drain
  to blob, compare length to content-length header.
- Kick off a stream, abandon it mid-drain (drop the reference),
  wait for GC, confirm no leak (`dotnet-counters` or similar).

## Chunks

### 2.1 — wire TStream of TypeReference through PT/RT/ValueType

Files:
- `backend/src/LibExecution/ProgramTypes.fs`: add
  `TStream of TypeReference`
- `backend/src/LibExecution/RuntimeTypes.fs`: add
  `TStream of TypeReference`, `KTStream of ValueType`
- `backend/src/LibExecution/ProgramTypesToRuntimeTypes.fs`: route
- `backend/src/LibExecution/TypeChecker.fs`: unify on element type
- `backend/src/LibExecution/Interpreter.fs`: match-arm coverage
- `backend/src/LibExecution/Dval.fs` `toValueType`: new branch

**Done when:** build green, existing tests pass. `TStream<UInt8>` is
a valid type but no Dval constructs it yet.

**Tests:** existing test-pass.

**Commit:** `stream: wire TStream and KTStream through pt/rt/valuetype`

### 2.2 — add DStream with StreamImpl; FromIO only

Files:
- `backend/src/LibExecution/RuntimeTypes.fs`: add `StreamImpl` DU
  with only the `FromIO` constructor for this chunk. Add
  `DStream of StreamImpl`.
- `backend/src/LibExecution/Dval.fs`: `newStream : ValueType -> (unit -> Ply<option<Dval>>) -> (unit -> unit) option -> Dval`; `readStreamNext : Dval -> Ply<option<Dval>>`.
- Internal lock: newtype a mutable struct with `obj` lock + `disposed: bool ref`; store on the root when wrapping.

**Done when:** F# test constructs a stream from a list, pulls
elements one by one, gets `None` on exhaustion.

**Tests:**
- F# (`backend/tests/Tests/Stream.Tests.fs`, new): basic pull from a
  FromIO over an in-memory list; second drain after Done returns
  `None` / consumed sentinel.

**Commit:** `stream: add DStream with FromIO constructor`

### 2.3 — add binary serializer — DStream raises on write

Files:
- `backend/src/LibSerialization/Binary/Serializers/PT/TypeReference.fs`:
  tag for `TStream`
- `backend/src/LibSerialization/Binary/Serializers/RT/TypeReference.fs`:
  mirror
- `backend/src/LibSerialization/Binary/Serializers/RT/ValueType.fs`:
  tag for `KTStream`
- `backend/src/LibSerialization/Binary/Serializers/RT/Dval.fs`:
  `DStream` write raises with message
  "cannot serialize stream — drain to blob first"; read path never
  fires (if it does, also raises with "corrupt: stream tag")
- `backend/src/LibCloud/DvalReprInternalRoundtrippable.fs`: mirror
  raise
- `backend/src/LibCloud/DvalReprInternalQueryable.fs`: mirror raise

**Done when:** F# test attempts to serialize a `DStream` and
receives the expected exception.

**Tests:**
- F# `Stream.Tests.fs`: raise-on-serialize assertion.

**Commit:** `stream: binary serializers for tstream; dstream raises`

### 2.4 — add PT↔Dark and RT↔Dark bridges for stream

Files:
- `backend/src/LibExecution/ProgramTypesToDarkTypes.fs`: add
  `TStream` case to `TypeReference` toDT/fromDT.
- `backend/src/LibExecution/RuntimeTypesToDarkTypes.fs`: add
  `TStream`, `KTStream`; `DStream` renders as an elided Dark record
  `{ kind: "stream", elementType: ..., state: "fresh|partial|done" }`.

**Done when:** LSP path still works; roundtrip test for `TStream<UInt8>`
passes.

**Tests:**
- F# roundtrip test for `TStream` across the bridge.

**Commit:** `stream: pt↔dark and rt↔dark bridges for stream types`

### 2.5 — Stream builtins: next, toList, toBlob (no transforms yet)

File: `backend/src/BuiltinExecution/Libs/Stream.fs` (new).

Builtins:
- `Stream.next : Stream<a> -> Option<a>`
- `Stream.toList : Stream<a> -> List<a>`
- `Stream.toBlob : Stream<UInt8> -> Blob` (drain into one ephemeral
  blob)
- `Stream.close : Stream<a> -> Unit` (optional; idempotent)

Also inline in this file, at top of module:

```
// CLEANUP revisit: considered alternatives we did not take.
// - Channels (Go-style with separate reader/writer ends): adds
//   synchronization semantics Dark does not have elsewhere. Skip
//   for v1; revisit if fan-in/fan-out becomes a real use case.
// - Actor-mailbox (Erlang-style, with a scheduler): out of scope;
//   Dark has no scheduler story.
```

Also add a `// CLEANUP sub-blob slicing` comment near `toBlob` —
currently copies; could share bytes with the source if profiles
demand (see 40-later L.5).

**Done when:** Dark expression
`(Builtin.Stream.fromList [1L; 2L; 3L] |> Builtin.Stream.toList)` returns `[1L; 2L; 3L]`.

**Tests:**
- F# `Stream.Tests.fs`: `toList`, `toBlob` (over `Stream<UInt8>`),
  `close` idempotence.
- .dark: `backend/testfiles/execution/stdlib/stream.tests` (new).

**Commit:** `stream: next, toList, toBlob, close builtins`

### 2.6 — lazy transforms: Mapped, Filtered, Take, Concat

File: `backend/src/LibExecution/RuntimeTypes.fs`

Extend `StreamImpl` with `Mapped`, `Filtered`, `Take`, `Concat`.
Extend the drain logic (`readStreamNext` in `Dval.fs`) to walk these
nodes. Mapped/Filtered invoke user lambdas — safe now that PR 5640
moved caches onto ExecutionState.

**Done when:** Internal F# helper can construct a `Mapped(FromIO(...), fn)` and drain it.

**Tests:**
- F# `Stream.Tests.fs`: mapped/filtered/take/concat correctness in
  isolation; drain order; early termination on Take.

**Commit:** `stream: lazy transforms mapped filtered take concat`

### 2.7 — Stream builtin map/filter/take/concat

File: `backend/src/BuiltinExecution/Libs/Stream.fs`

Add:
- `Stream.map : (a -> b) -> Stream<a> -> Stream<b>`
- `Stream.filter : (a -> Bool) -> Stream<a> -> Stream<a>`
- `Stream.take : Int64 -> Stream<a> -> Stream<a>`
- `Stream.concat : List<Stream<a>> -> Stream<a>`

Each just wraps the corresponding `StreamImpl` constructor.

**Done when:**
`Stream.fromList [1L..10L] |> Stream.filter even |> Stream.map (+ 1L) |> Stream.take 3L |> Stream.toList` yields `[3L; 5L; 7L]`.

**Tests:**
- .dark: add these compositional cases to `stream.tests`.

**Commit:** `stream: map filter take concat builtins`

### 2.8 — HttpClient.stream returns DStream

File: `backend/src/BuiltinExecution/Libs/HttpClient.fs`

Add `HttpClient.stream : method -> url -> headers -> Result<StreamResponse, String>` where `StreamResponse` contains
headers and a `Stream<UInt8>` body.

Implementation wraps the `HttpResponseMessage`'s body stream in a
`FromIO` with the disposer calling `.Dispose()` on the response.

**Done when:** Dark expression hitting a loopback Kestrel server
returns a streaming body that drains correctly.

**Tests:**
- F# `Stream.Tests.fs`: HTTP stream test against in-process Kestrel;
  drain; compare length.

**Commit:** `stream: HttpClient.stream returns DStream`

### 2.9 — delete StreamingHttpClient.fs; rebuild SSE as Dark-side

Files:
- Delete `backend/src/BuiltinExecution/Libs/StreamingHttpClient.fs`
- Delete `packages/darklang/stdlib/streamingHttpClient.dark`
- Remove from `BuiltinExecution.fsproj`
- Add `packages/darklang/stdlib/sse.dark` (new): SSE parser as Dark
  code over `Stream<UInt8>` — splits on double-newline, parses
  `data:` / `event:` / `id:` fields, emits
  `Stream<Darklang.Stdlib.Sse.Event>`.

**Done when:**
`HttpClient.stream "GET" sseUrl [] |> Sse.parse |> Stream.toList` returns the expected list of events.

**Tests:**
- .dark: `sse.tests` with a canned SSE body served from Kestrel.

**Manual check:** real SSE endpoint (Anthropic messages streaming
API or similar) via the CLI. See Phase 2 manual testing above.

**Commit:** `stream: delete streamingHttpClient; rebuild sse in dark`

### 2.10 — update tree-sitter grammar + Dark parser/pretty-printer

Files:
- `tree-sitter-darklang/grammar.js`: add `Stream<T>` generic to
  `builtin_type` (or introduce a `stream_type_reference` entry
  parallel to `list_type_reference`). Regenerate.
- `packages/darklang/languageTools/parser/typeReference.dark`: add
  `Stream` handling.
- `packages/darklang/prettyPrinter/programTypes.dark` and
  `runtimeTypes.dark`: pretty-print `TStream<UInt8>` as
  `Stream<UInt8>`. DStream pretty-prints as
  `<stream of UInt8: fresh>` / `...partial>` / `...done>`.

**Done when:** `let x: Stream<UInt8> = HttpClient.stream ...` parses
end-to-end.

**Tests:** existing parser/printer tests; one new roundtrip for a
`Stream<UInt8>` signature.

**Commit:** `stream: tree-sitter grammar and dark parser/printer`

### 2.11 — F# drain + disposal + lazy-ordering tests

File: `backend/tests/Tests/Stream.Tests.fs`

Cases:
- Drain order preserves producer order.
- Lazy: building `.map |> .filter |> .take 5` over an infinite
  stream terminates (produces 5 elements only).
- Disposal: drop reference to a stream backed by a fake
  `IDisposable` IO source; force GC; assert Disposed called.
- Abandoned mid-drain: after N elements, stop calling `next`; GC;
  disposer fires.
- Consumed error: drain to Done; next `next` returns the consumed
  error sentinel.

**Commit:** `stream: drain, disposal, lazy-ordering tests`

### 2.12 — .dark stream tests

File: `backend/testfiles/execution/stdlib/stream.tests`

Cases (on top of what 2.5/2.7 already added):
- Empty stream: `Stream.fromList [] |> Stream.toList = []`
- Concat: `Stream.concat [s1; s2] |> Stream.toList = s1 ++ s2`
- Take more than available: returns available only
- Filter all out: `Stream.filter (fun _ -> false) s |> Stream.toList = []`

**Commit:** `stream: .dark compositional tests`

### 2.13 — validate against real SSE endpoint (manual)

Not an automated chunk — the agent logs completion in
`phase-2-results.md` along with a short "ran against X, saw Y"
note. If the agent cannot access a real endpoint, it marks this as
a manual-test item open for the user and proceeds.

**Commit:** `stream: log manual sse validation in results` (only if
the validation actually ran; otherwise skip commit and leave a
blocker).

### 2.14 — capture phase-2-results.md

Re-run Phase 0 streaming scenario (3) and compare. Write
`thinking/blobs-and-streams/phase-2-results.md` with:
- streaming time-to-first-chunk
- allocation per chunk (should be near zero — just the ref and the
  buffer)
- `StreamingHttpClient.fs` gone from `BuiltinExecution.fsproj`
- Dark-side SSE parser works against canned and (if available)
  live endpoint.

**Commit:** `stream: write phase-2 results with before/after diff`

## Phase 2 exit criteria

- `StreamingHttpClient.fs` and its .dark counterpart deleted.
- `HttpClient.stream` returns `DStream`; SSE works.
- Per-chunk allocation for streamed HTTP bodies is near zero
  (independent of total body size).
- No tests regress; build green; formatted.
- `phase-2-results.md` exists and tells a legible story.

## Phase 2 open TODOs carried to Later

- Scope-based ephemeral-blob lifetime (L.1) — ephemeral blobs
  created during a long-running handler still accumulate until VM
  exits.
- Val persistability guard (L.2) — `val x = Builtin.HttpClient.stream ...` currently succeeds silently because `DStream` raises only at serialize time; the guard at `Inserts.fs` would catch it earlier with a friendlier message.
