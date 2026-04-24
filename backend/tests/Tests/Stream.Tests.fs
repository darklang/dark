/// Tests for the Stream Dval.
///
/// Covers pull-only FromIO streams, transform nodes
/// (Mapped/Filtered/Take/Concat), drain helpers (toList/toBlob),
/// disposal, and the chunked bulk-drain fast path.
module Tests.Stream

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module VT = LibExecution.ValueType
module BS = LibSerialization.Binary.Serialization
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes


/// Build a stream backed by an in-memory list. Each `next` call pulls
/// the head off a mutable ref cell. Simple scaffolding for the pull
/// semantics; real IO-backed streams live in HttpClient.stream.
let private streamOfList
  (items : List<RT.Dval>)
  (elemType : RT.ValueType)
  : RT.Dval =
  let remaining = ref items
  let next () : Ply<Option<RT.Dval>> =
    uply {
      match remaining.Value with
      | head :: tail ->
        remaining.Value <- tail
        return Some head
      | [] -> return None
    }
  Dval.newStream elemType next None


let pullsElementsInOrder =
  testTask "stream: pull returns elements in order then None" {
    let items = [ RT.DInt64 1L; RT.DInt64 2L; RT.DInt64 3L ]
    let s = streamOfList items VT.int64

    let! first = Dval.readStreamNext s |> Ply.toTask
    let! second = Dval.readStreamNext s |> Ply.toTask
    let! third = Dval.readStreamNext s |> Ply.toTask
    let! fourth = Dval.readStreamNext s |> Ply.toTask

    Expect.equal first (Some(RT.DInt64 1L)) "first pull = 1"
    Expect.equal second (Some(RT.DInt64 2L)) "second pull = 2"
    Expect.equal third (Some(RT.DInt64 3L)) "third pull = 3"
    Expect.equal fourth None "fourth pull = None (exhausted)"
  }


let staysDrainedAfterExhaustion =
  testTask "stream: subsequent pulls after exhaustion keep returning None" {
    let s = streamOfList [ RT.DInt64 42L ] VT.int64

    let! first = Dval.readStreamNext s |> Ply.toTask
    let! afterExhaust = Dval.readStreamNext s |> Ply.toTask
    let! stillDone = Dval.readStreamNext s |> Ply.toTask

    Expect.equal first (Some(RT.DInt64 42L)) "first pull = 42"
    Expect.equal afterExhaust None "pull past end = None"
    Expect.equal stillDone None "still None on further pulls"
  }


let emptyStreamImmediatelyDone =
  testTask "stream: empty FromIO returns None on first pull" {
    let s = streamOfList [] VT.int64
    let! result = Dval.readStreamNext s |> Ply.toTask
    Expect.equal result None "empty stream = None"
  }


let toValueTypeReflectsElemType =
  test "stream: Dval.toValueType returns KTStream of the element type" {
    let s = streamOfList [ RT.DString "hi" ] VT.string
    let vt = RT.Dval.toValueType s
    Expect.equal
      vt
      (RT.ValueType.Known(RT.KTStream VT.string))
      "toValueType reflects the elem type"
  }


let dstreamBinarySerializeRaises =
  testTask "DStream binary serialization raises (streams aren't persistable)" {
    let s = streamOfList [ RT.DInt64 1L ] VT.int64
    let mutable raised = false
    try
      let _ = BS.RT.Dval.serialize "dval" s
      ()
    with _ ->
      raised <- true
    Expect.isTrue
      raised
      "expected serialize(DStream) to raise; streams are non-persistable by design"
  }


let tstreamPtBinaryRoundtrip =
  test "PT.TStream roundtrips through the binary type-reference serializer" {
    let inner = PT.TInt64
    let bytes =
      let s = new System.IO.MemoryStream()
      let w = new System.IO.BinaryWriter(s)
      LibSerialization.Binary.Serializers.PT.TypeReference.write w (PT.TStream inner)
      s.ToArray()
    let restored =
      let r = new System.IO.BinaryReader(new System.IO.MemoryStream(bytes))
      LibSerialization.Binary.Serializers.PT.TypeReference.read r
    Expect.equal restored (PT.TStream PT.TInt64) "PT.TStream<Int64> roundtrips"
  }


let tstreamRtBinaryRoundtrip =
  test "RT.TStream roundtrips through the binary type-reference serializer" {
    let original = RT.TStream RT.TBlob
    let bytes =
      let s = new System.IO.MemoryStream()
      let w = new System.IO.BinaryWriter(s)
      LibSerialization.Binary.Serializers.RT.TypeReference.write w original
      s.ToArray()
    let restored =
      let r = new System.IO.BinaryReader(new System.IO.MemoryStream(bytes))
      LibSerialization.Binary.Serializers.RT.TypeReference.read r
    Expect.equal restored original "RT.TStream<Blob> roundtrips"
  }


let ktstreamBinaryRoundtrip =
  test "KTStream roundtrips through the RT ValueType binary serializer" {
    let original = RT.ValueType.Known(RT.KTStream VT.string)
    let bytes =
      let s = new System.IO.MemoryStream()
      let w = new System.IO.BinaryWriter(s)
      LibSerialization.Binary.Serializers.RT.ValueType.write w original
      s.ToArray()
    let restored =
      let r = new System.IO.BinaryReader(new System.IO.MemoryStream(bytes))
      LibSerialization.Binary.Serializers.RT.ValueType.read r
    Expect.equal restored original "KTStream<String> ValueType roundtrips"
  }


let tstreamPtDarkBridge =
  test "PT.TStream roundtrips through the Dark-side bridge" {
    let original = PT.TStream PT.TUInt8
    let restored = PT2DT.TypeReference.fromDT (PT2DT.TypeReference.toDT original)
    Expect.equal restored original "PT.TStream<UInt8> survives pt<->dark"
  }


let tstreamRtDarkBridge =
  test "RT.TStream roundtrips through the Dark-side bridge" {
    let original = RT.TStream RT.TBlob
    let restored = RT2DT.TypeReference.fromDT (RT2DT.TypeReference.toDT original)
    Expect.equal restored original "RT.TStream<Blob> survives rt<->dark"
  }


let ktstreamRtDarkBridge =
  test "RT.KTStream roundtrips through the Dark-side ValueType bridge" {
    let original = RT.ValueType.Known(RT.KTStream VT.string)
    let restored = RT2DT.ValueType.fromDT (RT2DT.ValueType.toDT original)
    Expect.equal restored original "KTStream<String> survives rt<->dark"
  }


let streamCloseMarksDisposed =
  testTask "Stream.close flips disposed; subsequent next returns None" {
    let s = streamOfList [ RT.DInt64 42L ] VT.int64

    // Replicate what the Stream.close builtin does: flip the flag.
    match s with
    | RT.DStream(_, disposed, _) -> disposed.Value <- true
    | _ -> failtest "expected DStream"

    let! after = Dval.readStreamNext s |> Ply.toTask
    Expect.equal after None "closed stream yields None on next"

    // Idempotent — calling close a second time (repeat the same flip)
    // should not throw.
    match s with
    | RT.DStream(_, disposed, _) -> disposed.Value <- true
    | _ -> failtest "unreachable"

    let! stillNone = Dval.readStreamNext s |> Ply.toTask
    Expect.equal stillNone None "still None after second close"
  }


let dstreamDarkBridgeStub =
  // DStream can't round-trip through the Dark-side bridge — the pull fn
  // is a closure, and the stub form is strictly for LSP/reflection.
  test "DStream rt<->dark bridge renders as DStreamStub; fromDT raises" {
    let s = streamOfList [ RT.DInt64 1L ] VT.int64
    let dt = RT2DT.Dval.toDT s
    // Expected shape: DEnum case "DStreamStub" carrying the element VT.
    match dt with
    | RT.DEnum(_, _, [], "DStreamStub", [ _ ]) -> ()
    | _ -> failtest $"expected DStreamStub case, got {dt}"

    Expect.throws
      (fun () -> RT2DT.Dval.fromDT dt |> ignore<RT.Dval>)
      "fromDT should raise on DStreamStub (no pull fn to rebuild from)"
  }


// ——————————————————————————————————————————————————————————
// Lazy transforms — Mapped, Filtered, Take, Concat
// ——————————————————————————————————————————————————————————

/// Build a raw StreamImpl-over-a-list without wrapping in DStream.
/// Used to compose transforms beneath a single DStream root.
let private streamImplOfList
  (items : List<RT.Dval>)
  (elemType : RT.ValueType)
  : RT.StreamImpl =
  let remaining = ref items
  let next () : Ply<Option<RT.Dval>> =
    uply {
      match remaining.Value with
      | head :: tail ->
        remaining.Value <- tail
        return Some head
      | [] -> return None
    }
  RT.FromIO(next, elemType, None, None)


/// Wrap a StreamImpl in a fresh DStream with its own lock/disposed +
/// GC-backed finalizer (via Dval.wrapStreamImpl).
let private wrap (impl : RT.StreamImpl) : RT.Dval = Dval.wrapStreamImpl impl


let mappedTransformsElements =
  testTask "stream: Mapped applies fn to each element in order" {
    let src = streamImplOfList [ RT.DInt64 1L; RT.DInt64 2L; RT.DInt64 3L ] VT.int64
    let double (dv : RT.Dval) : Ply<RT.Dval> =
      uply {
        match dv with
        | RT.DInt64 i -> return RT.DInt64(i * 2L)
        | _ -> return RT.DInt64 0L
      }
    let s = wrap (RT.Mapped(src, double, VT.int64))

    let! a = Dval.readStreamNext s |> Ply.toTask
    let! b = Dval.readStreamNext s |> Ply.toTask
    let! c = Dval.readStreamNext s |> Ply.toTask
    let! d = Dval.readStreamNext s |> Ply.toTask

    Expect.equal a (Some(RT.DInt64 2L)) "1*2"
    Expect.equal b (Some(RT.DInt64 4L)) "2*2"
    Expect.equal c (Some(RT.DInt64 6L)) "3*2"
    Expect.equal d None "exhausted"
  }


let filteredSkipsRejected =
  testTask "stream: Filtered skips elements whose predicate returns false" {
    let src =
      streamImplOfList
        [ RT.DInt64 1L; RT.DInt64 2L; RT.DInt64 3L; RT.DInt64 4L ]
        VT.int64
    let isEven (dv : RT.Dval) : Ply<bool> =
      uply {
        match dv with
        | RT.DInt64 i -> return i % 2L = 0L
        | _ -> return false
      }
    let s = wrap (RT.Filtered(src, isEven))

    let! a = Dval.readStreamNext s |> Ply.toTask
    let! b = Dval.readStreamNext s |> Ply.toTask
    let! c = Dval.readStreamNext s |> Ply.toTask

    Expect.equal a (Some(RT.DInt64 2L)) "first even"
    Expect.equal b (Some(RT.DInt64 4L)) "second even"
    Expect.equal c None "no more evens"
  }


let filteredAllRejected =
  testTask "stream: Filtered returns None when no element matches" {
    let src = streamImplOfList [ RT.DInt64 1L; RT.DInt64 3L ] VT.int64
    let isEven (dv : RT.Dval) : Ply<bool> =
      uply {
        match dv with
        | RT.DInt64 i -> return i % 2L = 0L
        | _ -> return false
      }
    let s = wrap (RT.Filtered(src, isEven))

    let! result = Dval.readStreamNext s |> Ply.toTask
    Expect.equal result None "all rejected = None"
  }


let takeCapsAtN =
  testTask "stream: Take returns only the first n elements, then None" {
    let src =
      streamImplOfList
        [ RT.DInt64 10L; RT.DInt64 20L; RT.DInt64 30L; RT.DInt64 40L ]
        VT.int64
    let s = wrap (RT.Take(src, 2L, ref 2L))

    let! a = Dval.readStreamNext s |> Ply.toTask
    let! b = Dval.readStreamNext s |> Ply.toTask
    let! c = Dval.readStreamNext s |> Ply.toTask

    Expect.equal a (Some(RT.DInt64 10L)) "first taken"
    Expect.equal b (Some(RT.DInt64 20L)) "second taken"
    Expect.equal c None "take limit reached; no further pulls against source"
  }


let takeOverInfiniteSourceTerminates =
  testTask "stream: Take 3 over an unbounded counter terminates after 3" {
    // Producer counts up forever; the Take node must early-terminate
    // without consulting the source past the limit.
    let counter = ref 0L
    let next () : Ply<Option<RT.Dval>> =
      uply {
        counter.Value <- counter.Value + 1L
        return Some(RT.DInt64 counter.Value)
      }
    let src = RT.FromIO(next, VT.int64, None, None)
    let s = wrap (RT.Take(src, 3L, ref 3L))

    let! a = Dval.readStreamNext s |> Ply.toTask
    let! b = Dval.readStreamNext s |> Ply.toTask
    let! c = Dval.readStreamNext s |> Ply.toTask
    let! d = Dval.readStreamNext s |> Ply.toTask

    Expect.equal a (Some(RT.DInt64 1L)) "counter=1"
    Expect.equal b (Some(RT.DInt64 2L)) "counter=2"
    Expect.equal c (Some(RT.DInt64 3L)) "counter=3"
    Expect.equal d None "take=3 exhausted without pulling source again"
    // Source advanced exactly 3 times — Take doesn't peek past the limit.
    Expect.equal counter.Value 3L "source pulled exactly 3 times"
  }


let takeLongerThanSource =
  testTask "stream: Take 10 on a 2-elem source returns only 2" {
    let src = streamImplOfList [ RT.DInt64 7L; RT.DInt64 8L ] VT.int64
    let s = wrap (RT.Take(src, 10L, ref 10L))

    let! a = Dval.readStreamNext s |> Ply.toTask
    let! b = Dval.readStreamNext s |> Ply.toTask
    let! c = Dval.readStreamNext s |> Ply.toTask

    Expect.equal a (Some(RT.DInt64 7L)) "first"
    Expect.equal b (Some(RT.DInt64 8L)) "second"
    Expect.equal c None "source dry; Take returns None"
  }


let concatSpansMultipleStreams =
  testTask "stream: Concat drains streams in order; emits every element" {
    let s1 = streamImplOfList [ RT.DInt64 1L; RT.DInt64 2L ] VT.int64
    let s2 = streamImplOfList [] VT.int64 // empty middle slot should be skipped
    let s3 = streamImplOfList [ RT.DInt64 3L; RT.DInt64 4L ] VT.int64
    let s = wrap (RT.Concat(ref [ s1; s2; s3 ]))

    let pulled = ResizeArray<RT.Dval>()
    let mutable keepGoing = true
    while keepGoing do
      let! result = Dval.readStreamNext s |> Ply.toTask
      match result with
      | Some v -> pulled.Add v
      | None -> keepGoing <- false

    Expect.equal
      (List.ofSeq pulled)
      [ RT.DInt64 1L; RT.DInt64 2L; RT.DInt64 3L; RT.DInt64 4L ]
      "concat yields every element across all sub-streams in order"
  }


let composedTransformsAreLazy =
  testTask "stream: map ∘ filter ∘ take over infinite source terminates" {
    // Unbounded counter from 1 upwards; with filter (even) and take 3,
    // we expect [4, 8, 12] after *mapping* (multiplying) by 2.
    let counter = ref 0L
    let next () : Ply<Option<RT.Dval>> =
      uply {
        counter.Value <- counter.Value + 1L
        return Some(RT.DInt64 counter.Value)
      }
    let src = RT.FromIO(next, VT.int64, None, None)
    let isEven (dv : RT.Dval) : Ply<bool> =
      uply {
        match dv with
        | RT.DInt64 i -> return i % 2L = 0L
        | _ -> return false
      }
    let double (dv : RT.Dval) : Ply<RT.Dval> =
      uply {
        match dv with
        | RT.DInt64 i -> return RT.DInt64(i * 2L)
        | _ -> return RT.DInt64 0L
      }
    let filtered = RT.Filtered(src, isEven)
    let mapped = RT.Mapped(filtered, double, VT.int64)
    let taken = RT.Take(mapped, 3L, ref 3L)
    let s = wrap taken

    let pulled = ResizeArray<RT.Dval>()
    let mutable keepGoing = true
    while keepGoing do
      let! result = Dval.readStreamNext s |> Ply.toTask
      match result with
      | Some v -> pulled.Add v
      | None -> keepGoing <- false

    Expect.equal
      (List.ofSeq pulled)
      [ RT.DInt64 4L; RT.DInt64 8L; RT.DInt64 12L ]
      "2,4,6 -> *2 -> 4,8,12; take 3 stops before source diverges"
    // Source pulled past 6 (until filter found the 3rd even), but not
    // unboundedly — proves the pipeline is pull-driven and lazy.
    Expect.isLessThan counter.Value 100L "source not over-pulled"
  }


let toValueTypeWalksTransforms =
  test "stream: Dval.toValueType returns the transform's element type" {
    let src = streamImplOfList [ RT.DInt64 1L ] VT.int64
    let toString (_ : RT.Dval) : Ply<RT.Dval> = uply { return RT.DString "x" }
    let mapped = RT.Mapped(src, toString, VT.string)
    let s = wrap mapped
    let vt = RT.Dval.toValueType s
    Expect.equal
      vt
      (RT.ValueType.Known(RT.KTStream VT.string))
      "Mapped's elemType reflects post-map type"
  }


// ——————————————————————————————————————————————————————————
// Disposal via the GC-backed StreamFinalizer
// ——————————————————————————————————————————————————————————
// The StreamFinalizer on each DStream's lockObj runs the disposer
// chain when the DStream becomes unreachable. Explicit streamClose
// and drain-to-EOF cover the normal paths; GC-driven finalization is
// the safety net for abandoned streams (IO sources still get
// released even if the user never drains them).
//
// Tests force `GC.Collect` + `WaitForPendingFinalizers` twice to
// flush pending finalizers. They use a WeakReference on the Dval so
// the strong ref doesn't pin it in the test's stack frame.


let gcFinalizesAbandonedStream =
  test "stream: GC finalizer runs disposer for a never-drained stream" {
    let disposerRan = ref false
    // Inner function so the strong ref to dv stays scoped here and
    // can be released on return.
    let makeWeakRef () : System.WeakReference<RT.Dval> =
      let remaining = ref [ RT.DInt64 1L; RT.DInt64 2L ]
      let next () : Ply<Option<RT.Dval>> =
        uply {
          match remaining.Value with
          | head :: tail ->
            remaining.Value <- tail
            return Some head
          | [] -> return None
        }
      let disposer () = disposerRan.Value <- true
      let dv = Dval.newStream VT.int64 next (Some disposer)
      System.WeakReference<RT.Dval>(dv)

    let weakRef = makeWeakRef ()

    System.GC.Collect()
    System.GC.WaitForPendingFinalizers()
    System.GC.Collect()
    System.GC.WaitForPendingFinalizers()

    let mutable dv : RT.Dval = Unchecked.defaultof<_>
    Expect.isFalse
      (weakRef.TryGetTarget(&dv))
      "DStream should be collectible after its strong ref is dropped"
    Expect.isTrue
      disposerRan.Value
      "finalizer runs the disposer for a never-drained stream"
  }


let gcFinalizesMidDrainStream =
  test "stream: GC finalizer runs disposer after partial drain, never-closed" {
    let disposerRan = ref false
    let makeWeakRef () : System.WeakReference<RT.Dval> =
      let remaining = ref [ RT.DInt64 1L; RT.DInt64 2L; RT.DInt64 3L ]
      let next () : Ply<Option<RT.Dval>> =
        uply {
          match remaining.Value with
          | head :: tail ->
            remaining.Value <- tail
            return Some head
          | [] -> return None
        }
      let disposer () = disposerRan.Value <- true
      let dv = Dval.newStream VT.int64 next (Some disposer)

      // Pull 2 of 3 elements, then return the weak ref (dropping
      // the strong ref on function exit).
      let pulled1 = (Dval.readStreamNext dv |> Ply.toTask).Result
      let pulled2 = (Dval.readStreamNext dv |> Ply.toTask).Result
      Expect.equal pulled1 (Some(RT.DInt64 1L)) "first pull"
      Expect.equal pulled2 (Some(RT.DInt64 2L)) "second pull"

      System.WeakReference<RT.Dval>(dv)

    let weakRef = makeWeakRef ()

    System.GC.Collect()
    System.GC.WaitForPendingFinalizers()
    System.GC.Collect()
    System.GC.WaitForPendingFinalizers()

    let mutable dv : RT.Dval = Unchecked.defaultof<_>
    Expect.isFalse
      (weakRef.TryGetTarget(&dv))
      "DStream should be collectible after mid-drain drop"
    Expect.isTrue
      disposerRan.Value
      "finalizer runs disposer even when the stream was partially drained"
  }


// ——————————————————————————————————————————————————————————
// Chunked bulk-drain fast path
// ——————————————————————————————————————————————————————————
// newStreamChunked hands back whole buffers per pull; readStreamChunk
// picks up those buffers directly instead of going byte-by-byte.
// Per-byte `readStreamNext` still works against the same source (via
// newStreamChunked's synthesised `next`) so existing callers don't
// notice the change.


let chunkedDrainMatchesByteDrain =
  testTask
    "chunked drain: readStreamChunk returns the same bytes readStreamNext would" {
    // Producer hands back one 8-byte chunk, then None.
    let chunks =
      ref [ [| 0x01uy; 0x02uy; 0x03uy; 0x04uy; 0x05uy; 0x06uy; 0x07uy; 0x08uy |] ]
    let nextChunk (_ : int) : Ply<Option<byte[]>> =
      uply {
        match chunks.Value with
        | head :: tail ->
          chunks.Value <- tail
          return Some head
        | [] -> return None
      }
    let s = Dval.newStreamChunked VT.uint8 nextChunk None

    let! first = Dval.readStreamChunk 4096 s |> Ply.toTask
    let! second = Dval.readStreamChunk 4096 s |> Ply.toTask

    match first with
    | Some buf ->
      Expect.equal
        buf
        [| 0x01uy; 0x02uy; 0x03uy; 0x04uy; 0x05uy; 0x06uy; 0x07uy; 0x08uy |]
        "first chunk comes through intact"
    | None -> failtest "expected first chunk"
    Expect.equal second None "second call returns None on exhaustion"
  }


let chunkedDrainAlsoServesByteNext =
  testTask "chunked drain: readStreamNext works on a chunked stream too" {
    let chunks = ref [ [| 0x10uy; 0x20uy |] ]
    let nextChunk (_ : int) : Ply<Option<byte[]>> =
      uply {
        match chunks.Value with
        | head :: tail ->
          chunks.Value <- tail
          return Some head
        | [] -> return None
      }
    let s = Dval.newStreamChunked VT.uint8 nextChunk None

    let! a = Dval.readStreamNext s |> Ply.toTask
    let! b = Dval.readStreamNext s |> Ply.toTask
    let! c = Dval.readStreamNext s |> Ply.toTask

    Expect.equal a (Some(RT.DUInt8 0x10uy)) "first byte"
    Expect.equal b (Some(RT.DUInt8 0x20uy)) "second byte"
    Expect.equal c None "exhausted"
  }


let chunkedDrainFallsBackToByteWise =
  testTask
    "chunked drain: readStreamChunk on a non-chunked stream falls back to byte pulls" {
    let remaining = ref [ RT.DUInt8 0xAAuy; RT.DUInt8 0xBBuy; RT.DUInt8 0xCCuy ]
    let next () : Ply<Option<RT.Dval>> =
      uply {
        match remaining.Value with
        | head :: tail ->
          remaining.Value <- tail
          return Some head
        | [] -> return None
      }
    // newStream (not newStreamChunked) — no nextChunk callback, so
    // readStreamChunk should fall back to per-byte accumulation.
    let s = Dval.newStream VT.uint8 next None

    let! chunk = Dval.readStreamChunk 4096 s |> Ply.toTask
    match chunk with
    | Some buf -> Expect.equal buf [| 0xAAuy; 0xBBuy; 0xCCuy |] "all bytes collected"
    | None -> failtest "expected a chunk"

    let! after = Dval.readStreamChunk 4096 s |> Ply.toTask
    Expect.equal after None "exhausted"
  }


let gcSkipsFinalizerAfterStreamClose =
  test "stream: finalizer doesn't re-fire disposer when close already ran" {
    // Assert the disposer runs exactly once across explicit close +
    // GC finalize.
    let disposeCount = ref 0
    let makeWeakRef () : System.WeakReference<RT.Dval> =
      let next () : Ply<Option<RT.Dval>> = uply { return None }
      let disposer () = disposeCount.Value <- disposeCount.Value + 1
      let dv = Dval.newStream VT.int64 next (Some disposer)

      // Replicate the streamClose builtin: flip disposed, walk the
      // impl chain.
      match dv with
      | RT.DStream(impl, disposed, _) ->
        disposed.Value <- true
        Dval.disposeStreamImpl impl
      | _ -> failtest "expected DStream"

      Expect.equal disposeCount.Value 1 "close ran disposer once"
      System.WeakReference<RT.Dval>(dv)

    let weakRef = makeWeakRef ()

    System.GC.Collect()
    System.GC.WaitForPendingFinalizers()
    System.GC.Collect()
    System.GC.WaitForPendingFinalizers()

    let mutable dv : RT.Dval = Unchecked.defaultof<_>
    // If the WeakRef still resolves, that's fine — we only care
    // about dispose-count staying at 1.
    weakRef.TryGetTarget(&dv) |> ignore<bool>
    Expect.equal
      disposeCount.Value
      1
      "finalizer short-circuited because disposed was already true"
  }


let tests =
  testList
    "stream"
    [ pullsElementsInOrder
      staysDrainedAfterExhaustion
      emptyStreamImmediatelyDone
      toValueTypeReflectsElemType
      dstreamBinarySerializeRaises
      tstreamPtBinaryRoundtrip
      tstreamRtBinaryRoundtrip
      ktstreamBinaryRoundtrip
      tstreamPtDarkBridge
      tstreamRtDarkBridge
      ktstreamRtDarkBridge
      streamCloseMarksDisposed
      dstreamDarkBridgeStub
      mappedTransformsElements
      filteredSkipsRejected
      filteredAllRejected
      takeCapsAtN
      takeOverInfiniteSourceTerminates
      takeLongerThanSource
      concatSpansMultipleStreams
      composedTransformsAreLazy
      toValueTypeWalksTransforms
      gcFinalizesAbandonedStream
      gcFinalizesMidDrainStream
      gcSkipsFinalizerAfterStreamClose
      chunkedDrainMatchesByteDrain
      chunkedDrainAlsoServesByteNext
      chunkedDrainFallsBackToByteWise ]
