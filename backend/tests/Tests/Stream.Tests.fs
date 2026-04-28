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
module Blob = LibExecution.Blob
module Stream = LibExecution.Stream
module VT = LibExecution.ValueType
module BS = LibSerialization.Binary.Serialization
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes


// ─────────────────────────────────────────────────────────────────────
// Helpers — stream construction, pulls, drains
// ─────────────────────────────────────────────────────────────────────

/// Pull-fn over a mutable list cell. Used to back streamOfList +
/// streamImplOfList without duplicating the closure body.
let private listPullFn (items : List<RT.Dval>) : (unit -> Ply<Option<RT.Dval>>) =
  let remaining = ref items
  fun () ->
    uply {
      match remaining.Value with
      | head :: tail ->
        remaining.Value <- tail
        return Some head
      | [] -> return None
    }

/// Build a DStream backed by an in-memory list (used at the FromIO
/// leaf level). For composing transforms beneath a single root, use
/// streamImplOfList instead.
let private streamOfList
  (items : List<RT.Dval>)
  (elemType : RT.ValueType)
  : RT.Dval =
  Stream.newFromIO elemType (listPullFn items) None

/// Build a raw StreamImpl over a list (no DStream wrapper). Composed
/// under a transform node + wrap.
let private streamImplOfList
  (items : List<RT.Dval>)
  (elemType : RT.ValueType)
  : RT.StreamImpl =
  RT.FromIO(listPullFn items, elemType, None, None)

let private wrap (impl : RT.StreamImpl) : RT.Dval = Stream.wrapImpl impl

let private pull (s : RT.Dval) : Task<Option<RT.Dval>> =
  Stream.readNext s |> Ply.toTask

/// Drain a stream to a list. Pulls until None.
let private drain (s : RT.Dval) : Task<List<RT.Dval>> =
  task {
    let acc = ResizeArray<RT.Dval>()
    let mutable keepGoing = true
    while keepGoing do
      let! r = Stream.readNext s |> Ply.toTask
      match r with
      | Some v -> acc.Add v
      | None -> keepGoing <- false
    return List.ofSeq acc
  }

/// Run a write/read pair through a fresh MemoryStream. Used by the
/// binary-serializer roundtrip tests.
let private binaryRoundtrip
  (write : System.IO.BinaryWriter -> 'a -> unit)
  (read : System.IO.BinaryReader -> 'a)
  (value : 'a)
  : 'a =
  use ms = new System.IO.MemoryStream()
  let w = new System.IO.BinaryWriter(ms)
  write w value
  let r = new System.IO.BinaryReader(new System.IO.MemoryStream(ms.ToArray()))
  read r

let private intPredEven (dv : RT.Dval) : Ply<bool> =
  uply {
    match dv with
    | RT.DInt64 i -> return i % 2L = 0L
    | _ -> return false
  }

let private intDouble (dv : RT.Dval) : Ply<RT.Dval> =
  uply {
    match dv with
    | RT.DInt64 i -> return RT.DInt64(i * 2L)
    | _ -> return RT.DInt64 0L
  }


// ─────────────────────────────────────────────────────────────────────
// Pull semantics
// ─────────────────────────────────────────────────────────────────────

let pullsElementsInOrder =
  testTask "stream: pull returns elements in order then None" {
    let s = streamOfList [ RT.DInt64 1L; RT.DInt64 2L; RT.DInt64 3L ] VT.int64
    let! result = drain s
    Expect.equal
      result
      [ RT.DInt64 1L; RT.DInt64 2L; RT.DInt64 3L ]
      "pulls in order then exhausts"
  }

let staysDrainedAfterExhaustion =
  testTask "stream: subsequent pulls after exhaustion keep returning None" {
    let s = streamOfList [ RT.DInt64 42L ] VT.int64
    let! first = pull s
    let! afterExhaust = pull s
    let! stillDone = pull s
    Expect.equal first (Some(RT.DInt64 42L)) "first pull = 42"
    Expect.equal afterExhaust None "pull past end = None"
    Expect.equal stillDone None "still None on further pulls"
  }

let emptyStreamImmediatelyDone =
  testTask "stream: empty FromIO returns None on first pull" {
    let! result = pull (streamOfList [] VT.int64)
    Expect.equal result None "empty stream = None"
  }

let toValueTypeReflectsElemType =
  test "stream: Dval.toValueType returns KTStream of the element type" {
    let s = streamOfList [ RT.DString "hi" ] VT.string
    Expect.equal
      (RT.Dval.toValueType s)
      (RT.ValueType.Known(RT.KTStream VT.string))
      "toValueType reflects the elem type"
  }


// ─────────────────────────────────────────────────────────────────────
// Binary serialization — DStream raises; TStream / KTStream roundtrip
// ─────────────────────────────────────────────────────────────────────

let dstreamBinarySerializeRaises =
  test "DStream binary serialization raises (streams aren't persistable)" {
    let s = streamOfList [ RT.DInt64 1L ] VT.int64
    Expect.throws
      (fun () -> BS.RT.Dval.serialize "dval" s |> ignore<byte[]>)
      "expected serialize(DStream) to raise; streams are non-persistable"
  }

let tstreamPtBinaryRoundtrip =
  test "PT.TStream roundtrips through the binary type-reference serializer" {
    let original = PT.TStream PT.TInt64
    let restored =
      binaryRoundtrip
        LibSerialization.Binary.Serializers.PT.TypeReference.write
        LibSerialization.Binary.Serializers.PT.TypeReference.read
        original
    Expect.equal restored original "PT.TStream<Int64> roundtrips"
  }

let tstreamRtBinaryRoundtrip =
  test "RT.TStream roundtrips through the binary type-reference serializer" {
    let original = RT.TStream RT.TBlob
    let restored =
      binaryRoundtrip
        LibSerialization.Binary.Serializers.RT.TypeReference.write
        LibSerialization.Binary.Serializers.RT.TypeReference.read
        original
    Expect.equal restored original "RT.TStream<Blob> roundtrips"
  }

let ktstreamBinaryRoundtrip =
  test "KTStream roundtrips through the RT ValueType binary serializer" {
    let original = RT.ValueType.Known(RT.KTStream VT.string)
    let restored =
      binaryRoundtrip
        LibSerialization.Binary.Serializers.RT.ValueType.write
        LibSerialization.Binary.Serializers.RT.ValueType.read
        original
    Expect.equal restored original "KTStream<String> ValueType roundtrips"
  }


// ─────────────────────────────────────────────────────────────────────
// Dark-side bridges (PT/RT/VT ↔ DT) + DStream stub
// ─────────────────────────────────────────────────────────────────────

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

let dstreamDarkBridgeStub =
  // DStream can't round-trip — pull fn is a closure. The stub form is
  // strictly for LSP/reflection.
  test "DStream rt<->dark bridge renders as DStreamStub; fromDT raises" {
    let s = streamOfList [ RT.DInt64 1L ] VT.int64
    let dt = RT2DT.Dval.toDT s
    match dt with
    | RT.DEnum(_, _, [], "DStreamStub", [ _ ]) -> ()
    | _ -> failtest $"expected DStreamStub case, got {dt}"
    Expect.throws
      (fun () -> RT2DT.Dval.fromDT dt |> ignore<RT.Dval>)
      "fromDT should raise on DStreamStub (no pull fn to rebuild from)"
  }


// ─────────────────────────────────────────────────────────────────────
// Close
// ─────────────────────────────────────────────────────────────────────

let streamCloseMarksDisposed =
  testTask "Stream.close flips disposed; subsequent next returns None" {
    let s = streamOfList [ RT.DInt64 42L ] VT.int64
    let setDisposed () =
      match s with
      | RT.DStream(_, disposed, _) -> disposed.Value <- true
      | _ -> failtest "expected DStream"
    setDisposed ()
    let! after = pull s
    Expect.equal after None "closed stream yields None on next"
    setDisposed () // idempotent — second close shouldn't throw
    let! stillNone = pull s
    Expect.equal stillNone None "still None after second close"
  }


// ─────────────────────────────────────────────────────────────────────
// Lazy transforms — Mapped / Filtered / Take / Concat
// ─────────────────────────────────────────────────────────────────────

let mappedTransformsElements =
  testTask "stream: Mapped applies fn to each element in order" {
    let src = streamImplOfList [ RT.DInt64 1L; RT.DInt64 2L; RT.DInt64 3L ] VT.int64
    let s = wrap (RT.Mapped(src, intDouble, VT.int64))
    let! result = drain s
    Expect.equal result [ RT.DInt64 2L; RT.DInt64 4L; RT.DInt64 6L ] "1,2,3 -> *2"
  }

let filteredSkipsRejected =
  testTask "stream: Filtered skips elements whose predicate returns false" {
    let src =
      streamImplOfList
        [ RT.DInt64 1L; RT.DInt64 2L; RT.DInt64 3L; RT.DInt64 4L ]
        VT.int64
    let s = wrap (RT.Filtered(src, intPredEven))
    let! result = drain s
    Expect.equal result [ RT.DInt64 2L; RT.DInt64 4L ] "evens only"
  }

let filteredAllRejected =
  testTask "stream: Filtered returns None when no element matches" {
    let src = streamImplOfList [ RT.DInt64 1L; RT.DInt64 3L ] VT.int64
    let s = wrap (RT.Filtered(src, intPredEven))
    let! result = pull s
    Expect.equal result None "all rejected = None"
  }

let takeCapsAtN =
  testTask "stream: Take returns only the first n elements, then None" {
    let src =
      streamImplOfList
        [ RT.DInt64 10L; RT.DInt64 20L; RT.DInt64 30L; RT.DInt64 40L ]
        VT.int64
    let s = wrap (RT.Take(src, 2L, ref 2L))
    let! result = drain s
    Expect.equal result [ RT.DInt64 10L; RT.DInt64 20L ] "first 2 only"
  }

let takeOverInfiniteSourceTerminates =
  testTask "stream: Take 3 over an unbounded counter terminates after 3" {
    // Producer counts up forever; Take must early-terminate without
    // pulling source past the limit.
    let counter = ref 0L
    let next () : Ply<Option<RT.Dval>> =
      uply {
        counter.Value <- counter.Value + 1L
        return Some(RT.DInt64 counter.Value)
      }
    let src = RT.FromIO(next, VT.int64, None, None)
    let s = wrap (RT.Take(src, 3L, ref 3L))
    let! result = drain s
    Expect.equal result [ RT.DInt64 1L; RT.DInt64 2L; RT.DInt64 3L ] "took exactly 3"
    Expect.equal counter.Value 3L "source pulled exactly 3 times"
  }

let takeLongerThanSource =
  testTask "stream: Take 10 on a 2-elem source returns only 2" {
    let src = streamImplOfList [ RT.DInt64 7L; RT.DInt64 8L ] VT.int64
    let s = wrap (RT.Take(src, 10L, ref 10L))
    let! result = drain s
    Expect.equal result [ RT.DInt64 7L; RT.DInt64 8L ] "all available"
  }

let concatSpansMultipleStreams =
  testTask "stream: Concat drains streams in order; emits every element" {
    let s1 = streamImplOfList [ RT.DInt64 1L; RT.DInt64 2L ] VT.int64
    let s2 = streamImplOfList [] VT.int64 // empty middle is skipped
    let s3 = streamImplOfList [ RT.DInt64 3L; RT.DInt64 4L ] VT.int64
    let s = wrap (RT.Concat(ref [ s1; s2; s3 ]))
    let! result = drain s
    Expect.equal
      result
      [ RT.DInt64 1L; RT.DInt64 2L; RT.DInt64 3L; RT.DInt64 4L ]
      "every element across all sub-streams in order"
  }

let composedTransformsAreLazy =
  testTask "stream: map ∘ filter ∘ take over infinite source terminates" {
    // Counter from 1 up; with filter (even), map (×2), take 3, expect
    // [4, 8, 12]. Source pulled past 6 to find the third even, but not
    // unboundedly — proves the pipeline is pull-driven.
    let counter = ref 0L
    let next () : Ply<Option<RT.Dval>> =
      uply {
        counter.Value <- counter.Value + 1L
        return Some(RT.DInt64 counter.Value)
      }
    let pipeline =
      RT.Take(
        RT.Mapped(
          RT.Filtered(RT.FromIO(next, VT.int64, None, None), intPredEven),
          intDouble,
          VT.int64
        ),
        3L,
        ref 3L
      )
    let! result = drain (wrap pipeline)
    Expect.equal
      result
      [ RT.DInt64 4L; RT.DInt64 8L; RT.DInt64 12L ]
      "2,4,6 -> *2 -> 4,8,12; take 3 stops before source diverges"
    Expect.isLessThan counter.Value 100L "source not over-pulled"
  }

let toValueTypeWalksTransforms =
  test "stream: Dval.toValueType returns the transform's element type" {
    let src = streamImplOfList [ RT.DInt64 1L ] VT.int64
    let toString (_ : RT.Dval) : Ply<RT.Dval> = uply { return RT.DString "x" }
    let s = wrap (RT.Mapped(src, toString, VT.string))
    Expect.equal
      (RT.Dval.toValueType s)
      (RT.ValueType.Known(RT.KTStream VT.string))
      "Mapped's elemType reflects post-map type"
  }


// ─────────────────────────────────────────────────────────────────────
// Disposal via the GC-backed StreamFinalizer
// ─────────────────────────────────────────────────────────────────────
// The StreamFinalizer on each DStream's lockObj runs the disposer
// chain when the DStream becomes unreachable. Tests force GC + flush
// pending finalizers; they use a WeakReference on the Dval so the
// strong ref doesn't pin it in the test stack frame.

let private gcCycle () =
  System.GC.Collect()
  System.GC.WaitForPendingFinalizers()
  System.GC.Collect()
  System.GC.WaitForPendingFinalizers()

let gcFinalizesAbandonedStream =
  test "stream: GC finalizer runs disposer for a never-drained stream" {
    let disposerRan = ref false
    let makeWeakRef () : System.WeakReference<RT.Dval> =
      let next = listPullFn [ RT.DInt64 1L; RT.DInt64 2L ]
      let disposer () = disposerRan.Value <- true
      System.WeakReference<RT.Dval>(Stream.newFromIO VT.int64 next (Some disposer))
    let weakRef = makeWeakRef ()
    gcCycle ()
    let mutable dv : RT.Dval = Unchecked.defaultof<_>
    Expect.isFalse
      (weakRef.TryGetTarget(&dv))
      "DStream collectible after ref dropped"
    Expect.isTrue disposerRan.Value "finalizer ran disposer for never-drained stream"
  }

let gcFinalizesMidDrainStream =
  test "stream: GC finalizer runs disposer after partial drain, never-closed" {
    let disposerRan = ref false
    let makeWeakRef () : System.WeakReference<RT.Dval> =
      let next = listPullFn [ RT.DInt64 1L; RT.DInt64 2L; RT.DInt64 3L ]
      let disposer () = disposerRan.Value <- true
      let dv = Stream.newFromIO VT.int64 next (Some disposer)
      // Pull 2 of 3 elements, then return the weak ref.
      let pulled1 = (Stream.readNext dv |> Ply.toTask).Result
      let pulled2 = (Stream.readNext dv |> Ply.toTask).Result
      Expect.equal pulled1 (Some(RT.DInt64 1L)) "first pull"
      Expect.equal pulled2 (Some(RT.DInt64 2L)) "second pull"
      System.WeakReference<RT.Dval>(dv)
    let weakRef = makeWeakRef ()
    gcCycle ()
    let mutable dv : RT.Dval = Unchecked.defaultof<_>
    Expect.isFalse (weakRef.TryGetTarget(&dv)) "DStream collectible mid-drain"
    Expect.isTrue disposerRan.Value "finalizer ran on partial-drain abandonment"
  }

let gcSkipsFinalizerAfterStreamClose =
  test "stream: finalizer doesn't re-fire disposer when close already ran" {
    let disposeCount = ref 0
    let makeWeakRef () : System.WeakReference<RT.Dval> =
      let next () : Ply<Option<RT.Dval>> = uply { return None }
      let disposer () = disposeCount.Value <- disposeCount.Value + 1
      let dv = Stream.newFromIO VT.int64 next (Some disposer)
      // Replicate streamClose: flip disposed, walk impl chain.
      match dv with
      | RT.DStream(impl, disposed, _) ->
        disposed.Value <- true
        Stream.disposeImpl impl
      | _ -> failtest "expected DStream"
      Expect.equal disposeCount.Value 1 "close ran disposer once"
      System.WeakReference<RT.Dval>(dv)
    let weakRef = makeWeakRef ()
    gcCycle ()
    let mutable dv : RT.Dval = Unchecked.defaultof<_>
    weakRef.TryGetTarget(&dv) |> ignore<bool>
    Expect.equal
      disposeCount.Value
      1
      "finalizer short-circuited because disposed was already true"
  }


// ─────────────────────────────────────────────────────────────────────
// Chunked bulk-drain fast path
// ─────────────────────────────────────────────────────────────────────
// newStreamChunked hands back whole buffers per pull; readStreamChunk
// picks them up directly. Per-byte readStreamNext still works against
// the same source via newStreamChunked's synthesised `next`.

/// A nextChunk callback that yields each entry of `buffers` once, then None.
let private chunkPullFn (buffers : List<byte[]>) : (int -> Ply<Option<byte[]>>) =
  let remaining = ref buffers
  fun (_ : int) ->
    uply {
      match remaining.Value with
      | head :: tail ->
        remaining.Value <- tail
        return Some head
      | [] -> return None
    }

let chunkedDrainMatchesByteDrain =
  testTask
    "chunked drain: readStreamChunk returns the same bytes readStreamNext would" {
    let buf = [| 0x01uy; 0x02uy; 0x03uy; 0x04uy; 0x05uy; 0x06uy; 0x07uy; 0x08uy |]
    let s = Stream.newChunked VT.uint8 (chunkPullFn [ buf ]) None
    let! first = Stream.readChunk 4096 s |> Ply.toTask
    let! second = Stream.readChunk 4096 s |> Ply.toTask
    Expect.equal first (Some buf) "first chunk comes through intact"
    Expect.equal second None "second call returns None on exhaustion"
  }

let chunkedDrainAlsoServesByteNext =
  testTask "chunked drain: readStreamNext works on a chunked stream too" {
    let s = Stream.newChunked VT.uint8 (chunkPullFn [ [| 0x10uy; 0x20uy |] ]) None
    let! a = pull s
    let! b = pull s
    let! c = pull s
    Expect.equal a (Some(RT.DUInt8 0x10uy)) "first byte"
    Expect.equal b (Some(RT.DUInt8 0x20uy)) "second byte"
    Expect.equal c None "exhausted"
  }

let chunkedDrainFallsBackToByteWise =
  testTask
    "chunked drain: readStreamChunk on a non-chunked stream falls back to byte pulls" {
    // newStream (no nextChunk callback) — readStreamChunk falls back
    // to per-byte accumulation.
    let s =
      Stream.newFromIO
        VT.uint8
        (listPullFn [ RT.DUInt8 0xAAuy; RT.DUInt8 0xBBuy; RT.DUInt8 0xCCuy ])
        None
    let! chunk = Stream.readChunk 4096 s |> Ply.toTask
    Expect.equal chunk (Some [| 0xAAuy; 0xBBuy; 0xCCuy |]) "all bytes collected"
    let! after = Stream.readChunk 4096 s |> Ply.toTask
    Expect.equal after None "exhausted"
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
