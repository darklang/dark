/// Tests for the Stream Dval.
///
/// Covers IO-backed streams (read once, kept for every holder), what Dark can't see of
/// the transform nodes, releasing IO sources, and a byte source's Bytes chunks.
module Tests.Stream

open Expecto

open System.Threading.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module Bytes = LibExecution.Bytes
module Stream = LibExecution.Stream
module VT = LibExecution.ValueType
module BS = LibSerialization.Binary.Serialization
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module StreamLog = TestUtils.LibTest.StreamLog


// ─────────────────────────────────────────────────────────────────────
// Helpers — stream construction, pulls, drains
// ─────────────────────────────────────────────────────────────────────

/// An IO-style source over a list: each call hands out the next item.
let private listPullFn
  (items : List<RT.Dval>)
  : (unit -> System.Threading.Tasks.ValueTask<Option<RT.Dval>>) =
  let remaining = ref items
  fun () ->
    match remaining.Value with
    | head :: tail ->
      remaining.Value <- tail
      System.Threading.Tasks.ValueTask<_>(Some head)
    | [] -> System.Threading.Tasks.ValueTask<_>(None)

/// An IO stream over the items.
let private streamOfList
  (items : List<RT.Dval>)
  (elemType : RT.ValueType)
  : RT.Dval =
  Stream.newFromIO elemType (listPullFn items) None

/// The next element and the rest, or None.
let private pull (s : RT.Dval) : Task<Option<RT.Dval * RT.Dval>> = Stream.readNext s

/// Every element of a stream.
let private drain (s : RT.Dval) : Task<List<RT.Dval>> =
  task {
    let acc = ResizeArray<RT.Dval>()
    let mutable rest = Some s
    while rest.IsSome do
      match! Stream.readNext rest.Value with
      | Some(v, after) ->
        acc.Add v
        rest <- Some after
      | None -> rest <- None
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

let readingAgainAnswersTheSame =
  testTask "stream: an IO stream read twice gives the same elements, read once" {
    let pulls = ref 0
    let next = listPullFn [ RT.DInt64 1L; RT.DInt64 2L ]
    let counted () =
      pulls.Value <- pulls.Value + 1
      next ()
    let s = Stream.newFromIO VT.int64 counted None
    let! first = drain s
    let! second = drain s
    let! head = pull s
    Expect.equal first [ RT.DInt64 1L; RT.DInt64 2L ] "first read"
    Expect.equal second first "second read sees the same"
    Expect.equal (head |> Option.map fst) (Some(RT.DInt64 1L)) "next still answers 1"
    Expect.equal pulls.Value 3 "the source was read once: 1, 2, the end"
  }

let emptyStreamImmediatelyDone =
  testTask "stream: empty FromIO returns None on first pull" {
    let! result = pull (streamOfList [] VT.int64)
    Expect.isNone result "empty stream = None"
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
    let original = RT.TStream RT.TBytes
    let restored =
      binaryRoundtrip
        LibSerialization.Binary.Serializers.RT.TypeReference.write
        LibSerialization.Binary.Serializers.RT.TypeReference.read
        original
    Expect.equal restored original "RT.TStream<Bytes> roundtrips"
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
    let original = RT.TStream RT.TBytes
    let restored = RT2DT.TypeReference.fromDT (RT2DT.TypeReference.toDT original)
    Expect.equal restored original "RT.TStream<Bytes> survives rt<->dark"
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

let closeEndsWhatWasNotRead =
  testTask "Stream.close: what was read stays, the rest ends, the disposer runs once" {
    let disposed = ref 0
    let s =
      Stream.newFromIO
        VT.int64
        (listPullFn [ RT.DInt64 1L; RT.DInt64 2L ])
        (Some(fun () -> disposed.Value <- disposed.Value + 1))
    let! head = pull s
    let close () =
      match s with
      | RT.DStream impl -> Stream.close impl
      | _ -> failtest "expected DStream"
    close ()
    close ()
    let! again = drain s
    Expect.equal
      (head |> Option.map fst)
      (Some(RT.DInt64 1L))
      "read before the close"
    Expect.equal again [ RT.DInt64 1L ] "1 stays; 2 was never read and ends"
    Expect.equal disposed.Value 1 "released once"
  }


// ─────────────────────────────────────────────────────────────────────
// Lazy transforms — Mapped / Filtered / Take / Concat
// ─────────────────────────────────────────────────────────────────────

// map, filter, take and concat are covered by packages/darklang/tests/stdlib/stream.dark.
// What stays here is what Dark can't see: the element type a transform node reports.

let toValueTypeWalksTransforms =
  test "stream: Dval.toValueType returns the transform's element type" {
    let src = RT.FromList([ RT.DInt64 1L ], VT.int64)
    // Never applied: only the node's element type is asked.
    let toString =
      RT.AppNamedFn
        { name = RT.FQFnName.fqBuiltin "int64ToString" 0
          typeSymbolTable = RT.TST.empty
          typeArgs = []
          approvalName = None
          access = None
          argsSoFar = []
          boundImpls = [] }
    let s = RT.DStream(RT.Mapped(src, toString, VT.string))
    Expect.equal
      (RT.Dval.toValueType s)
      (RT.ValueType.Known(RT.KTStream VT.string))
      "Mapped's elemType reflects post-map type"
  }


// ─────────────────────────────────────────────────────────────────────
// Lifecycle of the transforms over an IO source
// ─────────────────────────────────────────────────────────────────────
// `Builtin.testTrackedStream` logs each pull of the source and each run of its disposer,
// by label, so these see what a transform pulled and whether it released the source.

/// What a source saw by the time the program finished, and anything its GC finalizer added
/// after a forced collection. A source released exactly once shows nothing late.
type private Tracked = { promptly : List<string>; late : List<string> }

let private collect () =
  System.GC.Collect()
  System.GC.WaitForPendingFinalizers()
  System.GC.Collect()
  System.GC.WaitForPendingFinalizers()

/// Run `code` and hand back its value with what each source in `labels` saw.
let private runTracked
  (labels : List<string>)
  (code : string)
  : Task<RT.Dval * Map<string, Tracked>> =
  task {
    match! TestUtils.TestUtils.evalDarkExpr code with
    | Ok dv ->
      let promptly = labels |> List.map (fun l -> l, StreamLog.take l)
      collect ()
      let seen =
        promptly
        |> List.map (fun (l, p) -> l, { promptly = p; late = StreamLog.take l })
        |> Map.ofList
      return (dv, seen)
    | Error(rte, _) -> return failtest $"the Dark expression raised: {rte}"
  }

let private expectTracked
  (seen : Tracked)
  (expected : List<string>)
  (what : string)
  =
  Expect.equal seen.promptly expected what
  Expect.equal seen.late [] $"{what}; and nothing more once collected"

let closingATransformClosesItsSource =
  testTask "stream: closing a transform disposes its IO source once" {
    let transforms =
      [ "map", "Stdlib.Stream.map(s, fun n -> n)"
        "filter", "Stdlib.Stream.filter(s, fun n -> true)"
        "take", "Stdlib.Stream.take(s, 5)"
        "concat", "Stdlib.Stream.concat([ s ])" ]
    for (name, build) in transforms do
      let label = $"close-{name}"
      let! (_, seen) =
        runTracked
          [ label ]
          $"""let s = Builtin.testTrackedStream("{label}", [ 1, 2, 3 ])
let t = {build}
let _ = Stdlib.Stream.next(t)
Stdlib.Stream.close(t)"""
      expectTracked
        (Map.findUnsafe label seen)
        [ "pull"; "dispose" ]
        $"{name}: one pull, then closed"
  }

let concatClosesWhatItHolds =
  testTask "stream: concat closes each stream as it drains it, and the rest on close" {
    let! (result, seen) =
      runTracked
        [ "concat-a"; "concat-b"; "concat-c" ]
        """let a = Builtin.testTrackedStream("concat-a", [ 1 ])
let b = Builtin.testTrackedStream("concat-b", [ 2, 3 ])
let c = Builtin.testTrackedStream("concat-c", [ 4 ])
let joined = Stdlib.Stream.concat([ a, b, c ])
let (first, rest) = Builtin.unwrap(Stdlib.Stream.next(joined))
let (second, _) = Builtin.unwrap(Stdlib.Stream.next(rest))
let _ = Stdlib.Stream.close(joined)
[ first, second ]"""
    match result with
    | RT.DList(_, RT.Items(items)) ->
      Expect.equal items [ RT.DInt64 1L; RT.DInt64 2L ] "1 from a, then 2 from b"
    | other -> failtest $"expected a list, got {other}"
    expectTracked
      (Map.findUnsafe "concat-a" seen)
      [ "pull"; "pull"; "dispose" ]
      "a closed once it drained"
    expectTracked
      (Map.findUnsafe "concat-b" seen)
      [ "pull"; "dispose" ]
      "b closed by the close, mid-way"
    expectTracked
      (Map.findUnsafe "concat-c" seen)
      [ "dispose" ]
      "c closed by the close, never pulled"
  }


let twoReadersShareOneRead =
  testTask
    "stream: two reads of one IO stream from Dark see the same, the source read once" {
    let! (result, seen) =
      runTracked
        [ "shared" ]
        """let s = Builtin.testTrackedStream("shared", [ 1, 2 ])
let doubled = Stdlib.Stream.map(s, fun n -> n * 2)
(Stdlib.Stream.toList(s), Stdlib.Stream.toList(doubled), Stdlib.Stream.toList(s))"""
    let ints xs = RT.DList(VT.int64, RT.Items(List.map RT.DInt64 xs))
    Expect.equal
      result
      (RT.DTuple(ints [ 1L; 2L ], ints [ 2L; 4L ], [ ints [ 1L; 2L ] ]))
      "every reader sees 1, 2"
    expectTracked
      (Map.findUnsafe "shared" seen)
      [ "pull"; "pull"; "pull"; "dispose" ]
      "read once: 1, 2, the end"
  }


// ─────────────────────────────────────────────────────────────────────
// Disposal via the GC-backed StreamFinalizer
// ─────────────────────────────────────────────────────────────────────
// Each IO source's finalizer (`StreamSource`) runs its disposer once
// nothing that could read it is left. Tests force GC + flush
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
      // Read 2 of 3 elements, then return the weak ref.
      let (v1, rest) = Option.get (Stream.readNext dv).Result
      let (v2, _) = Option.get (Stream.readNext rest).Result
      Expect.equal v1 (RT.DInt64 1L) "first pull"
      Expect.equal v2 (RT.DInt64 2L) "second pull"
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
      let next () = System.Threading.Tasks.ValueTask<Option<RT.Dval>>(None)
      let disposer () = disposeCount.Value <- disposeCount.Value + 1
      let dv = Stream.newFromIO VT.int64 next (Some disposer)
      match dv with
      | RT.DStream impl -> Stream.close impl
      | _ -> failtest "expected DStream"
      Expect.equal disposeCount.Value 1 "close ran disposer once"
      System.WeakReference<RT.Dval>(dv)
    let weakRef = makeWeakRef ()
    gcCycle ()
    let mutable dv : RT.Dval = Unchecked.defaultof<_>
    weakRef.TryGetTarget(&dv) |> ignore<bool>
    Expect.equal disposeCount.Value 1 "the finalizer does not release it again"
  }


// ─────────────────────────────────────────────────────────────────────
// A byte source: one Bytes element per read
// ─────────────────────────────────────────────────────────────────────

/// A nextChunk callback that yields each entry of `buffers` once, then None.
let private chunkPullFn
  (buffers : List<byte[]>)
  : (int -> System.Threading.Tasks.ValueTask<Option<byte[]>>) =
  let remaining = ref buffers
  fun (_ : int) ->
    match remaining.Value with
    | head :: tail ->
      remaining.Value <- tail
      System.Threading.Tasks.ValueTask<_>(Some head)
    | [] -> System.Threading.Tasks.ValueTask<_>(None)

let private chunkBytes (dv : RT.Dval) : byte[] =
  match dv with
  | RT.DBytes(RT.Ephemeral b) -> b.ToArray()
  | other -> Exception.raiseInternal "expected Bytes" [ "got", other ]

let byteSourceYieldsOneBytesPerRead =
  testTask "bytes: one Bytes element per read, and the same again" {
    let s = Stream.newBytes (chunkPullFn [ [| 0x01uy; 0x02uy |]; [| 0x03uy |] ]) None
    let! first = drain s
    let! second = drain s
    Expect.equal
      (first |> List.map chunkBytes)
      [ [| 0x01uy; 0x02uy |]; [| 0x03uy |] ]
      "each read is one element"
    Expect.equal
      (second |> List.map chunkBytes)
      (first |> List.map chunkBytes)
      "the same again"
    Expect.equal
      (RT.Dval.toValueType s)
      (RT.ValueType.Known(RT.KTStream VT.bytes))
      "a Stream<Bytes>"
  }

let byteSourceEndsOnEmptyRead =
  testTask "bytes: an empty read ends the stream" {
    let s = Stream.newBytes (chunkPullFn [ [| 0x01uy |]; [||]; [| 0x02uy |] ]) None
    let! chunks = drain s
    Expect.equal
      (chunks |> List.map chunkBytes)
      [ [| 0x01uy |] ]
      "stops at the empty read"
  }


let tests =
  testList
    "stream"
    [ pullsElementsInOrder
      readingAgainAnswersTheSame
      emptyStreamImmediatelyDone
      toValueTypeReflectsElemType
      dstreamBinarySerializeRaises
      tstreamPtBinaryRoundtrip
      tstreamRtBinaryRoundtrip
      ktstreamBinaryRoundtrip
      tstreamPtDarkBridge
      tstreamRtDarkBridge
      ktstreamRtDarkBridge
      closeEndsWhatWasNotRead
      dstreamDarkBridgeStub
      toValueTypeWalksTransforms
      closingATransformClosesItsSource
      concatClosesWhatItHolds
      twoReadersShareOneRead
      gcFinalizesAbandonedStream
      gcFinalizesMidDrainStream
      gcSkipsFinalizerAfterStreamClose
      byteSourceYieldsOneBytesPerRead
      byteSourceEndsOnEmptyRead ]
