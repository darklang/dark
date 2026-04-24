/// Tests for the Stream Dval — see thinking/blobs-and-streams/.
///
/// Chunk 2.2: pull-only FromIO streams. Later chunks add transforms
/// (Mapped/Filtered/Take/Concat), drain helpers (toList/toBlob), and
/// disposal.
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
/// semantics; real IO-backed streams arrive in chunk 2.8 (HttpClient).
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
  Dval.newStream elemType next


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
    Expect.equal restored original "PT.TStream<UInt8> survives pt↔dark"
  }


let tstreamRtDarkBridge =
  test "RT.TStream roundtrips through the Dark-side bridge" {
    let original = RT.TStream RT.TBlob
    let restored = RT2DT.TypeReference.fromDT (RT2DT.TypeReference.toDT original)
    Expect.equal restored original "RT.TStream<Blob> survives rt↔dark"
  }


let ktstreamRtDarkBridge =
  test "RT.KTStream roundtrips through the Dark-side ValueType bridge" {
    let original = RT.ValueType.Known(RT.KTStream VT.string)
    let restored = RT2DT.ValueType.fromDT (RT2DT.ValueType.toDT original)
    Expect.equal restored original "KTStream<String> survives rt↔dark"
  }


let streamCloseMarksDisposed =
  testTask "Stream.close flips disposed; subsequent next returns None" {
    let s = streamOfList [ RT.DInt64 42L ] VT.int64

    // Replicate what the Stream.close builtin does: grab the lock,
    // flip the flag.
    match s with
    | RT.DStream(_, disposed, lockObj) ->
      System.Threading.Monitor.Enter(lockObj)
      try
        disposed.Value <- true
      finally
        System.Threading.Monitor.Exit(lockObj)
    | _ -> failtest "expected DStream"

    let! after = Dval.readStreamNext s |> Ply.toTask
    Expect.equal after None "closed stream yields None on next"

    // Idempotent — calling close a second time (repeat the same block)
    // should not throw.
    match s with
    | RT.DStream(_, disposed, lockObj) ->
      System.Threading.Monitor.Enter(lockObj)
      try
        disposed.Value <- true
      finally
        System.Threading.Monitor.Exit(lockObj)
    | _ -> failtest "unreachable"

    let! stillNone = Dval.readStreamNext s |> Ply.toTask
    Expect.equal stillNone None "still None after second close"
  }


let dstreamDarkBridgeElides =
  // DStream can't round-trip through the Dark-side bridge — the pull fn
  // is a closure, and the elided form is strictly for LSP/reflection.
  test "DStream rt↔dark bridge elides to DStreamElided; fromDT raises" {
    let s = streamOfList [ RT.DInt64 1L ] VT.int64
    let dt = RT2DT.Dval.toDT s
    // Expected shape: DEnum case "DStreamElided" carrying the element VT.
    match dt with
    | RT.DEnum(_, _, [], "DStreamElided", [ _ ]) -> ()
    | _ -> failtest $"expected DStreamElided case, got {dt}"

    Expect.throws
      (fun () -> RT2DT.Dval.fromDT dt |> ignore<RT.Dval>)
      "fromDT should raise on DStreamElided (no pull fn to rebuild from)"
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
      dstreamDarkBridgeElides ]
