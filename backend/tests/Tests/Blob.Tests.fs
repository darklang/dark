/// Tests for the Blob Dval — see thinking/blobs-and-streams/.
///
/// This file grows across Phase 1 chunks. Chunk 1.2 covers the
/// ephemeral-blob byte-store on ExecutionState; later chunks extend it
/// with serialization roundtrips, promotion, and memory-bound
/// assertions.
module Tests.Blob

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open TestUtils.TestUtils

module RT = LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module Exe = LibExecution.Execution
module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes


/// Minimal ExecutionState suitable for exercising blob helpers
/// without needing a full test canvas.
let private freshState () : RT.ExecutionState =
  let builtins = localBuiltIns pmPT
  Exe.createState
    builtins
    pmRT
    Exe.noTracing
    (fun _ _ _ _ -> uply { return () })
    (fun _ _ _ _ -> uply { return () })
    PT.mainBranchId
    { canvasID = System.Guid.NewGuid()
      internalFnsAllowed = false
      dbs = Map.empty
      secrets = [] }


let ephemeralRoundtrip =
  testTask "ephemeral blob roundtrips bytes through the store" {
    let state = freshState ()
    let payload = [| 1uy; 2uy; 3uy; 4uy; 5uy |]

    let dv = Dval.newEphemeralBlob state payload

    match dv with
    | RT.DBlob(RT.Ephemeral _) -> ()
    | _ -> failtest $"expected DBlob(Ephemeral _), got {dv}"

    let ref =
      match dv with
      | RT.DBlob ref -> ref
      | _ -> failtest "unreachable"

    let! bytes = Dval.readBlobBytes state ref |> Ply.toTask
    Expect.equal bytes payload "roundtripped bytes match original"
  }


let twoEphemeralsAreDistinct =
  testTask "two ephemeral blobs with same bytes have distinct handles" {
    let state = freshState ()

    let dv1 = Dval.newEphemeralBlob state [| 7uy; 7uy; 7uy |]
    let dv2 = Dval.newEphemeralBlob state [| 7uy; 7uy; 7uy |]

    let ref1, ref2 =
      match dv1, dv2 with
      | RT.DBlob r1, RT.DBlob r2 -> r1, r2
      | _ -> failtest "expected DBlob for both"

    match ref1, ref2 with
    | RT.Ephemeral id1, RT.Ephemeral id2 ->
      Expect.notEqual id1 id2 "each mint gets a fresh uuid"
    | _ -> failtest "expected Ephemeral for both"

    let! b1 = Dval.readBlobBytes state ref1 |> Ply.toTask
    let! b2 = Dval.readBlobBytes state ref2 |> Ply.toTask
    Expect.equal b1 [| 7uy; 7uy; 7uy |] "first blob reads its bytes"
    Expect.equal b2 [| 7uy; 7uy; 7uy |] "second blob reads its bytes"
  }


let missingEphemeralRaises =
  testTask "reading an unknown ephemeral id raises" {
    let state = freshState ()
    let bogusRef = RT.Ephemeral(System.Guid.NewGuid())

    let mutable raised = false
    try
      let! _ = Dval.readBlobBytes state bogusRef |> Ply.toTask
      ()
    with _ ->
      raised <- true

    Expect.isTrue raised "expected an exception on missing ephemeral id"
  }


let persistentBlobBinaryRoundtrip =
  test "DBlob(Persistent _) roundtrips through the binary serializer" {
    let original =
      RT.DBlob(
        RT.Persistent(
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
          1024L
        )
      )
    let bytes = BS.RT.Dval.serialize "dval" original
    let restored = BS.RT.Dval.deserialize "dval" bytes
    Expect.equal restored original "Persistent ref survives binary roundtrip"
  }


let ephemeralBlobBinaryRaises =
  test "DBlob(Ephemeral _) serialization raises until chunk 1.6 lands" {
    let dv = RT.DBlob(RT.Ephemeral(System.Guid.NewGuid()))
    Expect.throws
      (fun () -> BS.RT.Dval.serialize "dval" dv |> ignore<byte[]>)
      "ephemeral blob must raise on serialize (pending promotion in chunk 1.6)"
  }


let tblobBinaryRoundtrip =
  test "TBlob roundtrips through PT and RT binary type-reference serializers" {
    // PT side
    let ptBytes =
      let s = new System.IO.MemoryStream()
      let w = new System.IO.BinaryWriter(s)
      LibSerialization.Binary.Serializers.PT.TypeReference.write w PT.TBlob
      s.ToArray()
    let ptRestored =
      let r = new System.IO.BinaryReader(new System.IO.MemoryStream(ptBytes))
      LibSerialization.Binary.Serializers.PT.TypeReference.read r
    Expect.equal ptRestored PT.TBlob "PT.TBlob roundtrips"

    // RT side
    let rtBytes =
      let s = new System.IO.MemoryStream()
      let w = new System.IO.BinaryWriter(s)
      LibSerialization.Binary.Serializers.RT.TypeReference.write w RT.TBlob
      s.ToArray()
    let rtRestored =
      let r = new System.IO.BinaryReader(new System.IO.MemoryStream(rtBytes))
      LibSerialization.Binary.Serializers.RT.TypeReference.read r
    Expect.equal rtRestored RT.TBlob "RT.TBlob roundtrips"
  }


let ktblobBinaryRoundtrip =
  test "KTBlob roundtrips through the RT ValueType binary serializer" {
    let original = RT.ValueType.Known RT.KTBlob
    let bytes =
      let s = new System.IO.MemoryStream()
      let w = new System.IO.BinaryWriter(s)
      LibSerialization.Binary.Serializers.RT.ValueType.write w original
      s.ToArray()
    let restored =
      let r = new System.IO.BinaryReader(new System.IO.MemoryStream(bytes))
      LibSerialization.Binary.Serializers.RT.ValueType.read r
    Expect.equal restored original "KTBlob ValueType roundtrips"
  }


let tblobPtDarkBridge =
  test "PT.TBlob roundtrips through the Dark-side bridge" {
    let restored = PT2DT.TypeReference.fromDT (PT2DT.TypeReference.toDT PT.TBlob)
    Expect.equal restored PT.TBlob "PT.TBlob survives pt↔dark roundtrip"
  }


let tblobRtDarkBridge =
  test "RT.TBlob roundtrips through the Dark-side bridge" {
    let restored = RT2DT.TypeReference.fromDT (RT2DT.TypeReference.toDT RT.TBlob)
    Expect.equal restored RT.TBlob "RT.TBlob survives rt↔dark roundtrip"
  }


let ktblobRtDarkBridge =
  test "RT.KTBlob roundtrips through the Dark-side ValueType bridge" {
    let original = RT.ValueType.Known RT.KTBlob
    let restored = RT2DT.ValueType.fromDT (RT2DT.ValueType.toDT original)
    Expect.equal restored original "KTBlob survives rt↔dark roundtrip"
  }


let dblobPersistentDarkBridge =
  test "DBlob(Persistent _) roundtrips through the rt↔dark dval bridge" {
    let original =
      RT.DBlob(
        RT.Persistent(
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
          4096L
        )
      )
    let restored = RT2DT.Dval.fromDT (RT2DT.Dval.toDT original)
    Expect.equal restored original "DBlob(Persistent) survives dval bridge"
  }


let dblobEphemeralDarkBridge =
  // Chunk 1.4 note: the design doc suggests the ephemeral branch of
  // the rt↔dark dval bridge should eventually force promotion (1.6),
  // but LSP/reflection needs to render ephemeral blobs too. Current
  // encoding preserves both variants distinctly; this roundtrip
  // verifies that ephemeral survives the bridge without promotion.
  test "DBlob(Ephemeral _) survives rt↔dark dval bridge without promotion" {
    let id = System.Guid.NewGuid()
    let original = RT.DBlob(RT.Ephemeral id)
    let restored = RT2DT.Dval.fromDT (RT2DT.Dval.toDT original)
    Expect.equal restored original "DBlob(Ephemeral) survives dval bridge"
  }


let tests =
  testList
    "blob"
    [ ephemeralRoundtrip
      twoEphemeralsAreDistinct
      missingEphemeralRaises
      persistentBlobBinaryRoundtrip
      ephemeralBlobBinaryRaises
      tblobBinaryRoundtrip
      ktblobBinaryRoundtrip
      tblobPtDarkBridge
      tblobRtDarkBridge
      ktblobRtDarkBridge
      dblobPersistentDarkBridge
      dblobEphemeralDarkBridge ]
