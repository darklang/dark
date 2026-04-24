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
module PMBlob = LibPackageManager.RuntimeTypes.Blob
module RoundtrippableJson = LibExecution.DvalReprInternalRoundtrippable
module QueryableJson = LibExecution.DvalReprInternalQueryable


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

    let! bytes = Dval.readBlobBytes state pmRT ref |> Ply.toTask
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

    let! b1 = Dval.readBlobBytes state pmRT ref1 |> Ply.toTask
    let! b2 = Dval.readBlobBytes state pmRT ref2 |> Ply.toTask
    Expect.equal b1 [| 7uy; 7uy; 7uy |] "first blob reads its bytes"
    Expect.equal b2 [| 7uy; 7uy; 7uy |] "second blob reads its bytes"
  }


let missingEphemeralRaises =
  testTask "reading an unknown ephemeral id raises" {
    let state = freshState ()
    let bogusRef = RT.Ephemeral(System.Guid.NewGuid())

    let mutable raised = false
    try
      let! _ = Dval.readBlobBytes state pmRT bogusRef |> Ply.toTask
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


let packageBlobInsertLookup =
  testTask "package_blobs: insert then get returns the same bytes" {
    let bytes = [| 10uy; 20uy; 30uy; 40uy; 50uy |]
    // Unique hash per test so runs don't collide with prior rows
    let hash =
      $"test-insert-lookup-{System.Guid.NewGuid()}-e3b0c44298fc1c149afbf4c8996fb924"
    do! PMBlob.insert hash bytes |> Ply.toTask
    let! got = PMBlob.get hash |> Ply.toTask
    Expect.equal got (Some bytes) "get returns bytes for a freshly-inserted hash"
  }


let packageBlobDedupesOnSameHash =
  testTask "package_blobs: second insert under same hash is a no-op" {
    let bytes = [| 1uy; 1uy; 2uy; 3uy; 5uy; 8uy |]
    let hash =
      $"test-dedup-{System.Guid.NewGuid()}-e3b0c44298fc1c149afbf4c8996fb9242"
    do! PMBlob.insert hash bytes |> Ply.toTask
    // Second insert with same hash but different bytes MUST be ignored
    // (content-addressing invariant: hash determines content).
    do! PMBlob.insert hash [| 99uy; 99uy |] |> Ply.toTask
    let! got = PMBlob.get hash |> Ply.toTask
    Expect.equal got (Some bytes) "INSERT OR IGNORE preserves the original bytes"
  }


let packageBlobMissingHashReturnsNone =
  testTask "package_blobs: get on a missing hash returns None" {
    let hash = $"nonexistent-{System.Guid.NewGuid()}-hash-that-was-never-inserted"
    let! got = PMBlob.get hash |> Ply.toTask
    Expect.equal got None "missing hash yields None"
  }


let promotePersistsAndSwaps =
  testTask "promoteBlobs: ephemeral → persistent + row in package_blobs" {
    let state = freshState ()
    let payload =
      // unique bytes per run so the row is new (content-addressed)
      System.Text.Encoding.UTF8.GetBytes(
        $"promote-test-{System.Guid.NewGuid()}-bytes-to-hash"
      )

    let ephemeral = Dval.newEphemeralBlob state payload

    let! promoted = Dval.promoteBlobs state PMBlob.insert ephemeral |> Ply.toTask

    let expectedHash = Dval.sha256Hex payload

    match promoted with
    | RT.DBlob(RT.Persistent(h, n)) ->
      Expect.equal h expectedHash "hash matches SHA-256 of bytes"
      Expect.equal n (int64 payload.Length) "length matches"
    | _ -> failtest $"expected Persistent, got {promoted}"

    let! row = PMBlob.get expectedHash |> Ply.toTask
    Expect.equal row (Some payload) "package_blobs row exists with our bytes"
  }


let promoteThenSerializeRoundtrips =
  testTask "promoteBlobs then binary serialize roundtrips cleanly" {
    let state = freshState ()
    let payload =
      System.Text.Encoding.UTF8.GetBytes(
        $"promote-serialize-{System.Guid.NewGuid()}"
      )
    let ephemeral = Dval.newEphemeralBlob state payload
    let! promoted = Dval.promoteBlobs state PMBlob.insert ephemeral |> Ply.toTask
    let bytes = BS.RT.Dval.serialize "dval" promoted
    let restored = BS.RT.Dval.deserialize "dval" bytes
    Expect.equal
      restored
      promoted
      "post-promotion binary roundtrip preserves Persistent ref"
  }


let promoteSameBytesTwiceDedups =
  testTask "promoteBlobs: two ephemerals with identical bytes hit package_blobs once" {
    let state = freshState ()
    let payload =
      System.Text.Encoding.UTF8.GetBytes(
        $"dedup-test-{System.Guid.NewGuid()}-same-bytes-twice"
      )

    let eph1 = Dval.newEphemeralBlob state payload
    let eph2 = Dval.newEphemeralBlob state payload

    let! p1 = Dval.promoteBlobs state PMBlob.insert eph1 |> Ply.toTask
    let! p2 = Dval.promoteBlobs state PMBlob.insert eph2 |> Ply.toTask

    // Same bytes -> same hash -> same DBlob Persistent ref.
    Expect.equal p1 p2 "two promotions of identical bytes share the hash"

    // And the bytes still resolve (INSERT OR IGNORE didn't clobber the row).
    let hash = Dval.sha256Hex payload
    let! row = PMBlob.get hash |> Ply.toTask
    Expect.equal row (Some payload) "row still contains original bytes"
  }


let promotedBlobResolvesViaReadBlobBytes =
  testTask "readBlobBytes on a promoted blob reads from package_blobs" {
    let state = freshState ()
    let payload =
      System.Text.Encoding.UTF8.GetBytes(
        $"resolve-test-{System.Guid.NewGuid()}-after-promote"
      )
    let ephemeral = Dval.newEphemeralBlob state payload
    let! promoted = Dval.promoteBlobs state PMBlob.insert ephemeral |> Ply.toTask

    let ref =
      match promoted with
      | RT.DBlob r -> r
      | _ -> failtest "expected DBlob"

    let! bytes = Dval.readBlobBytes state pmRT ref |> Ply.toTask
    Expect.equal bytes payload "persistent blob resolves back to its bytes"
  }


let fileReadMemoryBound =
  testTask "fileRead allocation stays within 3x file size for a 10mb blob" {
    // Mirrors phase-0's fileRead scenario but with the new Blob-returning
    // builtin. Should allocate ~file-size bytes plus a little overhead
    // (the byte[] itself plus a single DBlob Dval + a UUID) instead of
    // N boxed DUInt8 cells.
    let path =
      System.IO.Path.Combine(System.IO.Path.GetTempPath(), "blob-memory-10mb.bin")
    if not (System.IO.File.Exists(path)) then
      let buf = Array.zeroCreate<byte> 10_000_000
      System.Random(0).NextBytes(buf)
      System.IO.File.WriteAllBytes(path, buf)

    // Build state first so its allocations don't contaminate the delta.
    let state = freshState ()

    System.GC.Collect()
    System.GC.WaitForPendingFinalizers()
    System.GC.Collect()
    let before = System.GC.GetTotalAllocatedBytes(precise = false)

    // Directly exercise the same code path Builtin.fileRead drives into:
    // ReadAllBytes + newEphemeralBlob.
    let! bytes = System.IO.File.ReadAllBytesAsync path
    let _dv = Dval.newEphemeralBlob state bytes

    let after = System.GC.GetTotalAllocatedBytes(precise = false)
    let delta = after - before

    // Phase 0 baseline: 10MB fileRead cost ~2GB alloc. The new blob
    // path should drop by ~100x. Bound is generous — anything remotely
    // resembling a list-boxing regression hits orders of magnitude over
    // this.
    Expect.isLessThan
      delta
      30_000_000L
      $"fileRead for 10MB allocated {delta} bytes — a list-boxing regression may have crept in"
  }


let queryableJsonRoundtrip =
  testTask "User-DB queryable JSON roundtrips a persistent blob dval" {
    let types = { RT.Types.empty with package = pmRT.getType }
    let threadID = System.Guid.NewGuid()

    let original =
      RT.DBlob(
        RT.Persistent(
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
          2048L
        )
      )

    let! json = QueryableJson.toJsonStringV0 types threadID original |> Ply.toTask

    // Envelope shape: {"type":"blob","hash":"...","length":N}
    Expect.stringContains json "\"type\":\"blob\"" "has blob envelope tag"
    Expect.stringContains
      json
      "\"hash\":\"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855\""
      "has the content hash"

    let! restored =
      QueryableJson.parseJsonV0 types threadID Map.empty RT.TBlob json |> Ply.toTask
    Expect.equal restored original "persistent blob survives queryable JSON"
  }


let queryableJsonEphemeralRaises =
  testTask "User-DB queryable JSON raises on ephemeral blob (promotion needed)" {
    let state = freshState ()
    let types = { RT.Types.empty with package = pmRT.getType }
    let threadID = System.Guid.NewGuid()

    let ephemeral = Dval.newEphemeralBlob state [| 1uy; 2uy; 3uy |]

    let mutable raised = false
    try
      let! _ = QueryableJson.toJsonStringV0 types threadID ephemeral |> Ply.toTask
      ()
    with _ ->
      raised <- true

    Expect.isTrue
      raised
      "ephemeral blob in queryable JSON should raise (promote first)"
  }


let roundtrippableJsonPersistentRoundtrip =
  test "rt_dval roundtrippable JSON preserves DBlob(Persistent _)" {
    let original = RT.DBlob(RT.Persistent("deadbeef" + String.replicate 56 "a", 99L))
    let json = RoundtrippableJson.toJsonV0 original
    let restored = RoundtrippableJson.parseJsonV0 json
    Expect.equal restored original "persistent blob survives rt_dval JSON"
  }


let roundtrippableJsonEphemeralRoundtrip =
  test "rt_dval roundtrippable JSON preserves DBlob(Ephemeral _)" {
    let id = System.Guid.NewGuid()
    let original = RT.DBlob(RT.Ephemeral id)
    let json = RoundtrippableJson.toJsonV0 original
    let restored = RoundtrippableJson.parseJsonV0 json
    Expect.equal
      restored
      original
      "ephemeral blob survives rt_dval JSON as its own case"
  }


// ——————————————————————————————————————————————————————————
// L.1 — scope-based ephemeral-blob lifetime
// ——————————————————————————————————————————————————————————


let pushPopReclaimsScopedBlobs =
  test "scope: popBlobScope drops blobs created inside the scope" {
    let state = freshState ()

    // Before any scope: plain ephemeral, leaks for the VM's life.
    let pre = Dval.newEphemeralBlob state [| 0xAAuy |]
    let preId =
      match pre with
      | RT.DBlob(RT.Ephemeral id) -> id
      | _ -> failtest "expected ephemeral"

    // Enter a scope; create two more blobs.
    Dval.pushBlobScope state
    let a = Dval.newEphemeralBlob state [| 0x01uy; 0x02uy |]
    let b = Dval.newEphemeralBlob state [| 0x03uy |]
    let aId, bId =
      match a, b with
      | RT.DBlob(RT.Ephemeral x), RT.DBlob(RT.Ephemeral y) -> x, y
      | _ -> failtest "expected ephemeral"

    Expect.isTrue (state.blobStore.ContainsKey aId) "a is in store pre-pop"
    Expect.isTrue (state.blobStore.ContainsKey bId) "b is in store pre-pop"

    Dval.popBlobScope state

    Expect.isFalse (state.blobStore.ContainsKey aId) "a dropped on pop"
    Expect.isFalse (state.blobStore.ContainsKey bId) "b dropped on pop"
    Expect.isTrue
      (state.blobStore.ContainsKey preId)
      "pre-scope blob survives the pop"
  }


let scopeNestsLikeAStack =
  test "scope: nested scopes each clean up only their own blobs" {
    let state = freshState ()

    Dval.pushBlobScope state
    let outer = Dval.newEphemeralBlob state [| 0x10uy |]
    let outerId =
      match outer with
      | RT.DBlob(RT.Ephemeral id) -> id
      | _ -> failtest "outer"

    Dval.pushBlobScope state
    let inner = Dval.newEphemeralBlob state [| 0x20uy |]
    let innerId =
      match inner with
      | RT.DBlob(RT.Ephemeral id) -> id
      | _ -> failtest "inner"

    // Pop the inner scope — outer blob still tracked by outer scope.
    Dval.popBlobScope state
    Expect.isFalse
      (state.blobStore.ContainsKey innerId)
      "inner blob dropped on inner pop"
    Expect.isTrue
      (state.blobStore.ContainsKey outerId)
      "outer blob survives inner pop"

    Dval.popBlobScope state
    Expect.isFalse
      (state.blobStore.ContainsKey outerId)
      "outer blob drops on outer pop"
  }


let popWithoutPushIsNoOp =
  test "scope: popBlobScope on an empty stack is a no-op" {
    let state = freshState ()
    // Should not throw.
    Dval.popBlobScope state
    Dval.popBlobScope state
    Expect.equal state.blobScopes.Count 0 "stack still empty"
  }


let promotedBlobsSurviveScopePop =
  testTask "scope: a blob promoted inside the scope stays resolvable via PM" {
    let state = freshState ()
    let payload = [| 0xDEuy; 0xADuy; 0xBEuy; 0xEFuy |]

    Dval.pushBlobScope state
    let eph = Dval.newEphemeralBlob state payload

    // Promote inside the scope — writes to package_blobs and returns
    // a Persistent ref with the content hash.
    let! promoted = Dval.promoteBlobs state pmRT.insertBlob eph |> Ply.toTask

    let hash =
      match promoted with
      | RT.DBlob(RT.Persistent(h, _)) -> h
      | _ -> failtest "expected Persistent after promotion"

    Dval.popBlobScope state

    // Ephemeral bytes gone from the in-memory store, but persistent
    // bytes remain in package_blobs. readBlobBytes on the persistent
    // ref resolves through PM.getBlob.
    let! bytes =
      Dval.readBlobBytes state pmRT (RT.Persistent(hash, int64 payload.Length))
      |> Ply.toTask
    Expect.equal bytes payload "persistent bytes survive the pop"
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
      dblobEphemeralDarkBridge
      packageBlobInsertLookup
      packageBlobDedupesOnSameHash
      packageBlobMissingHashReturnsNone
      promotePersistsAndSwaps
      promoteThenSerializeRoundtrips
      promoteSameBytesTwiceDedups
      promotedBlobResolvesViaReadBlobBytes
      fileReadMemoryBound
      queryableJsonRoundtrip
      queryableJsonEphemeralRaises
      roundtrippableJsonPersistentRoundtrip
      roundtrippableJsonEphemeralRoundtrip
      pushPopReclaimsScopedBlobs
      scopeNestsLikeAStack
      popWithoutPushIsNoOp
      promotedBlobsSurviveScopePop ]
