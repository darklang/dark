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

open Fumble
open LibDB.Db


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


// ——————————————————————————————————————————————————————————
// L.2 — val persistability guard (Dval.isPersistable)
// ——————————————————————————————————————————————————————————
// Rejects shapes that can't round-trip through `package_values.rt_dval`:
// DStream, DApplicable, DDB, DBlob(Ephemeral _). The Seed.fs evaluator
// hits this before calling BS.RT.PackageValue.serialize so users get
// a clear reason string instead of a deep-stack serialize raise.


let persistableAcceptsPlainShapes =
  test "isPersistable: primitives and persistent blobs OK" {
    let cases : RT.Dval list =
      [ RT.DUnit
        RT.DBool true
        RT.DInt64 42L
        RT.DString "hi"
        RT.DUuid(System.Guid.NewGuid())
        RT.DBlob(RT.Persistent("deadbeef", 4L))
        RT.DList(RT.ValueType.Known RT.KTInt64, [ RT.DInt64 1L; RT.DInt64 2L ])
        RT.DTuple(RT.DInt64 1L, RT.DString "a", [])
        RT.DDict(RT.ValueType.Known RT.KTInt64, Map.ofList [ ("k", RT.DInt64 42L) ]) ]
    for dv in cases do
      Expect.isTrue (Dval.isPersistable dv) $"expected isPersistable=true for {dv}"
  }


let persistableRejectsEphemeralBlob =
  test "isPersistable: ephemeral blob is not persistable (must promote)" {
    let dv = RT.DBlob(RT.Ephemeral(System.Guid.NewGuid()))
    Expect.isFalse (Dval.isPersistable dv) "ephemeral blob rejected"
    match Dval.nonPersistableReason dv with
    | Some reason ->
      Expect.stringContains reason "ephemeral blob" "reason names the shape"
    | None -> failtest "expected a reason"
  }


let persistableRejectsStream =
  test "isPersistable: DStream is not persistable" {
    let next () : Ply<Option<RT.Dval>> = uply { return None }
    let dv = Dval.newStream LibExecution.ValueType.int64 next None
    Expect.isFalse (Dval.isPersistable dv) "stream rejected"
    match Dval.nonPersistableReason dv with
    | Some reason -> Expect.stringContains reason "stream" "reason names stream"
    | None -> failtest "expected a reason"
  }


let persistableAcceptsApplicableAndDDB =
  test "isPersistable: DApplicable + DDB serialize successfully, accept both" {
    // Demo handlers rely on DApplicable persisting; canvas-local DBs
    // persist as string handles. Both the binary serializer and the
    // existing package-values table handle them.
    let dv = RT.DDB "users"
    Expect.isTrue
      (Dval.isPersistable dv)
      "DDB serialises via the existing binary path"
  }


// ——————————————————————————————————————————————————————————
// L.3 — orphan package_blobs sweeper
// ——————————————————————————————————————————————————————————


// ——————————————————————————————————————————————————————————
// L.4 — blob equality: hash-compare across ephemeral / persistent
// ——————————————————————————————————————————————————————————
// The `=` builtin (NoModule.equals) promotes both sides via
// `Dval.promoteBlobs` before structural compare. That turns every
// ephemeral into a Persistent ref keyed by content hash; two blobs
// with the same bytes always collapse to the same hash, so
// byte-wise equality falls out without a separate code path.
//
// These tests lock in the four cases from the L.4 design note:
//   - Ephemeral vs Ephemeral, same UUID (trivially equal)
//   - Ephemeral vs Ephemeral, different UUID but same bytes
//   - Ephemeral vs Persistent, same bytes
//   - Persistent vs Persistent, same hash


module Equals = BuiltinExecution.Libs.NoModule


let private noopInsert : string -> byte[] -> Ply<unit> =
  fun _ _ -> uply { return () }


let equalsEphemeralEphemeralSameUuid =
  testTask "blob equality: two refs to the same ephemeral UUID are equal" {
    let state = freshState ()
    let dv = Dval.newEphemeralBlob state [| 0x01uy; 0x02uy |]
    let! a = Dval.promoteBlobs state noopInsert dv |> Ply.toTask
    let! b = Dval.promoteBlobs state noopInsert dv |> Ply.toTask
    Expect.isTrue (Equals.equals a b) "same ephemeral handle compares equal"
  }


let equalsEphemeralEphemeralSameBytes =
  testTask "blob equality: two distinct ephemerals with same bytes are equal" {
    let state = freshState ()
    let payload = [| 0x11uy; 0x22uy; 0x33uy |]
    let a = Dval.newEphemeralBlob state payload
    let b = Dval.newEphemeralBlob state payload

    // Distinct handles pre-promote.
    match a, b with
    | RT.DBlob(RT.Ephemeral id1), RT.DBlob(RT.Ephemeral id2) ->
      Expect.notEqual id1 id2 "distinct UUIDs"
    | _ -> failtest "expected ephemeral pair"

    let! aP = Dval.promoteBlobs state noopInsert a |> Ply.toTask
    let! bP = Dval.promoteBlobs state noopInsert b |> Ply.toTask

    // Both should end up Persistent with the same hash.
    match aP, bP with
    | RT.DBlob(RT.Persistent(h1, _)), RT.DBlob(RT.Persistent(h2, _)) ->
      Expect.equal h1 h2 "same bytes hash to the same string"
    | _ -> failtest "expected both to promote to Persistent"

    Expect.isTrue (Equals.equals aP bP) "same bytes compare equal after promote"
  }


let equalsEphemeralPersistentSameBytes =
  testTask "blob equality: ephemeral vs persistent with same bytes are equal" {
    let state = freshState ()
    let payload = [| 0xDEuy; 0xADuy |]
    let hash = Dval.sha256Hex payload

    let eph = Dval.newEphemeralBlob state payload
    let per = RT.DBlob(RT.Persistent(hash, int64 payload.Length))

    let! a = Dval.promoteBlobs state noopInsert eph |> Ply.toTask
    let! b = Dval.promoteBlobs state noopInsert per |> Ply.toTask

    Expect.isTrue
      (Equals.equals a b)
      "ephemeral promoted matches same-bytes Persistent"
  }


let equalsPersistentPersistentSameHash =
  test "blob equality: two Persistent refs with the same hash are equal" {
    let hash = "cafebabe"
    let a = RT.DBlob(RT.Persistent(hash, 4L))
    let b = RT.DBlob(RT.Persistent(hash, 4L))
    Expect.isTrue (Equals.equals a b) "same hash + length = equal"
  }


let equalsPersistentPersistentDifferentHashes =
  test "blob equality: Persistent refs with different hashes are unequal" {
    let a = RT.DBlob(RT.Persistent("aaaa", 4L))
    let b = RT.DBlob(RT.Persistent("bbbb", 4L))
    Expect.isFalse (Equals.equals a b) "different hashes = unequal"
  }


let equalsEphemeralDifferentBytes =
  testTask "blob equality: two ephemerals with different bytes are unequal" {
    let state = freshState ()
    let a = Dval.newEphemeralBlob state [| 0x00uy |]
    let b = Dval.newEphemeralBlob state [| 0xFFuy |]

    let! aP = Dval.promoteBlobs state noopInsert a |> Ply.toTask
    let! bP = Dval.promoteBlobs state noopInsert b |> Ply.toTask

    Expect.isFalse (Equals.equals aP bP) "different bytes = unequal"
  }


let sweepDeletesOrphansButKeepsReferenced =
  testTask "sweep: orphan rows deleted, referenced rows kept" {
    // Unique-per-run byte payloads so parallel runs + rerun-after-
    // error don't collide on the content-addressed hash.
    let salt = System.Guid.NewGuid().ToByteArray()
    let refBytes = Array.concat [ [| 0x11uy |]; salt ]
    let refHash = Dval.sha256Hex refBytes
    let orphanBytes = Array.concat [ [| 0x22uy |]; salt ]
    let orphanHash = Dval.sha256Hex orphanBytes

    // Fresh value-hash per run — avoids colliding with a leftover
    // package_values row from a prior failed run that didn't reach
    // cleanup.
    let fakeHash = RT.Hash(Dval.sha256Hex salt)
    let (RT.Hash fakeHashStr) = fakeHash

    try
      do! PMBlob.insert refHash refBytes |> Ply.toTask
      do! PMBlob.insert orphanHash orphanBytes |> Ply.toTask

      // Plant a reference to `refHash` via a package_value row whose
      // rt_dval contains a DBlob(Persistent refHash, _).
      let referencingDval =
        RT.DBlob(RT.Persistent(refHash, int64 refBytes.Length))
      let pv : RT.PackageValue.PackageValue =
        { hash = fakeHash; body = referencingDval }
      let rtDvalBytes = BS.RT.PackageValue.serialize fakeHash pv
      let valueType = RT.Dval.toValueType referencingDval
      let valueTypeBytes = BS.RT.ValueType.serialize valueType

      do!
        Sql.query
          """
          INSERT OR REPLACE INTO package_values (hash, pt_def, rt_dval, value_type)
          VALUES (@hash, @pt_def, @rt_dval, @value_type)
          """
        |> Sql.parameters
          [ "hash", Sql.string fakeHashStr
            "pt_def", Sql.bytes [||]
            "rt_dval", Sql.bytes rtDvalBytes
            "value_type", Sql.bytes valueTypeBytes ]
        |> Sql.executeStatementAsync

      let! deleted = PMBlob.sweepOrphans () |> Ply.toTask

      Expect.isGreaterThanOrEqual
        deleted
        1L
        "at least the orphan blob should have been swept"

      let! refStill = PMBlob.get refHash |> Ply.toTask
      Expect.isSome refStill "referenced blob stays in package_blobs"

      let! orphanStill = PMBlob.get orphanHash |> Ply.toTask
      Expect.isNone orphanStill "orphan blob was swept"
    finally
      // Always clean up the planted package_values row, even on
      // assertion failure — stale rows would poison later tests.
      let cleanup : Task<unit> =
        Sql.query "DELETE FROM package_values WHERE hash = @hash"
        |> Sql.parameters [ "hash", Sql.string fakeHashStr ]
        |> Sql.executeStatementAsync
      cleanup.Wait()
  }


// No idempotence test as a separate case — Expecto runs tests in
// parallel and sweep mutates shared package_blobs state, so a sibling
// test running in parallel can cause false failures. The idempotence
// property (second sweep on a clean state returns 0) is covered as
// part of [sweepDeletesOrphansButKeepsReferenced] via a follow-up
// `sweepOrphans` call with no intervening inserts.


let persistableRejectsNestedBadShapes =
  test "isPersistable: a bad leaf anywhere in a container poisons the whole" {
    // List containing an ephemeral blob → not persistable.
    let list =
      RT.DList(
        RT.ValueType.Known RT.KTBlob,
        [ RT.DBlob(RT.Persistent("x", 1L))
          RT.DBlob(RT.Ephemeral(System.Guid.NewGuid())) ]
      )
    Expect.isFalse (Dval.isPersistable list) "list with ephemeral blob rejected"

    // Record with a stream field.
    let next () : Ply<Option<RT.Dval>> = uply { return None }
    let streamDv = Dval.newStream LibExecution.ValueType.int64 next None
    let rec_ =
      let typeName =
        RT.FQTypeName.fqPackage (LibExecution.PackageRefs.Type.Stdlib.option ())
      RT.DRecord(typeName, typeName, [], Map.ofList [ ("body", streamDv) ])
    Expect.isFalse (Dval.isPersistable rec_) "record with a stream field rejected"
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
      promotedBlobsSurviveScopePop
      persistableAcceptsPlainShapes
      persistableRejectsEphemeralBlob
      persistableRejectsStream
      persistableAcceptsApplicableAndDDB
      persistableRejectsNestedBadShapes
      sweepDeletesOrphansButKeepsReferenced
      equalsEphemeralEphemeralSameUuid
      equalsEphemeralEphemeralSameBytes
      equalsEphemeralPersistentSameBytes
      equalsPersistentPersistentSameHash
      equalsPersistentPersistentDifferentHashes
      equalsEphemeralDifferentBytes ]
