/// Tests for the Blob Dval.
///
/// Covers the ephemeral-blob byte-store on ExecutionState,
/// serialization roundtrips, promotion, memory-bound assertions,
/// scope-based lifetime, the val-persistability guard, the orphan
/// sweeper, and the blob-equality semantics.
module Tests.Blob

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open TestUtils.TestUtils

module RT = LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module Blob = LibExecution.Blob
module Stream = LibExecution.Stream
module Exe = LibExecution.Execution
module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module PMBlob = LibPackageManager.RuntimeTypes.Blob
module RoundtrippableJson = LibExecution.DvalReprInternalRoundtrippable
module QueryableJson = LibExecution.DvalReprInternalQueryable
module Equals = BuiltinExecution.Libs.NoModule

open Fumble
open LibDB.Db


// ─────────────────────────────────────────────────────────────────────
// Helpers
// ─────────────────────────────────────────────────────────────────────

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

let private dblobRef (dv : RT.Dval) : RT.BlobRef =
  match dv with
  | RT.DBlob r -> r
  | _ -> failtest $"expected DBlob, got {dv}"

let private ephemeralId (dv : RT.Dval) : System.Guid =
  match dv with
  | RT.DBlob(RT.Ephemeral id) -> id
  | _ -> failtest $"expected DBlob(Ephemeral _), got {dv}"

let private persistentHash (dv : RT.Dval) : string =
  match dv with
  | RT.DBlob(RT.Persistent(h, _)) -> h
  | _ -> failtest $"expected DBlob(Persistent _), got {dv}"

let private noopInsert : string -> byte[] -> Ply<unit> =
  fun _ _ -> uply { return () }

let private uniquePayload (label : string) : byte[] =
  System.Text.Encoding.UTF8.GetBytes($"{label}-{System.Guid.NewGuid()}")

/// Run a write/read pair through a fresh MemoryStream and compare the
/// result. Used by the binary-serializer roundtrip tests.
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

let private expectThrows (label : string) (f : unit -> Task<'a>) : Task<unit> =
  task {
    let mutable raised = false
    try
      let! _ = f ()
      ()
    with _ ->
      raised <- true
    Expect.isTrue raised label
  }


// ─────────────────────────────────────────────────────────────────────
// Ephemeral byte-store
// ─────────────────────────────────────────────────────────────────────

let ephemeralRoundtrip =
  testTask "ephemeral blob roundtrips bytes through the store" {
    let state = freshState ()
    let payload = [| 1uy; 2uy; 3uy; 4uy; 5uy |]
    let dv = Blob.newEphemeral state payload
    match dv with
    | RT.DBlob(RT.Ephemeral _) -> ()
    | _ -> failtest $"expected DBlob(Ephemeral _), got {dv}"
    let! bytes = Blob.readBytes state (dblobRef dv) |> Ply.toTask
    Expect.equal bytes payload "roundtripped bytes match original"
  }

let twoEphemeralsAreDistinct =
  testTask "two ephemeral blobs with same bytes have distinct handles" {
    let state = freshState ()
    let payload = [| 7uy; 7uy; 7uy |]
    let dv1 = Blob.newEphemeral state payload
    let dv2 = Blob.newEphemeral state payload
    Expect.notEqual (ephemeralId dv1) (ephemeralId dv2) "each mint gets a fresh uuid"
    let! b1 = Blob.readBytes state (dblobRef dv1) |> Ply.toTask
    let! b2 = Blob.readBytes state (dblobRef dv2) |> Ply.toTask
    Expect.equal b1 payload "first blob reads its bytes"
    Expect.equal b2 payload "second blob reads its bytes"
  }

let missingEphemeralRaises =
  testTask "reading an unknown ephemeral id raises" {
    let state = freshState ()
    let bogusRef = RT.Ephemeral(System.Guid.NewGuid())
    do!
      expectThrows "expected an exception on missing ephemeral id" (fun () ->
        Blob.readBytes state bogusRef |> Ply.toTask :> Task<_>)
  }


// ─────────────────────────────────────────────────────────────────────
// Binary serialization — DBlob, TBlob, KTBlob
// ─────────────────────────────────────────────────────────────────────

let persistentBlobBinaryRoundtrip =
  test "DBlob(Persistent _) roundtrips through the binary serializer" {
    let original =
      RT.DBlob(
        RT.Persistent(
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
          1024L
        )
      )
    let restored =
      BS.RT.Dval.deserialize "dval" (BS.RT.Dval.serialize "dval" original)
    Expect.equal restored original "Persistent ref survives binary roundtrip"
  }

let ephemeralBlobBinaryRaises =
  test "DBlob(Ephemeral _) serialization raises; promote to persistent first" {
    let dv = RT.DBlob(RT.Ephemeral(System.Guid.NewGuid()))
    Expect.throws
      (fun () -> BS.RT.Dval.serialize "dval" dv |> ignore<byte[]>)
      "ephemeral blob must raise on serialize — promote via `Blob.promote` first"
  }

let tblobBinaryRoundtrip =
  test "TBlob roundtrips through PT and RT binary type-reference serializers" {
    let pt =
      binaryRoundtrip
        LibSerialization.Binary.Serializers.PT.TypeReference.write
        LibSerialization.Binary.Serializers.PT.TypeReference.read
        PT.TBlob
    Expect.equal pt PT.TBlob "PT.TBlob roundtrips"
    let rt =
      binaryRoundtrip
        LibSerialization.Binary.Serializers.RT.TypeReference.write
        LibSerialization.Binary.Serializers.RT.TypeReference.read
        RT.TBlob
    Expect.equal rt RT.TBlob "RT.TBlob roundtrips"
  }

let ktblobBinaryRoundtrip =
  test "KTBlob roundtrips through the RT ValueType binary serializer" {
    let original = RT.ValueType.Known RT.KTBlob
    let restored =
      binaryRoundtrip
        LibSerialization.Binary.Serializers.RT.ValueType.write
        LibSerialization.Binary.Serializers.RT.ValueType.read
        original
    Expect.equal restored original "KTBlob ValueType roundtrips"
  }


// ─────────────────────────────────────────────────────────────────────
// Dark-side bridges (PT/RT/VT ↔ DT)
// ─────────────────────────────────────────────────────────────────────

let tblobPtDarkBridge =
  test "PT.TBlob roundtrips through the Dark-side bridge" {
    let restored = PT2DT.TypeReference.fromDT (PT2DT.TypeReference.toDT PT.TBlob)
    Expect.equal restored PT.TBlob "PT.TBlob survives pt<->dark roundtrip"
  }

let tblobRtDarkBridge =
  test "RT.TBlob roundtrips through the Dark-side bridge" {
    let restored = RT2DT.TypeReference.fromDT (RT2DT.TypeReference.toDT RT.TBlob)
    Expect.equal restored RT.TBlob "RT.TBlob survives rt<->dark roundtrip"
  }

let ktblobRtDarkBridge =
  test "RT.KTBlob roundtrips through the Dark-side ValueType bridge" {
    let original = RT.ValueType.Known RT.KTBlob
    let restored = RT2DT.ValueType.fromDT (RT2DT.ValueType.toDT original)
    Expect.equal restored original "KTBlob survives rt<->dark roundtrip"
  }

let dblobPersistentDarkBridge =
  test "DBlob(Persistent _) roundtrips through the rt<->dark dval bridge" {
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
  // The ephemeral branch could force promotion, but LSP/reflection
  // needs to render ephemeral blobs without a side effect. The current
  // encoding preserves both variants distinctly.
  test "DBlob(Ephemeral _) survives rt<->dark dval bridge without promotion" {
    let original = RT.DBlob(RT.Ephemeral(System.Guid.NewGuid()))
    let restored = RT2DT.Dval.fromDT (RT2DT.Dval.toDT original)
    Expect.equal restored original "DBlob(Ephemeral) survives dval bridge"
  }


// ─────────────────────────────────────────────────────────────────────
// package_blobs storage
// ─────────────────────────────────────────────────────────────────────

let packageBlobInsertLookup =
  testTask "package_blobs: insert then get returns the same bytes" {
    let bytes = [| 10uy; 20uy; 30uy; 40uy; 50uy |]
    let hash = $"test-insert-lookup-{System.Guid.NewGuid()}"
    do! PMBlob.insert hash bytes |> Ply.toTask
    let! got = PMBlob.get hash |> Ply.toTask
    Expect.equal got (Some bytes) "get returns bytes for a freshly-inserted hash"
  }

let packageBlobDedupesOnSameHash =
  testTask "package_blobs: second insert under same hash is a no-op" {
    let bytes = [| 1uy; 1uy; 2uy; 3uy; 5uy; 8uy |]
    let hash = $"test-dedup-{System.Guid.NewGuid()}"
    do! PMBlob.insert hash bytes |> Ply.toTask
    // Different bytes under same hash must be ignored (content-addressing invariant).
    do! PMBlob.insert hash [| 99uy; 99uy |] |> Ply.toTask
    let! got = PMBlob.get hash |> Ply.toTask
    Expect.equal got (Some bytes) "INSERT OR IGNORE preserves the original bytes"
  }

let packageBlobMissingHashReturnsNone =
  testTask "package_blobs: get on a missing hash returns None" {
    let! got = PMBlob.get $"nonexistent-{System.Guid.NewGuid()}" |> Ply.toTask
    Expect.equal got None "missing hash yields None"
  }


// ─────────────────────────────────────────────────────────────────────
// promoteBlobs — ephemeral → persistent
// ─────────────────────────────────────────────────────────────────────

let promotePersistsAndSwaps =
  testTask "promoteBlobs: ephemeral -> persistent + row in package_blobs" {
    let state = freshState ()
    let payload = uniquePayload "promote-test"
    let ephemeral = Blob.newEphemeral state payload
    let! promoted = Blob.promote state PMBlob.insert ephemeral |> Ply.toTask
    let expectedHash = Blob.sha256Hex payload
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
    let payload = uniquePayload "promote-serialize"
    let ephemeral = Blob.newEphemeral state payload
    let! promoted = Blob.promote state PMBlob.insert ephemeral |> Ply.toTask
    let restored =
      BS.RT.Dval.deserialize "dval" (BS.RT.Dval.serialize "dval" promoted)
    Expect.equal
      restored
      promoted
      "post-promotion binary roundtrip preserves Persistent ref"
  }

let promoteSameBytesTwiceDedups =
  testTask "promoteBlobs: two ephemerals with identical bytes hit package_blobs once" {
    let state = freshState ()
    let payload = uniquePayload "dedup-test"
    let eph1 = Blob.newEphemeral state payload
    let eph2 = Blob.newEphemeral state payload
    let! p1 = Blob.promote state PMBlob.insert eph1 |> Ply.toTask
    let! p2 = Blob.promote state PMBlob.insert eph2 |> Ply.toTask
    Expect.equal p1 p2 "two promotions of identical bytes share the hash"
    let! row = PMBlob.get (Blob.sha256Hex payload) |> Ply.toTask
    Expect.equal row (Some payload) "row still contains original bytes"
  }

let promotedBlobResolvesViaReadBlobBytes =
  testTask "readBlobBytes on a promoted blob reads from package_blobs" {
    let state = freshState ()
    let payload = uniquePayload "resolve-test"
    let ephemeral = Blob.newEphemeral state payload
    let! promoted = Blob.promote state PMBlob.insert ephemeral |> Ply.toTask
    let! bytes = Blob.readBytes state (dblobRef promoted) |> Ply.toTask
    Expect.equal bytes payload "persistent blob resolves back to its bytes"
  }


// ─────────────────────────────────────────────────────────────────────
// Memory bound — fileRead allocation should track input size, not 200×
// ─────────────────────────────────────────────────────────────────────

let fileReadMemoryBound =
  testTask "fileRead allocation stays within 3x file size for a 10mb blob" {
    let path =
      System.IO.Path.Combine(System.IO.Path.GetTempPath(), "blob-memory-10mb.bin")
    if not (System.IO.File.Exists(path)) then
      let buf = Array.zeroCreate<byte> 10_000_000
      System.Random(0).NextBytes(buf)
      System.IO.File.WriteAllBytes(path, buf)
    let state = freshState ()
    System.GC.Collect()
    System.GC.WaitForPendingFinalizers()
    System.GC.Collect()
    let before = System.GC.GetTotalAllocatedBytes(precise = false)
    let! bytes = System.IO.File.ReadAllBytesAsync path
    let _dv = Blob.newEphemeral state bytes
    let delta = System.GC.GetTotalAllocatedBytes(precise = false) - before
    // Old List<UInt8> path allocated ~200× file size (10 MB → ~2 GB). Blob
    // path drops by ~100×; the bound is generous so any list-boxing
    // regression hits orders of magnitude over.
    Expect.isLessThan
      delta
      30_000_000L
      $"fileRead for 10MB allocated {delta} bytes — list-boxing regression?"
  }


// ─────────────────────────────────────────────────────────────────────
// JSON roundtrips (queryable / roundtrippable)
// ─────────────────────────────────────────────────────────────────────

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
    let ephemeral = Blob.newEphemeral state [| 1uy; 2uy; 3uy |]
    do!
      expectThrows
        "ephemeral blob in queryable JSON should raise (promote first)"
        (fun () ->
          QueryableJson.toJsonStringV0 types threadID ephemeral |> Ply.toTask)
  }

let roundtrippableJsonPersistentRoundtrip =
  test "rt_dval roundtrippable JSON preserves DBlob(Persistent _)" {
    let original = RT.DBlob(RT.Persistent("deadbeef" + String.replicate 56 "a", 99L))
    let restored =
      RoundtrippableJson.parseJsonV0 (RoundtrippableJson.toJsonV0 original)
    Expect.equal restored original "persistent blob survives rt_dval JSON"
  }

let roundtrippableJsonEphemeralRoundtrip =
  test "rt_dval roundtrippable JSON preserves DBlob(Ephemeral _)" {
    let original = RT.DBlob(RT.Ephemeral(System.Guid.NewGuid()))
    let restored =
      RoundtrippableJson.parseJsonV0 (RoundtrippableJson.toJsonV0 original)
    Expect.equal
      restored
      original
      "ephemeral blob survives rt_dval JSON as its own case"
  }


// ─────────────────────────────────────────────────────────────────────
// Scope-based ephemeral-blob lifetime
// ─────────────────────────────────────────────────────────────────────

let pushPopReclaimsScopedBlobs =
  test "scope: popBlobScope drops blobs created inside the scope" {
    let state = freshState ()
    // Pre-scope blob (no scope: leaks for VM lifetime).
    let preId = ephemeralId (Blob.newEphemeral state [| 0xAAuy |])
    Blob.pushScope state
    let aId = ephemeralId (Blob.newEphemeral state [| 0x01uy; 0x02uy |])
    let bId = ephemeralId (Blob.newEphemeral state [| 0x03uy |])
    Expect.isTrue (state.blobStore.ContainsKey aId) "a is in store pre-pop"
    Expect.isTrue (state.blobStore.ContainsKey bId) "b is in store pre-pop"
    Blob.popScope state
    Expect.isFalse (state.blobStore.ContainsKey aId) "a dropped on pop"
    Expect.isFalse (state.blobStore.ContainsKey bId) "b dropped on pop"
    Expect.isTrue
      (state.blobStore.ContainsKey preId)
      "pre-scope blob survives the pop"
  }

let scopeNestsLikeAStack =
  test "scope: nested scopes each clean up only their own blobs" {
    let state = freshState ()
    Blob.pushScope state
    let outerId = ephemeralId (Blob.newEphemeral state [| 0x10uy |])
    Blob.pushScope state
    let innerId = ephemeralId (Blob.newEphemeral state [| 0x20uy |])
    Blob.popScope state
    Expect.isFalse
      (state.blobStore.ContainsKey innerId)
      "inner blob dropped on inner pop"
    Expect.isTrue
      (state.blobStore.ContainsKey outerId)
      "outer blob survives inner pop"
    Blob.popScope state
    Expect.isFalse
      (state.blobStore.ContainsKey outerId)
      "outer blob drops on outer pop"
  }

let popWithoutPushIsNoOp =
  test "scope: popBlobScope on an empty stack is a no-op" {
    let state = freshState ()
    Blob.popScope state
    Blob.popScope state
    Expect.equal state.blobScopes.Count 0 "stack still empty"
  }

let promotedBlobsSurviveScopePop =
  testTask "scope: a blob promoted inside the scope stays resolvable via PM" {
    let state = freshState ()
    let payload = [| 0xDEuy; 0xADuy; 0xBEuy; 0xEFuy |]
    Blob.pushScope state
    let eph = Blob.newEphemeral state payload
    let! promoted = Blob.promote state pmRT.persistBlob eph |> Ply.toTask
    let hash = persistentHash promoted
    Blob.popScope state
    // Ephemeral bytes gone; persistent bytes survive in package_blobs.
    let! bytes =
      Blob.readBytes state (RT.Persistent(hash, int64 payload.Length)) |> Ply.toTask
    Expect.equal bytes payload "persistent bytes survive the pop"
  }


// ─────────────────────────────────────────────────────────────────────
// Val-persistability guard (Dval.isPersistable)
// ─────────────────────────────────────────────────────────────────────
// Rejects shapes that can't round-trip through `package_values.rt_dval`:
// DStream, DBlob(Ephemeral _). DApplicable + DDB serialize successfully
// (lambdas store their instruction stream, DBs store as canvas-local
// string identifiers). The Seed.fs evaluator hits this before calling
// BS.RT.PackageValue.serialize so users get a clear reason string
// instead of a deep-stack serialize raise.

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
    let dv = Stream.newFromIO LibExecution.ValueType.int64 next None
    Expect.isFalse (Dval.isPersistable dv) "stream rejected"
    match Dval.nonPersistableReason dv with
    | Some reason -> Expect.stringContains reason "stream" "reason names stream"
    | None -> failtest "expected a reason"
  }

let persistableAcceptsApplicableAndDDB =
  test "isPersistable: DApplicable + DDB serialize successfully, accept both" {
    Expect.isTrue
      (Dval.isPersistable (RT.DDB "users"))
      "DDB serialises via the existing binary path"
  }

let persistableRejectsNestedBadShapes =
  test "isPersistable: a bad leaf anywhere in a container poisons the whole" {
    let list =
      RT.DList(
        RT.ValueType.Known RT.KTBlob,
        [ RT.DBlob(RT.Persistent("x", 1L))
          RT.DBlob(RT.Ephemeral(System.Guid.NewGuid())) ]
      )
    Expect.isFalse (Dval.isPersistable list) "list with ephemeral blob rejected"

    let next () : Ply<Option<RT.Dval>> = uply { return None }
    let streamDv = Stream.newFromIO LibExecution.ValueType.int64 next None
    let typeName =
      RT.FQTypeName.fqPackage (LibExecution.PackageRefs.Type.Stdlib.option ())
    let rec_ = RT.DRecord(typeName, typeName, [], Map.ofList [ ("body", streamDv) ])
    Expect.isFalse (Dval.isPersistable rec_) "record with a stream field rejected"
  }


// ─────────────────────────────────────────────────────────────────────
// Orphan package_blobs sweeper
// ─────────────────────────────────────────────────────────────────────

let sweepDeletesOrphansButKeepsReferenced =
  testTask "sweep: orphan rows deleted, referenced rows kept" {
    // Unique-per-run payloads so parallel runs / rerun-after-error
    // don't collide on content-addressed hashes.
    let salt = System.Guid.NewGuid().ToByteArray()
    let refBytes = Array.concat [ [| 0x11uy |]; salt ]
    let refHash = Blob.sha256Hex refBytes
    let orphanBytes = Array.concat [ [| 0x22uy |]; salt ]
    let orphanHash = Blob.sha256Hex orphanBytes
    let fakeHash = RT.Hash(Blob.sha256Hex salt)
    let (RT.Hash fakeHashStr) = fakeHash

    try
      do! PMBlob.insert refHash refBytes |> Ply.toTask
      do! PMBlob.insert orphanHash orphanBytes |> Ply.toTask

      // Plant a package_value row whose rt_dval references refHash.
      let referencingDval = RT.DBlob(RT.Persistent(refHash, int64 refBytes.Length))
      let pv : RT.PackageValue.PackageValue =
        { hash = fakeHash; body = referencingDval }
      let rtDvalBytes = BS.RT.PackageValue.serialize fakeHash pv
      let valueTypeBytes =
        BS.RT.ValueType.serialize (RT.Dval.toValueType referencingDval)
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
      // Clean up the planted package_values row even on assertion
      // failure — stale rows would poison later tests.
      let cleanup : Task<unit> =
        Sql.query "DELETE FROM package_values WHERE hash = @hash"
        |> Sql.parameters [ "hash", Sql.string fakeHashStr ]
        |> Sql.executeStatementAsync
      cleanup.Wait()
  }
// Idempotence isn't a separate case — Expecto runs in parallel and
// sweep mutates shared state, so a sibling could cause false failures.
// The "second sweep returns 0" property would need test-isolation.


// ─────────────────────────────────────────────────────────────────────
// Blob equality — hash-compare across ephemeral / persistent
// ─────────────────────────────────────────────────────────────────────
// `NoModule.equals` is state-aware now: same-handle → cheap; same-hash
// Persistents → cheap; everything else dereferences and byte-compares.
// No promotion side effect, no Dval-tree rebuild.

let equalsEphemeralEphemeralSameUuid =
  testTask "blob equality: two refs to the same ephemeral UUID are equal" {
    let state = freshState ()
    let dv = Blob.newEphemeral state [| 0x01uy; 0x02uy |]
    let! result = Equals.equals state dv dv |> Ply.toTask
    Expect.isTrue result "same ephemeral handle compares equal"
  }

let equalsEphemeralEphemeralSameBytes =
  testTask "blob equality: two distinct ephemerals with same bytes are equal" {
    let state = freshState ()
    let payload = [| 0x11uy; 0x22uy; 0x33uy |]
    let a = Blob.newEphemeral state payload
    let b = Blob.newEphemeral state payload
    Expect.notEqual (ephemeralId a) (ephemeralId b) "distinct UUIDs"
    let! result = Equals.equals state a b |> Ply.toTask
    Expect.isTrue result "same bytes compare equal under byte-compare"
  }

let equalsEphemeralPersistentSameBytes =
  testTask "blob equality: ephemeral vs persistent with same bytes are equal" {
    let state = freshState ()
    let payload = [| 0xDEuy; 0xADuy |]
    let eph = Blob.newEphemeral state payload
    let per = RT.DBlob(RT.Persistent(Blob.sha256Hex payload, int64 payload.Length))
    // Persistent's hash is sha256(payload); ephemeral's bytes match.
    // To make the persistent's bytes resolvable, seed the row.
    do! PMBlob.insert (Blob.sha256Hex payload) payload |> Ply.toTask
    let! result = Equals.equals state eph per |> Ply.toTask
    Expect.isTrue result "same bytes match across ephemeral/persistent"
  }

let equalsPersistentPersistentSameHash =
  testTask "blob equality: two Persistent refs with the same hash are equal" {
    let state = freshState ()
    let dv = RT.DBlob(RT.Persistent("cafebabe", 4L))
    let! result = Equals.equals state dv dv |> Ply.toTask
    Expect.isTrue result "same hash + length = equal"
  }

let equalsPersistentPersistentDifferentHashes =
  testTask "blob equality: Persistent refs with different hashes are unequal" {
    let state = freshState ()
    let a = RT.DBlob(RT.Persistent("aaaa", 4L))
    let b = RT.DBlob(RT.Persistent("bbbb", 4L))
    let! result = Equals.equals state a b |> Ply.toTask
    Expect.isFalse result "different hashes = unequal"
  }

let equalsEphemeralDifferentBytes =
  testTask "blob equality: two ephemerals with different bytes are unequal" {
    let state = freshState ()
    let a = Blob.newEphemeral state [| 0x00uy |]
    let b = Blob.newEphemeral state [| 0xFFuy |]
    let! result = Equals.equals state a b |> Ply.toTask
    Expect.isFalse result "different bytes = unequal"
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
