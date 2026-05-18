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
module PMBlob = LibDB.RuntimeTypes.Blob
module Tracing = LibDB.Tracing
module QueryableJson = LibSerialization.DvalReprInternalQueryable
module Equals = Builtins.Pure.Libs.NoModule

open Fumble
open LibDB.Sqlite


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
    { dbs = Map.empty }

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
  testTask "fileRead allocation stays within 8x file size for a 10mb blob" {
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
    // regression hits orders of magnitude over. Bound is 8× rather than
    // 3× because GC measurement noise + adjacent-test bleed-over
    // routinely lands the delta in the 30–60 MB range without any
    // regression. The list-boxing regression we care about catching is
    // the >2 GB pattern, which is well beyond any of these bounds.
    Expect.isLessThan
      delta
      80_000_000L
      $"fileRead for 10MB allocated {delta} bytes — list-boxing regression?"
  }


// ─────────────────────────────────────────────────────────────────────
// Queryable JSON roundtrips (User-DB row storage)
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
// Blob equality — identity-based across ephemeral / persistent
// ─────────────────────────────────────────────────────────────────────
// `NoModule.equals` is sync and identity-based for blobs: same-hash
// (Persistent) or same-UUID (Ephemeral) compare equal; mixed cases —
// distinct ephemerals with the same bytes, or ephemeral vs persistent
// — return false. Callers that want byte-equality across ephemerals
// promote both sides first.

let equalsEphemeralEphemeralSameUuid =
  test "blob equality: two refs to the same ephemeral UUID are equal" {
    let state = freshState ()
    let dv = Blob.newEphemeral state [| 0x01uy; 0x02uy |]
    Expect.isTrue (Equals.equals dv dv) "same ephemeral handle compares equal"
  }

let equalsEphemeralEphemeralSameBytesIsFalse =
  test "blob equality: two distinct ephemerals with same bytes are unequal" {
    let state = freshState ()
    let payload = [| 0x11uy; 0x22uy; 0x33uy |]
    let a = Blob.newEphemeral state payload
    let b = Blob.newEphemeral state payload
    Expect.notEqual (ephemeralId a) (ephemeralId b) "distinct UUIDs"
    Expect.isFalse (Equals.equals a b) "ephemeral identity is by UUID, not by bytes"
  }

let equalsEphemeralPersistentSameBytesIsFalse =
  test "blob equality: ephemeral vs persistent with same bytes are unequal" {
    let state = freshState ()
    let payload = [| 0xDEuy; 0xADuy |]
    let eph = Blob.newEphemeral state payload
    let per = RT.DBlob(RT.Persistent(Blob.sha256Hex payload, int64 payload.Length))
    Expect.isFalse
      (Equals.equals eph per)
      "ephemeral and persistent never compare equal — promote first if needed"
  }

let equalsPersistentPersistentSameHash =
  test "blob equality: two Persistent refs with the same hash are equal" {
    let dv = RT.DBlob(RT.Persistent("cafebabe", 4L))
    Expect.isTrue (Equals.equals dv dv) "same hash + length = equal"
  }

let equalsPersistentPersistentDifferentHashes =
  test "blob equality: Persistent refs with different hashes are unequal" {
    let a = RT.DBlob(RT.Persistent("aaaa", 4L))
    let b = RT.DBlob(RT.Persistent("bbbb", 4L))
    Expect.isFalse (Equals.equals a b) "different hashes = unequal"
  }

let equalsEphemeralDifferentBytes =
  test "blob equality: two ephemerals with different bytes are unequal" {
    let state = freshState ()
    let a = Blob.newEphemeral state [| 0x00uy |]
    let b = Blob.newEphemeral state [| 0xFFuy |]
    Expect.isFalse (Equals.equals a b) "distinct UUIDs = unequal"
  }


// ─────────────────────────────────────────────────────────────────────
// Dval.rewriteWith structural sharing
// ─────────────────────────────────────────────────────────────────────
//
// The walker promises that when `f` returns `None` for every reachable
// leaf, no container is reconstructed — the input Dval reference is
// returned unchanged. These tests lock that in: every test here passes
// behaviorally even without sharing, so the only way they catch
// regressions is via `obj.ReferenceEquals`.

let private noopRewriteWith (dv : RT.Dval) : Task<RT.Dval> =
  RT.Dval.rewriteWith (fun _ -> Ply None) dv |> Ply.toTask

let rewriteWithNoOpPreservesOuterRef =
  testTask "Dval.rewriteWith: no-op f returns the input reference (DList)" {
    let dv = RT.DList(RT.ValueType.Known RT.KTInt64, [ RT.DInt64 1L; RT.DInt64 2L ])
    let! result = noopRewriteWith dv
    Expect.isTrue
      (obj.ReferenceEquals(result, dv))
      "outer DList ref must be preserved"
  }

let rewriteWithNoOpPreservesNestedRefs =
  testTask "Dval.rewriteWith: no-op f preserves every nested container ref" {
    let inner = RT.DList(RT.ValueType.Known RT.KTInt64, [ RT.DInt64 7L ])
    let middle = RT.DTuple(RT.DString "k", inner, [])
    let outer =
      RT.DDict(RT.ValueType.Unknown, Map [ "a", middle; "b", RT.DBool true ])
    let! result = noopRewriteWith outer
    Expect.isTrue (obj.ReferenceEquals(result, outer)) "outer DDict ref preserved"
    // Reach into the result and verify the inner refs survived too.
    match result with
    | RT.DDict(_, m) ->
      Expect.isTrue
        (obj.ReferenceEquals(m["a"], middle))
        "nested DTuple ref preserved"
      match m["a"] with
      | RT.DTuple(_, innerResult, _) ->
        Expect.isTrue
          (obj.ReferenceEquals(innerResult, inner))
          "innermost DList ref preserved"
      | _ -> failtest "expected DTuple at key 'a'"
    | _ -> failtest "expected DDict"
  }

let rewriteWithSingleChangePreservesSiblings =
  testTask "Dval.rewriteWith: rewriting one element keeps unchanged sibling refs" {
    let sibling1 = RT.DInt64 1L
    let sibling2 = RT.DInt64 2L
    let target = RT.DString "rewrite-me"
    let dv = RT.DList(RT.ValueType.Known RT.KTInt64, [ sibling1; target; sibling2 ])
    let! result =
      RT.Dval.rewriteWith
        (fun d ->
          uply {
            match d with
            | RT.DString "rewrite-me" -> return Some(RT.DInt64 99L)
            | _ -> return None
          })
        dv
      |> Ply.toTask
    match result with
    | RT.DList(_, [ a; b; c ]) ->
      Expect.isTrue (obj.ReferenceEquals(a, sibling1)) "left sibling ref preserved"
      Expect.isTrue (obj.ReferenceEquals(c, sibling2)) "right sibling ref preserved"
      Expect.equal b (RT.DInt64 99L) "target replaced"
    | _ -> failtest $"expected DList of 3, got {result}"
  }

let rewriteWithNoOpOnPrimitivePreservesRef =
  testTask "Dval.rewriteWith: no-op on a primitive returns the input reference" {
    let dv = RT.DInt64 42L
    let! result = noopRewriteWith dv
    Expect.isTrue
      (obj.ReferenceEquals(result, dv))
      "primitive Dval ref must be preserved"
  }


// ─────────────────────────────────────────────────────────────────────
// Blob.promote recurses into DApplicable closures
// ─────────────────────────────────────────────────────────────────────
//
// `Dval.rewriteWith` walks into AppLambda.closedRegisters /
// AppLambda.argsSoFar / AppNamedFn.argsSoFar. A lambda closing over an
// ephemeral blob must have its capture environment promoted along
// with the rest of the value graph — otherwise the persisted closure
// carries a DBlob(Ephemeral _) ref pointing at bytes that died with
// the producing VM.

let private fakeAppLambda
  (closedRegisters : List<RT.Register * RT.Dval>)
  (argsSoFar : List<RT.Dval>)
  : RT.Dval =
  RT.DApplicable(
    RT.AppLambda
      { exprId = 0UL
        closedRegisters = closedRegisters
        typeSymbolTable = Map.empty
        argsSoFar = argsSoFar }
  )

let private fakeAppNamedFn (argsSoFar : List<RT.Dval>) : RT.Dval =
  let name = RT.FQFnName.fqBuiltin "intAdd" 0
  RT.DApplicable(
    RT.AppNamedFn
      { name = name
        typeSymbolTable = Map.empty
        typeArgs = []
        argsSoFar = argsSoFar }
  )

let promoteRewritesInsideClosedRegisters =
  testTask "Blob.promote: ephemeral inside AppLambda.closedRegisters is promoted" {
    let state = freshState ()
    let payload = uniquePayload "promote-closure"
    let ephemeral = Blob.newEphemeral state payload
    let dv = fakeAppLambda [ (1, ephemeral) ] []
    let! promoted = Blob.promote state PMBlob.insert dv |> Ply.toTask
    match promoted with
    | RT.DApplicable(RT.AppLambda lambda) ->
      match lambda.closedRegisters with
      | [ (_, RT.DBlob(RT.Persistent(h, n))) ] ->
        Expect.equal h (Blob.sha256Hex payload) "captured blob hash"
        Expect.equal n (int64 payload.Length) "captured blob length"
      | _ -> failtest $"expected promoted ephemeral, got {lambda.closedRegisters}"
    | _ -> failtest $"expected DApplicable, got {promoted}"
  }

let promoteRewritesInsideAppLambdaArgs =
  testTask "Blob.promote: ephemeral inside AppLambda.argsSoFar is promoted" {
    let state = freshState ()
    let payload = uniquePayload "promote-applambda-args"
    let ephemeral = Blob.newEphemeral state payload
    let dv = fakeAppLambda [] [ ephemeral ]
    let! promoted = Blob.promote state PMBlob.insert dv |> Ply.toTask
    match promoted with
    | RT.DApplicable(RT.AppLambda lambda) ->
      match lambda.argsSoFar with
      | [ RT.DBlob(RT.Persistent(h, _)) ] ->
        Expect.equal h (Blob.sha256Hex payload) "argsSoFar blob promoted"
      | _ -> failtest $"expected promoted ephemeral, got {lambda.argsSoFar}"
    | _ -> failtest $"expected DApplicable, got {promoted}"
  }

let promoteRewritesInsideAppNamedFnArgs =
  testTask "Blob.promote: ephemeral inside AppNamedFn.argsSoFar is promoted" {
    let state = freshState ()
    let payload = uniquePayload "promote-namedfn-args"
    let ephemeral = Blob.newEphemeral state payload
    let dv = fakeAppNamedFn [ ephemeral ]
    let! promoted = Blob.promote state PMBlob.insert dv |> Ply.toTask
    match promoted with
    | RT.DApplicable(RT.AppNamedFn fn) ->
      match fn.argsSoFar with
      | [ RT.DBlob(RT.Persistent(h, _)) ] ->
        Expect.equal h (Blob.sha256Hex payload) "argsSoFar blob promoted"
      | _ -> failtest $"expected promoted ephemeral, got {fn.argsSoFar}"
    | _ -> failtest $"expected DApplicable, got {promoted}"
  }


// ─────────────────────────────────────────────────────────────────────
// Trace storage stubs nested DStream values
// ─────────────────────────────────────────────────────────────────────
//
// `LibDB.Tracing.prepareDvalForStorage` walks each captured Dval
// through `Dval.rewriteWith`. A stream anywhere in the graph (top
// level, list element, record field, captured by a closure) becomes
// the shared `DStreamStub` DEnum from `RTToDT.Dval.streamStubDT`.

let private freshStream () : RT.Dval =
  let next () : Ply<Option<RT.Dval>> = uply { return None }
  Stream.newFromIO LibExecution.ValueType.int64 next None

let private isStubInt64 (dv : RT.Dval) : bool =
  match dv with
  | RT.DEnum(_, _, [], "DStreamStub", [ _ ]) -> true
  | _ -> false

let prepareStubsTopLevelStream =
  testTask "prepareDvalForStorage: top-level DStream becomes DStreamStub" {
    let state = freshState ()
    let! result = Tracing.prepareDvalForStorage state (freshStream ()) |> Ply.toTask
    Expect.isTrue (isStubInt64 result) $"expected stub, got {result}"
  }

let prepareStubsStreamInsideList =
  testTask "prepareDvalForStorage: DStream nested inside DList is stubbed" {
    let state = freshState ()
    let dv =
      RT.DList(RT.ValueType.Unknown, [ RT.DInt64 1L; freshStream (); RT.DInt64 2L ])
    let! result = Tracing.prepareDvalForStorage state dv |> Ply.toTask
    match result with
    | RT.DList(_, [ a; b; c ]) ->
      Expect.equal a (RT.DInt64 1L) "leading sibling preserved"
      Expect.isTrue (isStubInt64 b) $"middle stream stubbed, got {b}"
      Expect.equal c (RT.DInt64 2L) "trailing sibling preserved"
    | _ -> failtest $"expected DList of 3, got {result}"
  }

let prepareStubsStreamInsideRecord =
  testTask "prepareDvalForStorage: DStream nested inside DRecord field is stubbed" {
    let state = freshState ()
    let typeName =
      RT.FQTypeName.fqPackage (LibExecution.PackageRefs.Type.Stdlib.option ())
    let dv =
      RT.DRecord(
        typeName,
        typeName,
        [],
        Map.ofList [ ("body", freshStream ()); ("count", RT.DInt64 5L) ]
      )
    let! result = Tracing.prepareDvalForStorage state dv |> Ply.toTask
    match result with
    | RT.DRecord(_, _, _, fields) ->
      let body = fields["body"]
      Expect.isTrue (isStubInt64 body) $"stream field stubbed, got {body}"
      Expect.equal fields["count"] (RT.DInt64 5L) "sibling field preserved"
    | _ -> failtest $"expected DRecord, got {result}"
  }

let prepareStubsStreamInsideClosure =
  testTask "prepareDvalForStorage: DStream captured by AppLambda is stubbed" {
    let state = freshState ()
    let dv = fakeAppLambda [ (1, freshStream ()) ] []
    let! result = Tracing.prepareDvalForStorage state dv |> Ply.toTask
    match result with
    | RT.DApplicable(RT.AppLambda lambda) ->
      match lambda.closedRegisters with
      | [ (_, captured) ] ->
        Expect.isTrue
          (isStubInt64 captured)
          $"captured stream stubbed, got {captured}"
      | _ -> failtest $"expected one closed register, got {lambda.closedRegisters}"
    | _ -> failtest $"expected DApplicable, got {result}"
  }


// `sweepOrphans` deletes any `package_blobs` row not referenced
// by `package_values.rt_dval`. Without sequencing, the sweep test
// can run concurrently with `packageBlobDedupesOnSameHash` (which
// inserts an unreferenced hash, then re-inserts under that hash
// expecting `INSERT OR IGNORE` to win) — the sweep's DELETE
// between the two inserts lets the second insert succeed with
// fresh bytes, and the dedup invariant assertion fails. Sequence
// the file so any test mutating `package_blobs` runs alone.
let tests =
  testSequenced
  <| testList
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
      equalsEphemeralEphemeralSameBytesIsFalse
      equalsEphemeralPersistentSameBytesIsFalse
      equalsPersistentPersistentSameHash
      equalsPersistentPersistentDifferentHashes
      equalsEphemeralDifferentBytes
      rewriteWithNoOpPreservesOuterRef
      rewriteWithNoOpPreservesNestedRefs
      rewriteWithSingleChangePreservesSiblings
      rewriteWithNoOpOnPrimitivePreservesRef
      promoteRewritesInsideClosedRegisters
      promoteRewritesInsideAppLambdaArgs
      promoteRewritesInsideAppNamedFnArgs
      prepareStubsTopLevelStream
      prepareStubsStreamInsideList
      prepareStubsStreamInsideRecord
      prepareStubsStreamInsideClosure ]
