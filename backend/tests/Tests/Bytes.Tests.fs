/// Tests for the Bytes Dval.
///
/// Covers inline ephemeral-blob bytes, serialization roundtrips,
/// promotion, memory-bound assertions, the val-persistability guard,
/// the orphan sweeper, and the blob-equality semantics.
module Tests.Bytes

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open TestUtils.TestUtils

module RT = LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module Bytes = LibExecution.Bytes
module Stream = LibExecution.Stream
module Exe = LibExecution.Execution
module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module PMBytes = LibDB.RuntimeTypes.Bytes
module Tracing = LibDB.Tracing
module Equals = LibExecution.Dval

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

let private dbytesRef (dv : RT.Dval) : RT.BytesRef =
  match dv with
  | RT.DBytes r -> r
  | _ -> failtest $"expected DBytes, got {dv}"

let private ephemeralId (dv : RT.Dval) : System.Guid =
  match dv with
  | RT.DBytes(RT.Ephemeral eph) -> eph.id
  | _ -> failtest $"expected DBytes(Ephemeral _), got {dv}"

let private mkEphemeral (bytes : byte[]) : RT.Dval = Bytes.newEphemeral bytes

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
// Ephemeral blobs — inline bytes
// ─────────────────────────────────────────────────────────────────────

let ephemeralRoundtrip =
  testTask "ephemeral blob roundtrips its inline bytes" {
    let state = freshState ()
    let payload = [| 1uy; 2uy; 3uy; 4uy; 5uy |]
    let dv = Bytes.newEphemeral payload
    match dv with
    | RT.DBytes(RT.Ephemeral _) -> ()
    | _ -> failtest $"expected DBytes(Ephemeral _), got {dv}"
    let! bytes = Bytes.readBytes state (dbytesRef dv) |> Ply.toTask
    Expect.equal bytes payload "roundtripped bytes match original"
  }

let twoEphemeralsAreDistinct =
  testTask "two ephemeral blobs with same bytes have distinct handles" {
    let state = freshState ()
    let payload = [| 7uy; 7uy; 7uy |]
    let dv1 = Bytes.newEphemeral payload
    let dv2 = Bytes.newEphemeral payload
    Expect.notEqual (ephemeralId dv1) (ephemeralId dv2) "each mint gets a fresh uuid"
    let! b1 = Bytes.readBytes state (dbytesRef dv1) |> Ply.toTask
    let! b2 = Bytes.readBytes state (dbytesRef dv2) |> Ply.toTask
    Expect.equal b1 payload "first blob reads its bytes"
    Expect.equal b2 payload "second blob reads its bytes"
  }


// ─────────────────────────────────────────────────────────────────────
// Binary serialization — DBytes, TBytes, KTBytes
// ─────────────────────────────────────────────────────────────────────

let persistentBytesBinaryRoundtrip =
  test "DBytes(Persistent _) roundtrips through the binary serializer" {
    let original =
      RT.DBytes(
        RT.Persistent(
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
          1024L
        )
      )
    let restored =
      BS.RT.Dval.deserialize "dval" (BS.RT.Dval.serialize "dval" original)
    Expect.equal restored original "Persistent ref survives binary roundtrip"
  }

let ephemeralBytesBinaryRaises =
  test "DBytes(Ephemeral _) serialization raises; promote to persistent first" {
    let dv = mkEphemeral [| 1uy; 2uy; 3uy |]
    Expect.throws
      (fun () -> BS.RT.Dval.serialize "dval" dv |> ignore<byte[]>)
      "ephemeral blob must raise on serialize — promote via `Bytes.promote` first"
  }

let tbytesBinaryRoundtrip =
  test "TBytes roundtrips through PT and RT binary type-reference serializers" {
    let pt =
      binaryRoundtrip
        LibSerialization.Binary.Serializers.PT.TypeReference.write
        LibSerialization.Binary.Serializers.PT.TypeReference.read
        PT.TBytes
    Expect.equal pt PT.TBytes "PT.TBytes roundtrips"
    let rt =
      binaryRoundtrip
        LibSerialization.Binary.Serializers.RT.TypeReference.write
        LibSerialization.Binary.Serializers.RT.TypeReference.read
        RT.TBytes
    Expect.equal rt RT.TBytes "RT.TBytes roundtrips"
  }

let ktbytesBinaryRoundtrip =
  test "KTBytes roundtrips through the RT ValueType binary serializer" {
    let original = RT.ValueType.Known RT.KTBytes
    let restored =
      binaryRoundtrip
        LibSerialization.Binary.Serializers.RT.ValueType.write
        LibSerialization.Binary.Serializers.RT.ValueType.read
        original
    Expect.equal restored original "KTBytes ValueType roundtrips"
  }


// ─────────────────────────────────────────────────────────────────────
// Dark-side bridges (PT/RT/VT ↔ DT)
// ─────────────────────────────────────────────────────────────────────

let tbytesPtDarkBridge =
  test "PT.TBytes roundtrips through the Dark-side bridge" {
    let restored = PT2DT.TypeReference.fromDT (PT2DT.TypeReference.toDT PT.TBytes)
    Expect.equal restored PT.TBytes "PT.TBytes survives pt<->dark roundtrip"
  }

let tbytesRtDarkBridge =
  test "RT.TBytes roundtrips through the Dark-side bridge" {
    let restored = RT2DT.TypeReference.fromDT (RT2DT.TypeReference.toDT RT.TBytes)
    Expect.equal restored RT.TBytes "RT.TBytes survives rt<->dark roundtrip"
  }

let ktbytesRtDarkBridge =
  test "RT.KTBytes roundtrips through the Dark-side ValueType bridge" {
    let original = RT.ValueType.Known RT.KTBytes
    let restored = RT2DT.ValueType.fromDT (RT2DT.ValueType.toDT original)
    Expect.equal restored original "KTBytes survives rt<->dark roundtrip"
  }

let dbytesPersistentDarkBridge =
  test "DBytes(Persistent _) roundtrips through the rt<->dark dval bridge" {
    let original =
      RT.DBytes(
        RT.Persistent(
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
          4096L
        )
      )
    let restored = RT2DT.Dval.fromDT (RT2DT.Dval.toDT original)
    Expect.equal restored original "DBytes(Persistent) survives dval bridge"
  }

let dbytesEphemeralDarkBridge =
  test "in-memory Bytes round-trip through reflection without saving or hashing" {
    let original = mkEphemeral [| 0xAAuy; 0xBBuy |]
    let dt = RT2DT.Dval.toDT original
    match dt with
    | RT.DEnum(_, _, _, "DBytes", [ RT.DBytes _ ]) -> ()
    | _ -> failtest $"expected opaque DBytes reflection, got {dt}"
    let restored = RT2DT.Dval.fromDT dt
    Expect.isTrue (obj.ReferenceEquals(restored, original)) "same live value"
    match dbytesRef original with
    | RT.Ephemeral eph ->
      Expect.isFalse eph.IsHashed "reflection does not force hashing"
    | _ -> failtest "reflection must not promote bytes"
  }

let reflectionIgnoresByteStorage =
  testTask "equal Bytes have equal reflection before and after saving" {
    let original = mkEphemeral [| 0xAAuy; 0xBBuy |]
    let! saved = Bytes.promote noopInsert original |> Ply.toTask
    let ephemeralDT = RT2DT.Dval.toDT original
    let persistentDT = RT2DT.Dval.toDT saved
    Expect.isTrue (Equals.equals ephemeralDT persistentDT) "reflected values agree"
    Expect.equal ephemeralDT persistentDT "native reflected equality agrees"
    Expect.equal (hash ephemeralDT) (hash persistentDT) "reflected hashes agree"
    let! savedDT = Bytes.promote noopInsert ephemeralDT |> Ply.toTask
    let restoredDT =
      BS.RT.Dval.deserialize "dval" (BS.RT.Dval.serialize "dval" savedDT)
    Expect.isTrue
      (Equals.equals (RT2DT.Dval.fromDT restoredDT) original)
      "saved reflected bytes round-trip with their contents intact"
  }

let byteLengthDoesNotReadStorage =
  testTask "Bytes.length reads metadata without fetching or hashing the payload" {
    let builtin =
      (Builtins.Pure.Libs.Bytes.fns ())
      |> List.find (fun fn -> fn.name.name = "bytesLength")
      |> Option.defaultWith (fun () -> failtest "bytesLength builtin is missing")
    let state = freshState ()
    let state =
      { state with
          blobs =
            { get = (fun _ -> failtest "length fetched the blob store")
              persist = (fun _ _ -> failtest "length wrote the blob store") } }
    let inMemory = mkEphemeral [| 1uy; 2uy; 3uy |]
    // This payload intentionally does not exist in the store.
    let saved = RT.DBytes(RT.Persistent("not-loaded", 3L))
    for bytes in [ inMemory; saved ] do
      let! actual =
        builtin.fn (struct (state, Unchecked.defaultof<RT.VMState>, [], [| bytes |]))
        |> Ply.toTask
      Expect.equal actual (RT.Dval.int 3I) "length comes from the reference"
    match dbytesRef inMemory with
    | RT.Ephemeral eph -> Expect.isFalse eph.IsHashed "length does not hash bytes"
    | _ -> failtest "length must not promote bytes"
  }

/// Printing calls this for every Bytes it shows, so it must never load saved
/// bytes, and must copy only the prefix it was asked for.
let peekDoesNotReadStorage =
  testTask "bytesPeek copies only the prefix and never reads the store" {
    let builtin =
      (Builtins.Pure.Libs.Bytes.fns ())
      |> List.find (fun fn -> fn.name.name = "bytesPeek")
      |> Option.defaultWith (fun () -> failtest "bytesPeek is missing")
    let state = freshState ()
    let state =
      { state with
          blobs =
            { get = (fun _ -> failtest "the preview fetched the blob store")
              persist = (fun _ _ -> failtest "the preview wrote the blob store") } }
    let call bytes =
      builtin.fn (
        struct (state,
                Unchecked.defaultof<RT.VMState>,
                [],
                [| bytes; RT.Dval.int 16I |])
      )
      |> Ply.toTask
    let big = mkEphemeral (Array.create 1_000_000 7uy)
    match! call big with
    | RT.DEnum(_, _, _, "Some", [ RT.DBytes(RT.Ephemeral prefix) ]) ->
      Expect.equal prefix.bytes.Length 16 "only the prefix is copied"
    | other -> failtest $"expected Some prefix, got {other}"
    match dbytesRef big with
    | RT.Ephemeral eph -> Expect.isFalse eph.IsHashed "the preview does not hash"
    | _ -> failtest "the preview must not promote bytes"
    // This payload intentionally does not exist in the store.
    match! call (RT.DBytes(RT.Persistent("not-loaded", 3L))) with
    | RT.DEnum(_, _, _, "None", []) -> ()
    | other -> failtest $"expected None for saved bytes, got {other}"
  }

// ─────────────────────────────────────────────────────────────────────
// package_blobs storage
// ─────────────────────────────────────────────────────────────────────

let packageBytesInsertLookup =
  testTask "package_blobs: insert then get returns the same bytes" {
    let bytes = [| 10uy; 20uy; 30uy; 40uy; 50uy |]
    let hash = $"test-insert-lookup-{System.Guid.NewGuid()}"
    do! PMBytes.insert hash bytes |> Ply.toTask
    let! got = PMBytes.get hash |> Ply.toTask
    Expect.equal got (Some bytes) "get returns bytes for a freshly-inserted hash"
  }

let packageBytesDedupesOnSameHash =
  testTask "package_blobs: second insert under same hash is a no-op" {
    let bytes = [| 1uy; 1uy; 2uy; 3uy; 5uy; 8uy |]
    let hash = $"test-dedup-{System.Guid.NewGuid()}"
    do! PMBytes.insert hash bytes |> Ply.toTask
    // Different bytes under same hash must be ignored (content-addressing invariant).
    do! PMBytes.insert hash [| 99uy; 99uy |] |> Ply.toTask
    let! got = PMBytes.get hash |> Ply.toTask
    Expect.equal got (Some bytes) "INSERT OR IGNORE preserves the original bytes"
  }

let packageBytesMissingHashReturnsNone =
  testTask "package_blobs: get on a missing hash returns None" {
    let! got = PMBytes.get $"nonexistent-{System.Guid.NewGuid()}" |> Ply.toTask
    Expect.equal got None "missing hash yields None"
  }


// ─────────────────────────────────────────────────────────────────────
// promoteBytes — ephemeral → persistent
// ─────────────────────────────────────────────────────────────────────

let promotePersistsAndSwaps =
  testTask "promoteBytes: ephemeral -> persistent + row in package_blobs" {
    let payload = uniquePayload "promote-test"
    let ephemeral = Bytes.newEphemeral payload
    let! promoted = Bytes.promote PMBytes.insert ephemeral |> Ply.toTask
    let expectedHash = Bytes.sha256Hex payload
    match promoted with
    | RT.DBytes(RT.Persistent(h, n)) ->
      Expect.equal h expectedHash "hash matches SHA-256 of bytes"
      Expect.equal n (int64 payload.Length) "length matches"
    | _ -> failtest $"expected Persistent, got {promoted}"
    let! row = PMBytes.get expectedHash |> Ply.toTask
    Expect.equal row (Some payload) "package_blobs row exists with our bytes"
  }

let promoteThenSerializeRoundtrips =
  testTask "promoteBytes then binary serialize roundtrips cleanly" {
    let payload = uniquePayload "promote-serialize"
    let ephemeral = Bytes.newEphemeral payload
    let! promoted = Bytes.promote PMBytes.insert ephemeral |> Ply.toTask
    let restored =
      BS.RT.Dval.deserialize "dval" (BS.RT.Dval.serialize "dval" promoted)
    Expect.equal
      restored
      promoted
      "post-promotion binary roundtrip preserves Persistent ref"
  }

let promoteSameBytesTwiceDedups =
  testTask "promoteBytes: two ephemerals with identical bytes hit package_blobs once" {
    let payload = uniquePayload "dedup-test"
    let eph1 = Bytes.newEphemeral payload
    let eph2 = Bytes.newEphemeral payload
    let! p1 = Bytes.promote PMBytes.insert eph1 |> Ply.toTask
    let! p2 = Bytes.promote PMBytes.insert eph2 |> Ply.toTask
    Expect.equal p1 p2 "two promotions of identical bytes share the hash"
    let! row = PMBytes.get (Bytes.sha256Hex payload) |> Ply.toTask
    Expect.equal row (Some payload) "row still contains original bytes"
  }

let promotedBytesResolvesViaReadBytes =
  testTask "readBytes on a promoted blob reads from package_blobs" {
    let state = freshState ()
    let payload = uniquePayload "resolve-test"
    let ephemeral = Bytes.newEphemeral payload
    let! promoted = Bytes.promote PMBytes.insert ephemeral |> Ply.toTask
    let! bytes = Bytes.readBytes state (dbytesRef promoted) |> Ply.toTask
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
    System.GC.Collect()
    System.GC.WaitForPendingFinalizers()
    System.GC.Collect()
    let before = System.GC.GetTotalAllocatedBytes(precise = false)
    let! bytes = System.IO.File.ReadAllBytesAsync path
    let _dv = Bytes.newEphemeral bytes
    let delta = System.GC.GetTotalAllocatedBytes(precise = false) - before
    // Old List<UInt8> path allocated ~200x file size (10 MB -> ~2 GB). The bound
    // is 8x rather than 3x because GC noise plus adjacent-test bleed routinely
    // adds 30-60 MB; the list-boxing regression worth catching is the >2 GB
    // pattern, far beyond any bound.
    Expect.isLessThan
      delta
      80_000_000L
      $"fileRead for 10MB allocated {delta} bytes — list-boxing regression?"
  }



// ─────────────────────────────────────────────────────────────────────
// Val-persistability guard (Dval.isPersistable)
// ─────────────────────────────────────────────────────────────────────
// Rejects shapes that can't round-trip through `package_values.rt_dval`: DStream,
// DBytes(Ephemeral _), and a LAMBDA. A lambda serializes -- which is what made this look fine -- but
// it serializes as an exprId alone, and the instructions it names live in the `lambdaInstrCache` of
// the execution that created it. A stored one therefore reads back fine and dies where it is
// applied, in a later process, with `lambda not found`. A NAMED fn is a name and survives; so does a
// DDB, which stores as a canvas-local identifier.
//
// The Seed.fs evaluator asks before serializing, so the author gets a reason naming the shape
// instead of a deep-stack raise later.

let persistableAcceptsPlainShapes =
  test "isPersistable: primitives and persistent blobs OK" {
    let cases : RT.Dval list =
      [ RT.DUnit
        RT.DBool true
        RT.DInt64 42L
        RT.DString "hi"
        RT.DUuid(System.Guid.NewGuid())
        RT.DBytes(RT.Persistent("deadbeef", 4L))
        RT.DList(RT.ValueType.Known RT.KTInt64, [ RT.DInt64 1L; RT.DInt64 2L ])
        RT.DTuple(RT.DInt64 1L, RT.DString "a", [])
        Dval.stringDict RT.KTInt64 [ ("k", RT.DInt64 42L) ] ]
    for dv in cases do
      Expect.isTrue (Dval.isPersistable dv) $"expected isPersistable=true for {dv}"
  }

/// Ocean's round 2, finding 6: `dark propagate` in a terminal died with `lambda not found`, and
/// every Component-based TUI held its component in a `val`. Nothing about the crash pointed here.
let persistableRejectsLambda =
  test "isPersistable: a lambda is not persistable, a named fn is" {
    let lambda =
      RT.DApplicable(
        RT.AppLambda
          { exprId = 1234UL
            closedRegisters = [||]
            typeSymbolTable = RT.TST.empty
            argsSoFar = []
            access =
              LibExecution.Permissions.Access.start
                LibExecution.Permissions.Policy.denyAll
            expectedReturn = ValueNone
            writtenIn = ValueNone }
      )

    Expect.isFalse (Dval.isPersistable lambda) "a lambda cannot be stored"

    match Dval.nonPersistableReason lambda with
    | Some reason ->
      Expect.stringContains reason "lambda" "the reason names the shape"
      Expect.stringContains
        reason
        "let f () ="
        "and says what to write instead, which is the whole value of catching it here"
    | None -> failtest "expected a reason"
  }


let persistableRejectsEphemeralBytes =
  test "isPersistable: unsaved Bytes are not persistable (must be saved first)" {
    let dv = mkEphemeral [| 1uy; 2uy; 3uy |]
    Expect.isFalse (Dval.isPersistable dv) "unsaved Bytes rejected"
    match Dval.nonPersistableReason dv with
    | Some reason ->
      Expect.stringContains reason "unsaved Bytes" "reason names the shape"
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

let persistableRejectsNestedBadShapes =
  test "isPersistable: a bad leaf anywhere in a container poisons the whole" {
    let list =
      RT.DList(
        RT.ValueType.Known RT.KTBytes,
        [ RT.DBytes(RT.Persistent("x", 1L)); mkEphemeral [| 9uy |] ]
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
    let refHash = Bytes.sha256Hex refBytes
    let orphanBytes = Array.concat [ [| 0x22uy |]; salt ]
    let orphanHash = Bytes.sha256Hex orphanBytes
    let fakeHash = RT.Hash(Bytes.sha256Hex salt)
    let (RT.Hash fakeHashStr) = fakeHash

    try
      do! PMBytes.insert refHash refBytes |> Ply.toTask
      do! PMBytes.insert orphanHash orphanBytes |> Ply.toTask

      // Plant a package_value row whose rt_dval references refHash.
      let referencingDval = RT.DBytes(RT.Persistent(refHash, int64 refBytes.Length))
      let pv : RT.PackageValue.PackageValue =
        { hash = fakeHash; body = referencingDval }
      let rtDvalBytes = BS.RT.PackageValue.serialize fakeHash pv
      let valueTypeBytes =
        BS.RT.ValueType.serialize (RT.Dval.toValueType referencingDval)
      do!
        execSqlP
          """
          INSERT OR REPLACE INTO package_values (hash, pt_def, rt_dval, value_type)
          VALUES (@hash, @pt_def, @rt_dval, @value_type)
          """
          [ "hash", Sql.string fakeHashStr
            "pt_def", Sql.bytes [||]
            "rt_dval", Sql.bytes rtDvalBytes
            "value_type", Sql.bytes valueTypeBytes ]

      let! deleted = PMBytes.sweepOrphans () |> Ply.toTask
      Expect.isGreaterThanOrEqual
        deleted
        1L
        "at least the orphan blob should have been swept"
      let! refStill = PMBytes.get refHash |> Ply.toTask
      Expect.isSome refStill "referenced blob stays in package_blobs"
      let! orphanStill = PMBytes.get orphanHash |> Ply.toTask
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
// Bytes value semantics are independent of storage. All comparisons use the
// content hash already carried by persistent bytes and lazily cached in memory.

let equalsEphemeralEphemeralSameUuid =
  test "blob equality: two refs to the same ephemeral UUID are equal" {
    let dv = Bytes.newEphemeral [| 0x01uy; 0x02uy |]
    Expect.isTrue (Equals.equals dv dv) "same ephemeral handle compares equal"
  }

let equalsEphemeralEphemeralSameBytes =
  test "blob equality: two distinct ephemerals with same bytes are equal" {
    let payload = [| 0x11uy; 0x22uy; 0x33uy |]
    let a = Bytes.newEphemeral payload
    let b = Bytes.newEphemeral payload
    Expect.notEqual (ephemeralId a) (ephemeralId b) "distinct UUIDs"
    Expect.isTrue (Equals.equals a b) "equal contents despite distinct UUIDs"
  }

let equalsEphemeralPersistentSameBytes =
  test "blob equality: ephemeral vs persistent with same bytes are equal" {
    let payload = [| 0xDEuy; 0xADuy |]
    let eph = Bytes.newEphemeral payload
    let per = RT.DBytes(RT.Persistent(Bytes.sha256Hex payload, int64 payload.Length))
    Expect.isTrue (Equals.equals eph per) "storage does not affect equality"
    Expect.isTrue (Equals.equals per eph) "mixed-form equality is symmetric"
  }

let equalsPersistentPersistentSameHash =
  test "blob equality: two Persistent refs with the same hash are equal" {
    let a = RT.DBytes(RT.Persistent("cafebabe", 4L))
    let b = RT.DBytes(RT.Persistent("cafebabe", 4L))
    Expect.isTrue (Equals.equals a b) "same hash + length = equal"
  }

let equalsPersistentPersistentDifferentHashes =
  test "blob equality: Persistent refs with different hashes are unequal" {
    let a = RT.DBytes(RT.Persistent("aaaa", 4L))
    let b = RT.DBytes(RT.Persistent("bbbb", 4L))
    Expect.isFalse (Equals.equals a b) "different hashes = unequal"
  }

let equalsEphemeralDifferentBytes =
  test "blob equality: two ephemerals with different bytes are unequal" {
    let a = Bytes.newEphemeral [| 0x00uy |]
    let b = Bytes.newEphemeral [| 0xFFuy |]
    Expect.isFalse (Equals.equals a b) "different contents = unequal"
  }


let byteValuesKeepSemanticsAfterSaving =
  testTask "saving bytes preserves equality, hashing, ordering, and nested values" {
    for payload in [ [||]; [| 0uy |]; [| 0uy; 255uy; 1uy |]; Array.create 4096 42uy ] do
      let original = Bytes.newEphemeral payload
      let equalCopy = Bytes.newEphemeral (Array.copy payload)
      let! saved = Bytes.promote noopInsert original |> Ply.toTask
      let restored =
        BS.RT.Dval.deserialize "dval" (BS.RT.Dval.serialize "dval" saved)
      let different = Bytes.newEphemeral (Array.append payload [| 7uy |])
      for value in [ equalCopy; saved; restored ] do
        Expect.isTrue (Equals.equals original value) "Dark equality ignores storage"
        Expect.equal value original "F# structural equality agrees"
        Expect.equal (hash value) (hash original) "equal values have equal hashes"
        Expect.equal
          (hash (RT.DictKey value))
          (hash (RT.DictKey original))
          "internal container hashing agrees"
        Expect.equal
          (RT.DvalOrdering.compareForDictKey original value)
          0
          "equal dict-key order"
        Expect.equal
          (RT.DvalOrdering.compareForDictKey value different)
          (RT.DvalOrdering.compareForDictKey original different)
          "dict-key order is unchanged by saving"
        Expect.throws
          (fun () -> RT.DvalOrdering.compareForSort original value |> ignore<int>)
          "Bytes have no sort order"
        let nested item = RT.DTuple(RT.DString "payload", item, [])
        Expect.isTrue
          (Equals.equals (nested original) (nested value))
          "nested Dark equality ignores storage"
        Expect.equal (nested value) (nested original) "nested F# equality agrees"
        Expect.equal
          (hash (nested value))
          (hash (nested original))
          "nested hashes agree"
  }

let byteKeysSurviveSaving =
  testTask "Bytes dictionary keys support content lookup before and after saving" {
    for payload in [ [||]; [| 0uy; 255uy; 128uy |]; Array.create 4096 42uy ] do
      let original = Bytes.newEphemeral payload
      let equalCopy = Bytes.newEphemeral (Array.copy payload)
      let! savedKey = Bytes.promote PMBytes.insert original |> Ply.toTask
      let wrappers : List<RT.Dval -> RT.Dval> =
        [ (fun value -> value)
          (fun value -> RT.DTuple(RT.DUnit, value, []))
          (fun value -> RT.DList(RT.ValueType.Known RT.KTBytes, [ value ]))
          RT2DT.Dval.toDT ]
      for wrap in wrappers do
        let key = wrap original
        RT.Dval.assertUsableDictKey key
        let entries = Map [ RT.DictKey key, RT.DString "found" ]
        let dictionary =
          RT.DDict(RT.Dval.toValueType key, RT.ValueType.Known RT.KTString, entries)
        let! promoted = Bytes.promote PMBytes.insert dictionary |> Ply.toTask
        let restored =
          BS.RT.Dval.deserialize "dval" (BS.RT.Dval.serialize "dval" promoted)
        Expect.isTrue
          (Equals.equals dictionary restored)
          "saving preserves the dictionary"
        let! reflected =
          RT2DT.Dval.toDT dictionary |> Bytes.promote PMBytes.insert |> Ply.toTask
        let restoredReflection =
          BS.RT.Dval.deserialize "dval" (BS.RT.Dval.serialize "dval" reflected)
          |> RT2DT.Dval.fromDT
        Expect.isTrue
          (Equals.equals dictionary restoredReflection)
          "reflected dictionaries preserve byte keys through saving and loading"
        for candidate in [ dictionary; restored ] do
          match candidate with
          | RT.DDict(_, _, entries) ->
            for lookup in [ original; equalCopy; savedKey ] do
              let key = RT.DictKey(wrap lookup)
              Expect.equal
                (Map.tryFind key entries)
                (Some(RT.DString "found"))
                "content lookup"
              let updated = Map.add key (RT.DString "updated") entries
              Expect.equal (Map.count updated) 1 "equal content is the same key"
          | _ -> failtest "expected a dictionary"
  }

let byteHashingIsLazy =
  testTask "temporary bytes are hashed only when needed and reuse that hash on save" {
    let original = Bytes.newEphemeral [| 1uy; 2uy; 3uy |]
    let ref = dbytesRef original
    match ref with
    | RT.Ephemeral e ->
      Expect.isFalse e.IsHashed "creating bytes does not hash them"
      Expect.equal ref.Length 3L "length is available without hashing"
      Expect.isTrue (Equals.equals original original) "reflexive equality"
      Expect.isFalse e.IsHashed "self-comparison does not hash"
      let expectedHash = Bytes.sha256Hex e.bytes
      Expect.equal ref.ContentHash expectedHash "content hash matches storage format"
      Expect.isTrue e.IsHashed "hash is now cached"
      let mutable writes = 0
      let insert h bytes =
        writes <- writes + 1
        Expect.equal h expectedHash "saving uses the cached content hash"
        Expect.equal bytes e.bytes "saving preserves contents"
        uply { return () }
      let! saved = Bytes.promote insert original |> Ply.toTask
      Expect.equal writes 1 "only saving writes to the store"
      Expect.equal (dbytesRef saved).ContentHash expectedHash "saved hash agrees"
    | _ -> failtest "expected in-memory bytes"
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
      RT.DDict(
        RT.ValueType.Known RT.KTString,
        RT.ValueType.Unknown,
        Map
          [ RT.DictKey(RT.DString "a"), middle
            RT.DictKey(RT.DString "b"), RT.DBool true ]
      )
    let! result = noopRewriteWith outer
    Expect.isTrue (obj.ReferenceEquals(result, outer)) "outer DDict ref preserved"
    // Reach into the result and verify the inner refs survived too.
    match result with
    | RT.DDict(_, _, m) ->
      let a = m[RT.DictKey(RT.DString "a")]
      Expect.isTrue (obj.ReferenceEquals(a, middle)) "nested DTuple ref preserved"
      match a with
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
// Bytes.promote recurses into DApplicable closures
// ─────────────────────────────────────────────────────────────────────
//
// `Dval.rewriteWith` walks into AppLambda.closedRegisters /
// AppLambda.argsSoFar / AppNamedFn.argsSoFar. A lambda closing over an
// ephemeral blob must have its capture environment promoted along
// with the rest of the value graph — otherwise the persisted closure
// carries a DBytes(Ephemeral _) ref pointing at bytes that died with
// the producing VM.

let private fakeAppLambda
  (closedRegisters : List<RT.Register * RT.Dval>)
  (argsSoFar : List<RT.Dval>)
  : RT.Dval =
  RT.DApplicable(
    RT.AppLambda
      { exprId = 0UL
        closedRegisters = RT.Captures.ofList closedRegisters
        typeSymbolTable = RT.TST.empty
        access =
          LibExecution.Permissions.Access.start
            LibExecution.Permissions.Policy.denyAll
        argsSoFar = argsSoFar
        expectedReturn = ValueNone
        writtenIn = ValueNone }
  )

let private fakeAppNamedFn (argsSoFar : List<RT.Dval>) : RT.Dval =
  let name = RT.FQFnName.fqBuiltin "intAdd" 0
  RT.DApplicable(
    RT.AppNamedFn
      { name = name
        typeSymbolTable = RT.TST.empty
        typeArgs = []
        access = None
        argsSoFar = argsSoFar
        boundImpls = [] }
  )

let promoteRewritesInsideClosedRegisters =
  testTask "Bytes.promote: ephemeral inside AppLambda.closedRegisters is promoted" {
    let payload = uniquePayload "promote-closure"
    let ephemeral = Bytes.newEphemeral payload
    let dv = fakeAppLambda [ (1, ephemeral) ] []
    let! promoted = Bytes.promote PMBytes.insert dv |> Ply.toTask
    match promoted with
    | RT.DApplicable(RT.AppLambda lambda) ->
      match RT.Captures.toList lambda.closedRegisters with
      | [ (_, RT.DBytes(RT.Persistent(h, n))) ] ->
        Expect.equal h (Bytes.sha256Hex payload) "captured blob hash"
        Expect.equal n (int64 payload.Length) "captured blob length"
      | _ -> failtest $"expected promoted ephemeral, got {lambda.closedRegisters}"
    | _ -> failtest $"expected DApplicable, got {promoted}"
  }

let promoteRewritesInsideAppLambdaArgs =
  testTask "Bytes.promote: ephemeral inside AppLambda.argsSoFar is promoted" {
    let payload = uniquePayload "promote-applambda-args"
    let ephemeral = Bytes.newEphemeral payload
    let dv = fakeAppLambda [] [ ephemeral ]
    let! promoted = Bytes.promote PMBytes.insert dv |> Ply.toTask
    match promoted with
    | RT.DApplicable(RT.AppLambda lambda) ->
      match lambda.argsSoFar with
      | [ RT.DBytes(RT.Persistent(h, _)) ] ->
        Expect.equal h (Bytes.sha256Hex payload) "argsSoFar blob promoted"
      | _ -> failtest $"expected promoted ephemeral, got {lambda.argsSoFar}"
    | _ -> failtest $"expected DApplicable, got {promoted}"
  }

let promoteRewritesInsideAppNamedFnArgs =
  testTask "Bytes.promote: ephemeral inside AppNamedFn.argsSoFar is promoted" {
    let payload = uniquePayload "promote-namedfn-args"
    let ephemeral = Bytes.newEphemeral payload
    let dv = fakeAppNamedFn [ ephemeral ]
    let! promoted = Bytes.promote PMBytes.insert dv |> Ply.toTask
    match promoted with
    | RT.DApplicable(RT.AppNamedFn fn) ->
      match fn.argsSoFar with
      | [ RT.DBytes(RT.Persistent(h, _)) ] ->
        Expect.equal h (Bytes.sha256Hex payload) "argsSoFar blob promoted"
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
    let! result =
      Tracing.prepareDvalForStorage state.blobs.persist (freshStream ())
      |> Ply.toTask
    Expect.isTrue (isStubInt64 result) $"expected stub, got {result}"
  }

let prepareStubsStreamInsideList =
  testTask "prepareDvalForStorage: DStream nested inside DList is stubbed" {
    let state = freshState ()
    let dv =
      RT.DList(RT.ValueType.Unknown, [ RT.DInt64 1L; freshStream (); RT.DInt64 2L ])
    let! result = Tracing.prepareDvalForStorage state.blobs.persist dv |> Ply.toTask
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
    let! result = Tracing.prepareDvalForStorage state.blobs.persist dv |> Ply.toTask
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
    let! result = Tracing.prepareDvalForStorage state.blobs.persist dv |> Ply.toTask
    match result with
    | RT.DApplicable(RT.AppLambda lambda) ->
      match RT.Captures.toList lambda.closedRegisters with
      | [ (_, captured) ] ->
        Expect.isTrue
          (isStubInt64 captured)
          $"captured stream stubbed, got {captured}"
      | _ -> failtest $"expected one closed register, got {lambda.closedRegisters}"
    | _ -> failtest $"expected DApplicable, got {result}"
  }


// `sweepOrphans` deletes any `package_blobs` row not referenced
// by `package_values.rt_dval`. Without sequencing, the sweep test
// can run concurrently with `packageBytesDedupesOnSameHash` (which
// inserts an unreferenced hash, then re-inserts under that hash
// expecting `INSERT OR IGNORE` to win) — the sweep's DELETE
// between the two inserts lets the second insert succeed with
// fresh bytes, and the dedup invariant assertion fails. Sequence
// the file so any test mutating `package_blobs` runs alone.
let tests =
  testSequenced
  <| testList
    "bytes"
    [ ephemeralRoundtrip
      twoEphemeralsAreDistinct
      persistentBytesBinaryRoundtrip
      ephemeralBytesBinaryRaises
      tbytesBinaryRoundtrip
      ktbytesBinaryRoundtrip
      tbytesPtDarkBridge
      tbytesRtDarkBridge
      ktbytesRtDarkBridge
      dbytesPersistentDarkBridge
      dbytesEphemeralDarkBridge
      reflectionIgnoresByteStorage
      byteLengthDoesNotReadStorage
      peekDoesNotReadStorage
      packageBytesInsertLookup
      packageBytesDedupesOnSameHash
      packageBytesMissingHashReturnsNone
      promotePersistsAndSwaps
      promoteThenSerializeRoundtrips
      promoteSameBytesTwiceDedups
      promotedBytesResolvesViaReadBytes
      fileReadMemoryBound
      persistableAcceptsPlainShapes
      persistableRejectsEphemeralBytes
      persistableRejectsStream
      persistableRejectsLambda
      persistableRejectsNestedBadShapes
      sweepDeletesOrphansButKeepsReferenced
      equalsEphemeralEphemeralSameUuid
      equalsEphemeralEphemeralSameBytes
      equalsEphemeralPersistentSameBytes
      equalsPersistentPersistentSameHash
      equalsPersistentPersistentDifferentHashes
      equalsEphemeralDifferentBytes
      byteValuesKeepSemanticsAfterSaving
      byteKeysSurviveSaving
      byteHashingIsLazy
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
