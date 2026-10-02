module Tests.BinarySerialization

open Expecto
open System.Text.RegularExpressions

open Prelude
open TestUtils.TestUtils
module File = LibCloud.File
module Config = LibCloud.Config

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

module BS = LibSerialization.Binary.Serialization

module Values = SerializationTestValues


module Roundtripping =
  let testRoundtripMany name (roundtrip : 'T -> 'T) values =
    testMany
      name
      (fun value -> value |> roundtrip |> (=) value)
      (List.map (fun x -> x, true) values)

module HashTests =
  open LibExecution.ProgramTypes

  let roundtripTest =
    test "Hash binary roundtrip" {
      let hash = Values.hashPT
      let roundtripped =
        hash |> BS.PT.Hash.serialize "hash" |> BS.PT.Hash.deserialize "hash"
      Expect.equal roundtripped hash "roundtrip should preserve Hash"
    }


module PT =
  let packageLocationTests =
    Roundtripping.testRoundtripMany
      "packageLocations"
      (fun loc ->
        loc
        |> BS.PT.PackageLocation.serialize "packageLocation"
        |> BS.PT.PackageLocation.deserialize "packageLocation")
      Values.ProgramTypes.packageLocations

  let packageTypeTests =
    Roundtripping.testRoundtripMany
      "packageTypes"
      (fun typ ->
        typ
        |> BS.PT.PackageType.serialize typ.hash
        |> BS.PT.PackageType.deserialize typ.hash)
      Values.ProgramTypes.packageTypes

  let packageFnTests =
    Roundtripping.testRoundtripMany
      "packageFns"
      (fun fn ->
        fn
        |> BS.PT.PackageFn.serialize fn.hash
        |> BS.PT.PackageFn.deserialize fn.hash)
      Values.ProgramTypes.packageFns

  let packageValTests =
    Roundtripping.testRoundtripMany
      "packageVals"
      (fun c ->
        c
        |> BS.PT.PackageValue.serialize c.hash
        |> BS.PT.PackageValue.deserialize c.hash)
      Values.ProgramTypes.packageValues

  /// Every `PackageOp` case, through the writer and back.
  ///
  /// The op format is what two machines must agree on byte for byte. Storing an op and reading it
  /// back through the fold covers this only indirectly, and never at all for a case the fold does
  /// not reach: a `Decision` kind, a `BranchEvent`, a `SetName` carrying a predecessor.
  let packageOpTests =
    Roundtripping.testRoundtripMany
      "packageOps"
      (fun op ->
        op
        |> BS.PT.PackageOp.serialize (System.Guid.NewGuid())
        |> BS.PT.PackageOp.deserialize (System.Guid.NewGuid()))
      Values.ProgramTypes.packageOps

  let toplevelTests =
    Roundtripping.testRoundtripMany
      "toplevels"
      (fun tl ->
        let tlid = PT.Toplevel.toTLID tl
        tl |> BS.PT.Toplevel.serialize tlid |> BS.PT.Toplevel.deserialize tlid)
      Values.ProgramTypes.toplevels

  let legacyRecoveryHoleTagRejected =
    test "legacy ProgramTypes recovery-hole tag is rejected" {
      use stream = new System.IO.MemoryStream([| 36uy |])
      use reader = new System.IO.BinaryReader(stream)
      Expect.throws
        (fun () ->
          LibSerialization.Binary.Serializers.PT.Expr.Expr.read reader
          |> ignore<LibExecution.ProgramTypes.Expr>)
        "WrittenTypes recovery holes must not be deserialized as ProgramTypes"
    }


module RT =
  let packageTypeTests =
    Roundtripping.testRoundtripMany
      "packageTypes"
      (fun t ->
        t
        |> BS.RT.PackageType.serialize t.hash
        |> BS.RT.PackageType.deserialize t.hash)
      Values.RuntimeTypes.packageTypes

  let packageValueTests =
    Roundtripping.testRoundtripMany
      "packageValues"
      (fun c ->
        c
        |> BS.RT.PackageValue.serialize c.hash
        |> BS.RT.PackageValue.deserialize c.hash)
      Values.RuntimeTypes.packageValues

  let packageFnTests =
    Roundtripping.testRoundtripMany
      "packageFns"
      (fun fn ->
        fn
        |> BS.RT.PackageFn.serialize fn.hash
        |> BS.RT.PackageFn.deserialize fn.hash)
      Values.RuntimeTypes.packageFns

  let dvalTests =
    let dvalEquals (expected : RT.Dval) (actual : RT.Dval) : bool =
      match expected, actual with
      | RT.DFloat f1, RT.DFloat f2 when
        System.Double.IsNaN f1 && System.Double.IsNaN f2
        ->
        true
      | _ -> expected = actual

    testMany
      "vals"
      (fun dval ->
        let deserialized =
          dval |> BS.RT.Dval.serialize "dval" |> BS.RT.Dval.deserialize "dval"
        dvalEquals dval deserialized)
      (List.map (fun x -> x, true) (Values.RuntimeTypes.dvals ()))

  let closureAccessIsStripped =
    test "serialized closures lose runtime access" {
      let value =
        RT.DApplicable(
          RT.AppLambda
            { exprId = 1UL
              closedRegisters = []
              typeSymbolTable = RT.TST.empty
              access =
                LibExecution.Permissions.Access.start
                  LibExecution.Permissions.Policy.allowAll
              argsSoFar = [] }
        )
      let decoded =
        value |> BS.RT.Dval.serialize "closure" |> BS.RT.Dval.deserialize "closure"
      match decoded with
      | RT.DApplicable(RT.AppLambda lambda) ->
        Expect.isFalse
          (LibExecution.Permissions.Access.allows
            LibExecution.Permissions.Request.clock
            lambda.access)
          "serialization must not preserve authority"
      | _ -> failtest "expected a lambda"
    }

  let namedFnAccessIsStripped =
    test "serialized named-fn references lose runtime access" {
      let value =
        RT.DApplicable(
          RT.AppNamedFn
            { name = RT.FQFnName.fqBuiltin "timeNowMs" 0
              typeSymbolTable = RT.TST.empty
              typeArgs = []
              access =
                Some(
                  LibExecution.Permissions.Access.start
                    LibExecution.Permissions.Policy.allowAll
                )
              argsSoFar = [] }
        )
      let decoded =
        value |> BS.RT.Dval.serialize "namedFn" |> BS.RT.Dval.deserialize "namedFn"
      match decoded with
      | RT.DApplicable(RT.AppNamedFn namedFn) ->
        match namedFn.access with
        | None -> failtest "decoded named fn must carry deny-all access, not None"
        | Some access ->
          Expect.isFalse
            (LibExecution.Permissions.Access.allows
              LibExecution.Permissions.Request.clock
              access)
            "serialization must not preserve authority"
      | _ -> failtest "expected a named fn"
    }

  let instructionsTests =
    Roundtripping.testRoundtripMany
      "instrs"
      (fun i ->
        i
        |> BS.RT.Instructions.serialize "instrs"
        |> BS.RT.Instructions.deserialize "instrs")
      Values.RuntimeTypes.instructions

  /// Invalid keys are placed in one-entry maps so construction does not invoke the
  /// comparer. Deserialization must reject them with a format error.
  let private expectRefusedOnRead (name : string) (dv : RT.Dval) =
    let bytes = BS.RT.Dval.serialize "dval" dv
    let thrown =
      try
        BS.RT.Dval.deserialize "dval" bytes |> ignore<RT.Dval>
        None
      with e ->
        Some(e.ToString())
    match thrown with
    | None -> failtest $"expected deserialize to refuse {name} as a dict key"
    | Some message ->
      Expect.stringContains
        message
        "Dict key that cannot be a key"
        $"{name} must be refused as a format error, not by the comparer's guard"

  /// DB references are comparable but intentionally unsupported as keys.
  let dictWithDbKeyRejectedOnRead =
    test "a Dict keyed by a DB reference is refused on deserialize" {
      expectRefusedOnRead
        "a DB reference"
        (RT.DDict(
          RT.ValueType.Unknown,
          RT.ValueType.Unknown,
          Map [ RT.DictKey(RT.DDB "somedb"), RT.DUnit ]
        ))
    }

  /// Lambdas are neither valid nor comparable as keys.
  let dictWithLambdaKeyRejectedOnRead =
    test "a Dict keyed by a lambda is refused on deserialize" {
      let lambda =
        RT.DApplicable(
          RT.AppNamedFn
            { name = RT.FQFnName.Builtin { name = "someFn"; version = 0 }
              typeSymbolTable = RT.TST.empty
              typeArgs = []
              argsSoFar = []
              access = None }
        )
      expectRefusedOnRead
        "a lambda"
        (RT.DDict(
          RT.ValueType.Unknown,
          RT.ValueType.Unknown,
          Map [ RT.DictKey lambda, RT.DUnit ]
        ))
    }


module ConsistentSerializationTests =
  type Format =
    { name : string
      serializer : PT.DB.T -> byte array
      deserializer : byte array -> PT.DB.T
      prefix : string
      suffix : string }

  let formats =
    [ { name = "BinarySerialization"
        serializer = fun db -> BS.PT.Toplevel.serialize db.tlid db
        deserializer = fun data -> BS.PT.Toplevel.deserialize 0UL data
        prefix = "toplevels-binary"
        suffix = ".bin" } ]

  let nameFor (f : Format) (version : string) (idx : int) =
    $"{f.prefix}-{version}-{idx}{f.suffix}"


  /// Generates the timestamped fixture files that pin the binary format. Regenerate
  /// and commit them alongside any deliberate format or test-value change.
  let generateTestFiles () : unit =
    formats
    |> List.iter (fun f ->
      Values.ProgramTypes.toplevels
      |> List.iteri (fun i tl ->
        let output = f.serializer tl
        File.writefileBytes Config.Serialization (nameFor f "latest" i) output))


  // Each serialized toplevel must match its committed fixture. Regenerate
  // intentionally changed fixtures with DARK_CONFIG_SERIALIZATION_GENERATE_TEST_DATA=y.
  let testTestFiles =
    formats
    |> List.map (fun f ->
      test "binary serialization matches the committed golden files" {
        Values.ProgramTypes.toplevels
        |> List.iteri (fun i tl ->
          let serialized = f.serializer tl
          Expect.equal (f.deserializer serialized) tl "roundtrip should work"
          let golden =
            File.readfileBytes Config.Serialization (nameFor f "latest" i)
          Expect.equal
            serialized
            golden
            $"toplevel {i} matches its committed golden (regenerate if this change is intended)")
      })


/// The ABI regression net: blobs written by PAST builds, kept forever, read by this one.
///
/// Different question from the golden files above, and the difference is the whole point. A golden
/// pins "the current writer still produces these bytes", and is REGENERATED whenever a format change
/// is intended -- so the moment the format moves, the old bytes are gone and nothing checks that
/// they can still be read. This corpus is never regenerated. One file per (format version, type),
/// committed once and then only read.
///
/// What it asserts is the migrator's core operation: take a blob a previous build wrote, decode it
/// with whatever reader claims to understand that version, write it back out with TODAY'S writer,
/// and decode THAT. The two decoded values must be equal. A reader that mis-decodes an old layout
/// fails here even when the bytes it goes on to produce are perfectly self-consistent, which is
/// exactly the failure a migration cannot afford: the store is the only copy of the ops.
module Corpus =
  module BaseFormat = LibSerialization.Binary.BaseFormat

  /// One serialized type, and the two things a corpus needs of it: what this build would write, and
  /// whether a stored blob survives a trip through today's writer.
  ///
  /// `reread` hides the type, which is why this is a record of closures rather than a generic: the
  /// kinds have nothing in common at the type level and the test only ever asks these two questions.
  type Kind =
    {
      name : string
      /// What this build writes for each test value, in order. The generator's input.
      blobs : unit -> List<byte[]>
      /// Decode `blob`, re-encode it with today's writer, decode that, and compare. Raises if the
      /// blob cannot be decoded at all, which is itself the failure.
      reread : byte[] -> bool
    }

  let private kindEq
    (name : string)
    (ser : 'T -> byte[])
    (deser : byte[] -> 'T)
    (eq : 'T -> 'T -> bool)
    (values : List<'T>)
    : Kind =
    { name = name
      blobs = fun () -> values |> List.map ser
      reread = fun blob -> let v = deser blob in eq v (deser (ser v)) }

  let private kind name ser deser values = kindEq name ser deser (=) values

  /// Everything that goes INTO a store, which is what a migration has to carry forward. The op log
  /// first, because it is canonical and the rest is projection.
  let kinds : List<Kind> =
    [ kind
        "packageOp"
        (BS.PT.PackageOp.serialize "corpus")
        (BS.PT.PackageOp.deserialize "corpus")
        Values.ProgramTypes.packageOps
      kind
        "ptPackageType"
        (fun (t : PT.PackageType.PackageType) ->
          BS.PT.PackageType.serialize t.hash t)
        (BS.PT.PackageType.deserialize "corpus")
        Values.ProgramTypes.packageTypes
      kind
        "ptPackageValue"
        (fun (v : PT.PackageValue.PackageValue) ->
          BS.PT.PackageValue.serialize v.hash v)
        (BS.PT.PackageValue.deserialize "corpus")
        Values.ProgramTypes.packageValues
      kind
        "ptPackageFn"
        (fun (f : PT.PackageFn.PackageFn) -> BS.PT.PackageFn.serialize f.hash f)
        (BS.PT.PackageFn.deserialize "corpus")
        Values.ProgramTypes.packageFns
      kind
        "packageLocation"
        (BS.PT.PackageLocation.serialize "corpus")
        (BS.PT.PackageLocation.deserialize "corpus")
        Values.ProgramTypes.packageLocations
      kind
        "toplevel"
        (fun (tl : PT.DB.T) -> BS.PT.Toplevel.serialize tl.tlid tl)
        (BS.PT.Toplevel.deserialize 0UL)
        Values.ProgramTypes.toplevels
      // The RT side is stored too: `package_values.rt_dval` and the compiled fn bodies. A migration
      // that carried the op log forward and left these unreadable would look like it worked.
      //
      // NaN needs its own arm, as it does in the roundtrip tests above: `DFloat nan = DFloat nan` is
      // false, so plain equality reports a perfectly good decode as a mis-decode.
      kindEq
        "rtDval"
        (BS.RT.Dval.serialize "corpus")
        (BS.RT.Dval.deserialize "corpus")
        (fun a b ->
          match a, b with
          | RT.DFloat f1, RT.DFloat f2 when
            System.Double.IsNaN f1 && System.Double.IsNaN f2
            ->
            true
          | _ -> a = b)
        (Values.RuntimeTypes.dvals ())
      kind
        "rtPackageValue"
        (fun (v : RT.PackageValue.PackageValue) ->
          BS.RT.PackageValue.serialize v.hash v)
        (BS.RT.PackageValue.deserialize "corpus")
        Values.RuntimeTypes.packageValues
      kind
        "rtPackageFn"
        (fun (f : RT.PackageFn.PackageFn) -> BS.RT.PackageFn.serialize f.hash f)
        (BS.RT.PackageFn.deserialize "corpus")
        Values.RuntimeTypes.packageFns
      kind
        "rtInstructions"
        (BS.RT.Instructions.serialize "corpus")
        (BS.RT.Instructions.deserialize "corpus")
        Values.RuntimeTypes.instructions ]

  /// One file per (version, kind), holding every blob for it: `[count][len][bytes]...`, all
  /// little-endian uint32. A file per blob would be seven hundred files for the Dval set alone,
  /// which makes the corpus unreadable as a diff and unpleasant to carry.
  let private frame (blobs : List<byte[]>) : byte[] =
    use stream = new System.IO.MemoryStream()
    use w = new System.IO.BinaryWriter(stream)
    w.Write(uint32 (List.length blobs))
    blobs
    |> List.iter (fun b ->
      w.Write(uint32 b.Length)
      w.Write b)
    w.Flush()
    stream.ToArray()

  let private unframe (data : byte[]) : List<byte[]> =
    use stream = new System.IO.MemoryStream(data)
    use r = new System.IO.BinaryReader(stream)
    let count = r.ReadUInt32() |> int
    [ for _ in 1..count -> r.ReadBytes(r.ReadUInt32() |> int) ]

  let private fileFor (v : uint32) (k : Kind) = $"corpus/v{v}/{k.name}.bin"

  /// Write this build's blobs for the CURRENT version, for any kind not already stored.
  ///
  /// Never overwrites. A corpus entry is a historical fact about what some build actually wrote, and
  /// regenerating one turns the net into a mirror -- which is precisely how the golden files above
  /// stop covering an old format. Deliberately changing what a version's bytes are means deleting
  /// the file by hand, and having to justify it.
  let generate () : unit =
    let v = BaseFormat.currentVersion

    System.IO.Directory.CreateDirectory(Config.serializationDir + $"corpus/v{v}")
    |> ignore<System.IO.DirectoryInfo>

    kinds
    |> List.iter (fun k ->
      let f = fileFor v k
      if not (File.fileExists Config.Serialization f) then
        File.writefileBytes Config.Serialization f (frame (k.blobs ()))
        print $"  corpus: wrote {f}")

  let tests =
    // Every version from 1 up to this build's, since every one of them is a layout this binary
    // claims to read. A version with no file fails rather than being skipped quietly.
    [ 1u .. BaseFormat.currentVersion ]
    |> List.map (fun v ->
      testList
        $"v{v}"
        (kinds
         |> List.map (fun k ->
           test k.name {
             let f = fileFor v k

             Expect.isTrue
               (File.fileExists Config.Serialization f)
               $"v{v} has stored {k.name} blobs ({f}). Write missing ones with \
                 DARK_CONFIG_SERIALIZATION_GENERATE_TEST_DATA=y and COMMIT them: a format this \
                 build claims to read with no stored bytes is a claim nothing checks."

             let stored = File.readfileBytes Config.Serialization f |> unframe

             stored
             |> List.iteri (fun i blob ->
               Expect.isTrue
                 (k.reread blob)
                 $"{k.name}[{i}] written at v{v} still decodes to the same value through this \
                   build's writer")

             // At the CURRENT version there is a second, stronger thing to say: this build must
             // still produce these exact bytes. That is what catches a layout change made without
             // bumping `currentVersion`, which is the one format bug that corrupts silently -- two
             // builds both calling themselves v1 and disagreeing about what a v1 blob means.
             //
             // A PREFIX, so a new test value can be appended without destroying the stored bytes
             // for a version. Inserting one in the middle breaks this on purpose: the corpus is
             // positional, and history is what it is for.
             if v = BaseFormat.currentVersion then
               let current = k.blobs ()

               Expect.isGreaterThanOrEqual
                 (List.length current)
                 (List.length stored)
                 $"{k.name} still has at least the {List.length stored} test values its corpus was \
                   written from. Removing one leaves stored bytes nothing can be compared against; \
                   append rather than insert or delete."

               List.zip stored (List.truncate (List.length stored) current)
               |> List.iteri (fun i (storedBlob, currentBlob) ->
                 Expect.equal
                   currentBlob
                   storedBlob
                   $"this build writes {k.name}[{i}] exactly as v{v} did. If the layout genuinely \
                     changed, bump BaseFormat.currentVersion and keep a readVN -- do not rewrite \
                     this file.")
           })))


let generateTestFiles () =
  // Enabled in dev so we can see changes as git diffs
  // Disabled in CI so changes will fail the tests
  if Config.serializationGenerateTestData then
    ConsistentSerializationTests.generateTestFiles ()
    Corpus.generate ()
  ()


let tests =
  testList
    "Binary Serialization"
    [ testList "Hash" [ HashTests.roundtripTest ]

      testList
        "PT Roundtrip Tests"
        [ PT.packageLocationTests
          PT.packageTypeTests
          PT.packageValTests
          PT.packageFnTests
          PT.toplevelTests
          PT.packageOpTests
          PT.legacyRecoveryHoleTagRejected ]

      testList
        "RT Roundtrip Tests"
        [ RT.packageTypeTests
          RT.packageValueTests
          RT.packageFnTests
          RT.dvalTests
          RT.instructionsTests
          RT.dictWithDbKeyRejectedOnRead
          RT.dictWithLambdaKeyRejectedOnRead
          RT.closureAccessIsStripped
          RT.namedFnAccessIsStripped ]

      testList "consistent serialization" ConsistentSerializationTests.testTestFiles

      testList "stored blobs from past formats" Corpus.tests ]
