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

  let traitTests =
    Roundtripping.testRoundtripMany
      "traits"
      (fun (t : PT.Trait.Trait) ->
        t |> BS.PT.Trait.serialize t.hash |> BS.PT.Trait.deserialize t.hash)
      Values.ProgramTypes.traits

  let implTests =
    Roundtripping.testRoundtripMany
      "impls"
      (fun (i : PT.TraitImpl.TraitImpl) ->
        i |> BS.PT.TraitImpl.serialize i.hash |> BS.PT.TraitImpl.deserialize i.hash)
      Values.ProgramTypes.impls

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

  /// A v1 blob has no `bounds` list after the ceiling. A v2 reader handed a v1 header
  /// must stop there and answer `bounds = []`, or every fn stored before traits
  /// becomes unreadable. Built by writing with the v2 writer, dropping the trailing
  /// empty-list byte, and rewriting the header version and length.
  let v1PackageFnStillReads =
    test "a format-v1 PackageFn blob (no bounds) still reads" {
      let fn = Values.ProgramTypes.packageFn
      Expect.isEmpty fn.bounds "the fixture has no bounds"
      let v2 = BS.PT.PackageFn.serialize fn.hash fn
      // header: version (4) + length (4); payload follows. An empty List writes one
      // varint length byte (0), and bounds is the last field.
      let payloadLen = System.BitConverter.ToUInt32(v2, 4)
      Expect.equal (int payloadLen) (v2.Length - 8) "header length matches"
      Expect.equal v2[v2.Length - 1] 0uy "the trailing byte is the empty bounds list"
      let v1 = Array.sub v2 0 (v2.Length - 1)
      System.BitConverter.GetBytes(1u).CopyTo(v1, 0)
      System.BitConverter.GetBytes(payloadLen - 1u).CopyTo(v1, 4)
      let back = BS.PT.PackageFn.deserialize fn.hash v1
      Expect.equal back fn "reads as the same fn, with bounds = []"
    }

  let unknownVersionRejected =
    test "a format version newer than this build is rejected, not guessed at" {
      let fn = Values.ProgramTypes.packageFn
      let blob = BS.PT.PackageFn.serialize fn.hash fn
      System.BitConverter.GetBytes(99u).CopyTo(blob, 0)
      Expect.throws
        (fun () ->
          BS.PT.PackageFn.deserialize fn.hash blob |> ignore<PT.PackageFn.PackageFn>)
        "version 99 has no reader"
    }

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


let generateTestFiles () =
  // Enabled in dev so we can see changes as git diffs
  // Disabled in CI so changes will fail the tests
  if Config.serializationGenerateTestData then
    ConsistentSerializationTests.generateTestFiles ()
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
          PT.traitTests
          PT.implTests
          PT.toplevelTests
          PT.packageOpTests
          PT.legacyRecoveryHoleTagRejected
          PT.v1PackageFnStillReads
          PT.unknownVersionRejected ]

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

      testList "consistent serialization" ConsistentSerializationTests.testTestFiles ]
