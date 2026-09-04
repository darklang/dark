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


  /// Generates timestamped test files for binary serialization. These files are used
  /// to prove that the binary serialization format is compatible.  When we change the
  /// format, we should still be able to read the old files in addition to the new ones
  /// (though they will not necessarily have the same output). If we make changes to
  /// the binary serialization format (or to the test cases), we generate the files
  /// and commit them.
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
          PT.toplevelTests
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

      testList "consistent serialization" ConsistentSerializationTests.testTestFiles ]
