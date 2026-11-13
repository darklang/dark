module Tests.BinarySerialization

open Expecto
open System.Text.RegularExpressions

open Prelude
open TestUtils.TestUtils
module File = LibConfig.File
module Config = LibConfig.Dirs

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
  let packageTestTests =
    Roundtripping.testRoundtripMany
      "packageTests"
      (fun test ->
        test
        |> BS.PT.PackageTest.serialize test.hash
        |> BS.PT.PackageTest.deserialize test.hash)
      Values.ProgramTypes.packageTests

  /// Every `PackageOp` case, through the writer and back.
  ///
  /// The op format is what two machines must agree on byte for byte. Storing an op and reading it
  /// back through the fold covers this only indirectly, and never at all for a case the fold does
  /// not reach: a `Decision` kind, a `SetName` carrying a predecessor.
  let packageOpTests =
    Roundtripping.testRoundtripMany
      "packageOps"
      (fun op ->
        op
        |> BS.PT.PackageOp.serialize (System.Guid.NewGuid())
        |> BS.PT.PackageOp.deserialize (System.Guid.NewGuid()))
      Values.ProgramTypes.packageOps

  /// The fold stores an item's `pt_def` by cutting it out of its op's blob (`itemBlob`) rather than
  /// writing the item again, so the cut has to be exactly what the item's own writer gives. And it
  /// has to give nothing for an op that adds no item, or a blob in an older format, which the item's
  /// writer would lay out differently.
  let itemBlobIsTheItemsOwnBlob =
    test "an Add op's blob holds its item's blob" {
      let opId = System.Guid.NewGuid()
      for op in Values.ProgramTypes.packageOps do
        let expected =
          match op with
          | PT.PackageOp.AddType t -> Some(BS.PT.PackageType.serialize t.hash t)
          | PT.PackageOp.AddValue v -> Some(BS.PT.PackageValue.serialize v.hash v)
          | PT.PackageOp.AddFn f -> Some(BS.PT.PackageFn.serialize f.hash f)
          | PT.PackageOp.AddTrait t -> Some(BS.PT.Trait.serialize t.hash t)
          | PT.PackageOp.AddTraitImpl i -> Some(BS.PT.TraitImpl.serialize i.hash i)
          | PT.PackageOp.AddTest t -> Some(BS.PT.PackageTest.serialize t.hash t)
          | _ -> None
        let blob = BS.PT.PackageOp.serialize opId op
        Expect.equal
          (BS.PT.PackageOp.itemBlob blob)
          expected
          (BS.PT.PackageOp.kind op)

        // Older formats are unsupported, including the item-slicing shortcut.
        let older = Array.copy blob
        System.Buffers.Binary.BinaryPrimitives.WriteUInt32LittleEndian(
          System.Span<byte>(older, 0, 4),
          LibSerialization.Binary.BaseFormat.CurrentVersion - 1u
        )
        Expect.isNone (BS.PT.PackageOp.itemBlob older) "an older format is not cut"
    }

  let olderVersionsRejected =
    test "formats older than the oldest readable one are rejected" {
      let fn = Values.ProgramTypes.packageFn
      let blob = BS.PT.PackageFn.serialize fn.hash fn
      for version in
        1u .. LibSerialization.Binary.BaseFormat.OldestReadableVersion - 1u do
        let oldBlob = Array.copy blob
        System.BitConverter.GetBytes(version).CopyTo(oldBlob, 0)
        Expect.throws
          (fun () ->
            BS.PT.PackageFn.deserialize fn.hash oldBlob
            |> ignore<PT.PackageFn.PackageFn>)
          $"format {version} is unsupported"
    }

  // A store keeps its ops forever, and an upgrade does not rewrite a blob whose hash did not
  // change, so a build must read what the builds before it wrote.
  let readableVersionsRead =
    test "program blobs in every readable format still read" {
      let fn = Values.ProgramTypes.packageFn
      let blob = BS.PT.PackageFn.serialize fn.hash fn
      for version in
        LibSerialization.Binary.BaseFormat.OldestReadableVersion .. LibSerialization.Binary.BaseFormat.CurrentVersion do
        let oldBlob = Array.copy blob
        System.BitConverter.GetBytes(version).CopyTo(oldBlob, 0)
        Expect.equal
          (BS.PT.PackageFn.deserialize fn.hash oldBlob)
          fn
          $"format {version} reads"
    }

  let v10VariableReads =
    test "a v10 compiled variable, which had no name, reads as [variable]" {
      let read version (bytes : byte[]) =
        use stream = new System.IO.MemoryStream(bytes)
        use r = new System.IO.BinaryReader(stream)
        LibSerialization.Binary.Serializers.RT.Instructions.LetPattern.read version r
      // v10: the tag, then the register, and nothing after it.
      let v10 = Array.append [| 0uy |] (System.BitConverter.GetBytes 3)
      Expect.equal (read 10u v10) (RT.LPVariable(3, "[variable]")) "v10"

      use stream = new System.IO.MemoryStream()
      use w = new System.IO.BinaryWriter(stream)
      LibSerialization.Binary.Serializers.RT.Instructions.LetPattern.write
        w
        (RT.LPVariable(3, "a"))
      w.Flush()
      Expect.equal (read 11u (stream.ToArray())) (RT.LPVariable(3, "a")) "v11"
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
    // `symbols` travels in its own column, not in this blob, so a roundtrip through the
    // instructions cannot bring it back and is not meant to. Both sides are normalised to the
    // same empty table: two `Lazy` values are not equal to each other even when they compute
    // the same thing, so comparing them would fail on identity rather than on content.
    let normalise (fn : RT.PackageFn.PackageFn) =
      { fn with symbols = RT.DebugSymbols.emptyLazy }

    Roundtripping.testRoundtripMany
      "packageFns"
      (fun fn ->
        fn
        |> BS.RT.PackageFn.serialize fn.hash
        |> BS.RT.PackageFn.deserialize fn.hash
        |> normalise)
      (Values.RuntimeTypes.packageFns |> List.map normalise)

  let dvalTests =
    let dvalEquals (expected : RT.Dval) (actual : RT.Dval) : bool =
      match expected, actual with
      | RT.DFloat f1, RT.DFloat f2 when
        System.Double.IsNaN f1 && System.Double.IsNaN f2
        ->
        true
      // compare the bits, so NaN equals itself and -0.0f32 is not 0.0f32
      | RT.DFloat32 f1, RT.DFloat32 f2 ->
        System.BitConverter.SingleToUInt32Bits f1 = System.BitConverter.SingleToUInt32Bits
          f2
      // `=` on decimals ignores scale, so compare the bits: 1.10m must not come back 1.1m
      | RT.DDecimal d1, RT.DDecimal d2 ->
        System.Decimal.GetBits d1 = System.Decimal.GetBits d2
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
              closedRegisters = [||]
              typeSymbolTable = RT.TST.empty
              access =
                LibExecution.Permissions.Access.start
                  LibExecution.Permissions.Policy.allowAll
              expectedReturn = ValueNone
              writtenIn = ValueNone }
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

  // A dict, byte for byte, on both sides of the size where its entries move from a sorted
  // array to a tree. Either way they go on the wire as a count then each entry in key
  // order; inserted out of order, so a writer that kept insertion order would fail it.
  let dictsKeepTheirBytes =
    test "a dict keeps its wire format, small or large" {
      let entries n =
        List.init n (fun i -> RT.DString $"k{(i * 7) % n}", RT.DInt64(int64 i))
      let hex n =
        LibExecution.Dval.dict RT.KTString RT.KTInt64 (entries n)
        |> BS.RT.Dval.serialize "dict"
        |> System.Convert.ToHexString
      Expect.equal
        (hex 3)
        "0B0000002D00000013010E0108030E026B300800000000000000000E026B310801000000000000000E026B32080200000000000000"
        "a 3-entry dict"
      Expect.equal
        (hex 30)
        "0B000000A001000013010E01081E0E026B300800000000000000000E026B31080D000000000000000E036B3130080A000000000000000E036B31310817000000000000000E036B31320806000000000000000E036B31330813000000000000000E036B31340802000000000000000E036B3135080F000000000000000E036B3136081C000000000000000E036B3137080B000000000000000E036B31380818000000000000000E036B31390807000000000000000E026B32081A000000000000000E036B32300814000000000000000E036B32310803000000000000000E036B32320810000000000000000E036B3233081D000000000000000E036B3234080C000000000000000E036B32350819000000000000000E036B32360808000000000000000E036B32370815000000000000000E036B32380804000000000000000E036B32390811000000000000000E026B330809000000000000000E026B340816000000000000000E026B350805000000000000000E026B360812000000000000000E026B370801000000000000000E026B38080E000000000000000E026B39081B00000000000000"
        "a 30-entry dict"
    }

  // A record's fields go on the wire in ordinal name order, which is the order the F# `Map` they
  // used to be held in wrote them. The expected bytes come from that `Map`, so a change to the
  // order shows here even though a roundtrip would not notice: the reader sorts again.
  let recordsKeepTheirBytes =
    test "a record keeps its wire format" {
      let typeName = RT.FQTypeName.Package(RT.Hash "t")
      let fields =
        [ "b"; "B"; "_x"; "a"; "\u00e9" ]
        |> List.mapi (fun i n -> n, RT.DInt64(int64 i))
      let value = RT.DRecord(typeName, typeName, [], RT.RecordFields.ofList fields)
      let asMapWroteIt =
        use ms = new System.IO.MemoryStream()
        use w = new System.IO.BinaryWriter(ms)
        w.Write(20uy)
        LibSerialization.Binary.Serializers.RT.Common.FQTypeName.write w typeName
        LibSerialization.Binary.Serializers.RT.Common.FQTypeName.write w typeName
        LibSerialization.Binary.Serializers.Common.List.write w (fun _ _ -> ()) []
        LibSerialization.Binary.Serializers.Common.Map.write
          LibSerialization.Binary.Serializers.Common.String.write
          LibSerialization.Binary.Serializers.RT.Dval.writeDval
          w
          (Map.ofList fields)
        w.Flush()
        ms.ToArray()
      let bytes = BS.RT.Dval.serialize "record" value
      Expect.equal
        (Array.skip 8 bytes)
        asMapWroteIt
        "the bytes a Map of the fields wrote"
      Expect.equal (BS.RT.Dval.deserialize "record" bytes) value "and it reads back"
    }

  // A closure WITH captured values, byte for byte. The golden corpus and the roundtrip values
  // only carry closures that capture nothing, so neither would notice a change to how captures
  // go on the wire.
  let closureCapturesKeepTheirBytes =
    test "a closure's captured values keep their wire format" {
      let captures = [ (1, RT.DInt64 5L); (3, RT.DString "x") ]
      let value =
        RT.DApplicable(
          RT.AppLambda
            { exprId = 7UL
              closedRegisters = RT.Captures.ofList captures
              typeSymbolTable = RT.TST.empty
              access =
                LibExecution.Permissions.Access.start
                  LibExecution.Permissions.Policy.denyAll
              expectedReturn = ValueNone
              writtenIn = ValueNone }
        )
      let bytes = value |> BS.RT.Dval.serialize "closure"
      // The v11 header, then the lambda: its id, its two captures and an empty symbol table.
      Expect.equal
        (System.Convert.ToHexString bytes)
        "0B00000020000000160007000000000000000201000000080500000000000000030000000E017800"
        "the bytes a closure with captures serializes to"
      match BS.RT.Dval.deserialize "closure" bytes with
      | RT.DApplicable(RT.AppLambda lambda) ->
        Expect.equal
          (RT.Captures.toList lambda.closedRegisters)
          captures
          "captures roundtrip"
      | _ -> failtest "expected a lambda"
    }

  let namedFnAccessIsStripped =
    test "serialized named-fn references lose runtime access" {
      let value =
        RT.DApplicable(
          RT.AppNamedFn
            { name = RT.FQFnName.Package(RT.Hash "approved-code")
              typeSymbolTable = RT.TST.empty
              typeArgs = []
              approvalName = Some "Me.clock"
              access =
                Some(
                  LibExecution.Permissions.Access.start
                    LibExecution.Permissions.Policy.allowAll
                )
              argsSoFar = []
              boundImpls = [] }
        )
      let decoded =
        value |> BS.RT.Dval.serialize "namedFn" |> BS.RT.Dval.deserialize "namedFn"
      match decoded with
      | RT.DApplicable(RT.AppNamedFn namedFn) ->
        Expect.equal namedFn.approvalName (Some "Me.clock") "entry identity survives"
        match namedFn.access with
        | None -> failtest "decoded named fn must carry deny-all access, not None"
        | Some access ->
          Expect.isFalse
            (LibExecution.Permissions.Access.allows
              LibExecution.Permissions.Request.clock
              (access
               |> LibExecution.Permissions.Access.enter
                 "Me.clock"
                 LibExecution.Permissions.Policy.allowAll))
            "even a named grant cannot restore serialized authority"
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

  /// Bytes are valid dict keys, so a saved Bytes key reads back as the same entry.
  let dictWithBytesKeyRoundtrips =
    test "a Dict keyed by a saved Bytes reference roundtrips" {
      let dict =
        RT.DDict(
          RT.ValueType.Unknown,
          RT.ValueType.Unknown,
          RT.DictMap.ofList [ RT.DBytes(RT.Persistent("h", 0L)), RT.DUnit ]
        )
      let back = dict |> BS.RT.Dval.serialize "dval" |> BS.RT.Dval.deserialize "dval"
      Expect.isTrue (LibExecution.Dval.equals dict back) "the Bytes key survives"
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
              boundImpls = []
              approvalName = None
              access = None }
        )
      expectRefusedOnRead
        "a lambda"
        (RT.DDict(
          RT.ValueType.Unknown,
          RT.ValueType.Unknown,
          RT.DictMap.ofList [ lambda, RT.DUnit ]
        ))
    }


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
          PT.packageTestTests
          PT.packageOpTests
          PT.itemBlobIsTheItemsOwnBlob

          PT.legacyRecoveryHoleTagRejected



          PT.olderVersionsRejected
          PT.readableVersionsRead
          PT.v10VariableReads
          PT.unknownVersionRejected ]

      testList
        "RT Roundtrip Tests"
        [ RT.dictsKeepTheirBytes
          RT.recordsKeepTheirBytes
          RT.closureCapturesKeepTheirBytes
          RT.packageTypeTests
          RT.packageValueTests
          RT.packageFnTests
          RT.dvalTests
          RT.instructionsTests

          RT.dictWithBytesKeyRoundtrips
          RT.dictWithLambdaKeyRejectedOnRead
          RT.closureAccessIsStripped
          RT.namedFnAccessIsStripped ] ]
