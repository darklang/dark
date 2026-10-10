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

  /// The deprecation stamp rides on tags 4 and 5 as a trailing field rather than on tags of its
  /// own, and that is only safe if two things hold.
  ///
  /// A stamp-free op must still be the bytes every store already holds, or every deprecation ever
  /// authored changes id. So the expected payload is spelled out by hand here rather than taken
  /// from the writer: a writer compared against itself would agree with any layout.
  ///
  /// And a stamped op must be those same bytes with the field appended, so a binary built before
  /// the field reads the target, the kind and the message, stops, ignores the tail and APPLIES the
  /// deprecation. That is what makes the trailing field better than a new tag, which an earlier
  /// binary would have stored unapplied, leaving a `Harmful` item reading as fine.
  let deprecationStampRidesOnTheExistingTags =
    test "a stamped deprecation is the old blob plus a trailing field" {
      let target = PT.Reference.PackageFn Values.hashPT
      let stamp = "2026-01-01T00:00:00.000Z-0001"

      // The 8-byte header carries the payload's length, so the two blobs differ there as well as
      // in the tail. What has to match is the op, which starts after it.
      let payloadOf (op : PT.PackageOp) : byte array =
        BS.PT.PackageOp.serialize (System.Guid.NewGuid()) op |> Array.skip 8

      let handBuilt (writeOp : System.IO.BinaryWriter -> unit) : byte array =
        use ms = new System.IO.MemoryStream()
        use w = new System.IO.BinaryWriter(ms)
        writeOp w
        w.Flush()
        ms.ToArray()

      let oldDeprecate =
        handBuilt (fun w ->
          w.Write(4uy) // Deprecate
          w.Write(2uy) // Reference.PackageFn
          LibSerialization.Binary.Serializers.PT.Common.Hash.write w Values.hashPT
          w.Write(2uy) // DeprecationKind.Obsolete
          LibSerialization.Binary.Serializers.Common.String.write w "gone")

      let oldUndeprecate =
        handBuilt (fun w ->
          w.Write(5uy) // Undeprecate
          w.Write(2uy) // Reference.PackageFn
          LibSerialization.Binary.Serializers.PT.Common.Hash.write w Values.hashPT)

      Expect.equal
        (payloadOf (
          PT.PackageOp.Deprecate(target, PT.DeprecationKind.Obsolete, "gone", None)
        ))
        oldDeprecate
        "an unstamped Deprecate is byte for byte what earlier builds wrote"
      Expect.equal
        (payloadOf (PT.PackageOp.Undeprecate(target, None)))
        oldUndeprecate
        "and so is an unstamped Undeprecate"

      let stampedDeprecate =
        payloadOf (
          PT.PackageOp.Deprecate(
            target,
            PT.DeprecationKind.Obsolete,
            "gone",
            Some stamp
          )
        )
      let stampedUndeprecate =
        payloadOf (PT.PackageOp.Undeprecate(target, Some stamp))

      Expect.equal
        (stampedDeprecate |> Array.take oldDeprecate.Length)
        oldDeprecate
        "a stamped Deprecate starts with exactly those bytes, so an earlier reader applies it"
      Expect.equal
        (stampedUndeprecate |> Array.take oldUndeprecate.Length)
        oldUndeprecate
        "and so does a stamped Undeprecate"
      Expect.isGreaterThan
        stampedDeprecate.Length
        oldDeprecate.Length
        "the stamp is actually written"

      // And THIS build reading a blob written in the old layout: the op, with no stamp. The bytes
      // are the hand-built ones, so this is the old reader case rather than a round-trip.
      let header (payload : byte array) : byte array =
        handBuilt (fun w ->
          w.Write(LibSerialization.Binary.BaseFormat.CurrentVersion)
          w.Write(uint32 payload.Length))

      let id = System.Guid.NewGuid()
      Expect.equal
        (BS.PT.PackageOp.deserialize
          id
          (Array.append (header oldDeprecate) oldDeprecate))
        (PT.PackageOp.Deprecate(target, PT.DeprecationKind.Obsolete, "gone", None))
        "an old Deprecate blob reads as itself, unstamped"
      Expect.equal
        (BS.PT.PackageOp.deserialize
          id
          (Array.append (header oldUndeprecate) oldUndeprecate))
        (PT.PackageOp.Undeprecate(target, None))
        "an old Undeprecate blob reads as itself, unstamped"
    }

  /// A v1 blob has no `bounds` list after the ceiling. A reader handed a v1 header must stop
  /// there and answer `bounds = []`, or every fn stored before bounds existed becomes unreadable.
  /// Built by writing the current format, dropping the trailing empty-list byte, and rewriting
  /// the header version and length -- which is only a faithful v1 blob because this fn's body
  /// contains nothing whose encoding has changed since (no operator, no trait method).
  let v1PackageFnStillReads =
    test "a format-v1 PackageFn blob (no bounds) still reads" {
      let fn =
        { Values.ProgramTypes.packageFn with body = PT.EInt64(1UL, 5L); bounds = [] }
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

  /// The real compatibility case, which the test above cannot cover: a v2 blob has no
  /// implementation byte after an operator's operands, and no implementation byte after a trait
  /// method's name. A reader that takes them anyway consumes the NEXT expression's tag and
  /// misparses the rest of the body, so every fn stored before this branch that contains a `+`
  /// would break. The blob here is written by hand, exactly as the v2 writer would have.
  let v2ExpressionsStillRead =
    test "a format-v2 blob with an operator and a trait method still reads" {
      let write (f : System.IO.BinaryWriter -> unit) : byte[] =
        use stream = new System.IO.MemoryStream()
        use w = new System.IO.BinaryWriter(stream)
        f w
        w.Flush()
        stream.ToArray()

      // `1L + 2L` as v2 wrote it: EInfix (tag 29), the infix, then both operands, and stop.
      let payload =
        write (fun w ->
          w.Write 29uy
          w.Write 1UL
          w.Write 0uy // Infix.InfixFnCall
          w.Write 0uy // InfixFnName.ArithmeticPlus
          w.Write 0uy // EInt64
          w.Write 2UL
          w.Write 1L
          w.Write 0uy // EInt64
          w.Write 3UL
          w.Write 2L)

      use stream = new System.IO.MemoryStream(payload)
      use reader = new System.IO.BinaryReader(stream)
      let back = LibSerialization.Binary.Serializers.PT.Expr.Expr.read 2u reader

      match back with
      | PT.EInfix(_,
                  PT.InfixFnCall PT.ArithmeticPlus,
                  PT.EInt64(_, 1L),
                  PT.EInt64(_, 2L),
                  PT.FQFnName.Unknown) -> ()
      | other -> failtest $"a v2 `1L + 2L` read as {other}"
    }

  /// The same compatibility case one version on. v4 added a list to `EFnName` for the bounds
  /// the CALL worked out, and a v3 blob has no list byte there. A reader that takes one anyway
  /// reads the next expression's tag as a length, so this is written as v3 wrote it: an
  /// EStatement whose first half is the EFnName, so a wrong-sized read shows up as the second
  /// half failing rather than as a quietly different value.
  let v3FnNameStillReads =
    test "a format-v3 blob with a function reference still reads" {
      let write (f : System.IO.BinaryWriter -> unit) : byte[] =
        use stream = new System.IO.MemoryStream()
        use w = new System.IO.BinaryWriter(stream)
        f w
        w.Flush()
        stream.ToArray()

      let writeString (w : System.IO.BinaryWriter) (str : string) =
        // String.write: a varint byte count, then UTF-8 bytes. Short names fit one byte.
        let bytes = System.Text.Encoding.UTF8.GetBytes str
        w.Write(byte bytes.Length)
        w.Write bytes

      let payload =
        write (fun w ->
          w.Write 32uy // EStatement
          w.Write 1UL
          w.Write 31uy // EFnName, and in v3 it ends after the name
          w.Write 2UL
          w.Write 0uy // NameResolution.originalName: the empty list
          w.Write 0uy // resolved = Ok
          w.Write 1uy // FQFnName.Package
          w.Write 0uy // Hash: not the 32-byte raw form, so the string form follows
          writeString w "abc123"
          w.Write 0uy // no location
          w.Write 0uy // EInt64, the statement's second half
          w.Write 3UL
          w.Write 7L)

      use stream = new System.IO.MemoryStream(payload)
      use reader = new System.IO.BinaryReader(stream)
      let back = LibSerialization.Binary.Serializers.PT.Expr.Expr.read 3u reader

      match back with
      | PT.EStatement(_, PT.EFnName(_, nr, []), PT.EInt64(_, 7L)) ->
        match nr.resolved with
        | Ok { name = PT.FQFnName.Package(PT.Hash "abc123") } -> ()
        | other -> failtest $"the v3 function reference read as {other}"
      | other -> failtest $"a v3 EFnName in a statement read as {other}"
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
          LibSerialization.Binary.Serializers.PT.Expr.Expr.read 3u reader
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
              argsSoFar = []
              boundImpls = [] }
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
  /// v4 added the bounds a call worked out to a stored applicable, and a v3 blob ends with the
  /// captured-access bool instead. A reader that takes the list anyway reads that bool as a
  /// length and then runs off the end of the blob, so every `rt_instrs` row written before this
  /// change would fail to decode. `package_functions` is a projection, but nothing on the
  /// shipped path re-folds one when the format moves, so those rows are read as they stand.
  let v3ApplicableStillReads =
    test "a format-v3 stored applicable (no bounds) still reads" {
      let writeString (w : System.IO.BinaryWriter) (str : string) =
        let bytes = System.Text.Encoding.UTF8.GetBytes str
        w.Write(byte bytes.Length)
        w.Write bytes

      let payload =
        use stream = new System.IO.MemoryStream()
        use w = new System.IO.BinaryWriter(stream)
        w.Write 1uy // FQFnName.Package
        writeString w "abc123" // the hash
        w.Write 0uy // the type symbol table, empty
        w.Write 0uy // typeArgs, empty
        w.Write 0uy // argsSoFar, empty
        w.Write true // captured access: the last field a v3 applicable has
        w.Flush()
        stream.ToArray()

      use stream = new System.IO.MemoryStream(payload)
      use reader = new System.IO.BinaryReader(stream)
      let back =
        LibSerialization.Binary.Serializers.RT.Dval.readApplicableNamedFn 3u reader

      Expect.equal back.boundImpls [] "a v3 applicable has no recorded bounds"
      Expect.equal
        back.name
        (RT.FQFnName.Package(RT.Hash "abc123"))
        "the name reads as it always did"
      Expect.isTrue
        back.access.IsSome
        "the captured-access bool was still the last byte"
      Expect.equal
        stream.Position
        stream.Length
        "the reader consumed the blob exactly"
    }

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
              boundImpls = []
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


/// The golden corpus: committed bytes for every type the store keeps as a blob, so a change to what a
/// serializer WRITES fails here rather than in a store. A format change that is not a bump (the op
/// vocabulary changed at the source-control rewrite without the version moving, and every older op became
/// unreadable while claiming the same version) is exactly what this catches.
///
/// Each type's test values are serialized and compared with `<prefix>-latest-<i>.bin`, and must read back
/// equal. Regenerate deliberately with DARK_CONFIG_SERIALIZATION_GENERATE_TEST_DATA=y, and only alongside a
/// version bump or a change to the test values, never to make a red go away.
module ConsistentSerializationTests =
  /// One stored type: how to write its test values, and a check that each reads back as it was.
  type Golden =
    {
      prefix : string
      cases : unit -> List<byte array * (byte array -> unit)>
      /// Decode bytes, whatever value they hold: what `history/` is checked with.
      decodes : byte array -> unit
    }

  let private golden
    (prefix : string)
    (values : unit -> List<'a>)
    (serialize : 'a -> byte array)
    (deserialize : byte array -> 'a)
    : Golden =
    { prefix = prefix
      cases =
        fun () ->
          values ()
          |> List.map (fun v ->
            (serialize v,
             fun bytes -> Expect.equal (deserialize bytes) v $"{prefix} reads back"))
      decodes = fun bytes -> deserialize bytes |> ignore<'a> }

  let goldens : List<Golden> =
    [ // The prefix the one golden before the corpus used, kept so its file still means the same thing.
      golden
        "toplevels-binary"
        (fun () -> Values.ProgramTypes.toplevels)
        (fun db -> BS.PT.Toplevel.serialize db.tlid db)
        (BS.PT.Toplevel.deserialize 0UL)
      golden
        "pt-package-op"
        (fun () -> Values.ProgramTypes.packageOps)
        (BS.PT.PackageOp.serialize Values.uuid)
        (BS.PT.PackageOp.deserialize Values.uuid)
      golden
        "pt-package-fn"
        (fun () -> Values.ProgramTypes.packageFns)
        (BS.PT.PackageFn.serialize Values.uuid)
        (BS.PT.PackageFn.deserialize Values.uuid)
      golden
        "pt-package-type"
        (fun () -> Values.ProgramTypes.packageTypes)
        (BS.PT.PackageType.serialize Values.uuid)
        (BS.PT.PackageType.deserialize Values.uuid)
      golden
        "pt-package-value"
        (fun () -> Values.ProgramTypes.packageValues)
        (BS.PT.PackageValue.serialize Values.uuid)
        (BS.PT.PackageValue.deserialize Values.uuid)
      golden
        "pt-trait"
        (fun () -> Values.ProgramTypes.traits)
        (BS.PT.Trait.serialize Values.uuid)
        (BS.PT.Trait.deserialize Values.uuid)
      golden
        "pt-trait-impl"
        (fun () -> Values.ProgramTypes.impls)
        (BS.PT.TraitImpl.serialize Values.uuid)
        (BS.PT.TraitImpl.deserialize Values.uuid)
      golden
        "pt-package-location"
        (fun () -> Values.ProgramTypes.packageLocations)
        (BS.PT.PackageLocation.serialize Values.uuid)
        (BS.PT.PackageLocation.deserialize Values.uuid)
      golden
        "rt-package-fn"
        (fun () -> Values.RuntimeTypes.packageFns)
        (BS.RT.PackageFn.serialize Values.uuid)
        (BS.RT.PackageFn.deserialize Values.uuid)
      golden
        "rt-package-type"
        (fun () -> Values.RuntimeTypes.packageTypes)
        (BS.RT.PackageType.serialize Values.uuid)
        (BS.RT.PackageType.deserialize Values.uuid)
      golden
        "rt-package-value"
        (fun () -> Values.RuntimeTypes.packageValues)
        (BS.RT.PackageValue.serialize Values.uuid)
        (BS.RT.PackageValue.deserialize Values.uuid)
      golden
        "rt-instructions"
        (fun () -> Values.RuntimeTypes.instructions)
        (BS.RT.Instructions.serialize Values.uuid)
        (BS.RT.Instructions.deserialize Values.uuid)
      golden
        "rt-dval"
        (fun () -> Values.RuntimeTypes.vals)
        (BS.RT.Dval.serialize Values.uuid)
        (BS.RT.Dval.deserialize Values.uuid)
      golden
        "rt-debug-symbols"
        (fun () -> Values.RuntimeTypes.debugSymbols)
        (BS.RT.PackageFn.serializeDebugSymbols Values.uuid)
        (BS.RT.PackageFn.deserializeDebugSymbols Values.uuid)
      golden
        "rt-value-type"
        (fun () -> Values.RuntimeTypes.valueTypes)
        BS.RT.ValueType.serialize
        BS.RT.ValueType.deserialize ]

  let nameFor (g : Golden) (idx : int) = $"{g.prefix}-latest-{idx}.bin"

  /// Every golden's bytes, kept forever under their own hash. The `-latest-` files say what today's
  /// writer produces and are rewritten whenever a value changes, so the bytes of a value someone
  /// deletes are gone from them; here they stay, and must go on decoding.
  let historyDir = "history/"

  let historyNameFor (g : Golden) (bytes : byte array) =
    let digest = System.Security.Cryptography.SHA256.HashData bytes
    $"{historyDir}{g.prefix}-{(System.Convert.ToHexString digest).ToLowerInvariant().Substring(0, 12)}.bin"


  let generateTestFiles () : unit =
    System.IO.Directory.CreateDirectory(Config.dir Config.Serialization + historyDir)
    |> ignore<System.IO.DirectoryInfo>
    goldens
    |> List.iter (fun g ->
      g.cases ()
      |> List.iteri (fun i (bytes, _) ->
        File.writefileBytes Config.Serialization (nameFor g i) bytes
        // Added, never replaced or removed.
        let kept = historyNameFor g bytes
        if not (File.fileExists Config.Serialization kept) then
          File.writefileBytes Config.Serialization kept bytes))


  /// One test per type, so a red names the type. Each also reports, and floors, how many cases it
  /// examined: a type whose values list went empty would otherwise pass over nothing.
  let testTestFiles =
    goldens
    |> List.map (fun g ->
      test $"{g.prefix} writes its committed golden bytes, and reads them back" {
        let cases = g.cases ()
        Expect.isNonEmpty cases $"{g.prefix} has test values to examine"
        cases
        |> List.iteri (fun i (bytes, readsBack) ->
          let golden = File.readfileBytes Config.Serialization (nameFor g i)
          Expect.equal
            bytes
            golden
            $"{g.prefix} {i} writes what its committed golden holds (regenerate only with a version bump)"
          readsBack golden)
      })


/// The other half of a format's contract. `testTestFiles` says today's writer produces these bytes;
/// this says every golden ever committed still DECODES, whether or not any current value produces it.
/// Only this catches a reader being removed: the commit that dropped the retired `Propagation`
/// decision deleted its test value in the same change, so the writer test had nothing to notice.
/// Never delete a file from `history/`; a red here means a reader was lost, not a stale file.
let historyStillDecodes =
  test "every golden ever committed still decodes" {
    let files =
      File.lsdir Config.Serialization ConsistentSerializationTests.historyDir
      |> List.filter (fun f -> f.EndsWith ".bin")
    let goldens = ConsistentSerializationTests.goldens
    Expect.isGreaterThanOrEqual
      (List.length files)
      (goldens |> List.sumBy (fun g -> List.length (g.cases ())))
      "history holds at least every current golden"
    files
    |> List.iter (fun f ->
      // The longest prefix that names a type: `pt-trait-` also starts `pt-trait-impl-` files.
      let g =
        goldens
        |> List.filter (fun g -> f.StartsWith(g.prefix + "-"))
        |> List.sortByDescending (fun g -> g.prefix.Length)
        |> List.tryHead
      match g with
      | None ->
        Expect.isTrue
          false
          $"history/{f} names no golden type, so nothing checks it decodes"
      | Some g ->
        let bytes =
          File.readfileBytes
            Config.Serialization
            (ConsistentSerializationTests.historyDir + f)
        try
          g.decodes bytes
        with e ->
          Expect.isTrue
            false
            $"history/{f} no longer decodes as {g.prefix}: {e.Message}")
  }


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
          PT.deprecationStampRidesOnTheExistingTags
          PT.legacyRecoveryHoleTagRejected
          PT.v1PackageFnStillReads
          PT.v2ExpressionsStillRead
          PT.v3FnNameStillReads
          PT.unknownVersionRejected ]

      testList
        "RT Roundtrip Tests"
        [ RT.packageTypeTests
          RT.packageValueTests
          RT.packageFnTests
          RT.dvalTests
          RT.instructionsTests
          RT.v3ApplicableStillReads
          RT.dictWithDbKeyRejectedOnRead
          RT.dictWithLambdaKeyRejectedOnRead
          RT.closureAccessIsStripped
          RT.namedFnAccessIsStripped ]

      testList "consistent serialization" ConsistentSerializationTests.testTestFiles
      testList "committed bytes stay readable" [ historyStillDecodes ] ]
