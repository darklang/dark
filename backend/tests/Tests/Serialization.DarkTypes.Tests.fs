module Tests.DarkTypesSerialization

open Expecto

open Prelude

open TestUtils.TestUtils

module File = LibCloud.File
module Config = LibCloud.Config

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

module PT2DT = LibExecution.ProgramTypesToDarkTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module PackageIDs = LibExecution.PackageIDs


module V = SerializationTestValues


module RoundtripTests =
  // for each type/value:
  // perform internal -> DT -> internal -> DT -> internal
  // most of the time, it should end up being the same as the source.
  // if there are known exceptions, break down individual mappings as separate tests

  let types : RT.Types =
    // CLEANUP could the `package` fn just return None? Not sure if we're actually using the types there
    { package = pmRT.getType }

  let testRoundtrip
    (testName : string)
    (typeName : RT.FQTypeName.FQTypeName)
    (original : 'a)
    (toDT : 'a -> RT.Dval)
    (fromDT : RT.Dval -> 'a)
    (customExpect : Option<'a -> 'a -> string -> unit>)
    =
    testTask testName {
      let firstDT = toDT original

      let! typeChecked =
        LibExecution.TypeChecker.unify
          types
          Map.empty
          (RT.TCustomType(Ok typeName, []))
          firstDT
        |> Ply.toTask

      let msg =
        match typeChecked with
        | Error e -> string e
        | Ok _ -> $"typechecking succeeded"

      Expect.isOk typeChecked msg

      let roundtripped = fromDT firstDT

      let msg = $"{testName} does not roundtrip successfully"

      return
        match customExpect with
        | None -> Expect.equal roundtripped original msg
        | Some customExpect -> customExpect roundtripped original msg
    }


  let testRoundtripList
    (testName : string)
    (typeName : RT.FQTypeName.FQTypeName)
    (original : List<'a>)
    (toDT : 'a -> RT.Dval)
    (fromDT : RT.Dval -> 'a)
    (customExpect : Option<'a -> 'a -> string -> unit>)
    =
    testList
      testName
      (List.mapWithIndex
        (fun i t ->
          testRoundtrip $"{testName}[{i}]" typeName t toDT fromDT customExpect)
        original)


  module ProgramTypes =
    let pkg (hash : Hash) = RT.FQTypeName.fqPackage hash

    let tests =
      [ testRoundtripList
          "PT.PackageType"
          (pkg PackageIDs.Type.LanguageTools.ProgramTypes.PackageType.packageType)
          V.ProgramTypes.packageTypes
          PT2DT.PackageType.toDT
          PT2DT.PackageType.fromDT
          None

        testRoundtripList
          "PT.PackageValue"
          (pkg PackageIDs.Type.LanguageTools.ProgramTypes.PackageValue.packageValue)
          V.ProgramTypes.packageValues
          PT2DT.PackageValue.toDT
          PT2DT.PackageValue.fromDT
          None

        testRoundtripList
          "PT.PackageFn"
          (pkg PackageIDs.Type.LanguageTools.ProgramTypes.PackageFn.packageFn)
          V.ProgramTypes.packageFns
          PT2DT.PackageFn.toDT
          PT2DT.PackageFn.fromDT
          None

        testRoundtripList
          "PT.Secret"
          (pkg PackageIDs.Type.LanguageTools.ProgramTypes.secret)
          V.ProgramTypes.userSecrets
          PT2DT.Secret.toDT
          PT2DT.Secret.fromDT
          None

        testRoundtripList
          "PT.DB"
          (pkg PackageIDs.Type.LanguageTools.ProgramTypes.db)
          V.ProgramTypes.userDBs
          PT2DT.DB.toDT
          PT2DT.DB.fromDT
          None

        testRoundtripList
          "PT.Handler"
          (pkg PackageIDs.Type.LanguageTools.ProgramTypes.Handler.handler)
          V.ProgramTypes.Handler.handlers
          PT2DT.Handler.toDT
          PT2DT.Handler.fromDT
          None ]

  module RuntimeTypes =
    let pkg (hash : Hash) = RT.FQTypeName.fqPackage hash

    let tests =
      // CLEANUP backfill with more things from RT
      [ testRoundtripList
          "RT.FQTypeName"
          (pkg PackageIDs.Type.LanguageTools.RuntimeTypes.FQTypeName.fqTypeName)
          V.RuntimeTypes.fqTypeNames
          RT2DT.FQTypeName.toDT
          RT2DT.FQTypeName.fromDT
          None

        testRoundtripList
          "RT.FQValueName"
          (pkg PackageIDs.Type.LanguageTools.RuntimeTypes.FQValueName.fqValueName)
          V.RuntimeTypes.fqValueNames
          RT2DT.FQValueName.toDT
          RT2DT.FQValueName.fromDT
          None

        testRoundtripList
          "RT.FQFnName"
          (pkg PackageIDs.Type.LanguageTools.RuntimeTypes.FQFnName.fqFnName)
          V.RuntimeTypes.fqFnNames
          RT2DT.FQFnName.toDT
          RT2DT.FQFnName.fromDT
          None

        testRoundtripList
          "RT.TypeReference"
          (pkg PackageIDs.Type.LanguageTools.RuntimeTypes.typeReference)
          V.RuntimeTypes.typeReferences
          RT2DT.TypeReference.toDT
          RT2DT.TypeReference.fromDT
          None

        testRoundtripList
          "RT.ValueType"
          (pkg PackageIDs.Type.LanguageTools.RuntimeTypes.valueType)
          V.RuntimeTypes.valueTypes
          RT2DT.ValueType.toDT
          RT2DT.ValueType.fromDT
          None

        testRoundtripList
          "RT.Dval"
          (pkg PackageIDs.Type.LanguageTools.RuntimeTypes.dval)
          V.RuntimeTypes.dvals
          RT2DT.Dval.toDT
          RT2DT.Dval.fromDT
          (Some Expect.RT.equalDval)


        // CLEANUP consider adding roundtrip tests here around
        // RuntimeErrors, which consume these types.
        // We don't always have F# models for these types, though,
        // so it's not clear how to do this or if it's even useful
        ]


let tests =
  testList
    "DarkTypes Serialization"
    [ testList
        "roundtrip PTs between internal types and dark types"
        RoundtripTests.ProgramTypes.tests

      testList
        "roundtrip RTs between internal types and dark types"
        RoundtripTests.RuntimeTypes.tests ]
