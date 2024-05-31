module Tests.DarkTypesSerialization

open Expecto
open System.Text.RegularExpressions

open Prelude

open TestUtils.TestUtils

module File = LibCloud.File
module Config = LibCloud.Config

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

module PT2DT = LibExecution.ProgramTypesToDarkTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes


module V = SerializationTestValues


module RoundtripTests =
  // for each type/value:
  // perform internal -> DT -> internal -> DT -> internal
  // most of the time, it should end up being the same as the source.
  // if there are known exceptions, break down individual mappings as separate tests

  let types : RT.Types =
    { typeSymbolTable = Map.empty

      package = packageManager.getType }

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

      let context =
        LibExecution.TypeChecker.Context.FunctionCallResult(
          fnName =
            (RT.FQFnName.fqPackage
              "Darklang"
              [ "LanguageTools"; "ProgramTypes" ]
              "expr"),
          returnType = RT.TCustomType(Ok typeName, [])
        )

      let! typeChecked =
        LibExecution.TypeChecker.unify
          context
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


  module RuntimeTypes =

    let pkg mods name =
      RT.FQTypeName.fqPackage
        "Darklang"
        (([ "LanguageTools"; "RuntimeTypes" ] @ mods))
        name

    let tests =
      [ testRoundtripList
          "RT.FQTypeName"
          (pkg [ "FQTypeName" ] "FQTypeName")
          V.RuntimeTypes.fqTypeNames
          RT2DT.FQTypeName.toDT
          RT2DT.FQTypeName.fromDT
          None

        testRoundtripList
          "RT.FQFnName"
          (pkg [ "FQFnName" ] "FQFnName")
          V.RuntimeTypes.fqFnNames
          RT2DT.FQFnName.toDT
          RT2DT.FQFnName.fromDT
          None

        testRoundtripList
          "RT.FQConstantName"
          (pkg [ "FQConstantName" ] "FQConstantName")
          V.RuntimeTypes.fqConstantNames
          RT2DT.FQConstantName.toDT
          RT2DT.FQConstantName.fromDT
          None

        testRoundtripList
          "RT.TypeReference"
          (pkg [] "TypeReference")
          V.RuntimeTypes.typeReferences
          RT2DT.TypeReference.toDT
          RT2DT.TypeReference.fromDT
          None

        testRoundtripList
          "RT.Expr"
          (pkg [] "Expr")
          V.RuntimeTypes.exprs
          RT2DT.Expr.toDT
          RT2DT.Expr.fromDT
          None

        testRoundtripList
          "RT.ValueType"
          (pkg [] "ValueType")
          V.RuntimeTypes.valueTypes
          RT2DT.Dval.ValueType.toDT
          RT2DT.Dval.ValueType.fromDT
          None

        testRoundtripList
          "RT.Dval"
          (pkg [ "Dval" ] "Dval")
          V.RuntimeTypes.dvals
          RT2DT.Dval.toDT
          RT2DT.Dval.fromDT
          (Some Expect.equalDval)


        // CLEANUP consider adding roundtrip tests here around
        // RuntimeErrors, which consume these types.
        // We don't always have F# models for these types, though,
        // so it's not clear how to do this or if it's even useful
        ]

  module ProgramTypes =

    let pkg mods name =
      RT.FQTypeName.fqPackage
        "Darklang"
        (([ "LanguageTools"; "ProgramTypes" ] @ mods))
        name

    let tests =
      [ testRoundtripList
          "PT.PackageFn"
          (pkg [ "PackageFn" ] "PackageFn")
          V.ProgramTypes.packageFns
          PT2DT.PackageFn.toDT
          PT2DT.PackageFn.fromDT
          None

        testRoundtripList
          "PT.PackageType"
          (pkg [] "PackageType")
          V.ProgramTypes.packageTypes
          PT2DT.PackageType.toDT
          PT2DT.PackageType.fromDT
          None

        testRoundtripList
          "PT.PackageConstant"
          (pkg [] "PackageConstant")
          V.ProgramTypes.packageConstants
          PT2DT.PackageConstant.toDT
          PT2DT.PackageConstant.fromDT
          None

        testRoundtripList
          "PT.Secret"
          (pkg [] "Secret")
          V.ProgramTypes.userSecrets
          PT2DT.Secret.toDT
          PT2DT.Secret.fromDT
          None

        testRoundtripList
          "PT.DB"
          (pkg [] "DB")
          V.ProgramTypes.userDBs
          PT2DT.DB.toDT
          PT2DT.DB.fromDT
          None

        testRoundtripList
          "PT.Handler"
          (pkg [ "Handler" ] "Handler")
          V.ProgramTypes.Handler.handlers
          PT2DT.Handler.toDT
          PT2DT.Handler.fromDT
          None ]


let tests =
  testList
    "DarkTypes Serialization"
    [ testList
        "roundtrip PTs between internal types and dark types"
        RoundtripTests.ProgramTypes.tests

      testList
        "roundtrip RTs between internal types and dark types"
        RoundtripTests.RuntimeTypes.tests ]
