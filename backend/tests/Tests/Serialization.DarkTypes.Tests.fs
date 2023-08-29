module Tests.DarkTypesSerialization

open Expecto
open System.Text.RegularExpressions

open Prelude
open Tablecloth
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
    { builtIn = localBuiltIns.types
      package = packageManager.getType
      userProgram = Map.empty }

  let testRoundtrip
    (testName : string)
    (typeName : RT.TypeName.TypeName)
    (original : 'a)
    (toDT : 'a -> RT.Dval)
    (fromDT : RT.Dval -> 'a)
    (customExpect : Option<'a -> 'a -> string -> unit>)
    =
    testTask testName {
      let firstDT = original |> toDT

      let context =
        LibExecution.TypeChecker.Context.FunctionCallResult(
          fnName =
            (RT.FnName.fqPackage
              "Darklang"
              [ "LanguageTools"; "ProgramTypes" ]
              "expr"
              0),
          returnType = RT.TCustomType(Ok typeName, []),
          location = None
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
    (typeName : RT.TypeName.TypeName)
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

    let pkg mods name v =
      RT.TypeName.fqPackage
        "Darklang"
        (([ "LanguageTools"; "RuntimeTypes" ] @ mods))
        name
        v

    let tests =
      [ testRoundtripList
          "RT.TypeName"
          (pkg [ "TypeName" ] "TypeName" 0)
          V.RuntimeTypes.fqTypeNames
          RT2DT.TypeName.toDT
          RT2DT.TypeName.fromDT
          None

        testRoundtripList
          "RT.FnName"
          (pkg [ "FnName" ] "FnName" 0)
          V.RuntimeTypes.fqFnNames
          RT2DT.FnName.toDT
          RT2DT.FnName.fromDT
          None

        testRoundtripList
          "RT.ConstantName"
          (pkg [ "ConstantName" ] "ConstantName" 0)
          V.RuntimeTypes.fqConstantNames
          RT2DT.ConstantName.toDT
          RT2DT.ConstantName.fromDT
          None

        testRoundtripList
          "RT.TypeReference"
          (pkg [] "TypeReference" 0)
          V.RuntimeTypes.typeReferences
          RT2DT.TypeReference.toDT
          RT2DT.TypeReference.fromDT
          None

        testRoundtripList
          "RT.Expr"
          (pkg [] "Expr" 0)
          V.RuntimeTypes.exprs
          RT2DT.Expr.toDT
          RT2DT.Expr.fromDT
          None

        testRoundtripList
          "RT.Dval"
          (pkg [] "Dval" 0)
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

    let pkg mods name v =
      RT.TypeName.fqPackage
        "Darklang"
        (([ "LanguageTools"; "ProgramTypes" ] @ mods))
        name
        v

    let tests =
      [ testRoundtripList
          "PT.PackageFn"
          (pkg [ "PackageFn" ] "PackageFn" 0)
          V.ProgramTypes.packageFns
          PT2DT.PackageFn.toDT
          PT2DT.PackageFn.fromDT
          None

        testRoundtripList
          "PT.PackageType"
          (pkg [] "PackageType" 0)
          V.ProgramTypes.packageTypes
          PT2DT.PackageType.toDT
          PT2DT.PackageType.fromDT
          None

        testRoundtripList
          "PT.PackageConstant"
          (pkg [] "PackageConstant" 0)
          V.ProgramTypes.packageConstants
          PT2DT.PackageConstant.toDT
          PT2DT.PackageConstant.fromDT
          None

        testRoundtripList
          "PT.UserFunction"
          (pkg [ "UserFunction" ] "UserFunction" 0)
          V.ProgramTypes.userFunctions
          PT2DT.UserFunction.toDT
          PT2DT.UserFunction.fromDT
          None

        testRoundtripList
          "PT.UserTypes"
          (pkg [] "UserType" 0)
          V.ProgramTypes.userTypes
          PT2DT.UserType.toDT
          PT2DT.UserType.fromDT
          None

        testRoundtripList
          "PT.UserConstants"
          (pkg [] "UserConstant" 0)
          V.ProgramTypes.userConstants
          PT2DT.UserConstant.toDT
          PT2DT.UserConstant.fromDT
          None

        testRoundtripList
          "PT.Secret"
          (pkg [] "Secret" 0)
          V.ProgramTypes.userSecrets
          PT2DT.Secret.toDT
          PT2DT.Secret.fromDT
          None

        testRoundtripList
          "PT.DB"
          (pkg [] "DB" 0)
          V.ProgramTypes.userDBs
          PT2DT.DB.toDT
          PT2DT.DB.fromDT
          None

        testRoundtripList
          "PT.Handler"
          (pkg [ "Handler" ] "Handler" 0)
          V.ProgramTypes.Handler.handlers
          PT2DT.Handler.toDT
          PT2DT.Handler.fromDT
          None

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
