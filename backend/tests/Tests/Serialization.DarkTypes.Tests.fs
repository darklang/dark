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


module V = SerializationTestValues


module RoundtripTests =
  // for each type/value:
  // perform internal -> DT -> internal -> DT -> internal
  // most of the time, it should end up being the same as the source.
  // if there are known exceptions, break down individual mappings as separate tests

  let types : RT.Types =
    { builtIn = builtIns.types
      package =
        (LibCloud.PackageManager.packageManager (System.TimeSpan.FromMinutes 1.))
          .getType
      userProgram = Map.empty }

  let testRoundtrip
    (testName : string)
    (typeName : RT.TypeName.TypeName)
    (original : 'a)
    (toDT : 'a -> RT.Dval)
    (fromDT : RT.Dval -> 'a)
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

      let roundtripped = firstDT |> fromDT

      return
        Expect.equal
          roundtripped
          original
          $"{testName} does not roundtrip successfully"
    }


  let testRoundtripList
    (testName : string)
    (typeName : RT.TypeName.TypeName)
    (original : List<'a>)
    (toDT : 'a -> RT.Dval)
    (fromDT : RT.Dval -> 'a)
    =
    testList
      testName
      (List.mapWithIndex
        (fun i t -> testRoundtrip $"{testName}[{i}]" typeName t toDT fromDT)
        original)


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

        testRoundtripList
          "PT.PackageType"
          (pkg [] "PackageType" 0)
          V.ProgramTypes.packageTypes
          PT2DT.PackageType.toDT
          PT2DT.PackageType.fromDT

        testRoundtripList
          "PT.PackageConstant"
          (pkg [] "PackageConstant" 0)
          V.ProgramTypes.packageConstants
          PT2DT.PackageConstant.toDT
          PT2DT.PackageConstant.fromDT

        testRoundtripList
          "PT.UserFunction"
          (pkg [ "UserFunction" ] "UserFunction" 0)
          V.ProgramTypes.userFunctions
          PT2DT.UserFunction.toDT
          PT2DT.UserFunction.fromDT

        testRoundtripList
          "PT.UserTypes"
          (pkg [] "UserType" 0)
          V.ProgramTypes.userTypes
          PT2DT.UserType.toDT
          PT2DT.UserType.fromDT

        testRoundtripList
          "PT.UserConstants"
          (pkg [] "UserConstant" 0)
          V.ProgramTypes.userConstants
          PT2DT.UserConstant.toDT
          PT2DT.UserConstant.fromDT

        testRoundtripList
          "PT.Secret"
          (pkg [] "Secret" 0)
          V.ProgramTypes.userSecrets
          PT2DT.Secret.toDT
          PT2DT.Secret.fromDT

        testRoundtripList
          "PT.DB"
          (pkg [] "DB" 0)
          V.ProgramTypes.userDBs
          PT2DT.DB.toDT
          PT2DT.DB.fromDT

        testRoundtripList
          "PT.Handler"
          (pkg [ "Handler" ] "Handler" 0)
          V.ProgramTypes.Handler.handlers
          PT2DT.Handler.toDT
          PT2DT.Handler.fromDT

        ]


let tests =
  testList
    "DarkTypes Serialization"
    [ testList
        "roundtrip PTs between internal types and dark types"
        RoundtripTests.ProgramTypes.tests ]
