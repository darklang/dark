module Tests.DarkTypesSerialization

open Expecto
open System.Text.RegularExpressions

open Prelude
open Tablecloth
open TestUtils.TestUtils


module File = LibBackend.File
module Config = LibBackend.Config

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

module PT2DT = StdLibDarkInternal.Helpers.ProgramTypesToDarkTypes

module PT2RT = LibExecution.ProgramTypesToRuntimeTypes


module V = SerializationTestValues



module RoundtripTests =
  // for each type/value:
  // perform internal -> DT -> internal -> DT -> internal
  // most of the time, it should end up being the same as the source.
  // if there are known exceptions, break down individual mappings as separate tests

  let types : RT.Types =
    { builtIn = builtIns.types
      package = LibBackend.PackageManager.packageManager.getType
      userProgram = Map.empty }

  let testRoundtrip
    (testName : string)
    (typeName : RT.TypeName.T)
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
              (NonEmptyList.ofList [ "LanguageTools"; "ProgramTypes" ])
              "expr"
              0),
          returnType = RT.TCustomType(typeName, []),
          location = None
        )

      let! typeChecked =
        LibExecution.TypeChecker.unify
          context
          types
          Map.empty
          (RT.TCustomType(typeName, []))
          firstDT
        |> Ply.toTask

      let msg =
        match typeChecked with
        | Error e ->
          let error = LibExecution.Errors.TypeError e
          $"typechecking failed: {LibExecution.Errors.toString error}"
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
    (typeName : RT.TypeName.T)
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
        (NonEmptyList.ofList ([ "LanguageTools"; "ProgramTypes" ] @ mods))
        name
        v

    let tests =
      [ testRoundtrip
          "PT.PackageFn"
          (pkg [ "PackageFn" ] "T" 0)
          (V.ProgramTypes.packageFn)
          PT2DT.PackageFn.toDT
          PT2DT.PackageFn.fromDT

        testRoundtrip
          "PT.PackageType"
          (pkg [ "PackageType" ] "T" 0)
          (V.ProgramTypes.packageType)
          PT2DT.PackageType.toDT
          PT2DT.PackageType.fromDT

        testRoundtrip
          "PT.UserFunction"
          (pkg [ "UserFunction" ] "T" 0)
          (V.ProgramTypes.userFunction)
          PT2DT.UserFunction.toDT
          PT2DT.UserFunction.fromDT

        testRoundtrip
          "PT.UserRecordType"
          (pkg [ "UserType" ] "T" 0)
          (V.ProgramTypes.userRecordType)
          PT2DT.UserType.toDT
          PT2DT.UserType.fromDT

        testRoundtrip
          "PT.UserEnumType"
          (pkg [ "UserType" ] "T" 0)
          (V.ProgramTypes.userEnumType)
          PT2DT.UserType.toDT
          PT2DT.UserType.fromDT

        testRoundtrip
          "PT.Expr"
          (pkg [] "Expr" 0)
          (V.ProgramTypes.expr)
          PT2DT.Expr.toDT
          PT2DT.Expr.fromDT

        testRoundtrip
          "PT.TypeReference"
          (pkg [] "TypeReference" 0)
          V.ProgramTypes.typeReference
          PT2DT.TypeReference.toDT
          PT2DT.TypeReference.fromDT

        testRoundtrip
          "PT.Secret"
          (pkg [ "Secret" ] "T" 0)
          (V.ProgramTypes.userSecret)
          PT2DT.Secret.toDT
          PT2DT.Secret.fromDT

        testRoundtrip
          "PT.DB"
          (pkg [ "DB" ] "T" 0)
          (V.ProgramTypes.userDB)
          PT2DT.DB.toDT
          PT2DT.DB.fromDT

        testRoundtripList
          "PT.LetPatterns"
          (pkg [] "LetPattern" 0)
          V.ProgramTypes.letPatterns
          PT2DT.LetPattern.toDT
          PT2DT.LetPattern.fromDT

        testRoundtripList
          "PT.CronInterval"
          (pkg [ "Handler" ] "CronInterval" 0)
          V.ProgramTypes.Handler.cronIntervals
          PT2DT.Handler.CronInterval.toDT
          PT2DT.Handler.CronInterval.fromDT

        testRoundtripList
          "PT.HandlerSpec"
          (pkg [ "Handler" ] "Spec" 0)
          V.ProgramTypes.Handler.specs
          PT2DT.Handler.Spec.toDT
          PT2DT.Handler.Spec.fromDT

        testRoundtripList
          "PT.Handler"
          (pkg [ "Handler" ] "T" 0)
          V.ProgramTypes.Handler.handlers
          PT2DT.Handler.toDT
          PT2DT.Handler.fromDT

        testRoundtripList
          "PT.MatchPattern"
          (pkg [] "MatchPattern" 0)
          V.ProgramTypes.matchPatterns
          PT2DT.MatchPattern.toDT
          PT2DT.MatchPattern.fromDT ]


let tests =
  testList
    "DarkTypes Serialization"
    [ testList
        "roundtrip PTs between internal types and dark types"
        RoundtripTests.ProgramTypes.tests ]
