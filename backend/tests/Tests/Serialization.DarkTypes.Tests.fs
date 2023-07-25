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
          (RT.TCustomType(typeName, []))
          firstDT
        |> Ply.toTask

      let firstRoundtripped = firstDT |> fromDT
      let roundTrippedAgain = firstRoundtripped |> toDT |> fromDT

      return
        Expect.equal
          roundTrippedAgain
          original
          $"{testName} does not roundtrip successfully"
    }


  let testRoundtripList
    (testName : string)
    (typeName : RT.TypeName.T)
    (original : List<'a>)
    (toDT : 'a -> RT.Dval)
    (fromDT : RT.Dval -> 'a)
    (customEquality : Option<'a -> 'a -> unit>)
    =
    testTask testName {
      let mapped = original |> List.map toDT

      let context =
        LibExecution.TypeChecker.Context.FunctionCallResult(
          fnName =
            (RT.FnName.fqPackage
              "Darklang"
              (NonEmptyList.ofList [ "LanguageTools"; "ProgramTypes" ])
              "expr"
              0),
          returnType = RT.TList(RT.TCustomType(typeName, [])),
          location = None
        )

      let! typeChecked =
        LibExecution.TypeChecker.unify
          context
          types
          (RT.TList(RT.TCustomType(typeName, [])))
          (RT.DList(mapped))
        |> Ply.toTask

      let actual : List<'a> =
        mapped |> List.map fromDT |> List.map toDT |> List.map fromDT

      match customEquality with
      | None ->
        Expect.equal actual original $"{testName} does not roundtrip successfully"
      | Some customEquality ->
        List.zip original actual
        |> List.iter (fun (original, actual) -> customEquality original actual)
    }


  module ProgramTypes =
    let tests =
      [ testRoundtrip
          "PT.PackageFn"
          (RT.TypeName.fqPackage
            "Darklang"
            (NonEmptyList.ofList [ "LanguageTools"; "ProgramTypes"; "PackageFn" ])
            "T"
            0)
          (V.ProgramTypes.packageFn)
          PT2DT.PackageFn.toDT
          PT2DT.PackageFn.fromDT

        testRoundtrip
          "PT.PackageType"
          (RT.TypeName.fqPackage
            "Darklang"
            (NonEmptyList.ofList [ "LanguageTools"; "ProgramTypes"; "PackageType" ])
            "T"
            0)
          (V.ProgramTypes.packageType)
          PT2DT.PackageType.toDT
          PT2DT.PackageType.fromDT

        testRoundtrip
          "PT.UserFunction"
          (RT.TypeName.fqPackage
            "Darklang"
            (NonEmptyList.ofList [ "LanguageTools"; "ProgramTypes"; "UserFunction" ])
            "T"
            0)
          (V.ProgramTypes.userFunction)
          PT2DT.UserFunction.toDT
          PT2DT.UserFunction.fromDT

        testRoundtrip
          "PT.UserRecordType"
          (RT.TypeName.fqPackage
            "Darklang"
            (NonEmptyList.ofList [ "LanguageTools"; "ProgramTypes"; "UserType" ])
            "T"
            0)
          (V.ProgramTypes.userRecordType)
          PT2DT.UserType.toDT
          PT2DT.UserType.fromDT

        testRoundtrip
          "PT.UserEnumType"
          (RT.TypeName.fqPackage
            "Darklang"
            (NonEmptyList.ofList [ "LanguageTools"; "ProgramTypes"; "UserType" ])
            "T"
            0)
          (V.ProgramTypes.userEnumType)
          PT2DT.UserType.toDT
          PT2DT.UserType.fromDT

        testRoundtrip
          "PT.Expr"
          (RT.TypeName.fqPackage
            "Darklang"
            (NonEmptyList.ofList [ "LanguageTools"; "ProgramTypes" ])
            "Expr"
            0)
          (V.ProgramTypes.expr)
          PT2DT.Expr.toDT
          PT2DT.Expr.fromDT

        testRoundtrip
          "PT.TypeReference"
          (RT.TypeName.fqPackage
            "Darklang"
            (NonEmptyList.ofList [ "LanguageTools"; "ProgramTypes" ])
            "TypeReference"
            0)
          V.ProgramTypes.typeReference
          PT2DT.TypeReference.toDT
          PT2DT.TypeReference.fromDT

        testRoundtrip
          "PT.Secret"
          (RT.TypeName.fqPackage
            "Darklang"
            (NonEmptyList.ofList [ "LanguageTools"; "ProgramTypes"; "Secret" ])
            "T"
            0)
          (V.ProgramTypes.userSecret)
          PT2DT.Secret.toDT
          PT2DT.Secret.fromDT

        testRoundtrip
          "PT.DB"
          (RT.TypeName.fqPackage
            "Darklang"
            (NonEmptyList.ofList [ "LanguageTools"; "ProgramTypes"; "DB" ])
            "T"
            0)
          (V.ProgramTypes.userDB)
          PT2DT.DB.toDT
          PT2DT.DB.fromDT

        testRoundtripList
          "PT.LetPatterns"
          (RT.TypeName.fqPackage
            "Darklang"
            (NonEmptyList.ofList [ "LanguageTools"; "ProgramTypes" ])
            "LetPattern"
            0)
          V.ProgramTypes.letPatterns
          PT2DT.LetPattern.toDT
          PT2DT.LetPattern.fromDT
          None

        testRoundtripList
          "PT.CronInterval"
          (RT.TypeName.fqPackage
            "Darklang"
            (NonEmptyList.ofList [ "LanguageTools"; "ProgramTypes"; "Handler" ])
            "CronInterval"
            0)
          V.ProgramTypes.Handler.cronIntervals
          PT2DT.Handler.CronInterval.toDT
          PT2DT.Handler.CronInterval.fromDT
          None

        testRoundtripList
          "PT.HandlerSpec"
          (RT.TypeName.fqPackage
            "Darklang"
            (NonEmptyList.ofList [ "LanguageTools"; "ProgramTypes"; "Handler" ])
            "Spec"
            0)
          V.ProgramTypes.Handler.specs
          PT2DT.Handler.Spec.toDT
          PT2DT.Handler.Spec.fromDT
          None

        testRoundtripList
          "PT.Handler"
          (RT.TypeName.fqPackage
            "Darklang"
            (NonEmptyList.ofList [ "LanguageTools"; "ProgramTypes"; "Handler" ])
            "T"
            0)
          V.ProgramTypes.Handler.handlers
          PT2DT.Handler.toDT
          PT2DT.Handler.fromDT
          None

        testRoundtripList
          "PT.MatchPattern"
          (RT.TypeName.fqPackage
            "Darklang"
            (NonEmptyList.ofList [ "LanguageTools"; "ProgramTypes"; "Handler" ])
            "MatchPattern"
            0)
          V.ProgramTypes.matchPatterns
          PT2DT.MatchPattern.toDT
          PT2DT.MatchPattern.fromDT
          None ]


let tests =
  testList
    "DarkTypes Serialization"
    [ testList
        "roundtrip PTs between internal types and dark types"
        RoundtripTests.ProgramTypes.tests ]
