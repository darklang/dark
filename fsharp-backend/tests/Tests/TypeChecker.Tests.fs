module Tests.TypeChecker

// Test the type checker

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module S = LibExecution.Shortcuts
module Exe = LibExecution.Execution
module TypeChecker = LibExecution.TypeChecker


open TestUtils.TestUtils


let testBasicTypecheckWorks : Test =
  let t
    ((fn, args) : string * List<string * RT.Dval>)
    : Result<unit, TypeChecker.Error.T list> =
    let args = Map.ofList args in

    let fn =
      libraries
      |> Lazy.force
      |> fun l -> l.stdlib
      |> Map.get (PT.FQFnName.parse fn)
      |> Option.unwrapUnsafe
      |> RT.builtInFnToFn

    TypeChecker.checkFunctionCall Map.empty fn args

  testMany
    "basic type checking"
    t
    [ ("Int::add_v0", [ ("a", RT.DInt 5L); ("b", RT.DInt 4L) ]), Ok()
      (("Int::add_v0", [ ("a", RT.DInt 5L); ("b", RT.DBool true) ]),
       Error(
         [ TypeChecker.Error.TypeUnificationFailure
             { expectedType = RT.TInt; actualValue = RT.DBool true } ]
       ))

      ("toString_v0", [ ("v", RT.DInt 5L) ]), Ok() ]

let testErrorNotWrappedByErrorRail =
  testTask "error not wrapped by errorRail" {
    let expr = FSharpToExpr.parseRTExpr "Dict.get_v1 (List.empty_v0 []) \"hello\""
    let! meta = createTestCanvas "error-not-wrapper-rail"

    let! state = executionStateFor meta Map.empty Map.empty

    let! result = Exe.executeExpr state Map.empty expr

    Expect.isTrue
      (match result with
       | RT.DError _ -> true
       | _ -> false)
      ""
  }

let testArguments : Test =
  let t (name, returnType, body) =
    task {
      let! meta = createTestCanvas name
      let userFn : RT.UserFunction.T =
        { tlid = id 7
          name = name
          parameters = []
          returnType = returnType
          description = ""
          infix = false
          body = body }
      let! owner = testOwner.Force()

      let expr = S.eApply (S.eUserFnVal name) []
      let fns = Map.ofList [ name, userFn ]
      let! state = executionStateFor meta Map.empty fns
      let! result = Exe.executeExpr state Map.empty expr
      return normalizeDvalResult result
    }

  testManyTask
    "type check arguments"
    t
    [ (("myBadFn", RT.TStr, S.eInt 7),
       RT.DError(
         RT.SourceNone,
         "Type error(s) in return type: Expected to see a value of type String but found a Int"
       ))
      (("myGoodFn", RT.TStr, S.eStr "test"), RT.DStr "test")
      (("myAnyFn", RT.TVariable "a", S.eInt 5), RT.DInt 5L) ]




let tests =
  testList
    "typeChecker"
    [ testBasicTypecheckWorks; testErrorNotWrappedByErrorRail; testArguments ]
