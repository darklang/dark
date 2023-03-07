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
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PTParser = LibExecution.ProgramTypesParser
module Exe = LibExecution.Execution
module TypeChecker = LibExecution.TypeChecker


open TestUtils.TestUtils
module S = TestUtils.RTShortcuts


let testBasicTypecheckWorks : Test =
  let t
    ((fn, args) : string * List<string * RT.Dval>)
    : Result<unit, TypeChecker.Error.T list> =
    let args = Map.ofList args

    let fn =
      let fn = fn |> Parser.FQFnNameParser.parse |> PT2RT.FQFnName.toRT
      libraries
      |> Lazy.force
      |> fun l -> l.stdlib
      |> Map.get fn
      |> Exception.unwrapOptionInternal "missing library function" [ "fn", fn ]
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
       )) ]

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
         "Type error(s) in return type: Expected to see a value of type Str but found a Int"
       ))
      (("myGoodFn", RT.TStr, S.eStr "test"), RT.DStr "test")
      (("myAnyFn", RT.TVariable "a", S.eInt 5), RT.DInt 5L) ]




let tests = testList "typeChecker" [ testBasicTypecheckWorks; testArguments ]
