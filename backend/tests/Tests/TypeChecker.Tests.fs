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
    ((fn, args) : RT.FQFnName.T * List<string * RT.Dval>)
    : Result<unit, TypeChecker.Error.T> =
    let args = Map.ofList args

    let fn =
      libraries
      |> Lazy.force
      |> fun l -> l.stdlibFns
      |> Map.get fn
      |> Exception.unwrapOptionInternal "missing library function" [ "fn", fn ]
      |> RT.builtInFnToFn

    let typeArgs = [] // CLEANUP consider adding this as a param

    TypeChecker.checkFunctionCall Map.empty fn typeArgs args

  testMany
    "basic type checking"
    t
    (let intAdd =
      RT.FQFnName.Stdlib { module_ = "Int"; function_ = "add"; version = 0 }

     [ (intAdd, [ ("a", RT.DInt 5L); ("b", RT.DInt 4L) ]), Ok()
       ((intAdd, [ ("a", RT.DInt 5L); ("b", RT.DBool true) ]),
        Error(
          (TypeChecker.Error.TypeUnificationFailure
            { expectedType = RT.TInt; actualValue = RT.DBool true })
        )) ])

let testArguments : Test =
  let t (name, returnType, body) =
    task {
      let canvasID = System.Guid.NewGuid()
      let userFn : RT.UserFunction.T =
        { tlid = id 7
          name = name
          typeParams = []
          parameters = []
          returnType = returnType
          description = ""
          infix = false
          body = body }

      let expr = S.eUserFn name [] []
      let fns = Map.ofList [ name, userFn ]
      let! state = executionStateFor canvasID false Map.empty Map.empty fns
      let! result = Exe.executeExpr state Map.empty expr
      return normalizeDvalResult result
    }

  testManyTask
    "type check arguments"
    t
    [ (("myBadFn", RT.TString, S.eInt 7),
       RT.DError(
         RT.SourceNone,
         "Type error(s) in return type: Expected to see a value of type String but found a Int"
       ))
      (("myGoodFn", RT.TString, S.eStr "test"), RT.DString "test")
      (("myAnyFn", RT.TVariable "a", S.eInt 5), RT.DInt 5L) ]


// TODO: add tests around type args

let tests = testList "typeChecker" [ testBasicTypecheckWorks; testArguments ]
