module Tests.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude

open TestUtils.TestUtils
open LibExecution.RuntimeTypes
open TestUtils.RTShortcuts

module VT = ValueType
module Dval = LibExecution.Dval
module Exe = LibExecution.Execution
module RuntimeTypesAst = LibExecution.RuntimeTypesAst

module AT = LibExecution.AnalysisTypes
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

type Dictionary<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

let executionStateForPreview
  (name : string)
  (dbs : Map<string, DB.T>)
  (types : Map<TypeName.UserProgram, UserType.T>)
  (fns : Map<FnName.UserProgram, UserFunction.T>)
  (constants : Map<ConstantName.UserProgram, UserConstant.T>)
  : Task<AT.AnalysisResults * ExecutionState> =
  task {
    let canvasID = System.Guid.NewGuid()
    let! state = executionStateFor canvasID false false dbs types fns constants
    let results, traceFn = Exe.traceDvals ()

    let state = { state with tracing = { state.tracing with traceDval = traceFn } }
    return (results, state)
  }

let execSaveDvals
  (canvasName : string)
  (dbs : List<DB.T>)
  (userTypes : List<UserType.T>)
  (userFns : List<UserFunction.T>)
  (userConstants : List<UserConstant.T>)
  (ast : Expr)
  : Task<AT.AnalysisResults> =
  task {
    let types = userTypes |> List.map (fun typ -> typ.name, typ) |> Map.ofList
    let fns = userFns |> List.map (fun fn -> fn.name, fn) |> Map.ofList
    let dbs = dbs |> List.map (fun db -> db.name, db) |> Map.ofList
    let constants = userConstants |> List.map (fun c -> c.name, c) |> Map.ofList

    let! (results, state) =
      executionStateForPreview canvasName dbs types fns constants

    let inputVars = Map.empty
    let! _result = Exe.executeExpr state inputVars ast

    return results
  }


let testExecFunctionTLIDs : Test =
  testTask "test that exec function returns the right tlids in the trace" {
    let! meta = initializeTestCanvas "exec-function-tlids"
    let name = "testFunction"
    let ps = NEList.singleton "param"
    let (fn : UserFunction.T) =
      testUserFn name [] ps (PT.TVariable "a") (PT.EInt(gid (), 5))
      |> PT2RT.UserFunction.toRT
    let fns = Map.ofList [ (fn.name, fn) ]
    let! state = executionStateFor meta false false Map.empty Map.empty fns Map.empty

    let tlids, traceFn = Exe.traceTLIDs ()

    let state = { state with tracing = { state.tracing with traceTLID = traceFn } }

    let! value =
      Exe.executeFunction
        state
        None
        (FQName.UserProgram fn.name)
        []
        (NEList.singleton DUnit)

    Expect.equal (HashSet.toList tlids) [ fn.tlid ] "tlid of function is traced"
    Expect.equal value (Ok(DInt 5L)) "make sure"
  }

// TYPESCLEANUP add tests for non-record-shaped types


let tests = testList "ExecutionTests" [ testExecFunctionTLIDs ]
