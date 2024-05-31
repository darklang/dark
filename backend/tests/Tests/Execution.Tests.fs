module Tests.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude

open TestUtils.TestUtils
open LibExecution.RuntimeTypes
open TestUtils.RTShortcuts

module Exe = LibExecution.Execution

module AT = LibExecution.AnalysisTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes


let testExecFunctionTLIDs : Test =
  testTask "test that exec function returns the right tlids in the trace" {
    let! meta = initializeTestCanvas "exec-function-tlids"
    let name = "testFunction"
    let ps = NEList.singleton "param"
    let (fn : PackageFn.T) =
      testPackageFn "owner" name [] ps (PT.TVariable "a") (PT.EInt64(gid (), 5))
      |> PT2RT.PackageFn.toRT
    let pm = PackageManager.withExtras packageManager [] [] [ fn ]
    let! state = executionStateFor pm meta false false Map.empty

    let tlids, traceFn = Exe.traceTLIDs ()

    let state = { state with tracing.traceExecutionPoint = traceFn }

    let! value =
      Exe.executeFunction
        state
        None
        (FQFnName.Package fn.name)
        []
        (NEList.singleton DUnit)

    Expect.equal (HashSet.toList tlids) [ fn.tlid ] "tlid of function is traced"
    Expect.equal value (Ok(DInt64 5L)) "make sure"
  }

// TYPESCLEANUP add tests for non-record-shaped types


let tests = testList "ExecutionTests" [ testExecFunctionTLIDs ]
