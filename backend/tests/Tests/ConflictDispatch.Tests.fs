/// Tests for the runtime conflict-dispatch seam (LibExecution.RuntimeTypes
/// Conflict/Resolution/ConflictDispatch + ExecutionState.conflictDispatch).
/// Verifies the default dispatch is FailLoudly (the unchanged prior behavior) and
/// that an installed policy overrides it — the whole point of the hook. (SYNC conflicts
/// are decided separately by a `Sync.SyncPolicy`; see `SyncScenarios.Tests`.)
module Tests.ConflictDispatch

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open TestUtils.TestUtils

module RT = LibExecution.RuntimeTypes
module RTE = RT.RuntimeError
module Exe = LibExecution.Execution
module PT = LibExecution.ProgramTypes

let private freshState () : RT.ExecutionState =
  let builtins = localBuiltIns pmPT
  Exe.createState
    builtins
    pmRT
    Exe.noTracing
    (fun _ _ _ _ -> uply { return () })
    (fun _ _ _ _ -> uply { return () })
    PT.mainBranchId
    { dbs = Map.empty }

let private ctx (state : RT.ExecutionState) : RT.CallContext =
  { branchId = state.branchId; threadID = System.Guid.NewGuid() }

let private aName = RT.FQFnName.fqBuiltin "doesNotExist" 0

let tests =
  testList
    "ConflictDispatch"
    [ testTask "default dispatch maps FnNotFound to a FailLoudly FnNotFound" {
        let state = freshState ()
        let! res =
          state.conflictDispatch (RT.Conflict.FnNotFound aName) (ctx state)
          |> Ply.toTask
        match res with
        | RT.Resolution.FailLoudly(RTE.FnNotFound n) ->
          Expect.equal n aName "name preserved"
        | _ -> failtest "default dispatch should FailLoudly with FnNotFound"
      }

      testTask "an installed policy overrides the default (Substitute)" {
        let baseState = freshState ()
        let state =
          { baseState with
              conflictDispatch =
                fun _ _ -> uply { return RT.Resolution.Substitute(RT.DInt64 0L) } }
        let! res =
          state.conflictDispatch (RT.Conflict.FnNotFound aName) (ctx state)
          |> Ply.toTask
        match res with
        | RT.Resolution.Substitute(RT.DInt64 n) ->
          Expect.equal n 0L "installed policy substituted"
        | _ ->
          failtest
            "installed policy should Substitute, proving the hook is swappable"
      } ]
