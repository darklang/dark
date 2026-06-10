/// Tests for the conflict-dispatch seam (LibExecution.RuntimeTypes
/// Conflict/Resolution/ConflictDispatch + ExecutionState.conflictDispatch).
/// Verifies the default dispatch is FailLoudly (the unchanged prior behavior) and
/// that an installed policy overrides it — the whole point of the hook.
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
    [ testTask "default dispatch FailLoudly-s a RuntimeError unchanged" {
        let state = freshState ()
        let err = RTE.FnNotFound aName
        let! res =
          state.conflictDispatch (RT.CRuntimeError err) (ctx state) |> Ply.toTask
        match res with
        | RT.RFailLoudly e ->
          Expect.equal e err "FailLoudly carries the same RuntimeError"
        | _ -> failtest "default dispatch should FailLoudly"
      }

      testTask "default dispatch maps FnNotFound to a FailLoudly FnNotFound" {
        let state = freshState ()
        let! res =
          state.conflictDispatch (RT.CFnNotFound aName) (ctx state) |> Ply.toTask
        match res with
        | RT.RFailLoudly(RTE.FnNotFound n) -> Expect.equal n aName "name preserved"
        | _ -> failtest "default dispatch should FailLoudly with FnNotFound"
      }

      testTask "an installed policy overrides the default (Substitute)" {
        let baseState = freshState ()
        let state =
          { baseState with
              conflictDispatch =
                fun _ _ -> uply { return RT.RSubstitute(RT.DInt64 0L) } }
        let! res =
          state.conflictDispatch (RT.CFnNotFound aName) (ctx state) |> Ply.toTask
        match res with
        | RT.RSubstitute(RT.DInt64 n) ->
          Expect.equal n 0L "installed policy substituted"
        | _ ->
          failtest
            "installed policy should Substitute, proving the hook is swappable"
      }

      // CSyncDivergence — two peers bound the same name to different content. It flows
      // through the SAME conflict policy: strict default fails loudly; a sync policy resolves.
      testTask
        "a sync divergence fails loudly by default and is policy-resolvable (last-writer)" {
        let state = freshState ()
        let conflict = RT.CSyncDivergence("Stachu.foo", "hashA", "hashB")

        // strict default: surface the divergence as a loud error naming the location + both hashes
        let! def = state.conflictDispatch conflict (ctx state) |> Ply.toTask
        match def with
        | RT.RFailLoudly(RTE.UncaughtException(msg, _)) ->
          Expect.stringContains
            msg
            "sync divergence"
            "default surfaces the divergence loudly"
          Expect.stringContains msg "Stachu.foo" "the diverged location is named"
        | _ ->
          failtest "default dispatch should FailLoudly (surface) a sync divergence"

        // a last-writer sync policy resolves it without blocking: substitute the incoming hash.
        let lastWriter =
          { state with
              conflictDispatch =
                fun conflict _ ->
                  uply {
                    match conflict with
                    | RT.CSyncDivergence(_, _, incoming) ->
                      return RT.RSubstitute(RT.DString incoming)
                    | _ ->
                      return RT.RFailLoudly(RTE.UncaughtException("unexpected", []))
                  } }
        let! res = lastWriter.conflictDispatch conflict (ctx state) |> Ply.toTask
        match res with
        | RT.RSubstitute(RT.DString s) ->
          Expect.equal s "hashB" "the last-writer policy picked the incoming hash"
        | _ -> failtest "last-writer policy should RSubstitute the incoming hash"
      } ]
