/// CLI regressions for rename behavior.
module Tests.CliRename

open System.Threading.Tasks
open Expecto

module RT = LibExecution.RuntimeTypes

open Tests.CliTestHarness
open Tests.CliDsl

let refreshedRenameSnapshotsSurviveAProjectionRebuild =
  // This rebuilds the shared store, so parallel readers must finish first.
  testSequenced
  <| cliTest
    "refreshed rename snapshots survive projection rebuild and preserve ordering"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.RefreshLog.foo" "(): Int64 = 86801"
        do! commit state "refresh log baseline"
        do!
          fn
            state
            "Tests.RefreshLog.caller"
            "(): Int64 = Tests.RefreshLog.foo() + Tests.RefreshLog.later()"
        do! run state [ "rename"; "Tests.RefreshLog.foo"; "Tests.RefreshLog.bar" ]
        let before = LibDB.Queries.getDraftOps ()
        let rename =
          before
          |> List.find (function
            | LibExecution.ProgramTypes.PackageOp.Rename _ -> true
            | _ -> false)
          |> Option.defaultWith (fun () -> failtest "expected a draft rename")
        let stamps = LibDB.Queries.getWipOpOriginTs ()
        let stamp = Map.find (LibDB.Inserts.computeOpHash rename) stamps
        Expect.isSome stamp "the rename has an authoring time"
        // This new binding must stay after the rename, even if refreshing the rename changes its ID.
        do! fn state "Tests.RefreshLog.foo" "(): Int64 = 86809"
        do! fn state "Tests.RefreshLog.later" "(): Int64 = 2"
        let after = LibDB.Queries.getDraftOps ()
        let updated =
          after
          |> List.find (function
            | LibExecution.ProgramTypes.PackageOp.Rename _ -> true
            | _ -> false)
          |> Option.defaultWith (fun () -> failtest "expected a draft rename")
        Expect.notEqual
          updated
          rename
          "refresh updates the snapshot's caller version"
        let stamps = LibDB.Queries.getWipOpOriginTs ()
        Expect.equal
          (Map.find (LibDB.Inserts.computeOpHash updated) stamps)
          stamp
          "the renamed op keeps its authoring time"
        let _ = LibDB.Seed.rebuildProjections ()
        let _ =
          LibDB.Seed.evaluateAllValues
            (Builtins.CliHost.Libs.Cli.builtinsToUse ())
            LibDB.PackageManager.rt
        let invalid = LibDB.ReferenceNamesContent.validate after
        Expect.isNone
          invalid
          "all snapshots still have matching content after rebuilding from the canonical log"
        do!
          evals
            state
            "Tests.RefreshLog.foo()"
            "86809"
            "refresh did not reorder the rename after the replacement"
        do!
          evals
            state
            "Tests.RefreshLog.caller()"
            "86803"
            "the refreshed caller uses bar and later"
        do! commitOnly state "refreshed rename" "Tests.RefreshLog.bar"
        do! discardAll state
        do!
          evals
            state
            "Tests.RefreshLog.caller()"
            "86803"
            "the rename commits its refreshed caller and forward dependency"
        do!
          notFound
            state
            "Tests.RefreshLog.foo()"
            "the later old-name reuse stayed draft"
        do!
          shows
            state
            [ "deps"; "uses"; "Tests.RefreshLog.caller" ]
            "Tests.RefreshLog.bar"
            "refresh preserves the renamed dependency"
        do! fn state "Tests.RefreshLog.bar" "(): Int64 = 86810"
        do!
          evals
            state
            "Tests.RefreshLog.caller()"
            "86812"
            "the rebuilt and committed caller follows subsequent edits"
        do! discardAll state
      })

let tests : List<Test> = [ refreshedRenameSnapshotsSurviveAProjectionRebuild ]
