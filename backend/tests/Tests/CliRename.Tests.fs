/// CLI regressions for rename behavior.
module Tests.CliRename

open System.Threading.Tasks
open FSharp.Control.Tasks
open Expecto

module RT = LibExecution.RuntimeTypes

open Tests.CliTestHarness
open Tests.CliDsl

/// Renaming onto a name that already holds something. One name holds one item, so this would take
/// the other item off the shelf as a side effect -- hence the refusal, and hence the assertion that
/// BOTH survive.
let renameOntoALiveNameIsRefusedAndBothSurvive =
  instanceTest
    "rename onto a name that already holds something is refused, and both survive"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Auth.one" "() : Int64 = 7405L"
        do! fn state "Tests.Auth.two" "() : Int64 = 7406L"
        do! commit state "auth rename fixture"

        do!
          refuses
            state
            [ "rename"; "Tests.Auth.one"; "Tests.Auth.two" ]
            "already names something"
            "renamed"
            "renaming onto a live name is refused"

        do! evals state "Tests.Auth.one()" "7405" "the source keeps its name"
        do! evals state "Tests.Auth.two()" "7406" "and the target keeps its item"
        do! clean state "a refused rename authors nothing"
      })

/// A rename must not lose track of what depends on the renamed item. An edge keeps the name its
/// dependent typed, so after a rename the lookup by the new name found nothing, and `delete`
/// deprecated a fn with a live caller, and a trait with a live implementation, instead of refusing.
let deleteStillRefusesAfterARename =
  instanceTest
    "after a rename, delete still sees what depends on the item"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.RenDep.callee" "() : Int64 = 7431L"
        do! fn state "Tests.RenDep.caller" "(): Int64 = Tests.RenDep.callee()"
        do! run state [ "type"; "Tests.RenDep.Crate"; "{ renDepWidth: Int64 }" ]
        do!
          run
            state
            [ "trait"; "Tests.RenDep.Label"; "<'a> =\n  let label(v: 'a): String" ]
        do!
          run
            state
            [ "impl"
              "Tests.RenDep"
              "Label for Crate =\n  let label(c: Crate): String = \"crate\"" ]
        do! commit state "rename dependents fixture"

        do!
          run
            state
            [ "rename"; "Tests.RenDep.callee"; "Tests.RenDep.calleeRenamed" ]
        do! run state [ "rename"; "Tests.RenDep.Label"; "Tests.RenDep.Tag" ]

        do!
          refuses
            state
            [ "delete"; "fn"; "Tests.RenDep.calleeRenamed"; "-y" ]
            "live dependent"
            "Deprecated"
            "a renamed fn's caller still blocks delete"
        do!
          refuses
            state
            [ "delete"; "trait"; "Tests.RenDep.Tag"; "-y" ]
            "live dependent"
            "Deprecated"
            "a renamed trait's implementation still blocks delete"
        do! evals state "Tests.RenDep.caller()" "7431" "and the caller still runs"
        do! discardAll state
      })

/// After renaming fmt to toMoney, callers should show toMoney() and follow later edits.
/// Creating another fmt must not redirect those callers, and content hashes must stay the same.
/// Check main and a branch: branch bindings come from operations, not main's locations table.
let aRenamedItemsCallersFollowTheNewName =
  instanceTest
    "a renamed item's callers follow its new name, not whatever takes the old one"
    (fun state ->
      task {
        do! start state

        let scenario (m : string) =
          task {
            do! fn state $"Tests.{m}.fmt" "() : Int64 = 7461L"
            do! fn state $"Tests.{m}.caller" $"(): Int64 = Tests.{m}.fmt()"
            do! fn state $"Tests.{m}.alias" "() : Int64 = 7461L"
            do! fn state $"Tests.{m}.aliasCaller" $"(): Int64 = Tests.{m}.alias()"
            do!
              fn
                state
                $"Tests.{m}.samePair"
                $"(): Int64 = Tests.{m}.fmt() + Tests.{m}.fmt()"
            do!
              fn
                state
                $"Tests.{m}.aliasPair"
                $"(): Int64 = Tests.{m}.fmt() + Tests.{m}.alias()"
            do! commit state $"{m} fixture"
            let! callerHash = runCliPlain state [ "hash"; $"Tests.{m}.caller" ]
            let! itemHash = runCliPlain state [ "hash"; $"Tests.{m}.fmt" ]
            do! run state [ "rename"; $"Tests.{m}.fmt"; $"Tests.{m}.toMoney" ]
            let! renamedCallerHash =
              runCliPlain state [ "hash"; $"Tests.{m}.caller" ]
            let! renamedItemHash =
              runCliPlain state [ "hash"; $"Tests.{m}.toMoney" ]
            Expect.equal
              renamedCallerHash
              callerHash
              "rename preserves caller content identity"
            Expect.equal
              renamedItemHash
              itemHash
              "rename preserves target content identity"
            do!
              shows
                state
                [ "view"; $"Tests.{m}.aliasCaller"; "--raw" ]
                "alias()"
                "identical content at an alias keeps its name"

            do!
              shows
                state
                [ "view"; $"Tests.{m}.caller"; "--raw" ]
                "toMoney()"
                $"{m}: the caller reads as what it runs"

            do!
              shows
                state
                [ "deps"; "uses"; $"Tests.{m}.caller" ]
                $"Tests.{m}.toMoney"
                "forward dependencies follow the rename"
            let! jsonDependencies =
              runCliPlain state [ "deps"; "uses"; $"Tests.{m}.caller"; "--json" ]
            Expect.stringContains
              jsonDependencies
              $"Tests.{m}.toMoney"
              "JSON dependencies follow the rename"
            Expect.isFalse
              (jsonDependencies.Contains $"\"name\":\"Tests.{m}.fmt\"")
              "JSON dependencies omit the old reference name"
            let! aliasDependencies =
              runCliPlain state [ "deps"; "uses"; $"Tests.{m}.aliasCaller" ]
            Expect.stringContains
              aliasDependencies
              $"Tests.{m}.alias"
              "an alias keeps its own forward dependency name"
            Expect.isFalse
              (aliasDependencies.Contains $"Tests.{m}.toMoney")
              "shared content does not mix caller dependency names"

            do! commitOnly state $"{m} atomic rename" $"Tests.{m}.toMoney"
            do! clean state "a partial commit keeps the entire rename together"

            do! fn state $"Tests.{m}.fmt" "() : Int64 = 7462L"
            do!
              evals
                state
                $"Tests.{m}.caller()"
                "7461"
                $"{m}: a new fmt takes nobody over"
            do!
              lacks
                state
                [ "view"; $"Tests.{m}.caller"; "--raw" ]
                "fmt"
                $"{m}: nor does the caller read as calling it"

            do!
              fn state $"Tests.{m}.replacementCaller" $"(): Int64 = Tests.{m}.fmt()"
            do!
              fn
                state
                $"Tests.{m}.mixed"
                $"(): Int64 = Tests.{m}.toMoney() + Tests.{m}.fmt()"
            do! fn state $"Tests.{m}.toMoney" "() : Int64 = 7463L"
            do!
              evals
                state
                $"Tests.{m}.mixed()"
                "14925"
                "only the renamed reference moves in a mixed caller"
            do!
              evals
                state
                $"Tests.{m}.aliasCaller()"
                "7461"
                "the alias does not follow the renamed name"
            do!
              evals
                state
                $"Tests.{m}.samePair()"
                "14926"
                "both occurrences of the renamed reference follow"
            do!
              evals
                state
                $"Tests.{m}.aliasPair()"
                "14924"
                "an identical-hash alias occurrence stays separate"
            do!
              evals
                state
                $"Tests.{m}.replacementCaller()"
                "7462"
                $"{m}: toMoney leaves fmt's callers alone"
            do!
              evals
                state
                $"Tests.{m}.caller()"
                "7463"
                $"{m}: an edit of toMoney reaches it"

            do!
              lacks
                state
                [ "constraints"; "--json" ]
                $"Tests.{m}"
                "the reference graph reports no stale usages after rename"
            do! commit state $"{m} edits"
            do!
              evals
                state
                $"Tests.{m}.caller()"
                "7463"
                $"{m}: and committing keeps that"
            do!
              evals
                state
                $"Tests.{m}.replacementCaller()"
                "7462"
                $"{m}: committing preserves fmt's callers"

            // Restore exactly the content the old name held before the rename.
            do! fn state $"Tests.{m}.restored" "() : Int64 = 7464L"
            do! commit state $"{m} restore fixture"
            do! run state [ "rename"; $"Tests.{m}.restored"; $"Tests.{m}.kept" ]
            do! fn state $"Tests.{m}.restored" "() : Int64 = 7464L"
            do!
              fn
                state
                $"Tests.{m}.restoredCaller"
                $"(): Int64 = Tests.{m}.restored()"
            do! fn state $"Tests.{m}.restored" "() : Int64 = 7465L"
            do!
              evals
                state
                $"Tests.{m}.restoredCaller()"
                "7465"
                $"{m}: restored content propagates again"
            do!
              evals
                state
                $"Tests.{m}.kept()"
                "7464"
                $"{m}: the other binding keeps its content"
            do! commit state $"{m} restored edits"
            do!
              evals
                state
                $"Tests.{m}.restoredCaller()"
                "7465"
                $"{m}: committing preserves restored callers"

            // Save the function first so its recursive call can resolve.
            do!
              fn state $"Tests.{m}.recursive" "let recursive(n: Int64): Int64 = 0L"
            do!
              fn
                state
                $"Tests.{m}.recursive"
                $"let recursive(n: Int64): Int64 = if n == 0L then 0L else Tests.{m}.recursive(n - 1L)"
            do!
              fn
                state
                $"Tests.{m}.recursiveCaller"
                $"(): Int64 = Tests.{m}.recursive(3L)"
            do!
              evals
                state
                $"Tests.{m}.recursiveCaller()"
                "0"
                "the recursive fixture runs before rename"
            do! commit state $"{m} recursive fixture"
            do!
              run
                state
                [ "rename"; $"Tests.{m}.recursive"; $"Tests.{m}.renamedRecursive" ]
            do!
              shows
                state
                [ "view"; $"Tests.{m}.renamedRecursive"; "--raw" ]
                "renamedRecursive("
                "recursive references use the new name"
            do!
              evals
                state
                $"Tests.{m}.recursiveCaller()"
                "0"
                "recursive callers keep running"
            do!
              fn
                state
                $"Tests.{m}.renamedRecursive"
                $"let renamedRecursive(n: Int64): Int64 = if n == 0L then 7L else Tests.{m}.renamedRecursive(n - 1L)"
            do!
              evals
                state
                $"Tests.{m}.recursiveCaller()"
                "7"
                "recursive callers follow subsequent edits"
            do! commit state $"{m} recursive rename"
          }

        do! scenario "RenTo"
        do! fn state "Tests.RenInherited.leaf" "(): Int64 = 7471L"
        do!
          fn
            state
            "Tests.RenInherited.caller"
            "(): Int64 = Tests.RenInherited.leaf()"
        do! commit state "inherited rename fixture"
        do! switch state "renbr"
        do!
          run
            state
            [ "rename"; "Tests.RenInherited.leaf"; "Tests.RenInherited.moved" ]
        do!
          shows
            state
            [ "view"; "Tests.RenInherited.caller"; "--raw" ]
            "moved()"
            "the branch updates inherited reference metadata"
        do! fn state "Tests.RenInherited.moved" "(): Int64 = 7472L"
        do!
          evals
            state
            "Tests.RenInherited.caller()"
            "7472"
            "inherited callers follow edits on the branch"
        do! scenario "RenToBr"
        do! onMain state
        do!
          shows
            state
            [ "view"; "Tests.RenInherited.caller"; "--raw" ]
            "leaf()"
            "main retains its reference metadata"
        do!
          evals
            state
            "Tests.RenInherited.caller()"
            "7471"
            "main retains its content"
      })

let branchRenamesAndRemovalsDoNotResurrectCallers =
  instanceTest "branch renames and removals do not resurrect callers" (fun state ->
    task {
      do! start state
      do! fn state "Tests.CallerLive.foo" "(): Int64 = 86201L"
      do! fn state "Tests.CallerLive.caller" "(): Int64 = Tests.CallerLive.foo()"
      do! commit state "live callers baseline"
      do! switch state "caller-live"
      do!
        run state [ "rename"; "Tests.CallerLive.caller"; "Tests.CallerLive.moved" ]
      do! fn state "Tests.CallerLive.foo" "(): Int64 = 86202L"
      do!
        evals state "Tests.CallerLive.moved()" "86202" "the renamed caller follows"
      do! notFound state "Tests.CallerLive.caller()" "the old name stays removed"
      do!
        lacks
          state
          [ "deps"; "usedby"; "Tests.CallerLive.foo" ]
          "Tests.CallerLive.caller"
          "reverse queries omit the old name"
      do!
        run
          state
          [ "eval"
            """Darklang.SCM.PackageOps.add(Darklang.SCM.PackageOps.currentBranch(), [Darklang.LanguageTools.ProgramTypes.PackageOp.Unbind(Darklang.LanguageTools.ProgramTypes.PackageLocation { owner = "Tests", modules = ["CallerLive"], name = "moved" }, Darklang.Stdlib.Option.Option.None)])""" ]
      do! fn state "Tests.CallerLive.foo" "(): Int64 = 86203L"
      do!
        notFound state "Tests.CallerLive.moved()" "the deleted caller stays deleted"
      do! value state "Tests.CallerLive.caller" "99L"
      do! fn state "Tests.CallerLive.foo" "(): Int64 = 86204L"
      do!
        evals
          state
          "Tests.CallerLive.caller"
          "99"
          "a different kind at the old name is preserved"
      do! onMain state
      do!
        evals state "Tests.CallerLive.caller()" "86201" "main keeps its own caller"
    })

let refreshedRenameSnapshotsSurviveAProjectionRebuild =
  // This rebuilds the shared store, so parallel readers must finish first.
  testSequenced
  <| cliTest
    "refreshed rename snapshots survive projection rebuild and preserve ordering"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.RefreshLog.foo" "(): Int64 = 86801L"
        do! commit state "refresh log baseline"
        do!
          fn
            state
            "Tests.RefreshLog.caller"
            "(): Int64 = Tests.RefreshLog.foo() + Tests.RefreshLog.later()"
        do! run state [ "rename"; "Tests.RefreshLog.foo"; "Tests.RefreshLog.bar" ]
        let! before = LibDB.Queries.getDraftOps ()
        let rename =
          before
          |> List.find (function
            | LibExecution.ProgramTypes.PackageOp.Rename _ -> true
            | _ -> false)
          |> Option.defaultWith (fun () -> failtest "expected a draft rename")
        let! stamps = LibDB.Queries.getWipOpOriginTs ()
        let stamp = Map.find (LibDB.Inserts.computeOpHash rename) stamps
        Expect.isSome stamp "the rename has an authoring time"
        // This new binding must stay after the rename, even if refreshing the rename changes its ID.
        do! fn state "Tests.RefreshLog.foo" "(): Int64 = 86809L"
        do! fn state "Tests.RefreshLog.later" "(): Int64 = 2L"
        let! after = LibDB.Queries.getDraftOps ()
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
        let! stamps = LibDB.Queries.getWipOpOriginTs ()
        Expect.equal
          (Map.find (LibDB.Inserts.computeOpHash updated) stamps)
          stamp
          "the renamed op keeps its authoring time"
        let! _ = LibDB.Seed.rebuildProjections ()
        let! _ =
          LibDB.Seed.evaluateAllValues
            LibDB.Seed.TrustedSeed
            (Builtins.CliHost.Libs.Cli.builtinsToUse ())
            LibDB.PackageManager.rt
        let! invalid = LibDB.ReferenceNamesContent.validate after
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
        do! fn state "Tests.RefreshLog.bar" "(): Int64 = 86810L"
        do!
          evals
            state
            "Tests.RefreshLog.caller()"
            "86812"
            "the rebuilt and committed caller follows subsequent edits"
        do! discardAll state
      })

let forwardReferenceRefreshFollowsChainedRenames =
  instanceTest "forward reference refresh follows chained renames" (fun state ->
    task {
      do! start state
      do! fn state "Tests.RenameChain.a" "(): Int64 = Tests.RenameChain.later()"
      do! run state [ "rename"; "Tests.RenameChain.a"; "Tests.RenameChain.b" ]
      do! run state [ "rename"; "Tests.RenameChain.b"; "Tests.RenameChain.c" ]
      do! fn state "Tests.RenameChain.later" "(): Int64 = 86521L"
      do!
        evals
          state
          "Tests.RenameChain.c()"
          "86521"
          "the final name gets the resolved declaration"
      do! commit state "resolved after two renames"
      do! fn state "Tests.RenameChain.later" "(): Int64 = 86522L"
      do!
        evals
          state
          "Tests.RenameChain.c()"
          "86522"
          "the final name still follows later edits"
    })

let recursiveForwardReferenceRefreshFollowsChainedRenames =
  instanceTest
    "recursive forward reference refresh follows chained renames"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.RecursiveRenameChain.a" "(n: Int64): Int64 = 0L"
        do! commit state "recursive rename binding"
        do!
          fn
            state
            "Tests.RecursiveRenameChain.a"
            "(n: Int64): Int64 = if n == 0L then Tests.RecursiveRenameChain.later() else Tests.RecursiveRenameChain.a(n - 1L)"
        do!
          exits
            state
            [ "rename"
              "Tests.RecursiveRenameChain.a"
              "Tests.RecursiveRenameChain.b" ]
            0L
            "the first rename succeeds"
        do!
          exits
            state
            [ "rename"
              "Tests.RecursiveRenameChain.b"
              "Tests.RecursiveRenameChain.c" ]
            0L
            "the second rename succeeds"
        do! fn state "Tests.RecursiveRenameChain.later" "(): Int64 = 86531L"
        do!
          evals
            state
            "Tests.RecursiveRenameChain.c(0L)"
            "86531"
            "the forward reference resolves at the final name"
        do!
          evals
            state
            "Tests.RecursiveRenameChain.c(2L)"
            "86531"
            "recursive calls use the refreshed declaration too"
        do!
          shows
            state
            [ "deps"; "uses"; "Tests.RecursiveRenameChain.c" ]
            "Tests.RecursiveRenameChain.c"
            "the recursive reference uses the final name"
        do!
          exits
            state
            [ "commit"; "resolved recursive renames"; "-y" ]
            0L
            "the refreshed recursive declaration commits"
        do!
          evals
            state
            "Tests.RecursiveRenameChain.c(2L)"
            "86531"
            "recursive calls still work after committing"
      })

let refreshKeepsLaterEditsAfterChainedRenames =
  instanceTest "refresh keeps later edits after chained renames" (fun state ->
    task {
      do! start state
      do! fn state "Tests.RenameLatest.a" "(): Int64 = Tests.RenameLatest.later()"
      do! run state [ "rename"; "Tests.RenameLatest.a"; "Tests.RenameLatest.b" ]
      do! run state [ "rename"; "Tests.RenameLatest.b"; "Tests.RenameLatest.c" ]
      do!
        fn
          state
          "Tests.RenameLatest.a"
          "(): Int64 = Tests.RenameLatest.later() + 10L"
      do!
        fn
          state
          "Tests.RenameLatest.c"
          "(): Int64 = Tests.RenameLatest.later() + 1L"
      do! fn state "Tests.RenameLatest.later" "(): Int64 = 86541L"
      do!
        evals
          state
          "Tests.RenameLatest.c()"
          "86542"
          "refresh preserves the newer body at the renamed location"
      do!
        evals
          state
          "Tests.RenameLatest.a()"
          "86551"
          "reusing the old name keeps its own declaration"
    })

let aRenameCanBeDiscardedByEitherName =
  instanceTest
    "a rename can be discarded by either name on main and branches"
    (fun state ->
      task {
        do! start state
        let scenario name =
          task {
            let oldName = $"Tests.{name}.old"
            let moved = $"Tests.{name}.moved"
            do! fn state oldName "(): Int64 = 582731L"
            do! fn state $"Tests.{name}.caller" $"(): Int64 = {oldName}()"
            do! commit state "discard rename fixture"
            for discardName in [ moved; oldName ] do
              do! run state [ "rename"; oldName; moved ]
              do! run state [ "discard"; discardName; "-y" ]
              do!
                evals
                  state
                  $"{oldName}()"
                  "582731"
                  "discard restores the old binding"
              do! notFound state $"{moved}()" "discard removes the new binding"
              do!
                shows
                  state
                  [ "view"; $"Tests.{name}.caller"; "--raw" ]
                  "old()"
                  "discard restores caller names"
              do! clean state "the entire rename was discarded"
          }
        do! scenario "RenameDiscardMain"
        do! switch state "rename-discard-branch"
        do! scenario "RenameDiscardBranch"
        do! onMain state
      })

let renameIntoReusedLocationKeepsSourceReferences =
  instanceTest
    "rename into a reused location transfers source reference names"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.ReusedRename.original" "(): Int64 = 582771L"
        do! fn state "Tests.ReusedRename.alias" "(): Int64 = 582771L"
        do!
          fn
            state
            "Tests.ReusedRename.destination"
            "(): Int64 = Tests.ReusedRename.original()"
        do!
          fn
            state
            "Tests.ReusedRename.source"
            "(): Int64 = Tests.ReusedRename.alias()"
        do! commit state "reused destination baseline"
        do!
          run
            state
            [ "rename"; "Tests.ReusedRename.original"; "Tests.ReusedRename.moved" ]
        do! commit state "record destination metadata"
        do!
          run
            state
            [ "rename"
              "Tests.ReusedRename.destination"
              "Tests.ReusedRename.retired" ]
        do!
          run
            state
            [ "rename"
              "Tests.ReusedRename.source"
              "Tests.ReusedRename.destination" ]
        do!
          shows
            state
            [ "view"; "Tests.ReusedRename.destination"; "--raw" ]
            "alias()"
            "the source metadata replaces the vacant destination history"
        do! fn state "Tests.ReusedRename.moved" "(): Int64 = 582772L"
        do!
          evals
            state
            "Tests.ReusedRename.destination()"
            "582771"
            "the old destination dependency no longer controls it"
        do! fn state "Tests.ReusedRename.alias" "(): Int64 = 582773L"
        do!
          evals
            state
            "Tests.ReusedRename.destination()"
            "582773"
            "the transferred dependency propagates"
        do! commit state "reused destination rename"
      })

let independentRenamesCompose =
  instanceTest
    "independent renames preserve both caller edges in either merge order"
    (fun state ->
      task {
        do! start state
        for reverse in [ false; true ] do
          let prefix =
            if reverse then "Tests.RenameComposeReverse" else "Tests.RenameCompose"
          let branch =
            if reverse then "rename-compose-reverse" else "rename-compose"
          do! fn state $"{prefix}.a" "(): Int64 = 582751L"
          do! fn state $"{prefix}.b" "(): Int64 = 582752L"
          do! fn state $"{prefix}.caller" $"(): Int64 = {prefix}.a() + {prefix}.b()"
          do! commit state "independent rename baseline"
          let first, second = if reverse then "b", "a" else "a", "b"
          do! switch state branch
          do! run state [ "rename"; $"{prefix}.{first}"; $"{prefix}.{first}Moved" ]
          do! commit state "branch rename"
          do! onMain state
          do!
            run state [ "rename"; $"{prefix}.{second}"; $"{prefix}.{second}Moved" ]
          do! commit state "main rename"
          // Apply main's committed rename first, then the branch's rename.
          // Both changes should be kept.
          do! switch state branch
          do!
            shows
              state
              [ "view"; $"{prefix}.caller"; "--raw" ]
              "aMoved()"
              "branch retains the first edge"
          do!
            shows
              state
              [ "view"; $"{prefix}.caller"; "--raw" ]
              "bMoved()"
              "branch retains the second edge"
          do! onMain state
          do! run state [ "merge"; branch; "-y" ]
          do!
            shows
              state
              [ "view"; $"{prefix}.caller"; "--raw" ]
              "aMoved()"
              "merge retains the first edge"
          do!
            shows
              state
              [ "view"; $"{prefix}.caller"; "--raw" ]
              "bMoved()"
              "merge retains the second edge"
          do! fn state $"{prefix}.aMoved" "(): Int64 = 10L"
          do! fn state $"{prefix}.bMoved" "(): Int64 = 20L"
          do!
            evals
              state
              $"{prefix}.caller()"
              "30"
              "both renamed dependencies still propagate"
          do! commit state "updated renamed dependencies"
      })

/// A caller written on a branch against an older version arrives through merge already
/// moved to the parent's newer version (#6039); a rename afterwards still has to find it.
let aRenameIncludesACallerArrivingThroughMerge =
  instanceTest "a rename includes a caller arriving through merge" (fun state ->
    task {
      do! start state
      do! fn state "Tests.RenameOlder.foo" "(): Int64 = 582741L"
      do! commit state "older caller base"
      do! switch state "rename-older-caller"
      do! fn state "Tests.RenameOlder.caller" "(): Int64 = Tests.RenameOlder.foo()"
      do! commit state "caller on branch"
      do! onMain state
      do! fn state "Tests.RenameOlder.foo" "(): Int64 = 582742L"
      do! commit state "main moved on"
      do! run state [ "merge"; "rename-older-caller"; "-y" ]
      let! hash = runCliPlain state [ "hash"; "Tests.RenameOlder.caller" ]
      do!
        evals
          state
          "Tests.RenameOlder.caller()"
          "582742"
          "the merge moves the branch's caller to main's newer version"
      do! run state [ "rename"; "Tests.RenameOlder.foo"; "Tests.RenameOlder.bar" ]
      let! after = runCliPlain state [ "hash"; "Tests.RenameOlder.caller" ]
      Expect.equal after hash "rename preserves the caller's version"
      do!
        shows
          state
          [ "deps"; "uses"; "Tests.RenameOlder.caller" ]
          "Tests.RenameOlder.bar"
          "the merged caller's reference follows the name"
      do! fn state "Tests.RenameOlder.foo" "(): Int64 = 582743L"
      do!
        evals
          state
          "Tests.RenameOlder.caller()"
          "582742"
          "reusing the old name does not redirect the caller"
      do! fn state "Tests.RenameOlder.bar" "(): Int64 = 582744L"
      do!
        evals
          state
          "Tests.RenameOlder.caller()"
          "582744"
          "subsequent edits find the caller"
      do! commit state "renamed merged caller"
    })

/// A caller that reuses this code should keep its own reference names,
/// without picking up names used by earlier callers.
let aReusedCallerKeepsItsOwnReferenceNames =
  instanceTest "reused caller content keeps its own reference names" (fun state ->
    task {
      do! start state
      do! fn state "Tests.RefNames.old" "(): Int64 = 9734822L"
      do! fn state "Tests.RefNames.a" "(): Int64 = Tests.RefNames.old()"
      do! commit state "reference name fixture"
      do! run state [ "rename"; "Tests.RefNames.old"; "Tests.RefNames.moved" ]
      do! fn state "Tests.RefNames.b" "(): Int64 = Tests.RefNames.moved()"
      do! fn state "Tests.RefNames.a" "(): Int64 = 9734823L"
      do! fn state "Tests.RefNames.b" "(): Int64 = 9734824L"
      do! fn state "Tests.RefNames.old" "(): Int64 = 9734822L"
      do! fn state "Tests.RefNames.c" "(): Int64 = Tests.RefNames.old()"
      do! fn state "Tests.RefNames.moved" "(): Int64 = 9734825L"
      do!
        evals
          state
          "Tests.RefNames.c()"
          "9734822"
          "the new caller follows only the restored name"
      do!
        lacks
          state
          [ "constraints"; "--json" ]
          "Tests.RefNames.c"
          "historical alias edges do not report a false stale usage"
      do! commit state "reference name reuse"
      do!
        evals
          state
          "Tests.RefNames.c()"
          "9734822"
          "committing retains that reference choice"
    })

let branchBodyRevertKeepsNewAlias =
  instanceTest
    "a repeated branch body revert keeps its newly authored alias"
    (fun state ->
      task {
        do! start state
        do! switch state "revert-alias"
        do! fn state "Tests.RevertAlias.a" "(): Int64 = 910001L"
        do! fn state "Tests.RevertAlias.b" "(): Int64 = 910001L"
        // The final transition repeats a stored SetName, but changes the alias
        // inside the shared body. Restating the binding must not lose that edit.
        for body in [ "a() + 1L"; "a() + 2L"; "a() + 1L"; "b() + 2L" ] do
          do!
            fn
              state
              "Tests.RevertAlias.caller"
              $"(): Int64 = Tests.RevertAlias.{body}"
          do! commit state "caller revision"
        do!
          shows
            state
            [ "view"; "Tests.RevertAlias.caller"; "--raw" ]
            "b() + 2L"
            "the reverted body keeps the newly chosen alias"
        for body in [ "a() + 1L"; "a() + 2L"; "a() + 1L" ] do
          do! run state [ "undo"; "Tests.RevertAlias.caller" ]
          do!
            shows
              state
              [ "view"; "Tests.RevertAlias.caller"; "--raw" ]
              body
              "undo steps through complete authored revisions"
        do! discardAll state
        // A later alias-only edit must not be folded into the earlier override.
        do!
          fn
            state
            "Tests.RevertAlias.caller"
            "(): Int64 = Tests.RevertAlias.a() + 2L"
        do! run state [ "undo"; "Tests.RevertAlias.caller" ]
        do!
          shows
            state
            [ "view"; "Tests.RevertAlias.caller"; "--raw" ]
            "b() + 2L"
            "a subsequent alias edit has its own undo revision"
        do! discardAll state
        do! fn state "Tests.RevertAlias.a" "(): Int64 = 910010L"
        do!
          evals
            state
            "Tests.RevertAlias.caller()"
            "910003"
            "the old alias no longer controls the caller"
        do! fn state "Tests.RevertAlias.b" "(): Int64 = 910020L"
        do!
          evals
            state
            "Tests.RevertAlias.caller()"
            "910022"
            "the newly chosen alias propagates"
      })

let promotingBranchContentKeepsItsReferenceNames =
  instanceTest
    "main promotion preserves a branch caller's original alias"
    (fun state ->
      task {
        do! start state
        for alias in [ "a"; "b" ] do
          do! fn state $"Tests.PromoteAlias.{alias}" "(): Int64 = 91837261L"
        do! commit state "shared helpers"
        do! switch state "promote-alias"
        do!
          fn
            state
            "Tests.PromoteAlias.branchCaller"
            "(): Int64 = Tests.PromoteAlias.b()"
        do! onMain state
        do!
          fn
            state
            "Tests.PromoteAlias.mainCaller"
            "(): Int64 = Tests.PromoteAlias.a()"
        do! commit state "promote shared content"
        do! switch state "promote-alias"
        do!
          shows
            state
            [ "view"; "Tests.PromoteAlias.branchCaller"; "--raw" ]
            "b()"
            "main cannot replace the branch's canonical reference names"
        do! fn state "Tests.PromoteAlias.a" "(): Int64 = 91837262L"
        do!
          evals
            state
            "Tests.PromoteAlias.branchCaller()"
            "91837261"
            "the branch caller still ignores a"
        do! fn state "Tests.PromoteAlias.b" "(): Int64 = 91837263L"
        do!
          evals
            state
            "Tests.PromoteAlias.branchCaller()"
            "91837263"
            "the branch caller still follows b"
      })

let branchAliasEditsAppearInPreviewAndResolveToParent =
  instanceTest
    "alias-only branch edits appear in preview and resolve to the parent"
    (fun state ->
      task {
        do! start state
        for alias in [ "a"; "b" ] do
          do! fn state $"Tests.ResolveAlias.{alias}" "(): Int64 = 91837271L"
        do!
          fn state "Tests.ResolveAlias.caller" "(): Int64 = Tests.ResolveAlias.a()"
        do! commit state "alias baseline"
        do! switch state "resolve-alias"
        do!
          fn state "Tests.ResolveAlias.caller" "(): Int64 = Tests.ResolveAlias.b()"
        do! commit state "alias edit"
        for args in
          [ [ "diff"; "resolve-alias"; "--json" ]
            [ "merge"; "resolve-alias"; "--dry-run" ] ] do
          do!
            shows
              state
              args
              "Tests.ResolveAlias.caller"
              "the alias change is visible before merging"
        do!
          run
            state
            [ "resolve"; "resolve-alias"; "Tests.ResolveAlias.caller"; "theirs" ]
        do!
          shows
            state
            [ "view"; "Tests.ResolveAlias.caller"; "--raw" ]
            "a()"
            "taking theirs restores the parent's reference names"
        do! fn state "Tests.ResolveAlias.b" "(): Int64 = 91837272L"
        do!
          evals
            state
            "Tests.ResolveAlias.caller()"
            "91837271"
            "the resolved caller no longer follows the branch's alias"
      })

let tests : List<Test> =
  [ promotingBranchContentKeepsItsReferenceNames
    branchAliasEditsAppearInPreviewAndResolveToParent
    renameOntoALiveNameIsRefusedAndBothSurvive
    deleteStillRefusesAfterARename
    aRenamedItemsCallersFollowTheNewName
    branchRenamesAndRemovalsDoNotResurrectCallers
    refreshedRenameSnapshotsSurviveAProjectionRebuild
    forwardReferenceRefreshFollowsChainedRenames
    recursiveForwardReferenceRefreshFollowsChainedRenames
    refreshKeepsLaterEditsAfterChainedRenames
    aRenameCanBeDiscardedByEitherName
    renameIntoReusedLocationKeepsSourceReferences
    independentRenamesCompose
    aRenameIncludesACallerArrivingThroughMerge
    aReusedCallerKeepsItsOwnReferenceNames
    branchBodyRevertKeepsNewAlias ]
