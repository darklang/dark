/// CLI regressions for undo behavior.
module Tests.CliUndo

open System.Threading.Tasks
open FSharp.Control.Tasks
open Expecto

module RT = LibExecution.RuntimeTypes

open Tests.CliTestHarness
open Tests.CliDsl

let undoReferenceOnlyEdits =
  instanceTest
    "undo restores reference-only versions on main and branches"
    (fun state ->
      task {
        do! start state
        for branch in [ "main"; "undo-references" ] do
          do! switch state branch
          let prefix =
            if branch = "main" then "Tests.UndoRefsMain" else "Tests.UndoRefsBranch"
          for alias in [ "a"; "b"; "c" ] do
            do! fn state $"{prefix}.{alias}" "(): Int64 = 773991"
          do! fn state $"{prefix}.caller" $"(): Int64 = {prefix}.a()"
          do! commit state "initial reference version"
          do! fn state $"{prefix}.other" $"(): Int64 = {prefix}.b()"
          do! commit state "new caller using a shared body"
          do!
            shows
              state
              [ "undo"; $"{prefix}.other" ]
              "first version"
              "a new caller has no earlier alias version"
          for alias in [ "b"; "c" ] do
            do! fn state $"{prefix}.caller" $"(): Int64 = {prefix}.{alias}()"
            do! commit state "change followed alias"
          for alias in [ "b"; "a" ] do
            do! exits state [ "undo"; $"{prefix}.caller" ] 0L "undo succeeds"
            do!
              shows
                state
                [ "view"; $"{prefix}.caller"; "--raw" ]
                $"{alias}()"
                "undo restores the previous reference names"
          do!
            shows
              state
              [ "undo"; $"{prefix}.caller" ]
              "first version"
              "repeated undo stops at the first reference version"
          do! fn state $"{prefix}.a" "(): Int64 = 773992"
          do!
            evals
              state
              $"{prefix}.caller()"
              "773992"
              "the restored caller follows its old alias"
          do! discardAll state
          do!
            shows
              state
              [ "view"; $"{prefix}.caller"; "--raw" ]
              "c()"
              "discard restores the committed reference version"
          do! onMain state
      })

let undoKeepsRepeatedRevisions =
  instanceTest
    "undo keeps repeated alias revisions and edits after undo"
    (fun state ->
      task {
        do! start state
        for branch in [ "main"; "repeat-undo" ] do
          do! switch state branch
          let prefix =
            if branch = "main" then "Tests.RepeatMain" else "Tests.RepeatBranch"
          for alias in [ "a"; "b"; "c" ] do
            do! fn state $"{prefix}.{alias}" "(): Int64 = 781551"
          for alias in [ "a"; "b"; "a" ] do
            do! fn state $"{prefix}.caller" $"(): Int64 = {prefix}.{alias}()"
            do! commit state "alias revision"
          do! run state [ "undo"; $"{prefix}.caller" ]
          do!
            shows
              state
              [ "view"; $"{prefix}.caller"; "--raw" ]
              "b()"
              "undo retains repeated states"
          do! fn state $"{prefix}.caller" $"(): Int64 = {prefix}.c()"
          do! commit state "edit after undo"
          do!
            shows
              state
              [ "undo"; $"{prefix}.caller" ]
              "is back on the version before"
              "undo selects the revision before the new edit"
          do!
            shows
              state
              [ "view"; $"{prefix}.caller"; "--raw" ]
              "b()"
              "new edits follow the restored revision"
          do! fn state $"{prefix}.unrelated" "(): Int64 = 781552"
          do! commitOnly state "commit undo" $"{prefix}.caller"
          do! discardAll state
          do!
            shows
              state
              [ "view"; $"{prefix}.caller"; "--raw" ]
              "b()"
              "partial commit retains the restored names"
          do! run state [ "undo"; $"{prefix}.caller" ]
          do!
            shows
              state
              [ "view"; $"{prefix}.caller"; "--raw" ]
              "a()"
              "undo continues from the committed cursor"
          do!
            shows
              state
              [ "undo"; $"{prefix}.caller" ]
              "first version"
              "undo stops at the initial revision"
          do! discardAll state
          do!
            shows
              state
              [ "view"; $"{prefix}.caller"; "--raw" ]
              "b()"
              "discard restores the committed undo"
          do! onMain state
      })

let undoUsesBranchBaseline =
  instanceTest "undo uses the branch baseline before later main edits" (fun state ->
    task {
      do! start state
      for alias in [ "a"; "b"; "c" ] do
        do! fn state $"Tests.UndoBaseline.{alias}" "(): Int64 = 781561"
      do! fn state "Tests.UndoBaseline.caller" "(): Int64 = Tests.UndoBaseline.a()"
      do! commit state "baseline"
      do! switch state "undo-baseline"
      do! fn state "Tests.UndoBaseline.caller" "(): Int64 = Tests.UndoBaseline.b()"
      do! commit state "branch edit"
      do! onMain state
      do! fn state "Tests.UndoBaseline.caller" "(): Int64 = Tests.UndoBaseline.c()"
      do! commit state "later main edit"
      do! switch state "undo-baseline"
      do!
        shows
          state
          [ "undo"; "Tests.UndoBaseline.caller" ]
          "is back on the version before"
          "undo selects the branch's earlier revision"
      do!
        shows
          state
          [ "view"; "Tests.UndoBaseline.caller"; "--raw" ]
          "a()"
          "undo restores the branch's original alias"
      do! discardAll state
      do!
        shows
          state
          [ "view"; "Tests.UndoBaseline.caller"; "--raw" ]
          "b()"
          "discard restores the branch edit"
      do! onMain state
      do!
        shows
          state
          [ "view"; "Tests.UndoBaseline.caller"; "--raw" ]
          "c()"
          "main keeps its later edit"
    })

let undoOrdersBodyAndAliasEdits =
  instanceTest "undo follows body and alias edits in one order" (fun state ->
    task {
      do! start state
      for alias in [ "a"; "b" ] do
        do! fn state $"Tests.UndoMixed.{alias}" "(): Int64 = 781571"
      for body in [ "a()"; "b()"; "b() + 1"; "a() + 1" ] do
        do! fn state "Tests.UndoMixed.caller" $"(): Int64 = Tests.UndoMixed.{body}"
        do! commit state "mixed revision"
      for body in [ "b() + 1"; "b()"; "a()" ] do
        do! run state [ "undo"; "Tests.UndoMixed.caller" ]
        do!
          shows
            state
            [ "view"; "Tests.UndoMixed.caller"; "--raw" ]
            body
            "undo restores the preceding complete revision"
      do!
        shows
          state
          [ "undo"; "Tests.UndoMixed.caller" ]
          "first version"
          "undo stops after both kinds of edit"
    })

let undoSurvivesDraftRefresh =
  instanceTest
    "undo retains earlier draft revisions across forward reference refresh"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.UndoRefresh.f" "(): Int64 = 667741"
        do! commit state "undo refresh baseline"
        for value in [ 667742; 667743; 667744 ] do
          do! fn state "Tests.UndoRefresh.f" $"(): Int64 = {value}"
        do!
          fn
            state
            "Tests.UndoRefresh.pending"
            "(): Int64 = Tests.UndoRefresh.missing()"
        do! fn state "Tests.UndoRefresh.missing" "(): Int64 = 667745"
        do!
          evals
            state
            "Tests.UndoRefresh.pending()"
            "667745"
            "the forward reference actually refreshed"
        for value in [ "667743"; "667742"; "667741" ] do
          do! run state [ "undo"; "Tests.UndoRefresh.f" ]
          do!
            evals
              state
              "Tests.UndoRefresh.f()"
              value
              "undo keeps its earlier history after refresh"
        do!
          shows
            state
            [ "undo"; "Tests.UndoRefresh.f" ]
            "first version"
            "undo stops only at the original revision"
      })

let undoTracksRefreshedRevisionIds =
  instanceTest
    "undo follows its selected revision when forward references change its id"
    (fun state ->
      task {
        do! start state
        for alias in [ "a"; "b"; "c" ] do
          do! fn state $"Tests.UndoRefreshedId.{alias}" "(): Int64 = 667751"
        do! fn state "Tests.UndoRefreshedId.caller" "(): Int64 = 667750"
        do! commit state "undo id baseline"
        for alias in [ "a"; "b"; "c" ] do
          do!
            fn
              state
              "Tests.UndoRefreshedId.caller"
              $"(): Int64 = Tests.UndoRefreshedId.later() + Tests.UndoRefreshedId.{alias}()"
        do! run state [ "undo"; "Tests.UndoRefreshedId.caller" ]
        do! fn state "Tests.UndoRefreshedId.later" "(): Int64 = 2"
        do!
          evals
            state
            "Tests.UndoRefreshedId.caller()"
            "667753"
            "refresh resolves the selected revision"
        do! run state [ "undo"; "Tests.UndoRefreshedId.caller" ]
        do!
          shows
            state
            [ "view"; "Tests.UndoRefreshedId.caller"; "--raw" ]
            "a()"
            "refresh keeps each earlier alias choice"
        do! run state [ "undo"; "Tests.UndoRefreshedId.caller" ]
        do!
          evals
            state
            "Tests.UndoRefreshedId.caller()"
            "667750"
            "undo still reaches the preceding committed revision"
      })

/// `undo` steps back one version at a time and stops at the first, saying so rather than removing
/// the item -- which is what `discard` is for, and what the message points at.
let undoStepsBackAndStopsAtTheFirstVersion =
  instanceTest
    "undo steps back one version at a time, and says when there is no further back"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Auth.step" "() : Int64 = 7411"
        do! commit state "step v1"
        do! fn state "Tests.Auth.step" "() : Int64 = 7412"
        do! commit state "step v2"
        do! fn state "Tests.Auth.step" "() : Int64 = 7413"
        do! commit state "step v3"

        do!
          evals state "Tests.Auth.step()" "7413" "the name is at its newest version"

        do! run state [ "undo"; "Tests.Auth.step" ]
        do! evals state "Tests.Auth.step()" "7412" "undo steps back one"

        do! run state [ "undo"; "Tests.Auth.step" ]
        do! evals state "Tests.Auth.step()" "7411" "and again"

        do!
          shows
            state
            [ "undo"; "Tests.Auth.step" ]
            "first version"
            "at the first version it says so instead of removing the item"
        do!
          evals
            state
            "Tests.Auth.step()"
            "7411"
            "and the name still holds that first version"
        do! discardAll state
      })

let undoSurvivesFullCommit =
  instanceTest
    "undo continues after committing a restored draft revision"
    (fun state ->
      task {
        do! start state
        let name = "Tests.UndoCommit.f"
        do! fn state name "(): Int64 = 91837400"
        do! commit state "baseline"
        for value in [ 91837401; 91837402; 91837403 ] do
          do! fn state name $"(): Int64 = {value}"
        for value in [ "91837402"; "91837401" ] do
          do! run state [ "undo"; name ]
          do! evals state $"{name}()" value "undo selects an earlier draft revision"
        do! commit state "commit restored revision"
        do! evals state $"{name}()" "91837401" "commit retains the restored version"
        do! run state [ "undo"; name ]
        do! evals state $"{name}()" "91837400" "undo reaches the committed baseline"
        do!
          shows state [ "undo"; name ] "first version" "undo stops at the baseline"
        do! discardAll state
        do!
          evals
            state
            $"{name}()"
            "91837401"
            "discard returns to the committed restore"
      })

let refreshResolvesTheRestoredBody =
  instanceTest "refresh resolves the distinct body selected by undo" (fun state ->
    task {
      do! start state
      let name = "Tests.UndoForward.f"
      let helper = "Tests.UndoForward.helper"
      do! fn state name "(): Int64 = 91838000"
      do! commit state "baseline"
      for offset in [ 1; 2 ] do
        do! fn state name $"(): Int64 = {helper}() + {offset}"
      do! run state [ "undo"; name ]
      do! fn state helper "(): Int64 = 91838010"
      do! evals state $"{name}()" "91838011" "refresh resolves the restored body"
      do! commit state "resolved restore"
      do! run state [ "undo"; name ]
      do!
        evals
          state
          $"{name}()"
          "91838000"
          "refresh and commit retain the undo cursor"
      do! discardAll state
      do! evals state $"{name}()" "91838011" "discard restores the resolved commit"
    })

let tests : List<Test> =
  [ undoReferenceOnlyEdits
    undoKeepsRepeatedRevisions
    undoUsesBranchBaseline
    undoOrdersBodyAndAliasEdits
    undoSurvivesDraftRefresh
    undoTracksRefreshedRevisionIds
    undoSurvivesFullCommit
    refreshResolvesTheRestoredBody
    undoStepsBackAndStopsAtTheFirstVersion ]
