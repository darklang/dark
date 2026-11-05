/// CLI regressions for partialcommit behavior.
module Tests.CliPartialCommit

open System.Threading.Tasks
open FSharp.Control.Tasks
open Expecto

module RT = LibExecution.RuntimeTypes

open Tests.CliTestHarness
open Tests.CliDsl

/// Cover committed callers, draft caller versions, and later edits that must stay
/// outside the selection. Each case has its own instance and runs on main and a branch.
let partialRenameSelectsRequiredVersions
  (draftBeforeRename : bool)
  (laterEdits : bool)
  =
  let caseName =
    match draftBeforeRename, laterEdits with
    | false, _ -> "committed callers with later draft edits"
    | true, false -> "draft callers without later edits"
    | true, true -> "draft callers with later draft edits"
  instanceTest $"partial rename selects required versions: {caseName}" (fun state ->
    task {
      do! start state
      for branch in [ "main"; "partial-rename-versions" ] do
        do! switch state branch
        let prefix =
          if branch = "main" then "Tests.PartialMain" else "Tests.PartialBranch"
        let foo, bar, caller = $"{prefix}.foo", $"{prefix}.bar", $"{prefix}.caller"
        do! fn state foo "(): Int64 = 86401"
        do! fn state caller $"(): Int64 = {foo}()"
        do! commit state "partial rename baseline"
        if draftBeforeRename then
          do! fn state foo "(): Int64 = 86402"
          do! fn state caller $"(): Int64 = {foo}() + 1"
        do! run state [ "rename"; foo; bar ]
        if laterEdits then
          do! fn state foo "(): Int64 = 86409"
          do! fn state caller "(): Int64 = 86499"
        do! commitOnly state "selected rename" bar
        if laterEdits then
          do! dirty state "later edits remain uncommitted"
        else
          do! clean state "all required draft versions were committed"
        do! discardAll state
        do!
          notFound
            state
            $"{foo}()"
            "the old name and any later replacement are absent"
        let expected = if draftBeforeRename then "86403" else "86401"
        do!
          evals
            state
            $"{caller}()"
            expected
            "the selected caller version survives discard"
        do!
          shows
            state
            [ "deps"; "uses"; caller ]
            bar
            "the committed caller follows the renamed dependency"
        do! fn state foo "(): Int64 = 86409"
        do!
          evals
            state
            $"{caller}()"
            expected
            "reusing the old name does not redirect the caller"
        do! fn state bar "(): Int64 = 86410"
        let followed = if draftBeforeRename then "86411" else "86410"
        do! evals state $"{caller}()" followed "the caller follows subsequent edits"
        do! discardAll state
      do! onMain state
    })

let partialRenameChecksTheCapturedCallerVersion =
  instanceTest
    "partial rename rejects a broken captured caller even after a later fix"
    (fun state ->
      task {
        do! start state
        for branch in [ "main"; "rename-validation" ] do
          do! switch state branch
          let prefix =
            if branch = "main" then
              "Tests.RenameCheckMain"
            else
              "Tests.RenameCheckBranch"
          let original = $"{prefix}.original"
          let moved = $"{prefix}.moved"
          let caller = $"{prefix}.caller"
          do! fn state original "(): Int64 = 93701"
          do! commit state "validation baseline"
          // Saving type errors is allowed. Rename captures this broken version.
          do! fn state caller $"(): String = {original}()"
          do! run state [ "rename"; original; moved ]
          do! fn state caller $"(): Int64 = {moved}()"
          let! review = runCliPlain state [ "commit"; "--json" ]
          use reviewJson = System.Text.Json.JsonDocument.Parse(review)
          Expect.equal
            (reviewJson.RootElement.GetProperty("refused").GetString())
            ""
            "the latest whole draft passes validation after the fix"
          let! (refusal, exitCode) =
            runCliWithExit
              state
              [ "commit"; "rename only"; $"--include={moved}"; "-y" ]
          Expect.equal exitCode 1L "pretty commit refuses the captured type error"
          Expect.stringContains
            (plain refusal)
            "definite errors"
            "the refusal identifies the captured type error"
          let! refusal =
            runCliPlain
              state
              [ "commit"; "rename only"; $"--include={moved}"; "--json"; "-y" ]
          use refusalJson = System.Text.Json.JsonDocument.Parse(refusal)
          Expect.equal
            (refusalJson.RootElement.GetProperty("refused").GetString())
            "type-errors"
            "JSON commit also rejects the captured caller"
          Expect.isFalse
            (refusalJson.RootElement.GetProperty("committed").GetBoolean())
            "JSON refusal does not commit the selection"
          // A refusal must leave the rename uncommitted.
          do! discardAll state
          do!
            evals
              state
              $"{original}()"
              "93701"
              "the original binding remains committed"
          do! notFound state $"{moved}()" "the refused rename was not committed"
        do! onMain state
      })

let aPartialCommitIncludesDeepDependencies =
  instanceTest
    "a partial commit includes dependencies beyond eight names"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.DeepCommit.n0" "(): Int64 = 1"
        for i in 1..11 do
          do!
            fn
              state
              $"Tests.DeepCommit.n{i}"
              $"(): Int64 = Tests.DeepCommit.n{i - 1}() + 1"
        do! commitOnly state "deep dependency closure" "Tests.DeepCommit.n11"
        do! clean state "all twelve declarations and their names commit together"
        do!
          evals
            state
            "Tests.DeepCommit.n11()"
            "12"
            "the committed chain remains callable"
      })

let partialRenameKeepsTransitiveAliasNames =
  instanceTest
    "partial rename preserves historical transitive aliases on main and branches"
    (fun state ->
      task {
        do! start state
        for branch in [ "main"; "transitive-alias" ] do
          do! switch state branch
          let prefix =
            if branch = "main" then
              "Tests.TransitiveMain"
            else
              "Tests.TransitiveBranch"
          for alias in [ "x"; "y" ] do
            do! fn state $"{prefix}.{alias}" "(): Int64 = 941001"
          if branch <> "main" then
            do! fn state $"{prefix}.other" $"(): Int64 = {prefix}.y()"
          do! fn state $"{prefix}.helper" $"(): Int64 = {prefix}.x()"
          do! fn state $"{prefix}.source" "(): Int64 = 941002"
          do!
            fn
              state
              $"{prefix}.caller"
              $"(): Int64 = {prefix}.source() + {prefix}.helper()"
          do! run state [ "rename"; $"{prefix}.source"; $"{prefix}.moved" ]
          do! fn state $"{prefix}.helper" $"(): Int64 = {prefix}.y()"
          do!
            exits
              state
              [ "commit"
                "rename and original dependencies"
                $"--include={prefix}.moved"
                "-y" ]
              0L
              "the partial commit succeeds"
          do! discardAll state
          do!
            shows
              state
              [ "view"; $"{prefix}.x"; "--raw" ]
              "941001"
              "the original dependency name was committed"
          do!
            notFound
              state
              $"{prefix}.y()"
              "the later alias remains unrelated draft work"
          do!
            shows
              state
              [ "view"; $"{prefix}.helper"; "--raw" ]
              "x()"
              "the committed helper follows the original alias"
          do! fn state $"{prefix}.x" "(): Int64 = 941003"
          do!
            evals
              state
              $"{prefix}.caller()"
              "1882005"
              "the dependency chain still follows the committed name"
          do! discardAll state
          do! onMain state
      })

let partialReferenceOnlyEdits =
  instanceTest
    "partial commits include repeated reference-only edits on main and branches"
    (fun state ->
      task {
        do! start state
        for branch in [ "main"; "reference-only" ] do
          do! switch state branch
          let prefix =
            if branch = "main" then
              "Tests.ReferenceMain"
            else
              "Tests.ReferenceBranch"
          do! fn state $"{prefix}.a" "(): Int64 = 991273"
          do! fn state $"{prefix}.b" "(): Int64 = 991273"
          do! fn state $"{prefix}.caller" $"(): Int64 = {prefix}.a()"
          do! commit state "reference baseline"
          for alias in [ "b"; "a"; "b" ] do
            do! fn state $"{prefix}.caller" $"(): Int64 = {prefix}.{alias}()"
            do! fn state $"{prefix}.unrelated" "(): Int64 = 991274"
            for args in
              [ [ "status"; "--json" ]
                [ "commit"; "--json"; $"--include={prefix}.caller" ] ] do
              let! output = runCliPlain state args
              use json = System.Text.Json.JsonDocument.Parse(output)
              let caller =
                json.RootElement.GetProperty("changed").EnumerateArray()
                |> Seq.find (fun item ->
                  item.GetProperty("name").GetString() = $"{prefix}.caller")
              Expect.equal
                (caller.GetProperty("change").GetString())
                "updated"
                "a reference-only edit updates an existing declaration"
            do!
              exits
                state
                [ "commit"; "choose alias"; $"--include={prefix}.caller"; "-y" ]
                0L
                "a reference-only edit can be committed by name"
            do!
              shows
                state
                [ "status"; "--json" ]
                $"{prefix}.unrelated"
                "the unrelated edit stays in the draft"
            do! discardAll state
            do!
              shows
                state
                [ "view"; $"{prefix}.caller"; "--raw" ]
                $"{alias}()"
                "discard keeps the committed reference names"
            do! clean state "no older reference edits remain in the draft"
          do! fn state $"{prefix}.b" "(): Int64 = 991275"
          do!
            evals
              state
              $"{prefix}.caller()"
              "991275"
              "the committed caller follows the selected alias"
          do! discardAll state
          do! onMain state
      })

let partialRenameLeavesLaterAliasEdits =
  instanceTest
    "partial rename leaves later same-hash reference edits in draft"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.PartialAlias.original" "(): Int64 = 582781"
        do! fn state "Tests.PartialAlias.alias" "(): Int64 = 582781"
        do! commit state "alias partial baseline"
        do!
          fn
            state
            "Tests.PartialAlias.caller"
            "(): Int64 = Tests.PartialAlias.original()"
        do!
          run
            state
            [ "rename"; "Tests.PartialAlias.original"; "Tests.PartialAlias.moved" ]
        do!
          fn
            state
            "Tests.PartialAlias.caller"
            "(): Int64 = Tests.PartialAlias.alias()"
        do!
          commitOnly
            state
            "rename without later alias edit"
            "Tests.PartialAlias.moved"
        do! discardAll state
        do!
          shows
            state
            [ "view"; "Tests.PartialAlias.caller"; "--raw" ]
            "moved()"
            "discard reveals the caller version selected by the rename"
        do! fn state "Tests.PartialAlias.alias" "(): Int64 = 582782"
        do!
          evals
            state
            "Tests.PartialAlias.caller()"
            "582781"
            "the later alias edit was not committed"
        do! fn state "Tests.PartialAlias.moved" "(): Int64 = 582783"
        do!
          evals
            state
            "Tests.PartialAlias.caller()"
            "582783"
            "the committed caller follows the rename"
        do! commit state "partial alias verification"
      })

let tests : List<Test> =
  [ testList
      "Partial rename selection"
      [ partialRenameSelectsRequiredVersions false true
        partialRenameSelectsRequiredVersions true false
        partialRenameSelectsRequiredVersions true true ]
    partialRenameChecksTheCapturedCallerVersion
    aPartialCommitIncludesDeepDependencies
    partialRenameKeepsTransitiveAliasNames
    partialReferenceOnlyEdits
    partialRenameLeavesLaterAliasEdits ]
