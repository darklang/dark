/// Authoring, and what it does when you get it wrong.
///
/// The happy path has tests elsewhere. This file is the sad half, which is where the CLI's answers
/// matter most: a save that reports success and stores nothing, or a refusal that half-happened, is
/// worse than a plain error, and a fall-through arm answers plausibly instead of refusing.
///
/// Every claim is a pair: what it SAYS, and what the store holds afterwards. A refusal that leaves
/// the item deleted anyway is still a bug, however clear the message was.
module Tests.CliAuthoring

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

module RT = LibExecution.RuntimeTypes

open Tests.CliTestHarness
open Tests.CliDsl


/// A type authored, then used by a function authored after it. The interesting half is the second
/// save: the type has to be resolvable by NAME from a declaration written separately, which is a
/// different path from the one a whole file takes.
let aTypeIsUsableByAFunctionAuthoredAfterIt =
  cliTestOnMain
    "a type authored on its own is usable by a function authored after it"
    (fun state ->
      task {
        do! start state
        do!
          run state [ "type"; "Tests.Auth.Pair"; "{ left: Int64\n  right: Int64 }" ]
        do!
          fn
            state
            "Tests.Auth.mk"
            "let mk (): Tests.Auth.Pair =\n  Tests.Auth.Pair { left = 7401L; right = 7402L }"

        do!
          evals
            state
            "(Tests.Auth.mk ()).left"
            "7401"
            "the function resolves the type it was written against"
        do! commit state "auth pair"
        do!
          evals
            state
            "(Tests.Auth.mk ()).right"
            "7402"
            "and still does once committed"
        do! discardAll state
      })

/// A parse error changes NOTHING. The refusal is easy; the part worth pinning is that the draft is
/// exactly as it was, because a partial save is unrecoverable by anything the CLI offers.
let aParseErrorChangesNothing =
  cliTestOnMain "a parse error saves nothing and exits nonzero" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Auth.solid" "() : Int64 = 7403L"
      do! commit state "auth solid"

      do!
        refuses
          state
          [ "fn"; "Tests.Auth.solid"; "() : Int64 = ((" ]
          "Parse error"
          "Updated"
          "a body that does not parse is refused"

      do!
        exits
          state
          [ "fn"; "Tests.Auth.broken"; "() : Int64 = ((" ]
          1L
          "and exits nonzero"

      do!
        evals
          state
          "Tests.Auth.solid ()"
          "7403"
          "the item it failed to replace is untouched"
      do!
        notFound
          state
          "Tests.Auth.broken ()"
          "and the one it failed to create does not exist"
      do! clean state "a refused save leaves no draft behind"
    })

/// A declaration whose own name disagrees with the name you gave the command. Both names are in the
/// message, because either one could be the typo and the message is the only thing that says which
/// is which.
let aNameThatDisagreesWithTheDeclarationIsRefused =
  cliTestOnMain
    "a declaration whose name disagrees with the target is refused, naming both"
    (fun state ->
      task {
        do! start state
        do!
          showsAll
            state
            [ "fn"; "Tests.Auth.alpha"; "let beta (): Int64 =\n  7404L" ]
            [ "beta"; "alpha" ]
            "the refusal names the declaration and the target"

        do! notFound state "Tests.Auth.alpha ()" "and neither name was bound"
        do!
          notFound state "Tests.Auth.beta ()" "including the one in the declaration"
        do! clean state "a refused save leaves no draft behind"
      })

/// Renaming onto a name that already holds something. One name holds one item, so this would take
/// the other item off the shelf as a side effect -- hence the refusal, and hence the assertion that
/// BOTH survive.
let renameOntoALiveNameIsRefusedAndBothSurvive =
  cliTestOnMain
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

        do! evals state "Tests.Auth.one ()" "7405" "the source keeps its name"
        do! evals state "Tests.Auth.two ()" "7406" "and the target keeps its item"
        do! clean state "a refused rename authors nothing"
      })

/// `delete` with a live caller. The refusal names how many, and `--ignore-dependents` is the
/// documented way past it -- which retires the name without breaking the caller, because the caller
/// references content and content does not go anywhere.
let deleteRefusesWhileSomethingStillCallsIt =
  cliTestOnMain
    "delete refuses while something still calls it, and --ignore-dependents proceeds"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Auth.leaf" "() : Int64 = 7407L"
        do! fn state "Tests.Auth.caller" "() : Int64 = Tests.Auth.leaf ()"
        do! commit state "auth delete fixture"

        do!
          refuses
            state
            [ "delete"; "Tests.Auth.leaf"; "-y" ]
            "live dependent"
            "Deprecated"
            "delete refuses while a caller still references it"

        do! evals state "Tests.Auth.caller ()" "7407" "and the caller still runs"

        do!
          shows
            state
            [ "delete"; "Tests.Auth.leaf"; "--ignore-dependents"; "-y" ]
            "Deprecated"
            "--ignore-dependents is the documented way past it"

        // The caller keeps working: it references CONTENT, and retiring a name does not remove the
        // body the name pointed at.
        do!
          evals
            state
            "Tests.Auth.caller ()"
            "7407"
            "the caller is unaffected: it references content, not the name"
        do! discardAll state
      })

/// `undo` steps back one version at a time and stops at the first, saying so rather than removing
/// the item -- which is what `discard` is for, and what the message points at.
let undoStepsBackAndStopsAtTheFirstVersion =
  cliTestOnMain
    "undo steps back one version at a time, and says when there is no further back"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Auth.step" "() : Int64 = 7411L"
        do! commit state "step v1"
        do! fn state "Tests.Auth.step" "() : Int64 = 7412L"
        do! commit state "step v2"
        do! fn state "Tests.Auth.step" "() : Int64 = 7413L"
        do! commit state "step v3"

        do!
          evals
            state
            "Tests.Auth.step ()"
            "7413"
            "the name is at its newest version"

        do! run state [ "undo"; "Tests.Auth.step" ]
        do! evals state "Tests.Auth.step ()" "7412" "undo steps back one"

        do! run state [ "undo"; "Tests.Auth.step" ]
        do! evals state "Tests.Auth.step ()" "7411" "and again"

        do!
          shows
            state
            [ "undo"; "Tests.Auth.step" ]
            "first version"
            "at the first version it says so instead of removing the item"
        do!
          evals
            state
            "Tests.Auth.step ()"
            "7411"
            "and the name still holds that first version"
        do! discardAll state
      })

/// Re-running an authoring command with the same source saves nothing and SAYS nothing was saved.
/// Reporting it as an update is how a dropped edit hides: "Updated" is what you would see whether
/// the store took your change or ignored it.
let authoringIdenticalSourceReportsUnchanged =
  cliTestOnMain
    "authoring the same source twice reports that nothing was saved"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Auth.stable" "() : Int64 = 7414L"
        do! commit state "auth stable"

        do!
          shows
            state
            [ "fn"; "Tests.Auth.stable"; "() : Int64 = 7414L" ]
            "unchanged"
            "an identical re-author says nothing was saved"
        do!
          lacks
            state
            [ "fn"; "Tests.Auth.stable"; "() : Int64 = 7414L" ]
            "Updated"
            "and does not claim an update"

        // The VERSION not moving is the claim. The draft is not empty afterwards: re-authoring
        // produces a `SetName` naming the hash the name already holds, which is a real op that
        // folds to nothing. Committing that keeps the name -- see
        // `reAuthoringTheSameSourceSurvivesTheCommit`, which is where it used to lose it.
        let! before = runCliPlain state [ "hash"; "Tests.Auth.stable" ]
        do! fn state "Tests.Auth.stable" "() : Int64 = 7414L"
        let! after = runCliPlain state [ "hash"; "Tests.Auth.stable" ]
        Expect.equal
          after
          before
          $"the version did not move, got {after} from {before}"
        do! discardAll state
      })


let tests : List<Test> =
  [ aTypeIsUsableByAFunctionAuthoredAfterIt
    aParseErrorChangesNothing
    aNameThatDisagreesWithTheDeclarationIsRefused
    renameOntoALiveNameIsRefusedAndBothSurvive
    deleteRefusesWhileSomethingStillCallsIt
    undoStepsBackAndStopsAtTheFirstVersion
    authoringIdenticalSourceReportsUnchanged ]
