/// Ocean's round-two findings, each as the session that shows it.
///
/// Her repros, narrowed to what the in-process harness can drive. The two-instance ones (a stale
/// branch push beating a newer one, an identity that breaks the relay's owner list) need two stores
/// and a relay between them, so they live in `scripts/testing/_gates-sync` instead.
module Tests.CliScmRound2

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

module RT = LibExecution.RuntimeTypes

open Tests.CliTestHarness
open Tests.CliDsl


// ─── 1. saying a thing again ──────────────────────────────────────────────

/// Ops are content-addressed, so re-binding a name to a hash it held before is byte-identical to
/// the op that first bound it: it dedupes, folds nothing, and the revert silently does not happen
/// while the CLI reports success. `Decision`/`Override` is the op that means "this binding again,
/// and I mean it" -- it is what `propagate pin` already authors for exactly this reason.
let revertingToAnEarlierVersionTakesEffect =
  cliTestOnMain "going back to an earlier version actually goes back" (fun state ->
    task {
      do! start state
      do! switch state "revertbr"
      do! fn state "Tests.Revert.b" "() : Int64 = 611L"
      do! commit state "611"
      do! fn state "Tests.Revert.b" "() : Int64 = 622L"
      do! commit state "622"
      do! evals state "Tests.Revert.b ()" "622" "the second version is live"

      do! fn state "Tests.Revert.b" "() : Int64 = 611L"
      do!
        evals
          state
          "Tests.Revert.b ()"
          "611"
          "and going back to the first takes effect"
      do! dirty state "the revert is a change, so the draft holds it"

      do! commit state "back to 611"
      do! evals state "Tests.Revert.b ()" "611" "and it survives the commit"
      do! onMain state
    })

/// The same on main, where authoring is live-on-write.
let revertingOnMainTakesEffect =
  cliTestOnMain "going back to an earlier version works on main too" (fun state ->
    task {
      do! start state
      do! fn state "Tests.RevertMain.f" "() : Int64 = 1L"
      do! commit state "one"
      do! fn state "Tests.RevertMain.f" "() : Int64 = 2L"
      do! commit state "two"
      do! fn state "Tests.RevertMain.f" "() : Int64 = 1L"
      do! evals state "Tests.RevertMain.f ()" "1" "back to the first version"
      do! discardAll state
    })


// ─── 3. a partial commit ships what it needs ──────────────────────────────

/// `--include=<caller>` on a branch committed the caller and left its dependency's NAME in the
/// draft, so a reader downstream could run the caller but could not find what it called.
let aPartialCommitTakesItsDependencysName =
  cliTestOnMain
    "a partial commit on a branch carries the names it depends on"
    (fun state ->
      task {
        do! start state
        do! switch state "partialbr"
        do! fn state "Tests.Partial2.dep" "() : Int64 = 5L"
        do!
          fn state "Tests.Partial2.caller" "() : Int64 = Tests.Partial2.dep () + 1L"

        do! commitOnly state "caller" "Tests.Partial2.caller"
        do! evals state "Tests.Partial2.caller ()" "6" "the caller runs here"
        do!
          clean
            state
            "and nothing of it is left uncommitted: the dependency came along"
        do! onMain state
      })


// ─── 5. the parent chain ──────────────────────────────────────────────────

/// Commit B, then A, then C: C's parent must be A, the commit that actually precedes it. Taking
/// "the newest commit" by arrival instead put A on nobody's chain.
let commitsFollowTheCommitBeforeThem =
  cliTestOnMain
    "each commit follows the one before it, whatever order they were made in"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Chain.a" "() : Int64 = 1L"
        do! fn state "Tests.Chain.b" "() : Int64 = 2L"
        do! commitOnly state "B" "Tests.Chain.b"
        do! commitOnly state "A" "Tests.Chain.a"

        let! afterA = runCliPlain state [ "commits"; "1" ]
        let aHash =
          System.Text.RegularExpressions.Regex.Match(afterA, @"[0-9a-f]{8}").Value

        do! fn state "Tests.Chain.c" "() : Int64 = 3L"
        do! commit state "C"

        let! newest = runCliPlain state [ "commits"; "1" ]
        let cHash =
          System.Text.RegularExpressions.Regex.Match(newest, @"[0-9a-f]{8}").Value

        let! shown = runCliPlain state [ "show"; cHash ]
        Expect.stringContains
          shown
          aHash
          $"C ({cHash}) follows A ({aHash}), the commit before it, got: {shown}"
        do! discardAll state
      })


// ─── 7. a pin is visible where it applies ─────────────────────────────────

/// A pin made on a parent branch APPLIES on a child (resolution walks the chain), but `propagate`
/// listed only the child's own rows, so the child was told nothing was pinned while being governed
/// by one.
let aParentsPinIsListedOnTheChild =
  cliTestOnMain "a child branch lists the pins that govern it" (fun state ->
    task {
      do! start state
      do! switch state "pinpar"
      do! fn state "Tests.Pin2.base" "() : Int64 = 10L"
      do! fn state "Tests.Pin2.caller" "() : Int64 = Tests.Pin2.base () + 1L"
      do! pin state "Tests.Pin2.caller"
      do! commit state "par"

      do! switch state "pinkid"
      do!
        shows
          state
          [ "propagate" ]
          "Tests.Pin2.caller"
          "the child lists the pin it inherits"

      // And the pin is really in force here, which is what makes the listing's silence a lie.
      do! fn state "Tests.Pin2.base" "() : Int64 = 20L"
      do!
        evals
          state
          "Tests.Pin2.caller ()"
          "11"
          "the inherited pin held the caller back"
      do! onMain state
    })


// ─── 8. answering the conflict you were shown ─────────────────────────────

/// `conflicts override <name>` took the OLDEST row for that name, resolved or not, while the
/// listing shows only pending ones. With one name conflicted twice, answering by name acted on the
/// settled conflict and left the open one open, reporting success either way.
let overrideByNameAnswersThePendingConflict =
  cliTestOnMain "answering a conflict by name answers the open one" (fun state ->
    task {
      do! start state
      do! switch state "confl2"

      let record (id : string) (hash : string) =
        $"""Darklang.SCM.Conflicts.record (Builtin.scmCurrentBranch ()) [Darklang.SCM.Conflicts.Conflict {{ id = "{id}"; owner = "Tests"; modules = "Confl2"; name = "same"; itemType = "fn"; part = ""; kind = "same-name-different-hash"; candidates = []; autoResolvedTo = "{hash}"; reason = "test"; status = "pending"; resolvedBy = "" }}]"""

      do! run state [ "eval"; record "r2first001" "aaa" ]
      do! run state [ "conflicts"; "ack"; "r2first001" ]
      do! run state [ "eval"; record "r2second02" "bbb" ]

      // By NAME: the settled one must not be what answers.
      let! answered = runCliPlain state [ "conflicts"; "ack"; "Tests.Confl2.same" ]
      Expect.stringContains
        answered
        "r2second02"
        $"the open conflict is the one answered, got: {answered}"

      let! left = runCliPlain state [ "conflicts" ]
      Expect.isFalse
        (left.Contains "r2second02")
        $"and it is closed afterwards, got: {left}"
      do! onMain state
    })


// ─── 10. an identity that survives the round trip ─────────────────────────

/// An identity travels as a query parameter and comes back in the relay's owner listing, one per
/// line. A space makes that listing unparseable, so every peer's automatic branch sync skips you; an
/// `&` reads as a second parameter, so ops land under two owners. Both fail silently, which is why
/// the refusal belongs at the point the name is chosen.
let anIdentityIsRefusedIfItCannotTravel =
  cliTest "an identity that would break sync is refused when it is set" (fun state ->
    task {
      do!
        refuses
          state
          [ "identity"; "has a space" ]
          "can't contain"
          "is now"
          "a space is refused"
      do!
        refuses
          state
          [ "identity"; "amp&sand" ]
          "can't contain"
          "is now"
          "an ampersand is refused"
      do! exits state [ "identity"; "has a space" ] 1L "and it is a failed command"

      do!
        shows
          state
          [ "identity"; "alice-laptop_2.0" ]
          "alice-laptop_2.0"
          "an ordinary name is taken"
      // Leave the store as it was found: identity is per-instance config every later test reads.
      do! run state [ "identity"; "inst-test-restored" ]
    })


let tests : List<Test> =
  [ revertingToAnEarlierVersionTakesEffect
    revertingOnMainTakesEffect
    aPartialCommitTakesItsDependencysName
    commitsFollowTheCommitBeforeThem
    aParentsPinIsListedOnTheChild
    overrideByNameAnswersThePendingConflict
    anIdentityIsRefusedIfItCannotTravel ]
