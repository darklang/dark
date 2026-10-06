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
  instanceTest
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
  instanceTest "a parse error saves nothing and exits nonzero" (fun state ->
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
  instanceTest
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

        do! evals state "Tests.Auth.one ()" "7405" "the source keeps its name"
        do! evals state "Tests.Auth.two ()" "7406" "and the target keeps its item"
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
        do! fn state "Tests.RenDep.caller" "() : Int64 = Tests.RenDep.callee ()"
        do! run state [ "type"; "Tests.RenDep.Crate"; "{ renDepWidth: Int64 }" ]
        do!
          run
            state
            [ "trait"
              "Tests.RenDep.Label"
              "<'a> =\n  let label (v: 'a) : String" ]
        do!
          run
            state
            [ "impl"
              "Tests.RenDep"
              "Label for Crate =\n  let label (c: Crate) : String = \"crate\"" ]
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
        do! evals state "Tests.RenDep.caller ()" "7431" "and the caller still runs"
        do! discardAll state
      })

/// `delete` with a live caller. The refusal names how many, and `--ignore-dependents` is the
/// documented way past it -- which retires the name without breaking the caller, because the caller
/// references content and content does not go anywhere.
let deleteRefusesWhileSomethingStillCallsIt =
  instanceTest
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
  instanceTest
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
  instanceTest
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


/// Traits from the shell, end to end: `trait` and `impl` author items, a second
/// implementation of the same trait for the same type is a standing finding and an
/// error at the call, deprecating one resolves both, and rename/delete know the two
/// kinds. One test rather than five because each step needs the store the previous one
/// left, and every CLI test shares one store.
let traitsAreAuthoredListedAndDisambiguated =
  instanceTest
    "trait and impl author items; two implementations are a finding until one is deprecated"
    (fun state ->
      task {
        do! start state
        do! run state [ "type"; "Tests.Tr.Point"; "{ x: Int64\n  y: Int64 }" ]
        do!
          shows
            state
            [ "trait"
              "Tests.Tr.Describe"
              "<'a> =\n  let describe (v: 'a) : String\n  let short (v: 'a) : String" ]
            "Created trait Tests.Tr.Describe"
            "trait authors a trait item"
        do!
          shows
            state
            [ "impl"
              "Tests.Tr"
              "Describe for Point =\n  let describe (p: Point) : String = \"alpha\"\n  let short (p: Point) : String = \"t\"" ]
            "Created implementation Tests.Tr.Point.Describe"
            "impl authors an implementation at <module>.<Type>.<Trait>"
        do!
          evals
            state
            "Tests.Tr.Describe.describe (Tests.Tr.Point { x = 1L; y = 2L })"
            "alpha"
            "the implementation dispatches"
        do!
          shows
            state
            [ "impls"; "Tests.Tr.Describe" ]
            "Tests.Tr.Point.Describe"
            "impls lists it"
        do!
          shows
            state
            [ "impls"; "Tests.Tr.Describe"; "--json" ]
            "\"status\":\"active\""
            "impls --json says what a call would see for each implementation"

        // The method fns are authored by the same batch as the implementation that names them,
        // so the name resolver cannot see them yet. Left unbound, the at-rest check reported
        // `UnresolvedFunctionName` on every correct implementation anyone wrote at the CLI.
        do! run state [ "type"; "Tests.Tr.Other"; "{ n: Int64 }" ]
        do!
          lacks
            state
            [ "impl"
              "Tests.Tr"
              "Describe for Other =\n  let describe (o: Other) : String = \"other\"\n  let short (o: Other) : String = \"o\"" ]
            "Unresolved"
            "a correct implementation authored at the CLI reports nothing"
        do!
          evals
            state
            "Tests.Tr.Describe.describe (Tests.Tr.Other { n = 1L })"
            "other"
            "and it dispatches"

        // A saved caller stores the implementation it resolved to, so it keeps running that
        // one however the store moves afterwards. This is the whole point of the pin, and the
        // rival below is what would otherwise change it underneath.
        do!
          run
            state
            [ "fn"
              "Tests.Tr.callsIt"
              "() : String = Tests.Tr.Describe.describe (Tests.Tr.Point { x = 1L; y = 2L })" ]
        do!
          evals
            state
            "Tests.Tr.callsIt ()"
            "alpha"
            "the saved caller runs what it resolved to"
        do!
          shows
            state
            [ "deps"; "uses"; "Tests.Tr.callsIt" ]
            "Tests.Tr.Point.Describe.describe"
            "and depends on that implementation's fn, like any other call"

        // A rival, from another module, for the same type.
        do!
          run
            state
            [ "impl"
              "Tests.TrOther"
              "Tests.Tr.Describe for Tests.Tr.Point =\n  let describe (p: Tests.Tr.Point) : String = \"beta\"\n  let short (p: Tests.Tr.Point) : String = \"o\"" ]
        do!
          evals
            state
            "Tests.Tr.callsIt ()"
            "alpha"
            "the saved caller is untouched by an implementation written after it"
        // The call does not stop: the rival was written later, so it is the one that runs.
        do!
          evals
            state
            "Tests.Tr.Describe.describe (Tests.Tr.Point { x = 1L; y = 2L })"
            "beta"
            "the newer implementation runs"
        do!
          shows
            state
            [ "constraints"; "--kind"; "rival-implementations" ]
            "Tests.TrOther.Point.Describe"
            "and constraints records the pair"
        do!
          shows
            state
            [ "constraints"; "--kind"; "rival-implementations" ]
            "Calls run Tests.TrOther.Point.Describe, the later of the two"
            "naming the one that runs"

        // Editing the implementation's OTHER method leaves this caller alone. This is why the
        // call pins the method's fn rather than the implementation item: an unrelated edit to
        // the same implementation is not a change to what this call does.
        do!
          run
            state
            [ "impl"
              "Tests.Tr"
              "Describe for Point =\n  let describe (p: Point) : String = \"alpha\"\n  let short (p: Point) : String = \"edited\"" ]
        do!
          evals
            state
            "Tests.Tr.callsIt ()"
            "alpha"
            "an edit to another method of the implementation does not reach this call"

        // An edit to the implementation this caller DOES use is an ordinary update: the caller
        // stays on what it resolved to, and `constraints` offers the new version.
        do!
          run
            state
            [ "impl"
              "Tests.Tr"
              "Describe for Point =\n  let describe (p: Point) : String = \"gamma\"\n  let short (p: Point) : String = \"t\"" ]
        do!
          evals
            state
            "Tests.Tr.callsIt ()"
            "alpha"
            "the caller stays on the version it was written against"
        do!
          shows
            state
            [ "constraints" ]
            "Tests.Tr.callsIt"
            "and the newer implementation shows up as an outdated usage"
        // ...which `follow` catches up, which is the whole point of pinning to a fn: a newer
        // implementation is offered through the machinery that already exists, not forced.
        do!
          run
            state
            [ "propagate"; "follow"; "Tests.Tr.callsIt"; "catch up on the impl" ]
        do!
          evals
            state
            "Tests.Tr.callsIt ()"
            "gamma"
            "and following moves the caller onto the newer implementation"

        // Deprecating the one that RUNS leaves the other, which then runs.
        do!
          run
            state
            [ "deprecate"
              "impl"
              "Tests.TrOther.Point.Describe"
              "--kind"
              "obsolete"
              "-y" ]
        do!
          evals
            state
            "Tests.Tr.Describe.describe (Tests.Tr.Point { x = 1L; y = 2L })"
            // The surviving implementation was edited above, so a fresh call runs its current
            // version. Only a SAVED caller stays on what it resolved to.
            "gamma"
            "the surviving implementation dispatches again"
        do!
          lacks
            state
            [ "constraints"; "--kind"; "rival-implementations" ]
            "Tests.TrOther.Point.Describe"
            "and the finding is gone"

        // The two kinds are ordinary items to rename and delete.
        do!
          shows
            state
            [ "rename"; "Tests.Tr.Describe"; "Tests.Tr.Show" ]
            "renamed Tests.Tr.Describe -> Tests.Tr.Show"
            "rename finds a trait"
        do!
          shows
            state
            [ "delete"; "Tests.Tr.Point.Describe"; "-y" ]
            "Deprecated impl Tests.Tr.Point.Describe"
            "delete infers the impl kind"
        do! discardAll state
      })


/// A rival implementation cannot reach a call inside a BOUNDED generic either. The callee's body
/// records that its implementation comes from `'a` and the CALL records what `'a` implied, so
/// neither is decided again when it runs.
///
/// Its own names throughout, and its own trait: CLI tests share one store, so a test that authored
/// a third implementation for another test's type would change what that test's later steps see.
/// `gates trait-choice-survives-rival` covers the same property against a built CLI; this is the
/// copy that runs in the F# suite, and therefore in CI.
let aRivalCannotReachABoundedCall =
  instanceTest
    "a newer implementation does not reach a call inside a bounded generic"
    (fun state ->
      task {
        do! start state
        do! run state [ "type"; "Tests.Bnd.Pt"; "{ n: Int64 }" ]
        do!
          run
            state
            [ "trait"; "Tests.Bnd.Named"; "<'a> = let name (v: 'a) : String" ]
        do!
          run
            state
            [ "impl"
              "Tests.Bnd"
              "Named for Pt = let name (p: Pt) : String = \"first\"" ]

        // `dark fn` wants the type params adjacent to the name and `module` takes a path rather
        // than inline source, so the bounded generic goes in through a file, as `CliScm.Tests`
        // does for the same reason.
        let file =
          System.IO.Path.Combine(
            System.IO.Path.GetTempPath(),
            "dark-bounded-rival.dark"
          )
        System.IO.File.WriteAllText(
          file,
          "let nameIt<'a: Tests.Bnd.Named> (v: 'a) : String =\n"
          + "  Tests.Bnd.Named.name v\n\n"
          + "let callsBounded () : String =\n"
          + "  Tests.Bnd.nameIt (Tests.Bnd.Pt { n = 1L })\n"
        )
        do! run state [ "module"; "Tests.Bnd"; file ]
        do!
          evals
            state
            "Tests.Bnd.callsBounded ()"
            "first"
            "the bounded call runs what its caller's type argument implied"

        // A newer implementation, for the same trait and the same type.
        do!
          run
            state
            [ "impl"
              "Tests.BndRival"
              "Tests.Bnd.Named for Tests.Bnd.Pt = let name (p: Tests.Bnd.Pt) : String = \"rival\"" ]
        do!
          evals
            state
            "Tests.Bnd.callsBounded ()"
            "first"
            "and the saved bounded call is untouched by it"
        do!
          evals
            state
            "Tests.Bnd.Named.name (Tests.Bnd.Pt { n = 1L })"
            "rival"
            "while a fresh call takes the newer one, so the rival really is the winner"
      })


/// A refusal that writes the thing anyway. `impl` naming a trait that does not exist, or naming a
/// TYPE or a FUNCTION where the trait goes, printed "There is no trait called X" in red and then
/// "Created implementation", exit 0. A bound naming a non-trait did the same, even after
/// explaining that the thing it named is a type. The draft was left holding an implementation of
/// nothing, which `commit` then refused with a list of names nobody had meant to write.
let aRefusedImplementationOrBoundSavesNothing =
  instanceTest
    "an implementation or a bound naming no trait is refused, exits 1, and saves nothing"
    (fun state ->
      task {
        do! start state
        do! run state [ "type"; "Tests.Rfz.Pt"; "{ rfzField: Int64 }" ]
        do! fn state "Tests.Rfz.describe" "(i: Int64) : String = \"d\""

        let refusedImpls =
          [ "Nope", "Nope for Int64 = let nope (p: Int64) : String = \"x\""
            "Tests.Rfz.Pt",
            "Tests.Rfz.Pt for Int64 = let pt (p: Int64) : String = \"x\""
            "Tests.Rfz.describe",
            "Tests.Rfz.describe for Int64 = let describe (p: Int64) : String = \"x\"" ]

        for (named, source) in refusedImpls do
          do!
            refuses
              state
              [ "impl"; "Tests.RfzBad"; source ]
              $"There is no trait called {named}"
              "Created"
              $"impl naming {named}"
          do!
            exits
              state
              [ "impl"; "Tests.RfzBad"; source ]
              1L
              $"impl naming {named} is a failed command"

        do!
          lacks
            state
            [ "ls"; "Tests.RfzBad.Int64" ]
            "implementation of trait"
            "and none of the three was written"

        do!
          refuses
            state
            [ "fn"
              "Tests.RfzBnd.ghost"
              "<'a: Tests.Nope.Trait> (v: 'a) : String = \"x\"" ]
            "There is no trait called Tests.Nope.Trait"
            "Created"
            "a bound on a trait that does not exist"
        do!
          refuses
            state
            [ "fn"
              "Tests.RfzBnd.onAType"
              "<'a: Tests.Rfz.Pt> (v: 'a) : String = \"x\"" ]
            "Tests.Rfz.Pt is a type"
            "Created"
            "a bound naming a type says so"
        do!
          exits
            state
            [ "fn"
              "Tests.RfzBnd.onAType"
              "<'a: Tests.Rfz.Pt> (v: 'a) : String = \"x\"" ]
            1L
            "and is a failed command"
        do!
          lacks
            state
            [ "ls"; "Tests.RfzBnd" ]
            "ghost"
            "neither bounded fn was written"

        // The control, so the assertions above are not passing on an `impl` that refuses
        // everything: a real trait, the same command, written.
        do!
          run
            state
            [ "trait"; "Tests.Rfz.Show"; "<'a> = let show (v: 'a) : String" ]
        do!
          shows
            state
            [ "impl"
              "Tests.RfzOk"
              "Tests.Rfz.Show for Int64 = let show (p: Int64) : String = \"ok\"" ]
            "Created implementation"
            "an implementation of a trait that exists is still written"
      })


/// A commit took a function whose bound names a trait that does not exist: the unresolved-names
/// check read a function's body and signature and not its bounds. `fn` refuses such a bound now,
/// so this goes in through `module`, which is the other way an item arrives.
let commitRefusesABoundThatNamesNothing =
  instanceTest "commit refuses a bound on a trait that does not exist" (fun state ->
    task {
      do! start state
      let file =
        System.IO.Path.Combine(
          System.IO.Path.GetTempPath(),
          $"dark-ghost-bound-{System.Guid.NewGuid()}.dark"
        )
      System.IO.File.WriteAllText(
        file,
        "let ghost<'a: Tests.NoSuch.Trait> (v: 'a) : String = \"x\"\n"
      )
      do! run state [ "module"; "Tests.GhostBnd"; file ]
      do!
        showsAll
          state
          [ "commit"; "ghost"; "-y" ]
          [ "don't resolve"; "Tests.NoSuch.Trait" ]
          "the commit is refused and names the bound's trait"
      do!
        lacks
          state
          [ "log" ]
          "ghost"
          "and nothing was committed (the message would be in the log)"
    })


/// An implementation that does not match its trait saved with a check mark and failed at the
/// first call, naming the method fn and never the trait. `ImplMethodSet` and
/// `ImplMethodSignature` were declared and never produced. Now the save says which, the commit
/// refuses it, and a corrected implementation goes through.
let aMismatchedImplementationIsReportedAtSave =
  instanceTest
    "an implementation that does not match its trait is reported at save and refused at commit"
    (fun state ->
      task {
        do! start state
        do!
          run
            state
            [ "trait"; "Tests.Mism.Show"; "<'a> = let show (v: 'a) : String" ]

        let impl (body : string) =
          [ "impl"; "Tests.MismBad"; $"Tests.Mism.Show for Int64 = {body}" ]

        do!
          showsAll
            state
            (impl "let wrong (p: Int64) : String = \"x\"")
            [ "[ImplMethodSet]"; "does not provide `show`" ]
            "a method the trait does not declare, and one it does left out"
        do!
          showsAll
            state
            (impl "let show (p: Int64) : Int64 = 1L")
            [ "[ImplMethodSignature]"
              "the return type differs"
              "expected (Int64) -> String, got (Int64) -> Int64" ]
            "the wrong return type, at the implementation's type"
        do!
          showsAll
            state
            (impl "let show (p: String) : String = p")
            [ "[ImplMethodSignature]"; "got (String) -> String" ]
            "the wrong parameter type"
        do!
          showsAll
            state
            [ "commit"; "mismatched"; "-y" ]
            [ "cannot commit"; "Tests.MismBad.Int64.Show" ]
            "commit refuses the draft and names the implementation"

        do!
          lacks
            state
            (impl "let show (p: Int64) : String = \"ok\"")
            "type error"
            "a matching implementation reports nothing"
        do! shows state [ "commit"; "matched"; "-y" ] " ops." "and commits"
        do! evals state "Tests.Mism.Show.show 5L" "ok" "and dispatches"
      })


/// A failed module save must not end on a check mark.
///
/// `module` printed `✓ Defined N declarations` as its LAST line, underneath the reasons the save was
/// broken, where a single-item save through `core.dark` has always printed `!` and put the reason
/// after it. Anybody who reads the last line of a failed save read success.
let aFailedModuleSaveDoesNotEndOnATick =
  instanceTest "a failed module save says `!` and not a check mark" (fun state ->
    task {
      do! start state

      let source =
        "type Tick = { n: Int64 }\n\n"
        + "impl Compare for Tick =\n"
        + "  let compare (a: Tick) (b: Tick) : Int64 = 0L\n"

      let path =
        System.IO.Path.Combine(
          System.IO.Path.GetTempPath(),
          "tick-under-errors.dark"
        )

      System.IO.File.WriteAllText(path, source)

      try
        let! out = runCliPlain state [ "module"; "/Tests.TickBad"; path ]

        Expect.stringContains
          out
          "Type check failed"
          $"the save explains itself, got: {out}"
        Expect.stringContains
          out
          "! Defined"
          $"and the count line carries `!`, got: {out}"
        Expect.isFalse
          (out.Contains "✓")
          $"a failed save must not print a check mark anywhere, got: {out}"
      finally
        try
          System.IO.File.Delete path
        with _ ->
          ()

      // The good path still ticks, so the fix is not "never tick".
      let goodSource =
        "type TickOk = { n: Int64 }\n\n"
        + "impl Add for TickOk =\n"
        + "  let add (a: TickOk) (b: TickOk) : TickOk = TickOk { n = a.n + b.n }\n"

      let goodPath =
        System.IO.Path.Combine(System.IO.Path.GetTempPath(), "tick-clean.dark")

      System.IO.File.WriteAllText(goodPath, goodSource)

      try
        let! out = runCliPlain state [ "module"; "/Tests.TickGood"; goodPath ]
        Expect.stringContains
          out
          "✓ Defined"
          $"a clean save still ticks, got: {out}"
      finally
        try
          System.IO.File.Delete goodPath
        with _ ->
          ()
    })


let tests : List<Test> =
  [ aTypeIsUsableByAFunctionAuthoredAfterIt
    traitsAreAuthoredListedAndDisambiguated
    aParseErrorChangesNothing
    aNameThatDisagreesWithTheDeclarationIsRefused
    renameOntoALiveNameIsRefusedAndBothSurvive
    deleteRefusesWhileSomethingStillCallsIt
    deleteStillRefusesAfterARename
    undoStepsBackAndStopsAtTheFirstVersion
    authoringIdenticalSourceReportsUnchanged
    aRivalCannotReachABoundedCall
    aRefusedImplementationOrBoundSavesNothing
    commitRefusesABoundThatNamesNothing
    aMismatchedImplementationIsReportedAtSave
    aFailedModuleSaveDoesNotEndOnATick ]
