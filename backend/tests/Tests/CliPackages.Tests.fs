/// The reading half of the CLI: navigating the package tree, and looking at what is
/// in it.
///
/// Nothing here is about SCM. These are the commands a person runs before they
/// change anything -- `ls`, `view`, `tree`, `search`, `deps`, `hash` -- plus the
/// authoring verbs that pair with them (`fn`, `val`, `delete`, `restore`,
/// `deprecate`). They had almost no coverage, and Dark resolves names when the line
/// RUNS, so a rename in `packages/` leaves holes here that a green suite cannot see.
/// Two of these tests are regressions for exactly that: `hash` and `find-values`
/// each parsed a name with their own rule and answered "not found" about items
/// `view` printed happily.
module Tests.CliPackages

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

module RT = LibExecution.RuntimeTypes

open Tests.CliTestHarness
open Tests.CliDsl


// ─── looking around ───────────────────────────────────────────────────────

let lsNamesWhatIsThere =
  instanceTest "ls names what a module holds" (fun state ->
    task {
      do!
        showsAll
          state
          [ "ls"; "Darklang.Stdlib.Option" ]
          [ "Contents of"; "Option" ]
          "ls lists the module it was given"
      do!
        shows
          state
          [ "ls"; "Darklang.Nope.Nothing" ]
          "ot found"
          "ls says so when the module isn't there"
    })

let treeShowsDescendants =
  instanceTest "tree shows a module's descendants" (fun state ->
    task {
      do!
        showsAll
          state
          [ "tree"; "Darklang.Stdlib.Option" ]
          [ "Package tree"; "Option" ]
          "tree roots at what it was given"
    })

let viewPrintsSource =
  instanceTest "view prints an item's source" (fun state ->
    task {
      do!
        shows
          state
          [ "view"; "Darklang.Stdlib.List.head" ]
          "let head"
          "view prints the declaration"
      do!
        shows
          state
          [ "view"; "Darklang.Stdlib.List.head"; "--raw" ]
          "let head"
          "and --raw prints it without the trimmings"
    })

let viewRefusesWhatIsNotThere =
  instanceTest "view refuses a name that holds nothing" (fun state ->
    task {
      do!
        shows
          state
          [ "view"; "Darklang.Nope.nope" ]
          "Not found"
          "view names what it looked for"
      // Nonzero, because `dark view X || echo missing` is a reasonable thing to
      // write.
      do!
        exits
          state
          [ "view"; "Darklang.Nope.nope" ]
          1L
          "a failed view is a failed command"
    })

let searchFindsByText =
  instanceTest "search finds items by text" (fun state ->
    task {
      do!
        showsAll
          state
          [ "search"; "head" ]
          [ "Search results for"; "head" ]
          "search reports what it matched"
    })

let depsNamesWhatAnItemUses =
  instanceTest "deps names what an item uses" (fun state ->
    task {
      do!
        shows
          state
          [ "deps"; "Darklang.Stdlib.List.head" ]
          "Option"
          "head returns an Option, so it depends on one"
      do!
        shows
          state
          [ "deps"; "Darklang.Nope.nope" ]
          "Not found"
          "deps says so when there's nothing to walk"
    })

/// Regression: `hash` split the name itself and read the FIRST segment as the owner,
/// so `hash Stdlib.List.head` looked for an owner called Stdlib and said "Not found"
/// about a name `view` prints. It traverses now, like everything else that takes a
/// name.
let hashResolvesNamesLikeViewDoes =
  instanceTest "hash takes the same names view takes" (fun state ->
    task {
      do!
        shows
          state
          [ "view"; "Stdlib.List.head" ]
          "let head"
          "view takes the un-owner-qualified name"
      do! shows state [ "hash"; "Stdlib.List.head" ] "fn " "and so does hash"
      do!
        shows
          state
          [ "hash"; "Darklang.Stdlib.List.head" ]
          "fn "
          "the fully qualified one too"
      do! lacks state [ "hash"; "Stdlib.List.head" ] "Not found" "the name resolves"
    })

let hashLongIsTheShortOneSpelledOut =
  instanceTest "hash --long extends the short hash" (fun state ->
    task {
      let! short = runCli state [ "hash"; "Stdlib.List.head" ]
      let! long = runCli state [ "hash"; "--long"; "Stdlib.List.head" ]
      let trim (s : string) = s.Replace("fn ", "").Trim()
      Expect.isTrue
        ((trim long).StartsWith(trim short))
        $"the short hash is a prefix of the long one, got short={short} long={long}"
    })

let hashOfAModuleSaysSo =
  instanceTest "hash of a module explains itself" (fun state ->
    task {
      do!
        shows
          state
          [ "hash"; "Stdlib.Option" ]
          "module"
          "a module is a path, not content"
      do!
        shows
          state
          [ "hash"; "Darklang.Nope.nope" ]
          "Not found"
          "and a missing name is missing"
    })

/// `dark nav X` moves for the length of that one command, like a shell's cwd inside a subshell.
/// The move has to SAY so, or the next `dark ls` disagreeing with it is how you find out.
let navIsHonestAboutHowLongItLasts =
  instanceTest "nav moves, and says how long the move lasts" (fun state ->
    task {
      do!
        showsAll
          state
          [ "nav"; "Darklang.Stdlib.List" ]
          [ "Changed to"; "Stdlib.List" ]
          "nav reports the move"
      do!
        shows
          state
          [ "nav"; "Darklang.Stdlib.List" ]
          "doesn't carry between"
          "and says it does not persist"
      do!
        shows
          state
          [ "nav"; "Darklang.Nope.Nowhere" ]
          "failed"
          "nav says so when the path isn't there"
      // Reading elsewhere without moving is what actually works from a shell.
      do!
        shows
          state
          [ "ls"; "Darklang.Stdlib.List" ]
          "Contents of"
          "ls reads anywhere by path"
    })


/// Regression, same family as `hash`: find-values split the type name itself.
let findValuesTakesTheSameNames =
  instanceTest "find-values takes the same names view takes" (fun state ->
    task {
      do!
        lacks
          state
          [ "find-values"; "Stdlib.Option.Option" ]
          "Type not found"
          "the type resolves un-owner-qualified"
      do!
        sane
          state
          [ "find-values"; "Darklang.Stdlib.Option.Option" ]
          "and fully qualified"
    })

let referenceCommandsAnswer =
  instanceTest "the reference commands answer" (fun state ->
    task {
      do! shows state [ "builtins" ] "Int:" "builtins lists them by module"
      do! shows state [ "commands" ] "nav" "commands lists the registry"
      do! shows state [ "docs" ] "for-ai" "docs lists its topics"
      do! sane state [ "docs"; "syntax" ] "and prints one"
      do!
        shows
          state
          [ "docs"; "nosuchtopic" ]
          "nosuchtopic"
          "an unknown topic names itself back"
    })


// ─── authoring ────────────────────────────────────────────────────────────

let authoringRoundTrips =
  instanceTest "what you author is what you read back" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Round.answer" "() : Int64 = 42L"
      do!
        shows
          state
          [ "view"; "Tests.Round.answer" ]
          "42"
          "view shows what was just authored"
      do! evals state "Tests.Round.answer ()" "42" "and it runs"
      do! shows state [ "hash"; "Tests.Round.answer" ] "fn " "and it has a hash"
      do! discardAll state
    })

let valuesRoundTrip =
  instanceTest "a value round-trips too" (fun state ->
    task {
      do! start state
      do! value state "Tests.Round.eleven" "11L"
      do! evals state "Tests.Round.eleven" "11" "the value is readable by name"
      do! discardAll state
    })

/// `delete` retires an item rather than unbinding the name: the log is append-only,
/// and something that called it yesterday still has to resolve. So the observable
/// effect is the deprecation badge, and `restore` takes it off again.
let deleteAndRestore =
  instanceTest "delete retires an item, restore brings it back" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Retire.f" "() : Int64 = 50505L"
      do! commit state "add f"
      do! evals state "Tests.Retire.f ()" "50505" "it runs before the delete"

      do!
        showsAnyCase
          state
          [ "delete"; "Tests.Retire.f"; "-y" ]
          "deprecated"
          "delete says what it did"
      do!
        showsAnyCase
          state
          [ "view"; "Tests.Retire.f" ]
          "deprecated"
          "the item is marked retired"
      do!
        evals
          state
          "Tests.Retire.f ()"
          "50505"
          "and it still runs, because callers still resolve it"

      // A Deprecate binds no name, so a `status` that counts only bindings calls this clean while
      // the op sits in the draft, waiting to be swept into the next commit.
      do!
        lacks
          state
          [ "status" ]
          "clean"
          "an uncommitted deprecation is not a clean tree"
      do!
        shows
          state
          [ "status" ]
          "no new version"
          "and status says what kind of op it is"

      do!
        showsAnyCase
          state
          [ "restore"; "Tests.Retire.f"; "-y" ]
          "undeprecated"
          "restore takes it back"
      do!
        lacksAnyCase
          state
          [ "view"; "Tests.Retire.f" ]
          "deprecated"
          "and the mark is gone"
      do! discardAll state
    })

let deleteRefusesWhatIsNotThere =
  instanceTest "delete refuses a name that holds nothing" (fun state ->
    task {
      do!
        refuses
          state
          [ "delete"; "Tests.Nope.nope"; "-y" ]
          "live item"
          "deleted"
          "delete says what it looked for"
    })

let deprecateAndUndeprecate =
  instanceTest "deprecate marks an item, undeprecate clears it" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Dep.old" "() : Int64 = 1L"
      do! commit state "add old"

      do! deprecate state "Tests.Dep.old"
      do!
        showsAnyCase
          state
          [ "view"; "Tests.Dep.old" ]
          "deprecated"
          "the badge shows on the item"
      do! evals state "Tests.Dep.old ()" "1" "a deprecated item still runs"

      do! run state [ "undeprecate"; "Tests.Dep.old" ]
      do!
        lacksAnyCase
          state
          [ "view"; "Tests.Dep.old" ]
          "deprecated"
          "and the badge goes away again"
      do! discardAll state
    })

/// Ocean's 3, and the shape it settled into. A doc comment is not behaviour, so editing one leaves
/// the item's hash alone -- which means the edit cannot ride on an `AddFn` (ops are
/// content-addressed, so that op IS the earlier one and folds to nothing) and rides on an `UpdateDoc`
/// instead.
let aDocOnlyEditKeepsTheVersionAndStillLands =
  instanceTest
    "editing only the docs changes the docs and nothing else"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Docs.f" "() : Int64 = 7L"
        do! fn state "Tests.Docs.caller" "() : Int64 = Tests.Docs.f () + 1L"
        do! commit state "add f and its caller"

        let! before = runCliPlain state [ "hash"; "Tests.Docs.f" ]

        do! fn state "Tests.Docs.f" "/// Returns seven.\nlet f (): Int64 =\n  7L"

        let! after = runCliPlain state [ "hash"; "Tests.Docs.f" ]
        Expect.equal
          after
          before
          $"the version did not move, got {after} from {before}"

        do!
          shows
            state
            [ "view"; "Tests.Docs.f" ]
            "Returns seven"
            "and the new text is what you read"
        do! shows state [ "ops" ] "UpdateDoc" "the op log says what happened"
        do! dirty state "an uncommitted doc edit is not a clean tree"
        do! evals state "Tests.Docs.caller ()" "8" "its caller is untouched"

        do! commit state "document f"
        do!
          shows
            state
            [ "view"; "Tests.Docs.f" ]
            "Returns seven"
            "and it survives the commit"
        do! discardAll state
      })

/// The other half: a doc edit made on a branch is the branch's opinion until it merges.
let aBranchesDocEditStaysOnTheBranch =
  instanceTest
    "a doc edit on a branch is invisible to main until it merges"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.DocsBr.f" "() : Int64 = 3L"
        do! fn state "Tests.DocsBr.f" "/// Main's wording.\nlet f (): Int64 =\n  3L"
        do! commit state "add f, documented"

        do! switch state "docsbr"
        do!
          fn
            state
            "Tests.DocsBr.f"
            "/// The branch's wording.\nlet f (): Int64 =\n  3L"
        do!
          shows
            state
            [ "view"; "Tests.DocsBr.f" ]
            "branch's wording"
            "the branch reads its own"
        do! commit state "reword"

        do! onMain state
        do!
          shows
            state
            [ "view"; "Tests.DocsBr.f" ]
            "Main's wording"
            "main still reads main's"
        do!
          lacks
            state
            [ "view"; "Tests.DocsBr.f" ]
            "branch's wording"
            "and not the branch's"

        do! merge state "docsbr"
        do!
          shows
            state
            [ "view"; "Tests.DocsBr.f" ]
            "branch's wording"
            "the merge brings it over"
        do! discardAll state
      })

/// The nested targets, which are the ones that used to be dropped on the floor.
///
/// A record field's, an enum case's and a parameter's doc are all outside the identity hash, so an
/// edit to one alone leaves the item's version exactly where it was -- and before `UpdateDoc` there
/// was no op that could carry it, so the CLI said "unchanged: nothing saved" and the words went
/// nowhere. Each is asserted separately because each reaches a different part of the declaration.
let aFieldsDocEditLands =
  instanceTest "editing only a record field's doc saves it" (fun state ->
    task {
      do! start state
      do!
        run
          state
          [ "type"
            "Tests.FieldDocs.Coord"
            "{\n  /// how far along\n  alongwards: Int64\n}" ]
      do! commit state "add Coord"

      let! before = runCliPlain state [ "hash"; "Tests.FieldDocs.Coord" ]

      do!
        run
          state
          [ "type"
            "Tests.FieldDocs.Coord"
            "{\n  /// distance along the axis\n  alongwards: Int64\n}" ]

      let! after = runCliPlain state [ "hash"; "Tests.FieldDocs.Coord" ]
      Expect.equal
        after
        before
        $"the version did not move, got {after} from {before}"

      do!
        shows
          state
          [ "view"; "Tests.FieldDocs.Coord" ]
          "distance along the axis"
          "the field's new wording is what you read"
      do!
        shows
          state
          [ "ops" ]
          "UpdateDoc Tests.FieldDocs.Coord field"
          "and the op names the NAME and WHICH part of it"
      do! dirty state "an uncommitted field-doc edit is not a clean tree"
      do! commit state "reword the field"
      do!
        shows
          state
          [ "view"; "Tests.FieldDocs.Coord" ]
          "distance along the axis"
          "and it survives the commit"
      do! discardAll state
    })

let anEnumCasesDocEditLands =
  instanceTest "editing only an enum case's doc saves it" (fun state ->
    task {
      do! start state
      do!
        run
          state
          [ "type"
            "Tests.CaseDocs.Signal"
            "| /// stop here\n  Halting\n| /// carry on\n  Proceeding" ]
      do! commit state "add Signal"

      let! before = runCliPlain state [ "hash"; "Tests.CaseDocs.Signal" ]

      do!
        run
          state
          [ "type"
            "Tests.CaseDocs.Signal"
            "| /// come to a full stop\n  Halting\n| /// carry on\n  Proceeding" ]

      let! after = runCliPlain state [ "hash"; "Tests.CaseDocs.Signal" ]
      Expect.equal
        after
        before
        $"the version did not move, got {after} from {before}"

      do!
        shows
          state
          [ "view"; "Tests.CaseDocs.Signal" ]
          "come to a full stop"
          "the case's new wording is what you read"
      do!
        shows
          state
          [ "ops" ]
          "UpdateDoc Tests.CaseDocs.Signal case"
          "and the op names the NAME and WHICH case"
      do! discardAll state
    })

/// A parameter's doc, which is the one addressed by POSITION rather than by name: a parameter name
/// is not in the identity hash, so two functions differing only in their parameter names are one
/// item and a name would not identify anything.
let aParametersDocEditLands =
  instanceTest "editing only a parameter's doc saves it" (fun state ->
    task {
      do! start state
      do!
        fn
          state
          "Tests.ParamDocs.scale"
          "/// scales\nlet scale (/// the multiplier\n           factorly: Int64) : Int64 =\n  factorly"
      do! commit state "add scale"

      let! before = runCliPlain state [ "hash"; "Tests.ParamDocs.scale" ]

      do!
        fn
          state
          "Tests.ParamDocs.scale"
          "/// scales\nlet scale (/// what to multiply by\n           factorly: Int64) : Int64 =\n  factorly"

      let! after = runCliPlain state [ "hash"; "Tests.ParamDocs.scale" ]
      Expect.equal
        after
        before
        $"the version did not move, got {after} from {before}"

      do!
        shows
          state
          [ "view"; "Tests.ParamDocs.scale" ]
          "what to multiply by"
          "the parameter's new wording is what you read"
      do!
        shows
          state
          [ "ops" ]
          "parameter 1"
          "and the op names the parameter by position, since its name is not identity"
      do! discardAll state
    })


/// Two people writing different prose for one doc, neither having seen the other's.
///
/// `previous` is what makes this detectable: an incoming doc op that names a text this store never
/// held was not written on top of what we hold. The newer text still wins, so the store stays
/// usable, and the record is what keeps the loser's words findable instead of gone.
///
/// The op is authored here rather than synced because the sync gates cover the transport; what
/// needs asserting is the FOLD's answer, which is the same either way.
/// The reason docs are scoped to a NAME rather than to content.
///
/// Ten `ParseError` types in the stdlib are literally one item: same declaration, so one hash. What
/// `Int64.ParseError` means is not what `UInt64.ParseError` means, so one doc for the item cannot
/// be right. A doc edit at one name must not be visible at the other.
let twoNamesHoldingOneItemHaveTheirOwnDocs =
  instanceTest "two names holding one item document it separately" (fun state ->
    task {
      do! start state
      do! fn state "Tests.DocOne.f" "() : Int64 = 5301L"
      do! fn state "Tests.DocTwo.f" "() : Int64 = 5301L"
      do! commit state "one body, two names"

      // Same body, so ONE item: this is the premise, not an accident of the fixture.
      let! one = runCliPlain state [ "hash"; "Tests.DocOne.f" ]
      let! two = runCliPlain state [ "hash"; "Tests.DocTwo.f" ]
      Expect.equal two one $"the two names hold one item, got {one} and {two}"

      do!
        fn
          state
          "Tests.DocOne.f"
          "/// what the first name means\nlet f (): Int64 =\n  5301L"

      do!
        shows
          state
          [ "view"; "Tests.DocOne.f" ]
          "what the first name means"
          "the name that was edited says the new thing"
      do!
        lacks
          state
          [ "view"; "Tests.DocTwo.f" ]
          "what the first name means"
          "and the other name holding the same item does not"

      do! commit state "document the first name"
      do!
        lacks
          state
          [ "view"; "Tests.DocTwo.f" ]
          "what the first name means"
          "still not, once committed"
      do! discardAll state
    })


let twoWordingsForOneDocRecordAConflict =
  instanceTest
    "a doc edit made against a text this store never had is a conflict"
    (fun state ->
      task {
        do! start state
        do!
          fn
            state
            "Tests.DocClash.f"
            "/// ours, written here\nlet f (): Int64 =\n  5101L"
        do! commit state "add f, documented"

        // What a peer's op looks like on arrival: it names a predecessor nobody here ever wrote.
        let theirs =
          "Darklang.SCM.PackageOps.add (Builtin.scmCurrentBranch ()) "
          + "[Darklang.LanguageTools.ProgramTypes.PackageOp.UpdateDoc("
          + "Darklang.LanguageTools.ProgramTypes.PackageLocation "
          + "{ owner = \"Tests\"; modules = [\"DocClash\"]; name = \"f\" }, "
          + "Darklang.LanguageTools.ProgramTypes.DocPart.WholeItem, "
          + "\"theirs, written somewhere else\", "
          + "Stdlib.Option.Option.Some (Darklang.LanguageTools.ProgramTypes.hashOfText "
          + "\"a text this store never had\"), Stdlib.Option.Option.None)]"

        do! run state [ "eval"; theirs ]

        do!
          shows
            state
            [ "view"; "Tests.DocClash.f" ]
            "theirs, written somewhere else"
            "the newer wording wins, so the store stays usable"
        do!
          showsAll
            state
            [ "conflicts" ]
            [ "Tests.DocClash.f"; "neither made from the other" ]
            "and the divergence is recorded rather than swallowed"

        // A doc divergence has no side to take -- both candidates are TEXT -- so `override` says so
        // instead of trying to bind a name to the hash of a sentence.
        // THIS conflict's id, not the first one listed: every CLI test shares one store, and the
        // doc tests above leave their own divergences in it.
        let! listed = runCliPlain state [ "conflicts" ]

        let id =
          listed.Split('\n')
          |> Array.filter (fun line -> line.Contains "Tests.DocClash.f")
          |> Array.tryHead
          |> Option.defaultWith (fun () ->
            Tests.failtestf
              "no conflict listed for Tests.DocClash.f, got: %s"
              listed)
          |> fun line -> line.Split('#') |> Array.item 1
          |> fun rest -> rest.Split(' ') |> Array.head

        // The review screen shows the two SENTENCES. A hash of a sentence tells a reader nothing
        // they can choose between, and choosing is the point of the screen.
        do!
          showsAll
            state
            [ "conflicts"; "show"; id ]
            [ "two wordings"
              "ours, written here"
              "theirs, written somewhere else" ]
            "both wordings are on the screen you choose from"

        // Taking a side SAYS the chosen wording: it authors the op, so the choice syncs and holds.
        do!
          shows
            state
            [ "conflicts"; "override"; id; "A" ]
            "now says"
            "override takes one of the two wordings"
        do!
          shows
            state
            [ "view"; "Tests.DocClash.f" ]
            "ours, written here"
            "and the name says what was chosen"

        // Settled, and ASSERTED settled: this test deliberately puts a divergence in a store every
        // other CLI test shares, so leaving one behind is leaving a landmine.
        do! lacks state [ "conflicts" ] id "and it is gone from the pending list"
        do! discardAll state
      })


/// Writing a declaration without a `///` does not wipe the doc that is there.
///
/// Content is shared: two functions with the same body are ONE item, and a doc is said about the
/// item. So omitting a doc comment cannot mean "nobody's words apply any more" -- it means this
/// author did not write any.
let authoringWithoutADocDoesNotClearOne =
  instanceTest
    "saving a declaration with no doc comment leaves the existing one alone"
    (fun state ->
      task {
        do! start state
        do!
          fn
            state
            "Tests.DocKeep.f"
            "/// what it is for\nlet f (): Int64 =\n  5201L"
        do! commit state "add f, documented"

        do! fn state "Tests.DocKeep.f" "() : Int64 = 5201L"
        do!
          shows
            state
            [ "view"; "Tests.DocKeep.f" ]
            "what it is for"
            "the doc survives a save that did not mention it"
        do! discardAll state
      })


/// Re-authoring the same source before committing, which is what an editor's save button does.
///
/// A rebind to the hash a name already holds folds to nothing -- no new `locations` row -- so the
/// draft ends with two namings and only the FIRST wrote anything. Collapse keeps one naming per
/// name at commit, and keeping the wrong one of those two deleted the only row: the commit
/// reported success and the function stopped existing.
let reAuthoringTheSameSourceSurvivesTheCommit =
  instanceTest
    "authoring the same source twice, then committing, keeps the name"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Twice.f" "() : Int64 = 4901L"
        do! fn state "Tests.Twice.f" "() : Int64 = 4901L"
        do! commit state "add f, said twice"

        do!
          shows
            state
            [ "view"; "Tests.Twice.f" ]
            "4901"
            "the name still holds what was authored"
        do! evals state "Tests.Twice.f ()" "4901" "and it still runs"
        do! discardAll state
      })

/// The other half of the same rule: when the hash really does move and move back inside one draft,
/// the LAST naming is the one that wrote the live row, and it is the one to keep.
let aVersionMovedAndMovedBackKeepsTheLastNaming =
  instanceTest
    "editing a function and putting it back, then committing, keeps the version put back"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.ThereAndBack.f" "() : Int64 = 4902L"
        do! fn state "Tests.ThereAndBack.f" "() : Int64 = 4903L"
        do! fn state "Tests.ThereAndBack.f" "() : Int64 = 4902L"
        do! commit state "there and back"

        do!
          evals
            state
            "Tests.ThereAndBack.f ()"
            "4902"
            "the name holds the version it was put back to"
        do! discardAll state
      })


/// `remove` is the only thing that writes an `Unbind`, and the claim it prints is the
/// interesting one: the name ends, the content does not, so callers go on working. That is only
/// true because a reference points at content rather than at a name, which is the property worth
/// a test rather than a comment.
/// `grep` searches BODIES, which is the half `search` does not do. Scoped to one module on
/// purpose: unscoped it renders every live item, and the point of the scope argument is that a
/// test, like a person, usually knows roughly where to look.
let grepFindsSourceAndNotJustNames =
  instanceTest
    "grep matches a function body, and reports name and line"
    (fun state ->
      task {
        do! start state
        // A string LITERAL, not a comment: grep reads rendered source, and the renderer prints
        // the AST, which keeps `///` docs and drops `//` asides.
        do! fn state "Tests.Grep.needle" "() : String = \"findmeplease\""
        do! fn state "Tests.Grep.other" "() : String = \"something else\""
        do! commit state "grep fixture"

        do!
          shows
            state
            [ "grep"; "findmeplease"; "Tests.Grep" ]
            "Tests.Grep.needle:"
            "the hit names the item"
        do!
          shows
            state
            [ "grep"; "findmeplease"; "Tests.Grep" ]
            "findmeplease"
            "and shows the matching line"
        do!
          lacks
            state
            [ "grep"; "findmeplease"; "Tests.Grep" ]
            "Tests.Grep.other"
            "an item whose body does not match is not reported"
        do! discardAll state
      })

/// The cache is the feature, so the second run has to agree with the first. Keyed by content
/// hash, which is why it never needs invalidating.
let grepAgreesWithItselfOnceCached =
  instanceTest "a second grep, reading the cache, finds the same thing" (fun state ->
    task {
      do! start state
      do! fn state "Tests.GrepTwice.f" "() : String = \"cachedtoken\""
      do! commit state "grep cache fixture"

      do!
        shows
          state
          [ "grep"; "cachedtoken"; "Tests.GrepTwice" ]
          "Tests.GrepTwice.f:"
          "cold"
      do!
        shows
          state
          [ "grep"; "cachedtoken"; "Tests.GrepTwice" ]
          "Tests.GrepTwice.f:"
          "warm"
      do! discardAll state
    })

/// A search tool you cannot trust a negative answer from is worse than none, so "no hits" and
/// "could not read it" must not look alike.
/// The bug the SCM silo's guard exists to stop, in grep's shape: `locations` is main's
/// projection, so an enumeration that read it directly would search MAIN from a branch and report
/// nothing wrong. This is the test that the overlay is actually consulted.
let grepSeesTheBranchYouAreStandingOn =
  instanceTest "grep finds a branch's own work, and main does not" (fun state ->
    task {
      do! start state
      do! switch state "grep-branch"
      do! fn state "Tests.GrepBranch.only" "() : String = \"branchonlytoken\""

      do!
        shows
          state
          [ "grep"; "branchonlytoken"; "Tests.GrepBranch" ]
          "Tests.GrepBranch.only:"
          "the branch sees its own work"

      do! onMain state
      do!
        shows
          state
          [ "grep"; "branchonlytoken"; "Tests.GrepBranch" ]
          "no live item's source contains"
          "main does not"

      do! archiveBranches state [ "grep-branch" ]
    })

let grepSaysWhenItFindsNothing =
  instanceTest "grep says so when nothing matches" (fun state ->
    task {
      do! start state
      do!
        shows
          state
          [ "grep"; "zzz-not-in-any-source-zzz"; "Darklang.Stdlib.Option" ]
          "no live item's source contains"
          "an honest empty answer"
      do! shows state [ "grep" ] "usage: dark grep" "bare prints usage"
    })

let removeEndsTheNameAndLeavesTheCallersAlone =
  instanceTest "remove ends a name, and what called it still runs" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Rm.leaf" "() : Int64 = 4242L"
      do! fn state "Tests.Rm.caller" "() : Int64 = Tests.Rm.leaf ()"
      do! commit state "add leaf and caller"

      do! evals state "Tests.Rm.caller ()" "4242" "the caller works to begin with"

      do! run state [ "remove"; "Tests.Rm.leaf"; "-y" ]

      do!
        shows
          state
          [ "view"; "Tests.Rm.leaf" ]
          "Not found"
          "the name holds nothing now"
      do!
        evals
          state
          "Tests.Rm.caller ()"
          "4242"
          "but the caller still runs, because it references content and not a name"
      do! discardAll state
    })

/// Bare and wrong-argument shapes, which is where this kind of command goes wrong: a confirming
/// verb that cannot find its target must refuse rather than ask about nothing.
let removeRefusesWhatIsNotThere =
  instanceTest
    "remove refuses a name that holds nothing, and refuses to run bare"
    (fun state ->
      task {
        do! start state
        do!
          shows
            state
            [ "remove"; "Tests.Rm.nothingHere"; "-y" ]
            "nothing here is named"
            "an unknown name is named back"
        do! shows state [ "remove" ] "usage: dark remove" "bare prints usage"
      })

let renameIsVisibleToEverythingThatReads =
  instanceTest
    "a renamed item is readable at its new name, by every reader"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Ren.before" "() : Int64 = 3L"
        do! commit state "add before"
        do! run state [ "rename"; "Tests.Ren.before"; "Tests.Ren.after" ]

        do!
          shows
            state
            [ "view"; "Tests.Ren.after" ]
            "3"
            "view finds it at the new name"
        do! shows state [ "hash"; "Tests.Ren.after" ] "fn " "hash does too"
        do! evals state "Tests.Ren.after ()" "3" "and it runs there"
        do!
          shows
            state
            [ "view"; "Tests.Ren.before" ]
            "Not found"
            "the old name holds nothing"
        do! discardAll state
      })


let tests : List<Test> =
  [ lsNamesWhatIsThere
    treeShowsDescendants
    viewPrintsSource
    viewRefusesWhatIsNotThere
    searchFindsByText
    depsNamesWhatAnItemUses
    hashResolvesNamesLikeViewDoes
    hashLongIsTheShortOneSpelledOut
    hashOfAModuleSaysSo
    navIsHonestAboutHowLongItLasts
    findValuesTakesTheSameNames
    referenceCommandsAnswer
    authoringRoundTrips
    valuesRoundTrip
    deleteAndRestore
    deleteRefusesWhatIsNotThere
    deprecateAndUndeprecate
    renameIsVisibleToEverythingThatReads
    removeEndsTheNameAndLeavesTheCallersAlone
    removeRefusesWhatIsNotThere
    grepFindsSourceAndNotJustNames
    grepAgreesWithItselfOnceCached
    grepSaysWhenItFindsNothing
    grepSeesTheBranchYouAreStandingOn
    aDocOnlyEditKeepsTheVersionAndStillLands
    aFieldsDocEditLands
    anEnumCasesDocEditLands
    aParametersDocEditLands
    aBranchesDocEditStaysOnTheBranch
    reAuthoringTheSameSourceSurvivesTheCommit
    aVersionMovedAndMovedBackKeepsTheLastNaming
    twoNamesHoldingOneItemHaveTheirOwnDocs
    twoWordingsForOneDocRecordAConflict
    authoringWithoutADocDoesNotClearOne ]
