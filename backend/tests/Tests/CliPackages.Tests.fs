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
  cliTest "ls names what a module holds" (fun state ->
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
  cliTest "tree shows a module's descendants" (fun state ->
    task {
      do!
        showsAll
          state
          [ "tree"; "Darklang.Stdlib.Option" ]
          [ "Package tree"; "Option" ]
          "tree roots at what it was given"
    })

let viewPrintsSource =
  cliTest "view prints an item's source" (fun state ->
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
  cliTest "view refuses a name that holds nothing" (fun state ->
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
  cliTest "search finds items by text" (fun state ->
    task {
      do!
        showsAll
          state
          [ "search"; "head" ]
          [ "Search results for"; "head" ]
          "search reports what it matched"
    })

let depsNamesWhatAnItemUses =
  cliTest "deps names what an item uses" (fun state ->
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
  cliTest "hash takes the same names view takes" (fun state ->
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
  cliTest "hash --long extends the short hash" (fun state ->
    task {
      let! short = runCli state [ "hash"; "Stdlib.List.head" ]
      let! long = runCli state [ "hash"; "--long"; "Stdlib.List.head" ]
      let trim (s : string) = s.Replace("fn ", "").Trim()
      Expect.isTrue
        ((trim long).StartsWith(trim short))
        $"the short hash is a prefix of the long one, got short={short} long={long}"
    })

let hashOfAModuleSaysSo =
  cliTest "hash of a module explains itself" (fun state ->
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
  cliTest "nav moves, and says how long the move lasts" (fun state ->
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
  cliTest "find-values takes the same names view takes" (fun state ->
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
  cliTest "the reference commands answer" (fun state ->
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
  cliTestOnMain "what you author is what you read back" (fun state ->
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
  cliTestOnMain "a value round-trips too" (fun state ->
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
  cliTestOnMain "delete retires an item, restore brings it back" (fun state ->
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
  cliTest "delete refuses a name that holds nothing" (fun state ->
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
  cliTestOnMain "deprecate marks an item, undeprecate clears it" (fun state ->
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
/// content-addressed, so that op IS the earlier one and folds to nothing) and rides on a `Describe`
/// instead.
let aDocOnlyEditKeepsTheVersionAndStillLands =
  cliTestOnMain
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
        do! shows state [ "ops" ] "Describe" "the op log says what happened"
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
  cliTestOnMain
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

let renameIsVisibleToEverythingThatReads =
  cliTestOnMain
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
    aDocOnlyEditKeepsTheVersionAndStillLands
    aBranchesDocEditStaysOnTheBranch ]
