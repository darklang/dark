/// The `--json` outputs, as a CONTRACT rather than as whatever the renderer happens to emit.
///
/// These are the agent-facing surface: `dark status --json` is what something reads to decide what
/// to do next, and it has no eyes to notice that a field was renamed or that an empty listing
/// stopped being an empty array and became an absent key. Nothing asserted the SHAPE of any of them
/// before this file; the pretty output had tests and the machine-readable output did not.
///
/// Three claims per command, and the third is the one that breaks agents: it parses, its documented
/// keys are all there, and they are STILL there when the thing it lists is empty. A caller reading
/// `.changed` gets `[]` on a clean tree, not a missing key it has to guard.
///
/// The key sets here are golden on purpose. Renaming a field is allowed; doing it without noticing
/// is what this stops.
module Tests.CliJson

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

module RT = LibExecution.RuntimeTypes

open Tests.CliTestHarness
open Tests.CliDsl


/// The command's output, parsed. The failure names the command and shows what it printed, because a
/// parse failure here is usually prose that leaked onto stdout ahead of the payload.
let private parsed
  (state : Target)
  (args : List<string>)
  : Task<System.Text.Json.JsonElement> =
  task {
    let! out = runCliPlain state args
    // The payload is the LAST line: some commands print a heading first when they are not in
    // --json mode, and a regression that reintroduces one should fail on the keys, not here.
    let line = out.Trim().Split('\n') |> Array.last

    try
      let doc = System.Text.Json.JsonDocument.Parse(line)
      return doc.RootElement
    with e ->
      return
        Tests.failtestf
          "`dark %s` did not print JSON: %s\nfull output: %s"
          (String.concat " " args)
          e.Message
          out
  }

/// Exactly these keys, no more and no fewer. Extra keys are as much a change as missing ones: a
/// caller that switches on the shape sees a new one as an unknown variant.
let private hasKeys
  (state : Target)
  (args : List<string>)
  (expected : List<string>)
  : Task<unit> =
  task {
    let! root = parsed state args

    if root.ValueKind <> System.Text.Json.JsonValueKind.Object then
      Tests.failtestf
        "`dark %s` should answer with an object, got %A"
        (String.concat " " args)
        root.ValueKind

    let actual =
      root.EnumerateObject() |> Seq.map (fun p -> p.Name) |> List.ofSeq |> List.sort

    Expect.equal
      actual
      (List.sort expected)
      $"""`dark {String.concat " " args}` keys"""
  }

/// The payload is an array. `commits`, `branches`, `diff` and `traces list` are lists, and a list
/// that answers `{}` or `null` when it is empty is the same break as a missing key.
let private isArray (state : Target) (args : List<string>) : Task<unit> =
  task {
    let! root = parsed state args

    Expect.equal
      root.ValueKind
      System.Text.Json.JsonValueKind.Array
      $"""`dark {String.concat " " args}` should answer with an array"""
  }

/// Every element of an array payload carries these keys.
let private rowsHaveKeys
  (state : Target)
  (args : List<string>)
  (expected : List<string>)
  : Task<unit> =
  task {
    let! root = parsed state args
    let rows = root.EnumerateArray() |> List.ofSeq

    Expect.isNonEmpty
      rows
      $"""`dark {String.concat " " args}` needs at least one row for this claim"""

    for row in rows do
      let actual =
        row.EnumerateObject() |> Seq.map (fun p -> p.Name) |> List.ofSeq |> List.sort

      Expect.equal
        actual
        (List.sort expected)
        $"""`dark {String.concat " " args}` row keys"""
  }


// ─── the object-shaped answers ────────────────────────────────────────────

/// `status` is the one an agent reads most, and the one whose empty case matters most: on a clean
/// tree every collection here is empty, and every key still has to be present.
let statusKeepsItsShapeWhenNothingChanged =
  instanceTest "status --json keeps every key on a clean tree" (fun state ->
    task {
      do! start state
      do!
        hasKeys
          state
          [ "status"; "--json" ]
          [ "branch"
            "changed"
            "conflicts"
            "constraints"
            "draftOps"
            "propagates"
            "removed" ]

      let! root = parsed state [ "status"; "--json" ]

      for name in [ "changed"; "conflicts"; "propagates"; "removed" ] do
        Expect.equal
          (root.GetProperty(name).ValueKind)
          System.Text.Json.JsonValueKind.Array
          $"status --json: `{name}` is an empty ARRAY on a clean tree, not absent or null"
    })

/// And with something in it, because "the keys are there" is easy to satisfy by accident and
/// "`changed` describes the change" is not.
let statusDescribesADraft =
  instanceTest "status --json describes what is in the draft" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Json.f" "() : Int64 = 7201L"

      let! root = parsed state [ "status"; "--json" ]
      let changed = root.GetProperty("changed").EnumerateArray() |> List.ofSeq

      Expect.isNonEmpty changed "status --json lists the authored item"

      let row : System.Text.Json.JsonElement = changed[0]

      let keys =
        row.EnumerateObject()
        |> Seq.map (fun p -> p.Name)
        |> List.ofSeq
        |> List.sort

      Expect.equal
        keys
        (List.sort [ "change"; "followed"; "hash"; "kind"; "name" ])
        "status --json changed-row keys"

      Expect.isTrue
        (root.GetProperty("draftOps").GetInt32() > 0)
        "status --json counts the draft's ops"

      do! discardAll state
    })

let conflictsKeepsItsShapeWhenThereAreNone =
  instanceTest
    "conflicts --json keeps its keys when nothing is pending"
    (fun state ->
      task {
        do! start state
        do! hasKeys state [ "conflicts"; "--json" ] [ "conflicts"; "pending" ]
      })

/// `findings: []` means "nothing is wrong" only when nothing stopped a detector from looking.
let constraintsKeepsItsShape =
  instanceTest "constraints --json keeps its keys" (fun state ->
    task {
      do! hasKeys state [ "constraints"; "--json" ] [ "findings"; "blocked" ]

      let! root = parsed state [ "constraints"; "--json" ]

      Expect.equal
        (root.GetProperty("blocked").ValueKind)
        System.Text.Json.JsonValueKind.Array
        "blocked is an array, empty in the normal case rather than absent"
    })

let depsAnswersBothDirections =
  instanceTest "deps --json answers with both directions" (fun state ->
    task {
      do! start state
      do! fn state "Tests.JsonDeps.leaf" "() : Int64 = 7202L"
      do! fn state "Tests.JsonDeps.caller" "() : Int64 = Tests.JsonDeps.leaf ()"
      do! commit state "json deps fixture"

      do!
        hasKeys
          state
          [ "deps"; "Tests.JsonDeps.leaf"; "--json" ]
          [ "dependencies"; "dependents"; "item" ]

      let! root = parsed state [ "deps"; "Tests.JsonDeps.leaf"; "--json" ]

      // A leaf with one caller: `dependencies` is the empty ARRAY, and that is the case a caller
      // reading `.dependencies` has to be able to trust.
      Expect.equal
        (root.GetProperty("dependencies").ValueKind)
        System.Text.Json.JsonValueKind.Array
        "deps --json: `dependencies` is an array even when the item calls nothing"

      Expect.isNonEmpty
        (root.GetProperty("dependents").EnumerateArray() |> List.ofSeq)
        "deps --json lists the caller"

      do! discardAll state
    })

let searchAnswersWithItsQuery =
  instanceTest "search --json echoes the query beside the results" (fun state ->
    task {
      do!
        hasKeys
          state
          [ "search"; "Tests.JsonDeps"; "--json" ]
          [ "query"; "results" ]

      // Including when nothing matches, which is where a renderer is most tempted to print
      // "no results" and nothing else.
      do!
        hasKeys
          state
          [ "search"; "zzz-nothing-matches-this"; "--json" ]
          [ "query"; "results" ]
    })

/// `commit --json` with no `-y` is the documented DRY review: an agent runs it to see what would
/// happen before deciding. So its refusals are data, not prose -- there must be nothing on stdout
/// but the payload.
let commitDryRunAnswersInJson =
  instanceTest
    "commit --json reviews without committing, and says so in the payload"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Json.g" "() : Int64 = 7203L"

        do!
          hasKeys
            state
            [ "commit"; "--json" ]
            [ "branch"
              "changed"
              "commit"
              "committed"
              "conflicts"
              "dependentsRepointed"
              "message"
              "opsCommitted"
              "propagates"
              "refused"
              "removed" ]

        let! root = parsed state [ "commit"; "--json" ]

        Expect.isFalse
          (root.GetProperty("committed").GetBoolean())
          "commit --json without -y does not commit"

        do! dirty state "the draft is still there after a dry review"
        do! discardAll state
      })


// ─── the array-shaped answers ─────────────────────────────────────────────

let commitsIsAnArrayOfCommits =
  instanceTest "commits --json is an array, with the documented row" (fun state ->
    task {
      do! start state
      do! fn state "Tests.Json.h" "() : Int64 = 7204L"
      do! commit state "json commits fixture"

      do! isArray state [ "commits"; "--json" ]
      do!
        rowsHaveKeys
          state
          [ "commits"; "--json" ]
          [ "author"; "hash"; "message"; "ops" ]
    })

let branchesIsAnArrayOfBranches =
  instanceTest "branches --json is an array, with the documented row" (fun state ->
    task {
      do! start state
      do! switch state "jsonbr"
      do! onMain state

      do! isArray state [ "branches"; "--json" ]
      do!
        rowsHaveKeys
          state
          [ "branches"; "--json" ]
          [ "current"; "id"; "merged"; "name"; "ops"; "parent"; "parentName" ]
    })

/// An empty listing is still a listing. `traces list` on a store with no traces, and `diff` against
/// a branch that has done nothing, both have to answer `[]`.
let emptyListingsAreStillArrays =
  instanceTest
    "an empty listing answers with an empty array, not with prose"
    (fun state ->
      task {
        do! start state
        do! switch state "jsonempty"
        do! onMain state

        do! isArray state [ "diff"; "jsonempty"; "--json" ]
        do! isArray state [ "traces"; "list"; "--json" ]
      })


/// `typecheck --json` is the at-rest audit, which is the one an agent reads before deciding a branch
/// is safe to merge. Its five counters and its verdict are the whole contract; `items` carries the
/// per-item rows, and stays an array when the audit found nothing to complain about.
/// A module scopes the audit to that subtree, which is what keeps the contract test above cheap.
/// Compares a narrow scope against a wider one, not against the whole branch: auditing the branch
/// here would reintroduce the cost the scoping exists to avoid.
let typecheckScopesToAModule =
  instanceTest
    "typecheck <module> audits that module rather than everything"
    (fun state ->
      task {
        do! start state

        let total (root : System.Text.Json.JsonElement) : int =
          root.GetProperty("total").GetInt32()

        let! narrow = parsed state [ "typecheck"; "Darklang.Stdlib.List"; "--json" ]
        let! wider = parsed state [ "typecheck"; "Darklang.Stdlib"; "--json" ]

        Expect.isGreaterThan
          (total narrow)
          0
          "a real module has declarations to audit"
        Expect.isLessThan
          (total narrow)
          (total wider)
          "a narrower scope audits fewer declarations than the module containing it"

        // `Darklang.Stdlib` is checked in pieces across the cores. Every declaration under it
        // comes back, whichever piece it was in: counted here by the search, not by the checker.
        let! declared =
          runCliPlain
            state
            [ "eval"
              String.concat
                "\n"
                [ "let (_, types, values, fns, _, impls) ="
                  "  Darklang.LanguageTools.PackageManager.Search.searchNamesAndHashes"
                  "    Darklang.SCM.Branch.mainBranchId"
                  "    (Darklang.LanguageTools.ProgramTypes.Search.SearchQuery"
                  "      { currentModule = [ \"Darklang\", \"Stdlib\" ]"
                  "        text = \"\""
                  "        searchDepth = Darklang.LanguageTools.ProgramTypes.Search.SearchDepth.AllDescendants"
                  "        entityTypes = []"
                  "        exactMatch = false })"
                  "let distinct (found: List<(String * Darklang.LanguageTools.ProgramTypes.Hash)>) : Int ="
                  "  Stdlib.List.length (Stdlib.List.unique (Stdlib.List.map found (fun (_, h) -> h)))"
                  "(distinct types) + (distinct values) + (distinct fns) + (distinct impls)" ] ]
        Expect.equal
          (string (total wider))
          (declared.Trim().Split('\n') |> Array.last)
          "every declaration under the module is audited"

        // A module nobody has defined is not an error, it is an empty audit. A caller scoping to
        // a name it got wrong should see zero rather than a refusal it has to special-case.
        let! missing =
          parsed state [ "typecheck"; "Darklang.NoSuchModule"; "--json" ]
        Expect.equal (total missing) 0 "an unknown module audits nothing"
      })


let typecheckAnswersWithItsCounts =
  instanceTest "typecheck --json answers with the audit's counts" (fun state ->
    task {
      do! start state

      do!
        hasKeys
          state
          [ "typecheck"; "Darklang.Stdlib.List"; "--json" ]
          [ "verdict"; "checked"; "failed"; "incomplete"; "total"; "items" ]

      let! root = parsed state [ "typecheck"; "Darklang.Stdlib.List"; "--json" ]

      // The one field a caller branches on. Anything outside these three is a new variant, and a
      // caller switching on it would fall through.
      let verdict = root.GetProperty("verdict").GetString()
      Expect.contains
        [ "checked"; "failed"; "incomplete" ]
        verdict
        "verdict is one of the three the renderer can produce"

      Expect.equal
        (root.GetProperty("items").ValueKind)
        System.Text.Json.JsonValueKind.Array
        "items is an array even when the audit is clean"
    })


/// At save, a name that resolves to nothing is "not decided yet": a caller may be written before
/// its callee. In an audit of what is stored it means the item calls something that is not
/// there, so the audit fails it rather than calling it incomplete.
let typecheckFailsACallToNothing =
  instanceTest
    "typecheck fails a stored item that calls a function which does not exist"
    (fun state ->
      task {
        do! start state
        do!
          fn
            state
            "Tests.TcGone.callsGone"
            "(n: Int64) : String = Stdlib.Int.toString n"

        let! root = parsed state [ "typecheck"; "Tests.TcGone"; "--json" ]
        Expect.equal
          (root.GetProperty("failed").GetInt32())
          1
          "the call to nothing is failed"
        Expect.equal
          (root.GetProperty("incomplete").GetInt32())
          0
          "and not reported as incomplete"

        do!
          exits
            state
            [ "typecheck"; "Tests.TcGone" ]
            1L
            "an audit that fails exits non-zero"
        do! discardAll state
      })


// ─── the ones that do NOT take --json ─────────────────────────────────────

/// A command that does not answer in JSON has to SAY so and exit nonzero, rather than printing its
/// pretty output and letting a caller parse prose as a payload. `ops` is the one that came up: it
/// takes no flags at all.
let aCommandWithoutJsonRefusesTheFlag =
  instanceTest
    "a command with no --json says so instead of printing prose"
    (fun state ->
      task {
        do!
          refuses
            state
            [ "ops"; "--json" ]
            "doesn't understand --json"
            "{"
            "`ops` has no JSON form and says so"
      })


/// An implementation is one result. Its method fn lives one level under it and the path it lives at
/// reads as a module, so without collapsing them `search %` listed every `Modulo` implementation
/// three times: as an impl, as a fn and as a module.
let searchListsAnImplementationOnce =
  instanceTest
    "search lists an implementation once, not as its method fn and module too"
    (fun state ->
      task {
        let! root = parsed state [ "search"; "%"; "--json" ]
        let rows = root.GetProperty("results").EnumerateArray() |> Seq.toList
        let named (kind : string) =
          rows
          |> List.filter (fun r -> r.GetProperty("kind").GetString() = kind)
          |> List.map (fun r -> r.GetProperty("name").GetString())
        let impls = named "impl"

        Expect.isNonEmpty impls "search % finds the Modulo implementations"
        let underAnImpl (name : string) =
          impls |> List.exists (fun i -> name = i || name.StartsWith(i + "."))
        Expect.isEmpty
          (named "fn" |> List.filter underAnImpl)
          "an implementation's method fn is not listed beside it"
        Expect.isEmpty
          (named "module" |> List.filter underAnImpl)
          "an implementation's path is not listed as a module"
      })


/// Each of these runs the CLI as a CHILD, against a store of its own, so this list is not
/// in `CliTraces`'s sequenced pile and does not need to be. Nothing here reaches into F#;
/// every claim is about what a command printed.
let tests : List<Test> =
  [ statusKeepsItsShapeWhenNothingChanged
    statusDescribesADraft
    conflictsKeepsItsShapeWhenThereAreNone
    constraintsKeepsItsShape
    depsAnswersBothDirections
    searchAnswersWithItsQuery
    searchListsAnImplementationOnce
    commitDryRunAnswersInJson
    commitsIsAnArrayOfCommits
    branchesIsAnArrayOfBranches
    typecheckScopesToAModule
    typecheckAnswersWithItsCounts
    typecheckFailsACallToNothing
    emptyListingsAreStillArrays
    aCommandWithoutJsonRefusesTheFlag ]
