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
  (state : RT.ExecutionState)
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
  (state : RT.ExecutionState)
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
let private isArray (state : RT.ExecutionState) (args : List<string>) : Task<unit> =
  task {
    let! root = parsed state args

    Expect.equal
      root.ValueKind
      System.Text.Json.JsonValueKind.Array
      $"""`dark {String.concat " " args}` should answer with an array"""
  }

/// Every element of an array payload carries these keys.
let private rowsHaveKeys
  (state : RT.ExecutionState)
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
  cliTestOnMain "status --json keeps every key on a clean tree" (fun state ->
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
            "leftBehind"
            "propagates"
            "removed" ]

      let! root = parsed state [ "status"; "--json" ]

      for name in [ "changed"; "conflicts"; "leftBehind"; "propagates"; "removed" ] do
        Expect.equal
          (root.GetProperty(name).ValueKind)
          System.Text.Json.JsonValueKind.Array
          $"status --json: `{name}` is an empty ARRAY on a clean tree, not absent or null"
    })

/// And with something in it, because "the keys are there" is easy to satisfy by accident and
/// "`changed` describes the change" is not.
let statusDescribesADraft =
  cliTestOnMain "status --json describes what is in the draft" (fun state ->
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
  cliTestOnMain
    "conflicts --json keeps its keys when nothing is pending"
    (fun state ->
      task {
        do! start state
        do! hasKeys state [ "conflicts"; "--json" ] [ "conflicts"; "pending" ]
      })

let propagateKeepsItsShapeWhenThereIsNothingToChoose =
  cliTestOnMain
    "propagate --json keeps its keys when there is nothing to choose"
    (fun state ->
      task {
        do! start state
        do! hasKeys state [ "propagate"; "--json" ] [ "choices" ]
      })

let constraintsKeepsItsShape =
  cliTest "constraints --json keeps its keys" (fun state ->
    hasKeys state [ "constraints"; "--json" ] [ "findings" ])

let depsAnswersBothDirections =
  cliTestOnMain "deps --json answers with both directions" (fun state ->
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
  cliTest "search --json echoes the query beside the results" (fun state ->
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
  cliTestOnMain
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
              "leftBehind"
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
  cliTestOnMain "commits --json is an array, with the documented row" (fun state ->
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
  cliTestOnMain "branches --json is an array, with the documented row" (fun state ->
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
  cliTestOnMain
    "an empty listing answers with an empty array, not with prose"
    (fun state ->
      task {
        do! start state
        do! switch state "jsonempty"
        do! onMain state

        do! isArray state [ "diff"; "jsonempty"; "--json" ]
        do! isArray state [ "traces"; "list"; "--json" ]
      })


// ─── the ones that do NOT take --json ─────────────────────────────────────

/// A command that does not answer in JSON has to SAY so and exit nonzero, rather than printing its
/// pretty output and letting a caller parse prose as a payload. `ops` is the one that came up: it
/// takes no flags at all.
let aCommandWithoutJsonRefusesTheFlag =
  cliTest "a command with no --json says so instead of printing prose" (fun state ->
    task {
      do!
        refuses
          state
          [ "ops"; "--json" ]
          "doesn't understand --json"
          "{"
          "`ops` has no JSON form and says so"
    })


let tests : List<Test> =
  [ statusKeepsItsShapeWhenNothingChanged
    statusDescribesADraft
    conflictsKeepsItsShapeWhenThereAreNone
    propagateKeepsItsShapeWhenThereIsNothingToChoose
    constraintsKeepsItsShape
    depsAnswersBothDirections
    searchAnswersWithItsQuery
    commitDryRunAnswersInJson
    commitsIsAnArrayOfCommits
    branchesIsAnArrayOfBranches
    emptyListingsAreStillArrays
    aCommandWithoutJsonRefusesTheFlag ]
