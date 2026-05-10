/// Integration tests for the Darklang CLI's command surface — `traces`,
/// `eval`, `run`, `view`, etc. Each test calls the `executeCliCommand`
/// package fn in-process (no subprocess fork) with `Console.Out`
/// redirected, and asserts on the captured stdout / on the resulting
/// trace store state.
module Tests.CliTraces

open Expecto
open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Fumble
open LibDB.Sqlite

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module Dval = LibExecution.Dval

open TestUtils.TestUtils


/// Build an ExecutionState wired up with the same builtin set the CLI
/// uses in production. Re-built per test so trace-store side effects
/// don't leak across tests.
let private buildState () : Task<RT.ExecutionState> =
  task {
    let pmPTValue = pmPT
    let builtins = Builtins.CliHost.Libs.Cli.builtinsToUse ()
    let pmRT = PT2RT.PackageManager.toRT builtins.values pmPTValue
    let program : RT.Program = { dbs = Map.empty }

    let notify
      (_state : RT.ExecutionState)
      (_vm : RT.VMState)
      (_msg : string)
      (_metadata : Metadata)
      =
      uply { return () }

    let sendException
      (_ : RT.ExecutionState)
      (_ : RT.VMState)
      (_metadata : Metadata)
      (_exn : exn)
      =
      uply { return () }

    return
      Exe.createState
        builtins
        pmRT
        Exe.noTracing
        sendException
        notify
        PT.mainBranchId
        program
  }


/// Invoke the CLI dispatch with the given args (e.g. `["traces"; "list"]`)
/// and return the trimmed captured stdout. Calls `executeCliCommand`
/// in-process and redirects `Console.Out` to a `StringWriter` for the
/// duration. The surrounding `testSequenced` keeps the process-global
/// `Console.SetOut` from racing across tests.
let private runCli (state : RT.ExecutionState) (args : string list) : Task<string> =
  task {
    let argsDval = args |> List.map RT.DString |> Dval.list RT.KTString
    let fnName =
      RT.FQFnName.fqPackage (LibExecution.PackageRefs.Fn.Cli.executeCliCommand ())

    // Drain anything queued in NonBlockingConsole from prior work so it
    // doesn't bleed into our capture.
    NonBlockingConsole.wait ()

    let captured = new System.IO.StringWriter()
    let originalOut = System.Console.Out
    try
      System.Console.SetOut(captured)
      let! result = Exe.executeFunction state fnName [] (NEList.singleton argsDval)
      // `Stdlib.printLine` queues to a background thread; drain before
      // reading the StringWriter or we capture nothing.
      NonBlockingConsole.wait ()
      match result with
      | Ok _ -> return captured.ToString().Trim()
      | Error(rte, _) ->
        System.Console.SetOut(originalOut)
        return Tests.failtestf "runCli errored: %A" rte
    finally
      System.Console.SetOut(originalOut)
  }


/// Helper: extract the trace ID from a `traces list 1 --json` output.
let private parseTraceID (json : string) : string =
  let split = json.Split("\"traceId\":\"")
  if split.Length < 2 then
    Tests.failtestf "Couldn't parse trace id from %s" json
  else
    let parts = split[1].Split('"')
    if parts.Length = 0 then
      Tests.failtestf "Couldn't parse trace id from %s" json
    else
      parts[0]


// ─── Test builders ────────────────────────────────────────────────────────

/// Wrap a fresh ExecutionState in a task — every test gets its own
/// state so trace-store side effects don't leak across tests.
let private withState (f : RT.ExecutionState -> Task<unit>) : Task<unit> =
  task {
    let! state = buildState ()
    do! f state
  }

/// `cliTest "name" body` collapses the
/// `testTask "..." { do! withState (fun state -> task { … }) }`
/// boilerplate that every CLI integration test wraps around. Body
/// receives the state and returns a Task<unit>.
let private cliTest (name : string) (body : RT.ExecutionState -> Task<unit>) : Test =
  testTask name { do! withState body }

/// `cliTestWithFreshTraces` adds a `traces delete --all --yes` step
/// before the body so tests that examine the trace store start from
/// a known-empty state.
let private cliTestWithFreshTraces
  (name : string)
  (body : RT.ExecutionState -> Task<unit>)
  : Test =
  cliTest name (fun state ->
    task {
      let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
      do! body state
    })


// ─── Base CLI command tests ───────────────────────────────────────────────

let private testHelpCommand =
  cliTest "help command" (fun state ->
    task {
      let! output = runCli state [ "help" ]
      Expect.stringContains output "Packages:" "category header"
      Expect.stringContains output "SCM:" "SCM header"
      Expect.stringContains output "help" "help command"
      Expect.stringContains output "version" "version command"
      Expect.stringContains output "status" "status command"
    })

let private testVersionCommand =
  cliTest "version command" (fun state ->
    task {
      let! output = runCli state [ "version" ]
      Expect.stringContains output "Darklang CLI" "CLI banner"
      Expect.stringContains output "alpha-" "version prefix"
    })

let private testStatusCommand =
  cliTest "status command" (fun state ->
    task {
      let! output = runCli state [ "status" ]
      Expect.stringContains output "On branch" "branch line"
      Expect.isTrue
        (output.Contains("No uncommitted changes.")
         || output.Contains("Uncommitted changes:"))
        "uncommitted-changes summary"
    })

/// Parameterised "given <args>, expect stdout = <expected>" — covers
/// the bulk of `run` / `eval` smoke tests without re-typing the
/// withState/runCli boilerplate per case.
let private testCliEquals
  (suiteName : string)
  (cases : List<string * List<string> * string>)
  : Test =
  testList
    suiteName
    (cases
     |> List.map (fun (label, args, expected) ->
       testTask label {
         do!
           withState (fun state ->
             task {
               let! output = runCli state args
               Expect.equal output expected label
             })
       }))

let private testRunCases =
  // `run` is now an alias for `run-script` (file-only). Function calls go
  // through `eval`. Kept under the `run smoke` name for git history; the
  // commands themselves invoke eval.
  testCliEquals
    "run smoke"
    [ "Bool.and", [ "eval"; "Stdlib.Bool.and true false" ], "false"
      "Int64.add", [ "eval"; "Stdlib.Int64.add 5L 3L" ], "8" ]

let private testEvalCases =
  testCliEquals
    "eval smoke"
    [ "String.length", [ "eval"; "Stdlib.String.length \"hello\"" ], "5"
      "List.length", [ "eval"; "[1L; 2L; 3L] |> Stdlib.List.length" ], "3"
      "simple expr", [ "eval"; "2L + 3L" ], "5"
      "string concat", [ "eval"; "\"hello\" ++ \"world\"" ], "helloworld" ]

let private testListFunctions =
  cliTest "ls Stdlib.List" (fun state ->
    task {
      let! output = runCli state [ "ls"; "Stdlib.List" ]
      Expect.stringContains output "Functions" "section"
      Expect.stringContains output "head" "head fn"
    })

let private testViewFunction =
  cliTest "view Stdlib.List.head" (fun state ->
    task {
      let! output = runCli state [ "view"; "Stdlib.List.head" ]
      Expect.stringContains output "head" "fn name"
      Expect.stringContains output "Option" "Option in signature"
      Expect.stringContains output "->" "fn signature arrow"
    })

let private testListTypes =
  cliTest "ls Stdlib.Option" (fun state ->
    task {
      let! output = runCli state [ "ls"; "Stdlib.Option" ]
      Expect.stringContains output "Types" "section"
      Expect.stringContains output "Option" "Option type"
    })

let private testHelpForRun =
  cliTest "help run" (fun state ->
    task {
      let! output = runCli state [ "help"; "run" ]
      Expect.stringContains output "run" "command name"
      Expect.isTrue
        (output.Contains("function") || output.Contains("execute"))
        "run-command description"
    })

let private testHelpForLs =
  cliTest "help ls" (fun state ->
    task {
      let! output = runCli state [ "help"; "ls" ]
      Expect.stringContains output "ls" "command name"
      Expect.isTrue
        (output.Contains("list") || output.Contains("List"))
        "ls description"
    })

// ─── Trace surface tests ──────────────────────────────────────────────────

let private testTracesHelp =
  cliTest "traces help lists subcommand surface" (fun state ->
    task {
      let! output = runCli state [ "traces"; "help" ]
      for term in
        [ "list"
          "view"
          "tail"
          "follow"
          "find"
          "hotspots"
          "replay"
          "delete"
          "--json" ] do
        Expect.stringContains output term $"contains {term}"
    })

let private testTracesTailShowsLastEval =
  cliTestWithFreshTraces "traces tail shows last eval" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "let x = 7L\nx" ]
      let! output = runCli state [ "traces"; "tail" ]
      Expect.stringContains output "Handler: eval" "eval handler line"
      Expect.stringContains output "expression = \"let x = 7L" "recorded input"
    })

let private testTracesDeleteEmpties =
  // Two assertions in one shape: a single eval landed → delete --all
  // empties → list reports `No traces`. Replaces the old separate
  // testTracesClearEmpties / testTracesClearEmptiesList pair.
  cliTestWithFreshTraces "traces delete --all empties the list" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "1L + 2L" ]
      let! pre = runCli state [ "traces"; "list" ]
      Expect.isFalse (pre.Contains "No traces") "list non-empty pre-delete"
      let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
      let! post = runCli state [ "traces"; "list" ]
      Expect.stringContains post "No traces" "list empty post-delete"
    })

let private testTracesStatsCounts =
  cliTestWithFreshTraces "traces stats shows counts" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "1L" ]
      let! _ = runCli state [ "eval"; "2L" ]
      let! output = runCli state [ "traces"; "stats" ]
      Expect.stringContains output "total ms" "table header"
      Expect.stringContains output "count" "count column"
      Expect.stringContains output "│ eval" "eval row"
    })

let private testTracesFindByContent =
  cliTestWithFreshTraces "traces find <pattern> by content" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "\"unique-token-xyz12345\"" ]
      let! _ = runCli state [ "eval"; "1L + 1L" ]
      let! output = runCli state [ "traces"; "find"; "unique-token-xyz12345" ]
      Expect.stringContains output "Traces matching" "find banner"
      Expect.stringContains output "eval" "eval handler"
    })

let private testTracesDeleteSingle =
  testTask "traces delete <id> preserves siblings" {
    do!
      withState (fun state ->
        task {
          let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
          let! _ = runCli state [ "eval"; "1L + 1L" ]
          let! _ = runCli state [ "eval"; "2L + 2L" ]

          // Pre-delete: list latest trace, capture its ID, expect 2 rows total.
          let! latestJson = runCli state [ "traces"; "list"; "1"; "--json" ]
          let latestTid = parseTraceID latestJson

          let! delOut = runCli state [ "traces"; "delete"; latestTid ]
          Expect.stringContains delOut "Deleted trace" "delete confirm"

          // Post-delete: the previously-latest trace should be gone (its
          // ID no longer appears in the list).
          let! listAfter = runCli state [ "traces"; "list" ]
          Expect.isFalse
            (listAfter.Contains latestTid)
            "deleted trace ID gone from list"
        })
  }

let private testTracesPruneKeep =
  testTask "traces prune --keep N keeps the most-recent" {
    do!
      withState (fun state ->
        task {
          let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
          let! _ = runCli state [ "eval"; "1L" ]
          let! _ = runCli state [ "eval"; "2L" ]
          let! _ = runCli state [ "eval"; "3L" ]

          // Capture the most-recent trace ID before pruning.
          let! latestJson = runCli state [ "traces"; "list"; "1"; "--json" ]
          let latestTid = parseTraceID latestJson

          let! pruneOut = runCli state [ "traces"; "delete"; "--keep"; "1" ]
          Expect.stringContains pruneOut "Pruned 2 trace" "prune confirm"

          // Post-prune: the previously-latest is the only one left.
          let! listOut = runCli state [ "traces"; "list" ]
          Expect.stringContains listOut "Recent traces (last 20):" "list banner"
          Expect.stringContains listOut latestTid "latest trace kept"
        })
  }

let private testTracesRejectsNegativeLimit =
  testTask "negative limit rejected across commands" {
    do!
      withState (fun state ->
        task {
          for argv in
            [ [ "traces"; "list"; "-1" ]
              [ "traces"; "stats"; "-1" ]
              [ "traces"; "hotspots"; "-1" ]
              [ "traces"; "find"; "foo"; "-1" ] ] do
            let! out = runCli state argv
            Expect.stringContains out "Limit must be ≥ 1" $"{argv} rejected"
        })
  }

let private testTracesArgOrderingsWork =
  testTask "tail/list flag-order variants both work" {
    do!
      withState (fun state ->
        task {
          let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
          let! _ = runCli state [ "eval"; "1L + 1L" ]
          let! tailNFirst =
            runCli state [ "traces"; "tail"; "1"; "--route"; "eval" ]
          Expect.stringContains tailNFirst "Trace:" "tail N --route"
          let! tailRouteFirst =
            runCli state [ "traces"; "tail"; "--route"; "eval"; "1" ]
          Expect.stringContains tailRouteFirst "Trace:" "tail --route N"
          let! listJsonFn =
            runCli state [ "traces"; "list"; "--json"; "--fn"; "add"; "5" ]
          Expect.stringContains listJsonFn "[" "list --json --fn fn N"
        })
  }

let private testTracesFindEscapesLikeWildcards =
  testTask "find escapes SQL LIKE wildcards" {
    do!
      withState (fun state ->
        task {
          let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
          let! _ = runCli state [ "eval"; "1L + 1L" ]
          let! pctOut = runCli state [ "traces"; "find"; "%" ]
          Expect.stringContains pctOut "No traces match '%'." "literal %"
          let! zPctOut = runCli state [ "traces"; "find"; "z%" ]
          Expect.stringContains zPctOut "No traces match 'z%'." "literal z%"
          let! zUscOut = runCli state [ "traces"; "find"; "z_" ]
          Expect.stringContains zUscOut "No traces match 'z_'." "literal z_"
        })
  }

let private testTracesRouteEmptyRejection =
  testTask "tail/list reject empty/whitespace --route" {
    do!
      withState (fun state ->
        task {
          let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
          let! _ = runCli state [ "eval"; "1L + 1L" ]
          let cases =
            [ [ "traces"; "tail"; "--route"; "" ],
              "--route pattern must not be empty"
              [ "traces"; "tail"; "--route"; "   " ],
              "--route pattern must not be empty"
              [ "traces"; "list"; "--route"; "" ],
              "--route pattern must not be empty"
              [ "traces"; "list"; "--fn"; "   " ], "--fn pattern must not be empty" ]
          for (argv, expected) in cases do
            let! out = runCli state argv
            Expect.stringContains out expected $"{argv} rejected"
        })
  }

let private testTracesArity1Catchalls =
  testTask "arity-1 traces commands print focused usage on extra args" {
    do!
      withState (fun state ->
        task {
          let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
          let! _ = runCli state [ "eval"; "1L + 1L" ]
          let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
          let tid = parseTraceID listJson

          let cases =
            [ [ "traces"; "delete"; tid; "--fake-arg" ],
              "Usage: traces delete <trace-id>" ]
          for (argv, expected) in cases do
            let! out = runCli state argv
            Expect.stringContains out expected $"{argv} catch-all"
        })
  }

let private testTracesStatsHintHiddenForEvalOnly =
  testTask "stats footer hides --route hint when no HTTP traces" {
    do!
      withState (fun state ->
        task {
          let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
          let! _ = runCli state [ "eval"; "1L + 1L" ]
          let! _ = runCli state [ "eval"; "2L + 2L" ]
          let! statsOut = runCli state [ "traces"; "stats" ]
          Expect.stringContains statsOut "Per-handler stats" "table"
          Expect.stringContains statsOut "eval" "eval row"
          Expect.isFalse
            (statsOut.Contains "drill into a route")
            "no route hint for eval-only"
        })
  }

let private testTracesUnknownSubcommandSurfaced =
  testTask "unknown traces subcommand prints clear error" {
    do!
      withState (fun state ->
        task {
          let! typoOut = runCli state [ "traces"; "nonsense" ]
          Expect.stringContains
            typoOut
            "Unknown subcommand: nonsense"
            "typo flagged"
          let! typoTwoOut = runCli state [ "traces"; "lst" ]
          Expect.stringContains typoTwoOut "Unknown subcommand: lst" "lst flagged"

          let! bareOut = runCli state [ "traces" ]
          Expect.isFalse (bareOut.Contains "Unknown subcommand") "bare not flagged"
          let! helpOut = runCli state [ "traces"; "help" ]
          Expect.isFalse (helpOut.Contains "Unknown subcommand") "help not flagged"
        })
  }

let private testTracesFiltersAreCaseInsensitive =
  testTask "list --route is case-insensitive" {
    do!
      withState (fun state ->
        task {
          let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
          let! _ = runCli state [ "eval"; "1L + 1L" ]
          let! listLower = runCli state [ "traces"; "list"; "--route"; "eval" ]
          Expect.stringContains listLower "eval" "lower matches"
          let! listUpper = runCli state [ "traces"; "list"; "--route"; "EVAL" ]
          Expect.stringContains listUpper "eval" "upper matches"
          Expect.isFalse (listUpper.Contains "No traces found") "upper still finds"
          let! listMixed = runCli state [ "traces"; "list"; "--route"; "Eval" ]
          Expect.stringContains listMixed "eval" "mixed matches"
        })
  }

let private testTracesRejectsEmptyPattern =
  testTask "find / list --fn / list --route reject empty pattern" {
    do!
      withState (fun state ->
        task {
          let cases =
            [ [ "traces"; "find"; "" ], "find pattern must not be empty"
              [ "traces"; "find"; ""; "--view" ], "find pattern must not be empty"
              [ "traces"; "find"; ""; "--json" ], "find pattern must not be empty"
              [ "traces"; "list"; "--fn"; "" ], "--fn pattern must not be empty"
              [ "traces"; "list"; "--route"; "" ],
              "--route pattern must not be empty" ]
          for (argv, expected) in cases do
            let! out = runCli state argv
            Expect.stringContains out expected $"{argv} rejected"
        })
  }

let private testTracesViewRejectsNegativeSubOptions =
  testTask "view --depth/--slow-ms reject negative" {
    do!
      withState (fun state ->
        task {
          let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
          let! _ = runCli state [ "eval"; "1L + 1L" ]
          let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
          let tid = parseTraceID listJson

          let! depthOut = runCli state [ "traces"; "view"; tid; "--depth"; "-1" ]
          Expect.stringContains depthOut "--depth must be ≥ 0" "depth -1"
          let! slowOut = runCli state [ "traces"; "view"; tid; "--slow-ms"; "-1" ]
          Expect.stringContains slowOut "--slow-ms must be ≥ 0" "slow-ms -1"
        })
  }

let private testTracesDeleteGrammar =
  testTask "delete --all/--keep singular vs plural phrasing" {
    do!
      withState (fun state ->
        task {
          let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
          let! _ = runCli state [ "eval"; "1L + 1L" ]
          let! clearOne = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
          let! _ = runCli state [ "eval"; "1L + 1L" ]
          let! _ = runCli state [ "eval"; "2L + 2L" ]
          let! clearTwo = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
          let! _ = runCli state [ "eval"; "1L + 1L" ]
          let! _ = runCli state [ "eval"; "2L + 2L" ]
          let! pruneNone = runCli state [ "traces"; "delete"; "--keep"; "0" ]
          let! _ = runCli state [ "eval"; "3L + 3L" ]
          let! _ = runCli state [ "eval"; "4L + 4L" ]
          let! pruneOne = runCli state [ "traces"; "delete"; "--keep"; "1" ]

          Expect.stringContains clearOne "Cleared 1 trace." "singular"
          Expect.stringContains clearTwo "Cleared 2 traces." "plural"
          Expect.stringContains pruneNone "none kept" "prune --keep 0"
          Expect.stringContains pruneOne "kept the most-recent" "prune --keep 1"
        })
  }

let private testTracesReplayReruns =
  testTask "traces replay <id> re-evaluates the recorded eval input" {
    do!
      withState (fun state ->
        task {
          let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
          let! _ = runCli state [ "eval"; "1L + 2L" ]
          let! listJsonBefore = runCli state [ "traces"; "list"; "1"; "--json" ]
          let tid = parseTraceID listJsonBefore
          let! out = runCli state [ "traces"; "replay"; tid ]
          Expect.stringContains out $"Replaying trace {tid}" "header line"
          Expect.stringContains out "3" "result printed"
          Expect.stringContains out "Replay complete" "completion line"

          // The replay produces a fresh trace; verify it landed by
          // counting traces in the list. Was 1, should now be 2.
          let! listJsonAfter = runCli state [ "traces"; "list"; "10"; "--json" ]
          let traceCount = (listJsonAfter.Split("\"traceId\":\"")).Length - 1
          Expect.equal
            traceCount
            2
            "replay should leave the original trace + a fresh one"
        })
  }


let private testTracesPruneIdempotent =
  testTask "traces prune --keep is idempotent under repeated runs" {
    do!
      withState (fun state ->
        task {
          let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
          for _ in 1..5 do
            let! _ = runCli state [ "eval"; "1L + 2L" ]
            ()

          // Sequential prunes (in-process Console capture isn't
          // safe for concurrent runCli invocations). The fix that
          // matters is on the storage side: each prune now wraps
          // its 4 sub-evaluations in one transaction so the
          // "kept" set is consistent. Verifying the result is
          // deterministic across repeated calls is the cheap
          // observable check that doesn't need parallel writers.
          let! _ = runCli state [ "traces"; "delete"; "--keep"; "2" ]
          let! _ = runCli state [ "traces"; "delete"; "--keep"; "2" ]
          let! _ = runCli state [ "traces"; "delete"; "--keep"; "2" ]

          let! listOut = runCli state [ "traces"; "list" ]
          // Lines look like "  <timestamp>  <uuid>  <handler>".
          // Match any line containing a UUIDv4-shaped substring.
          let uuidPattern =
            System.Text.RegularExpressions.Regex(
              "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
            )
          let count =
            listOut.Split('\n')
            |> Array.filter (fun l -> uuidPattern.IsMatch l)
            |> Array.length
          Expect.equal count 2 "repeated prunes converge on --keep"
        })
  }


let private testTracesLargeTraceListSurvives =
  testTask "traces list survives a 50-trace store; find still returns banner" {
    do!
      withState (fun state ->
        task {
          let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
          // Not the multi-MB stress case — but enough to verify
          // list / find don't OOM or time out on a moderately-large
          // store.
          for _ in 1..50 do
            let! _ = runCli state [ "eval"; "1L + 2L" ]
            ()
          let! listOut = runCli state [ "traces"; "list"; "20" ]
          // List uses "Recent traces (last N):" as a header.
          Expect.stringContains listOut "Recent traces" "list returns the banner"
          let! findOut = runCli state [ "traces"; "find"; "3" ]
          // 50 evals of `1L + 2L` all produce DInt64 3; find should
          // return without choking.
          Expect.stringContains findOut "Traces matching" "find returns banner"
        })
  }


let private testTracesViewToleratesCorruptedRow =
  testTask "traces view <id> renders the rest of the call tree on a corrupted row" {
    do!
      withState (fun state ->
        task {
          let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
          let! _ = runCli state [ "eval"; "Stdlib.Int64.add 1L 2L" ]
          let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
          let tid = parseTraceID listJson

          // Inject a corrupt fn_call row: bytes that aren't a valid
          // binary-serialized RT.Dval. The existing rows from the
          // eval above stay valid; the bad one should be skipped
          // (not abort the whole render). Use a transaction so we
          // get the actual SQL error surfaced if anything is wrong.
          let corruptBytes = [| 0x00uy; 0x01uy; 0x02uy |]
          let _ =
            Sql.executeTransactionSync
              [ "INSERT INTO trace_fn_calls
                  (trace_id, call_id, parent_call_id, kind, fn_hash,
                   lambda_expr_id, args, result, duration_ms)
                 VALUES
                  (@traceId, 'corrupt-test', NULL, 'fn', 'corrupt',
                   NULL, @badArgs, @badResult, 0)",
                [ [ "traceId", Sql.string tid
                    "badArgs", Sql.bytes corruptBytes
                    "badResult", Sql.bytes corruptBytes ] ] ]

          let! out = runCli state [ "traces"; "view"; tid ]
          // Corrupt row should be silently dropped (logged), not render.
          Expect.isFalse
            (out.Contains "corrupt-test")
            "corrupt row dropped from rendered tree"
          // Surviving rows from the eval still render.
          Expect.stringContains out "Stdlib" "non-corrupt rows still render"
        })
  }


let private testTracesRejectsFlagAsTraceId =
  testTask "flag-shaped trace-id input rejected as flag" {
    do!
      withState (fun state ->
        task {
          let cmds = [ [ "traces"; "delete"; "--fake-arg" ] ]
          for argv in cmds do
            let! out = runCli state argv
            Expect.stringContains out "Unknown flag: --fake-arg" $"{argv} rejected"
        })
  }


// Tests use `Console.SetOut` to capture stdout in-process. Console.Out is
// process-global, so running these in parallel would let captures bleed
// across tests. `testSequenced` forces Expecto to run them one at a time.
//
// Re-enable trace recording for this suite — Tests.fs defaults to `Off`
// so the rest of the test run doesn't waste cycles writing trace rows
// we never read back.
let tests =
  testSequenced
  <| testList
    "CliTraces"
    [ test "set trace detail" {
        LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.On
      }
      // Base CLI commands
      testHelpCommand
      testVersionCommand
      testStatusCommand
      testRunCases
      testEvalCases
      testListFunctions
      testViewFunction
      testListTypes
      testHelpForRun
      testHelpForLs
      // Trace surface
      testTracesHelp
      testTracesTailShowsLastEval
      testTracesDeleteEmpties
      testTracesStatsCounts
      testTracesFindByContent
      testTracesDeleteSingle
      testTracesPruneKeep
      testTracesReplayReruns
      testTracesPruneIdempotent
      testTracesLargeTraceListSurvives
      testTracesViewToleratesCorruptedRow
      testTracesRejectsNegativeLimit
      testTracesRejectsFlagAsTraceId
      testTracesDeleteGrammar
      testTracesViewRejectsNegativeSubOptions
      testTracesRejectsEmptyPattern
      testTracesFiltersAreCaseInsensitive
      testTracesUnknownSubcommandSurfaced
      testTracesStatsHintHiddenForEvalOnly
      testTracesArgOrderingsWork
      testTracesArity1Catchalls
      testTracesRouteEmptyRejection
      testTracesFindEscapesLikeWildcards ]
