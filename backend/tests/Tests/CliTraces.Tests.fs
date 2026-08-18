/// Integration tests for the Darklang CLI's command surface. Each test calls the
/// `executeCliCommand` package fn in-process (no subprocess fork) with `Console.Out`
/// redirected, and asserts on the captured stdout or the resulting trace store state.
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


/// Build an ExecutionState wired up with the same builtin set the CLI uses in
/// production. Re-built per test so trace-store side effects don't leak across tests.
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

    return Exe.createState builtins pmRT Exe.noTracing sendException notify program
  }


/// Invoke the CLI dispatch with the given args (e.g. `["traces"; "list"]`) and return
/// the trimmed captured stdout, with `Console.Out` redirected to a `StringWriter` for
/// the duration. The surrounding `testSequenced` keeps the process-global
/// `Console.SetOut` from racing across tests.
let private runCli (state : RT.ExecutionState) (args : string list) : Task<string> =
  task {
    let argsDval = args |> List.map RT.DString |> Dval.list RT.KTString
    let fnName =
      RT.FQFnName.fqPackage (LibExecution.PackageRefs.Fn.Cli.executeCliCommand ())

    // Drain prior work queued in NonBlockingConsole, so it stays out of our capture.
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

/// Wrap a fresh ExecutionState in a task.
let private withState (f : RT.ExecutionState -> Task<unit>) : Task<unit> =
  task {
    let! state = buildState ()
    do! f state
  }

/// `cliTest "name" body` collapses the `testTask "..." { do! withState ... }`
/// boilerplate. Body receives the state and returns a Task<unit>.
let private cliTest (name : string) (body : RT.ExecutionState -> Task<unit>) : Test =
  testTask name { do! withState body }

/// Adds a `traces delete --all --yes` step before the body, so tests that examine
/// the trace store start from a known-empty state.
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
      Expect.stringContains output "Changes:" "changes header"
      Expect.stringContains output "Branches:" "branches header"
      Expect.stringContains output "Sync:" "sync header"
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
      // The branch line appears only when you're on one; on main it is noise. Which of
      // "clean: ..." / "draft: N items changed" is left depends on what ran before.
      Expect.isTrue
        (output.Contains("clean:") || output.Contains("draft:"))
        $"status says whether there's uncommitted work, got: {output}"
      Expect.isFalse
        (output.Contains("On branch"))
        "no branch line on main -- it only appears when you're on a branch"
    })

/// Parameterised "given <args>, expect stdout = <expected>": the bulk of the
/// `run` / `eval` smoke tests, without the per-case boilerplate.
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
  // `run` is an alias for `run-script` (file-only); function calls go through `eval`.
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

          let! latestJson = runCli state [ "traces"; "list"; "1"; "--json" ]
          let latestTid = parseTraceID latestJson

          let! delOut = runCli state [ "traces"; "delete"; latestTid ]
          Expect.stringContains delOut "Deleted trace" "delete confirm"

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

          let! latestJson = runCli state [ "traces"; "list"; "1"; "--json" ]
          let latestTid = parseTraceID latestJson

          let! pruneOut = runCli state [ "traces"; "delete"; "--keep"; "1" ]
          Expect.stringContains pruneOut "Pruned 2 trace" "prune confirm"

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

          // The replay produces a fresh trace, so the count goes 1 -> 2.
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

          // Sequential: in-process Console capture isn't safe for
          // concurrent runCli calls. Each prune wraps its four
          // sub-evaluations in one transaction, so "kept" is stable.
          let! _ = runCli state [ "traces"; "delete"; "--keep"; "2" ]
          let! _ = runCli state [ "traces"; "delete"; "--keep"; "2" ]
          let! _ = runCli state [ "traces"; "delete"; "--keep"; "2" ]

          let! listOut = runCli state [ "traces"; "list" ]
          // Lines look like "  <timestamp>  <uuid>  <handler>".
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
          // Not the multi-MB stress case, but enough to OOM or time out.
          for _ in 1..50 do
            let! _ = runCli state [ "eval"; "1L + 2L" ]
            ()
          let! listOut = runCli state [ "traces"; "list"; "20" ]
          Expect.stringContains listOut "Recent traces" "list returns the banner"
          let! findOut = runCli state [ "traces"; "find"; "3" ]
          // 50 evals of `1L + 2L` all produce DInt64 3.
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
          // binary-serialized RT.Dval. The eval's own rows stay
          // valid; the bad one must be skipped, not abort the render.
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
          Expect.isFalse
            (out.Contains "corrupt-test")
            "corrupt row dropped from rendered tree"
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


/// A trace that hits the event cap must still be a walkable tree.
///
/// Events are recorded when a call *completes*, so they arrive innermost-first and the entry point
/// is last: a cap that stops at N keeps the deepest calls and drops their ancestors, leaving the
/// viewer nothing to walk down from. `addEvent` reserves a slot for every frame still on the stack,
/// which are exactly those ancestors.
let private testTracesTruncatedStillShowsRoot =
  testSequenced
  <| testTask "a truncated trace still renders its root" {
    do!
      withState (fun state ->
        task {
          // Set both here rather than relying on suite ordering, so this still means something alone.
          LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.On
          LibDB.Tracing.TraceLimits.useMaxEventsForTesting 20
          try
            let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
            // Comfortably over the cap of 20: each element costs a lambda call and an add.
            let! evalOut =
              runCli
                state
                [ "eval"
                  "Stdlib.List.length (Stdlib.List.map (Stdlib.List.range 1 40) (fun x -> x + 1))" ]
            Expect.stringContains evalOut "40" "the eval itself succeeded"
            let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
            let tid = parseTraceID listJson
            let! view = runCli state [ "traces"; "view"; tid ]

            Expect.stringContains
              view
              "trace truncated"
              "the marker says the trace was capped"
            Expect.stringContains
              view
              "eval"
              "the root is still there, so the tree can be walked down from it"
          finally
            LibDB.Tracing.TraceLimits.resetMaxEventsForTesting ()
            // Leaving detail on would hand full tracing to every later sequenced suite.
            LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.Off
        })
  }


/// Every registered command name, read out of `dark help`.
///
/// `help` prints each as `  name (alias, ...) - description`, so a name is the first token of any
/// indented line containing " - ". Parsing the human output is deliberate: a command that stops
/// appearing on the surface a person sees has stopped existing.
let private registeredCommands (state : RT.ExecutionState) : Task<List<string>> =
  task {
    let! output = runCli state [ "help" ]

    return
      output.Split('\n')
      |> Array.toList
      |> List.choose (fun line ->
        if line.StartsWith "  " && line.Contains " - " then
          let name = line.Trim().Split(' ')[0]
          if name = "" || name.StartsWith "-" then None else Some name
        else
          None)
      |> List.distinct
  }

/// Phrases that mean "you used this wrong". A request for HELP must never be answered with one.
///
/// Deliberately NOT "usage:" or "required": both appear in good help text, and a check that flags
/// them flags forty commands and gets switched off. `--help` is handled centrally; bare `help` is
/// each command's own job.
let private soundsLikeMisuse (output : string) : bool =
  let o = output.ToLower()
  [ "error:"
    "internal error"
    "is not a valid"
    "unknown topic"
    "unknown command" ]
  |> List.exists (fun phrase -> o.Contains phrase)

// The workbench renders. `initialState` builds a state without seizing the terminal, and this goes
// through `dark eval` rather than a `.dark` testfile because building one reads the package tree,
// and the execution testfiles are for pure functions (see `scm/propagation-policy.dark`).

/// One line, because `eval` takes the expression as a single argument.
let private renderExpr (body : string) : string =
  "let st = Darklang.Cli.Workbench.initialState \"\" (Stdlib.Option.Option.None) \"Tester\" \"test-instance\" [] in "
  + "let s = Darklang.Cli.Workbench.refreshScmStatus st st in "
  + "let frame = fun v w h -> (Darklang.Cli.Workbench.viewAtSize { s with activeView = v } (Darklang.Cli.Tui.Size { width = w; height = h })).rows in "
  + body

let private workbenchViewsRender =
  cliTest "every workbench view renders a full frame" (fun state ->
    task {
      // A view that throws produces no frame at all, which is what these row counts
      // are really checking.
      let! output =
        runCli
          state
          [ "eval"
            renderExpr
              "let n = fun v -> Stdlib.List.length (frame v 120 40) in [ n 0; n 1; n 3; n 4; n 6; n 7; n 8; n 9 ]" ]

      Expect.stringContains
        output
        "[40, 40, 40, 40, 40, 40, 40, 40]"
        "all eight views render a full 40-row frame"
    })

/// The key-hint row must not drop the way out.
///
/// Clipping one long string from the right loses the hints at the END -- `?` (the keymap) and
/// `esc/q` (quit), the two a person needs most when lost. The row drops whole hints cheapest-first
/// instead: secondary globals, then the view's own actions, and `?`/`esc/q` never.
let private hintRowKeepsTheWayOut =
  cliTest
    "the hint row drops secondary keys before it drops help and quit"
    (fun state ->
      task {
        let lastRow (w : int) : string =
          "let st = Darklang.Cli.Workbench.initialState \"\" (Stdlib.Option.Option.None) \"Stachu\" \"i\" [] in "
          + "let s0 = Darklang.Cli.Workbench.refreshScmStatus st st in "
          + "let s1 = { s0 with activeView = 4 } in "
          + "let s = { s1 with items = Darklang.Cli.Workbench.reloadItems s1 } in "
          // Tall enough that the hint row is always in frame: a clean tree renders a
          // roomier panel than a draft, so a short frame loses the hints in one store.
          + $"Stdlib.String.join (Darklang.Cli.Workbench.viewAtSize s (Darklang.Cli.Tui.Size {{ width = {w}; height = 26 }})).rows \"\\n\""

        let! wide = runCli state [ "eval"; lastRow 160 ]
        Expect.stringContains
          wide
          "views"
          "the full row advertises the secondary keys too"

        // Narrow enough that something has to go. Single words, not "? help": the key
        // and its label are coloured separately, so an escape sequence sits between
        // them and a two-word substring never matches.
        let! narrow = runCli state [ "eval"; lastRow 95 ]
        Expect.stringContains narrow "help" "the keymap key survives"
        Expect.stringContains narrow "quit" "and so does the way out"
        Expect.stringContains
          narrow
          "commit"
          "the view's own actions outlive the secondary globals"

        // Narrower still: actions start giving way, but never the escape hatches.
        let! tiny = runCli state [ "eval"; lastRow 72 ]
        Expect.stringContains tiny "help" "the keymap key still survives"
        Expect.stringContains tiny "quit" "and the way out is still there"
      })

/// The context row must not overwrite its own tail on a narrower terminal.
///
/// It writes the left-hand text at column 0 and the sync glance right-aligned over the same row, so
/// anything the left side spills past the glance is lost, starting with the END of the draft
/// summary. It drops whole segments in priority order instead (`instance:` first, then the account
/// name), so the draft split survives far narrower.
let private contextRowKeepsTheDraftWhenNarrow =
  cliTest
    "the context row drops labels before it drops the draft summary"
    (fun state ->
      task {
        // A deliberately long instance name, so the row is over-full whatever the shared
        // store holds in its draft: the test creates the condition rather than hoping.
        let row (w : int) : string =
          "let st = Darklang.Cli.Workbench.initialState \"\" (Stdlib.Option.Option.None) \"Stachu\" \"inst-with-a-deliberately-long-name-for-this-test\" [] in "
          + "let s0 = Darklang.Cli.Workbench.refreshScmStatus st st in "
          + "let s1 = { s0 with activeView = 4 } in "
          + "let s = { s1 with items = Darklang.Cli.Workbench.reloadItems s1 } in "
          + $"Stdlib.String.join (Stdlib.List.take (Darklang.Cli.Workbench.viewAtSize s (Darklang.Cli.Tui.Size {{ width = {w}; height = 4 }})).rows 1) \"\""

        let! wide = runCli state [ "eval"; row 150 ]
        Expect.stringContains
          wide
          "instance:"
          "the full row shows the instance label"
        Expect.stringContains wide "branch:" "and the branch"

        let! narrow = runCli state [ "eval"; row 90 ]
        Expect.isFalse
          (narrow.Contains "instance:")
          "the instance label is dropped to make room, rather than the row colliding"
        Expect.stringContains
          narrow
          "branch:"
          "the branch survives, being worth more than the label"
        // Asserted WHOLE, not by name: which glance wins is a priority decision, and the
        // shared store decides which exist, so naming one makes this hostage to other tests.
        let glanceIsWhole =
          [ "waiting"; "need you"; "in sync" ]
          |> List.exists (fun g -> narrow.Contains g)

        Expect.isTrue
          glanceIsWhole
          $"the right-aligned glance is whole, not overwritten (row: {narrow})"
      })

/// The workbench's own mutating actions, driven the way a keypress drives them.
///
/// `b` in the SCM view prompts for a name and then runs `branch-create`. `Branch.create` returns an
/// `Option<Branch>`, so reading `.id` off it throws. The render tests cannot reach any of this: it
/// is behind a prompt, not a render.
let private workbenchBranchActionsWork =
  cliTest
    "the workbench can start, switch and merge a branch without throwing"
    (fun state ->
      task {
        let act (action : string) (text : string) : string =
          "let st = Darklang.Cli.Workbench.initialState \"\" (Stdlib.Option.Option.None) \"T\" \"t\" [] in "
          + "let s0 = Darklang.Cli.Workbench.refreshScmStatus st st in "
          + "let s = { s0 with activeView = 4 } in "
          + $"match Darklang.Cli.Workbench.performInputAction s (Darklang.Cli.Workbench.InputState {{ prompt = \"p\"; text = \"{text}\"; action = \"{action}\" }}) with "
          + "| Continue ns -> ns.message | _ -> \"(exit)\""

        let! created = runCli state [ "eval"; act "branch-create" "wbTestBranch" ]
        Expect.stringContains
          created
          "on branch wbTestBranch"
          "starting a branch lands you on it, rather than throwing on an Option"

        let! switched = runCli state [ "eval"; act "branch-switch" "wbTestBranch" ]
        Expect.stringContains switched "switched to" "and it can be switched to"

        // These act on `state.branchId`, which is main here, so merge refuses because
        // you are not on a branch at all. That is the gate, not a crash.
        let! merged = runCli state [ "eval"; act "merge" "y" ]
        Expect.stringContains
          merged
          "on main"
          "and merge reports the gate rather than throwing"
      })

/// Neither merge nor rebase means anything on main, and both must say so.
///
/// A rebase on main rewrites nothing -- it loops over a branch's `branch_name_bases` rows and main
/// has none -- so a success message would be a no-op you believe, and "the branch has no changes"
/// counts the changes of a branch you are not on.
let private mergeAndRebaseRefuseOnMain =
  cliTest
    "merge and rebase say you're on main, rather than claiming to have run"
    (fun state ->
      task {
        let act (action : string) : string =
          "let st = Darklang.Cli.Workbench.initialState \"\" (Stdlib.Option.Option.None) \"T\" \"t\" [] in "
          + "let s0 = Darklang.Cli.Workbench.refreshScmStatus st st in "
          + "let s = { s0 with activeView = 4 } in "
          + $"match Darklang.Cli.Workbench.performInputAction s (Darklang.Cli.Workbench.InputState {{ prompt = \"p\"; text = \"y\"; action = \"{action}\" }}) with "
          + "| Continue ns -> ns.message | _ -> \"(exit)\""

        let! rebased = runCli state [ "eval"; act "rebase" ]
        Expect.stringContains
          rebased
          "on main"
          "rebase names the reason rather than reporting a rebase that did not happen"
        Expect.isFalse
          (rebased.Contains "rebased onto parent")
          "and does not claim success"

        let! merged = runCli state [ "eval"; act "merge" ]
        Expect.stringContains merged "on main" "merge names the same reason"
      })

/// Displaying a commit's ops must not fetch all of them.
///
/// The seed commit holds about twelve thousand. Taking the whole list and keeping the first handful
/// deserializes every blob to discard almost all of them, which in the workbench is a freeze on a
/// keypress. Asserted on the OUTPUT, not a stopwatch: if the cap goes, so does "showing the first 50".
let private showingACommitDoesNotFetchEveryOp =
  cliTest "showing a big commit's ops is capped, not fetched whole" (fun state ->
    task {
      let! commits = runCli state [ "commits"; "--json" ]
      // The baseline commit is the one with thousands of ops: the last row, oldest first.
      let hashes =
        commits.Split("\"hash\":\"")
        |> Array.skip 1
        |> Array.map (fun (s : string) -> s.Split('"')[0])
      let hash = hashes[hashes.Length - 1]

      let! shown = runCli state [ "show"; hash ]
      Expect.stringContains
        shown
        "showing the first 50 of"
        "the op list is capped and says so, rather than printing thousands"
    })

/// The SCM view has four sections behind `tab`, and the view-level render test above never switches
/// section, so it only exercises Changes. Dark catches a wrong type at runtime, so a bad section
/// throws on the keypress that switches to it rather than at build time.
let private everyScmSectionRenders =
  cliTest "every SCM section renders, not just the one it opens on" (fun state ->
    task {
      let! output =
        runCli
          state
          [ "eval"
            "let st = Darklang.Cli.Workbench.initialState \"\" (Stdlib.Option.Option.None) \"Tester\" \"test-instance\" [] in "
            + "let s0 = Darklang.Cli.Workbench.refreshScmStatus st st in "
            + "let sect = fun n -> "
            + "  let s1 = { s0 with activeView = 4; scmSection = n } in "
            + "  let s = { s1 with items = Darklang.Cli.Workbench.itemsForView 4 s1.branchId s1.location n s1.aiSection s1.matterLens } in "
            + "  Stdlib.List.length (Darklang.Cli.Workbench.viewAtSize s (Darklang.Cli.Tui.Size { width = 120; height = 40 })).rows in "
            + "[ sect 0; sect 1; sect 2; sect 3 ]" ]

      Expect.stringContains
        output
        "[40, 40, 40, 40]"
        "Changes, History, Conflicts and Branches each render a full frame"
    })

let private workbenchHandlesTerminalSizes =
  cliTest "the workbench frames a tiny terminal instead of breaking" (fun state ->
    task {
      // 56x12 is the documented floor: at it you get a real frame, below it a resize
      // message rather than a mangled layout. Both fill the space they were given.
      let! atFloor =
        runCli state [ "eval"; renderExpr "Stdlib.List.length (frame 0 56 12)" ]
      Expect.stringContains atFloor "12" "a frame at the documented minimum size"

      let! tooSmall =
        runCli
          state
          [ "eval"
            renderExpr
              "Stdlib.String.join (frame 0 40 10) \"|\" |> Stdlib.String.contains \"too small\"" ]
      Expect.stringContains
        tooSmall
        "true"
        "below the minimum, the frame says to resize"
    })

let private workbenchContextRowSaysWhereYouAre =
  cliTest "the workbench context row names the branch, in every view" (fun state ->
    task {
      // Matches the LABEL, not "branch: main": the context row styles its label and value separately,
      // so colour codes sit between them and they are never adjacent in the string.
      let! output =
        runCli
          state
          [ "eval"
            renderExpr
              "let has = fun v -> Stdlib.String.contains (Stdlib.String.join (frame v 120 40) \"|\") \"branch:\" in [ has 0; has 1; has 4 ]" ]

      Expect.stringContains
        output
        "[true, true, true]"
        "Home, Matter and SCM all carry the context row"

      let! named =
        runCli
          state
          [ "eval"
            renderExpr
              "Stdlib.String.contains (Stdlib.String.join (frame 0 120 40) \"|\") \"test-instance\"" ]

      Expect.stringContains named "true" "and it names the instance you're on"
    })

let private everyCommandAnswersHelp =
  cliTest "every registered command answers `help` with help" (fun state ->
    task {
      let! commands = registeredCommands state
      Expect.isGreaterThan (List.length commands) 20 "the registry was read"

      let mutable failures : List<string * string> = []

      for cmd in commands do
        // `quit` ends the session rather than printing, and is the one command whose
        // help can't be asked for this way. Everything else must answer.
        if cmd <> "quit" then
          let! output = runCli state [ cmd; "help" ]

          if output = "" then
            failures <- (cmd, "printed nothing") :: failures
          elif soundsLikeMisuse output then
            let first = output.Split('\n')[0]
            failures <- (cmd, first) :: failures

      // Reported together: when this breaks it usually breaks for a whole group of
      // commands at once, and finding that out one re-run at a time is the slow way.
      if not (List.isEmpty failures) then
        let detail =
          failures
          |> List.rev
          |> List.map (fun (c, why) -> $"  dark {c} help -> {why}")
          |> String.concat "\n"

        Tests.failtestf "commands that don't answer `help`:\n%s" detail
    })

let private everyCommandSurvivesABogusArgument =
  cliTest
    "no registered command crashes on an argument that means nothing"
    (fun state ->
      task {
        let! commands = registeredCommands state

        // Excluded because RUNNING them is the problem, not the argument: they start
        // servers, rewrite the install, open a full-screen UI, or reach the network.
        let skip =
          Set.ofList
            [ "quit"
              "install"
              "uninstall"
              "update"
              "install-status"
              "serve"
              "outliner"
              "views"
              "text-editor"
              "apps"
              "agent"
              "login"
              "logout"
              "export-seed"
              "devices"
              "clear" ]

        let mutable failures : List<string * string> = []

        for cmd in commands do
          if not (Set.contains cmd skip) then
            let! output = runCli state [ cmd; "zzz-no-such-thing-zzz" ]

            // A command that silently ignores an argument it didn't understand looks
            // exactly like one that did what you asked.
            if output.Trim() = "" then failures <- (cmd, "said nothing") :: failures

        if not (List.isEmpty failures) then
          let detail =
            failures
            |> List.rev
            |> List.map (fun (c, why) ->
              $"  dark {c} zzz-no-such-thing-zzz -> {why}")
            |> String.concat "\n"

          Tests.failtestf
            "commands that ignore an argument they don't understand:\n%s"
            detail
      })



/// Commands that take a target, invoked against one that doesn't exist.
///
/// The rule: SAY WHICH THING you could not find. "nothing to merge" is the identical sentence a
/// real but empty target produces, so a typo reads as "already done" -- or, for `conflicts branch`,
/// as a green light to merge. Asserting the target is NAMED pins no wording, so a better message
/// stays green.
let private nonexistentTargets : List<string * List<string>> =
  [ "view", [ "view"; "Zzz.Nope.nope" ]
    "deps", [ "deps"; "Zzz.Nope.nope" ]
    "undo", [ "undo"; "Zzz.Nope.nope" ]
    "merge", [ "merge"; "zzznope" ]
    "rebase", [ "rebase"; "zzznope" ]
    "diff", [ "diff"; "zzznope" ]
    "log", [ "log"; "zzznope" ]
    "show", [ "show"; "zzznope" ]
    "branch archive", [ "branch"; "archive"; "zzznope" ]
    "review approve", [ "review"; "approve"; "zzznope" ]
    "review reject", [ "review"; "reject"; "zzznope" ]
    "conflicts show", [ "conflicts"; "show"; "zzznope" ]
    "conflicts branch", [ "conflicts"; "branch"; "zzznope" ]
    "propagate show", [ "propagate"; "show"; "Zzz.Nope.nope" ]
    "propagate pin", [ "propagate"; "pin"; "Zzz.Nope.nope" ]
    "propagate follow", [ "propagate"; "follow"; "Zzz.Nope.nope" ]
    "constraints resolve", [ "constraints"; "resolve"; "zzznope" ]
    "ack", [ "ack"; "zzznope" ] ]

let private missingTargetsAreNamed =
  cliTest "a command that can't find its target says which target" (fun state ->
    task {
      // On main, deliberately: `ack` refuses on a branch (an ack is a statement about
      // the store), and the process is a shared global another test may have moved.
      let! _ = runCli state [ "switch"; "main" ]

      let mutable failures : List<string * string> = []

      for (label, args) in nonexistentTargets do
        let! output = runCli state args
        let target = args |> List.last |> Option.defaultValue ""

        if output.Trim() = "" then
          failures <- (label, "said nothing") :: failures
        elif not (output.Contains target) then
          failures <- (label, output.Split('\n')[0]) :: failures

      if not (List.isEmpty failures) then
        let detail =
          failures
          |> List.rev
          |> List.map (fun (c, why) -> $"  dark {c} <missing> -> {why}")
          |> String.concat "\n"

        Tests.failtestf
          "commands that don't name the target they couldn't find:\n%s"
          detail
    })



/// Every `dark <word>` the in-CLI docs mention has to be a real command.
///
/// A doc that confidently describes a command that is not there is worse than no doc, and nothing
/// else checks. Narrow by design: only the WORD after `dark`, which is what is verifiable
/// mechanically. It cannot tell you the prose is wrong, only that the commands are real.
let private docTopicsToCheck = [ "scm"; "for-ai"; "cli" ]

let private documentedCommandsAreReal =
  cliTest "every command the docs mention exists" (fun state ->
    task {
      let! registered = registeredCommands state
      // Aliases don't appear in the group listing's first token, so read them out of the parenthesised
      // part of each line: `  status (wip, changes) - ...`.
      let! help = runCli state [ "help" ]

      let aliases =
        help.Split('\n')
        |> Array.toList
        |> List.collect (fun line ->
          if line.Contains "(" && line.Contains ")" && line.Contains " - " then
            let inner = line.Substring(line.IndexOf "(" + 1)
            let inner = inner.Substring(0, inner.IndexOf ")")
            inner.Split(',') |> Array.toList |> List.map (fun s -> s.Trim())
          else
            [])

      let known = Set.ofList (registered @ aliases)
      let mutable failures : List<string * string> = []

      for topic in docTopicsToCheck do
        let! doc = runCli state [ "docs"; topic ]

        let mentioned =
          doc.Split([| ' '; '\n'; '\t' |])
          |> Array.toList
          |> List.pairwise
          |> List.choose (fun (a, b) ->
            if a = "dark" then
              let cmd = b.Trim([| '`'; ','; '.'; ':'; ')'; '"' |])
              // `dark --branch <id> <cmd>` and `dark <path>` placeholders aren't commands.
              if cmd = "" || cmd.StartsWith "-" || cmd.StartsWith "<" then
                None
              else
                Some cmd
            else
              None)
          |> List.distinct

        for cmd in mentioned do
          if not (Set.contains cmd known) then failures <- (topic, cmd) :: failures

      if not (List.isEmpty failures) then
        let detail =
          failures
          |> List.rev
          |> List.map (fun (topic, cmd) ->
            $"  docs {topic} says `dark {cmd}`, which isn't a command")
          |> String.concat "\n"

        Tests.failtestf "the docs describe commands that don't exist:\n%s" detail
    })



let private deprecationIsReversible =
  cliTest "delete can be undone" (fun state ->
    task {
      let! _ =
        runCli
          state
          [ "fn"
            "Tests.Undep.item"
            "(x: Int64) : Int64 = Stdlib.Int64.add x 4242L" ]

      let! _ =
        runCli state [ "delete"; "fn"; "Tests.Undep.item"; "-m"; "t"; "--yes" ]
      let! hidden = runCli state [ "ls"; "Tests.Undep" ]
      Expect.isFalse (hidden.Contains "item") "a deleted item is hidden"

      let! restored = runCli state [ "undeprecate"; "Tests.Undep.item" ]
      Expect.stringContains
        restored
        "Undeprecated"
        "undeprecate reports what it did"

      let! back = runCli state [ "ls"; "Tests.Undep" ]
      Expect.stringContains back "item" "and it is back on the shelf"

      let! ran = runCli state [ "eval"; "Tests.Undep.item 1L" ]
      Expect.stringContains ran "4243" "and it still runs"
    })



// The docs' worked example, executed: `docs scm` ends with one whose comments claim specific
// output. The doc is the source, not a copy, so the two cannot drift.

/// Split a command line the way a shell would: whitespace-separated, except in quotes.
let private tokenize (line : string) : List<string> =
  let tokens = ResizeArray<string>()
  let current = System.Text.StringBuilder()
  let mutable inQuotes = false
  let mutable any = false

  for ch in line do
    if ch = '"' then
      inQuotes <- not inQuotes
      any <- true
    elif ch = ' ' && not inQuotes then
      if any then
        tokens.Add(current.ToString())
        current.Clear() |> ignore<System.Text.StringBuilder>
        any <- false
    else
      current.Append(ch) |> ignore<System.Text.StringBuilder>
      any <- true

  if any then tokens.Add(current.ToString())
  tokens |> List.ofSeq

/// One step of a worked example: what to run, and what the doc claims comes back.
type private ExampleStep = { args : List<string>; expected : Option<string> }

/// Parse the `## Worked example` block: indented lines, with an optional `# claim` after
/// the command. A claim's ` -- ` tail is prose, so only the part before it is asserted.
let private parseWorkedExample (doc : string) : List<ExampleStep> =
  let lines = doc.Split('\n') |> Array.toList

  let block =
    lines
    |> List.skipWhile (fun l -> not (l.StartsWith "## Worked example"))
    |> List.skip 1
    |> List.takeWhile (fun l -> not (l.StartsWith "## "))

  block
  |> List.choose (fun line ->
    let trimmed = line.Trim()
    if trimmed = "" then
      None
    else
      let (cmd, claim) =
        match trimmed.IndexOf " #" with
        | -1 -> (trimmed, None)
        | i ->
          let rest = trimmed.Substring(i + 2).Trim()
          let claim =
            match rest.IndexOf " -- " with
            | -1 -> rest
            | j -> rest.Substring(0, j)
          (trimmed.Substring(0, i).Trim(), Some(claim.Trim()))

      Some { args = tokenize cmd; expected = claim })

/// Remove what a previous run of the example left behind. Not tidiness: ops are content-addressed,
/// so re-authoring the example's first version dedups to nothing, leaving a clean draft that makes
/// every claim about it false. Targeted by the example's commit messages rather than sweeping.
let private resetWorkedExample () : Task<unit> =
  task {
    let messages = "('money helpers', 'cents in mills')"

    do!
      Sql.query
        $"DELETE FROM package_ops WHERE commit_hash IN
            (SELECT hash FROM commits WHERE message IN {messages})"
      |> Sql.executeStatementAsync

    do!
      Sql.query $"DELETE FROM commits WHERE message IN {messages}"
      |> Sql.executeStatementAsync

    do!
      Sql.query
        "DELETE FROM package_ops WHERE commit_hash IS NULL
           AND id NOT IN (SELECT op_id FROM op_branches)
           AND id IN (SELECT DISTINCT p.id FROM package_ops p
                      JOIN locations l ON l.origin_ts = p.origin_ts
                      WHERE l.owner = 'Ux' AND l.modules = 'Money')"
      |> Sql.executeStatementAsync

    do!
      Sql.query "DELETE FROM locations WHERE owner = 'Ux' AND modules = 'Money'"
      |> Sql.executeStatementAsync

    do!
      Sql.query
        "DELETE FROM propagation_policy WHERE owner = 'Ux' AND modules = 'Money'"
      |> Sql.executeStatementAsync
  }

let private theWorkedExampleWorks =
  cliTest "the worked example in `docs scm` does what it says" (fun state ->
    task {
      do! resetWorkedExample ()
      let! _ = runCli state [ "switch"; "main" ]

      // Commit whatever else is uncommitted: the example claims counts ("2 items
      // changed"), which are only about the example if the draft starts empty.
      // Committing is non-destructive; discarding would take other tests' work with it.
      let! _ = LibDB.Inserts.commitAllAsBaseline "worked-example setup"

      let! doc = runCli state [ "docs"; "scm" ]
      let steps = parseWorkedExample doc

      Expect.isGreaterThan (List.length steps) 8 "the example was found and parsed"

      let mutable failures : List<string> = []

      for step in steps do
        let! output = runCli state step.args
        let cmd = String.concat " " step.args

        match step.expected with
        | None -> ()
        | Some claim ->
          // A comma-separated claim is several substrings: some outputs are several
          // lines, and a doc reads better as "CHANGED (1), STAYING BEHIND (1)".
          let parts =
            claim.Split(',') |> Array.toList |> List.map (fun s -> s.Trim())

          for part in parts do
            if part <> "" && not (output.Contains part) then
              let got = output.Replace("\n", " | ")
              failures <- $"  `{cmd}` claims \"{part}\", got \"{got}\"" :: failures

      do! resetWorkedExample ()

      if not (List.isEmpty failures) then
        Tests.failtestf
          "the worked example in `docs scm` doesn't do what it says:\n%s"
          (failures |> List.rev |> String.concat "\n")
    })



let private editsAreVisibleInTheSameProcess =
  cliTest "an edit is visible to a later command in the SAME process" (fun state ->
    task {
      // One-shot `dark` invocations never hit this: each is a fresh process. In a REPL,
      // the LSP or a daemon, the name-resolution cache decides whether you see your edit.
      let! _ =
        runCli
          state
          [ "fn"; "Tests.Cache.v"; "(x: Int64) : Int64 = Stdlib.Int64.add x 1L" ]

      let! first = runCli state [ "eval"; "Tests.Cache.v 0L" ]
      Expect.stringContains first "1" "the first version runs"

      let! _ =
        runCli
          state
          [ "fn"; "Tests.Cache.v"; "(x: Int64) : Int64 = Stdlib.Int64.add x 2L" ]

      let! second = runCli state [ "eval"; "Tests.Cache.v 0L" ]
      Expect.stringContains second "2" "and so does the edit, without restarting"
    })



let private deprecationTakesEffectInTheSameProcess =
  cliTest "marking a fn harmful takes effect without restarting" (fun state ->
    task {
      let! _ =
        runCli
          state
          [ "fn"; "Tests.Harm.f"; "(x: Int64) : Int64 = Stdlib.Int64.add x 31L" ]

      let! before = runCli state [ "eval"; "Tests.Harm.f 0L" ]
      Expect.stringContains before "31" "it runs to begin with"

      let! _ =
        runCli
          state
          [ "deprecate"
            "fn"
            "Tests.Harm.f"
            "--kind"
            "harmful"
            "-m"
            "t"
            "--yes" ]

      // The harmful set is cached for the life of the process, so a REPL session that
      // marks something dangerous and keeps running is where it has to be dropped.
      let! during = runCli state [ "eval"; "Tests.Harm.f 0L" ]
      Expect.stringContains during "Harmful" "and it halts as soon as it's marked"

      let! _ = runCli state [ "undeprecate"; "Tests.Harm.f" ]

      let! after = runCli state [ "eval"; "Tests.Harm.f 0L" ]
      Expect.stringContains after "31" "and runs again as soon as it's unmarked"
    })


/// The halt message names `--allow-harmful`, so the flag has to be able to do what the message promises.
/// It is parsed in Dark, threaded through the builtin, and lands on the execution state -- and the gate
/// fires in the interpreter, so it has to be the state the BODY runs under, not the one used to parse.
let private allowHarmfulOverridesTheHalt =
  cliTest "--allow-harmful runs a fn marked harmful, for both eval and run" (fun state ->
    task {
      let! _ =
        runCli
          state
          [ "fn"; "Tests.Harm.g"; "(x: Int64) : Int64 = Stdlib.Int64.add x 31L" ]

      let! _ =
        runCli
          state
          [ "deprecate"; "fn"; "Tests.Harm.g"; "--kind"; "harmful"; "-m"; "t"; "--yes" ]

      let! halted = runCli state [ "eval"; "Tests.Harm.g 0L" ]
      Expect.stringContains halted "Harmful" "it halts without the flag"

      let! allowed = runCli state [ "eval"; "Tests.Harm.g 0L"; "--allow-harmful" ]
      Expect.stringContains allowed "31" "and runs with it"
    })



/// `commit --include=` turns part of a draft into history: an unnamed item stays in the draft, a
/// named item's uncommitted dependency comes WITH it (a commit referencing uncommitted content
/// would be internally inconsistent), and a name the draft does not hold is refused.
let private partialCommitTakesOnlyWhatYouNamed =
  cliTest
    "commit --include= takes the named items plus their dependencies"
    (fun state ->
      task {
        let! _ = runCli state [ "switch"; "main" ]
        let! _ = runCli state [ "discard"; "--yes" ]

        let! _ =
          runCli
            state
            [ "fn"; "Tests.Pc.solo"; "(x: Int64) : Int64 = Stdlib.Int64.add x 1L" ]
        let! _ =
          runCli
            state
            [ "fn"; "Tests.Pc.base"; "(x: Int64) : Int64 = Stdlib.Int64.add x 2L" ]
        let! _ =
          runCli
            state
            [ "fn"; "Tests.Pc.user"; "(x: Int64) : Int64 = Tests.Pc.base x" ]

        let! refused =
          runCli state [ "commit"; "nope"; "--include=Tests.Pc.absent"; "-y" ]
        Expect.stringContains
          refused
          "nothing in your draft is named"
          "an unknown name is refused"

        let! stillThere = runCli state [ "status" ]
        Expect.stringContains
          stillThere
          "changed"
          "and the draft is untouched by the refusal"

        // Naming `user` has to bring `base`, which it references and which is still a draft.
        let! partial =
          runCli state [ "commit"; "user"; "--include=Tests.Pc.user"; "-y" ]
        Expect.stringContains partial "commit" "the selection was committed"
        Expect.stringContains
          partial
          "Tests.Pc.base"
          "and said which dependency it pulled in"

        // `solo` was never named, so it is still a draft.
        let! after = runCli state [ "status" ]
        Expect.stringContains after "1 item" "the unnamed item stayed in the draft"

        let! evalUser = runCli state [ "eval"; "Tests.Pc.user 0L" ]
        Expect.stringContains evalUser "2" "the committed item still evaluates"

        let! _ = runCli state [ "discard"; "--yes" ]
        return ()
      })


/// A branch verb takes the name you can see, not the id it resolves to: every branch has a uuid
/// behind it, and a verb handed the name printed by `dark branches` must not treat it as an id.
let private branchVerbsTakeTheNameYouSee =
  cliTest "every branch verb accepts the name the listing prints" (fun state ->
    task {
      let! _ = runCli state [ "switch"; "verbname" ]
      let! _ =
        runCli
          state
          [ "fn"; "Tests.Vn.one"; "(x: Int64) : Int64 = Stdlib.Int64.add x 1L" ]
      let! _ = runCli state [ "switch"; "main" ]

      let! listing = runCli state [ "branches" ]
      Expect.stringContains listing "verbname" "the listing prints the name"

      for verb in [ "diff"; "log"; "rebase" ] do
        let! out = runCli state [ verb; "verbname" ]
        Expect.isFalse
          (out.Contains "no branch")
          $"`dark {verb} verbname` resolves the name the listing just printed"

      let! preview = runCli state [ "conflicts"; "branch"; "verbname" ]
      Expect.isFalse
        (preview.Contains "no branch")
        "and so does the conflicts preview"

      let! bogus = runCli state [ "diff"; "notabranch" ]
      Expect.stringContains
        bogus
        "no branch"
        "a name we don't have is still an error"
    })


/// A review queue is a branch you name, so the verbs have to resolve it like any other: `review
/// import` stages under a minted id, and the typed queue name is not that id.
let private reviewQueueRoundTrips =
  cliTest
    "a review queue can be inspected and approved by the name you gave it"
    (fun state ->
      task {
        let! _ = runCli state [ "switch"; "rqsrc" ]
        let! _ =
          runCli
            state
            [ "fn"; "Tests.Rq.one"; "(x: Int64) : Int64 = Stdlib.Int64.add x 5L" ]
        let! _ = runCli state [ "switch"; "main" ]

        let path = $"{LibConfig.Config.runDir}/rq-test-bundle.json"
        let! _ = runCli state [ "sync"; "export"; path ]

        let! staged = runCli state [ "review"; "import"; path; "rqueue" ]
        Expect.isFalse
          (staged.Contains "no review queue")
          "the queue is created by name"

        let! shown = runCli state [ "review"; "rqueue" ]
        Expect.isFalse
          (shown.Contains "no review queue")
          "and is inspectable by that name"

        let! approved = runCli state [ "review"; "approve"; "rqueue" ]
        Expect.isFalse
          (approved.Contains "no review queue")
          "and approvable by it, which is the whole workflow"
      })


let private otherBranchAnswersStayCurrent =
  cliTest "asking about a branch you're not on gives a current answer" (fun state ->
    task {
      let! _ = runCli state [ "switch"; "cachebr" ]
      let! _ =
        runCli
          state
          [ "fn"; "Tests.Br.one"; "(x: Int64) : Int64 = Stdlib.Int64.add x 11L" ]

      let! _ = runCli state [ "switch"; "main" ]

      // Populates the memo of "ops for a branch I'm not on".
      let! first = runCli state [ "diff"; "cachebr" ]
      Expect.stringContains first "one" "the first item shows up"

      let! _ = runCli state [ "switch"; "cachebr" ]
      let! _ =
        runCli
          state
          [ "fn"; "Tests.Br.two"; "(x: Int64) : Int64 = Stdlib.Int64.add x 22L" ]

      let! _ = runCli state [ "switch"; "main" ]

      // The branch moved while we weren't on it, and a memo loaded once and never
      // dropped answers with the branch as it was -- right before you decide to merge.
      let! second = runCli state [ "diff"; "cachebr" ]
      Expect.stringContains second "two" "and so does what was added since"

      let! _ = runCli state [ "branch"; "archive"; "cachebr" ]
      ()
    })



/// A `record` call as an eval expression. The candidates are empty because nothing
/// here reads them; what is under test is which branch the row lands on.
let private recordConflictOn (branchId : string) (id : string) : string =
  $"""Darklang.SCM.Conflicts.record "{branchId}" [Darklang.SCM.Conflicts.Conflict {{ id = "{id}"; owner = "Zz"; modules = "Confl"; name = "f"; itemType = "fn"; kind = "same-name-different-hash"; candidates = []; autoResolvedTo = "bbb"; reason = "test"; status = "pending"; resolvedBy = "" }}]"""


/// A branch bundle is a UNIT: one op it cannot decode means none of it is imported, and the branch
/// is not registered either. Skip-and-log is right for the bulk sync path -- one op of thousands,
/// and it comes round again -- and wrong here: a branch arriving three ops short resolves
/// differently than on the machine that sent it, and nothing downstream can tell. Built from a REAL
/// export plus one undecodable record, since a wholly-corrupt bundle would pass under either rule.
let private branchBundleImportIsAllOrNothing =
  cliTest "one undecodable op means the whole branch bundle is refused" (fun state ->
    task {
      let! _ = runCli state [ "switch"; "bundlebr" ]
      let! sourceId = runCli state [ "eval"; "Builtin.scmCurrentBranch ()" ]
      let! _ =
        runCli
          state
          [ "fn"
            "Tests.Bundle.only"
            "(x: Int64) : Int64 = Stdlib.Int64.add x 1L" ]
      let! _ = runCli state [ "switch"; "main" ]

      let exported = $"{LibConfig.Config.runDir}/bundle-allornothing.json"
      let! _ = runCli state [ "branch"; "export"; "bundlebr"; exported ]
      let json = System.IO.File.ReadAllText exported

      // Retarget at a branch this store lacks, so "was it registered" is answerable.
      let freshId = "0e51f3a2-9c4d-4b7a-8f61-2d3e4c5b6a70"
      let retargeted =
        json.Replace(sourceId, freshId).Replace("bundlebr", "importedbr")

      // `blobHex` is decoded with FromHexString, so "zz" cannot parse. Appended rather
      // than substituted, so every real op in the bundle stays valid: the partial case.
      let bad =
        """,{"blobHex":"zz","id":"7c9e6679-7425-40de-944b-e07fc1f90ae7","ts":"2026-01-01T00:00:00.000Z"}"""
      // The serializer emits fields alphabetically, so `parent` follows `ops` and the
      // ops array does not end the document. Splice at the array's own close.
      let marker = """],"parent":"""
      let cut = retargeted.IndexOf marker
      Expect.isGreaterThan
        cut
        0
        "the exported bundle has an ops array followed by parent"
      let corrupted = retargeted.Substring(0, cut) + bad + retargeted.Substring(cut)

      let corruptPath = $"{LibConfig.Config.runDir}/bundle-allornothing-bad.json"
      System.IO.File.WriteAllText(corruptPath, corrupted)

      let! result = runCli state [ "branch"; "import"; corruptPath ]
      Expect.stringContains
        result
        "Nothing was imported"
        "the import is refused as a whole"

      // Decoding happens BEFORE the branch is registered, so a refused bundle leaves
      // no trace to clean up or be confused by.
      let! listed = runCli state [ "branch"; "list" ]
      Expect.isFalse
        (listed.Contains "importedbr")
        "and the branch it would have created does not exist"

      let! _ = runCli state [ "branch"; "archive"; "bundlebr" ]
      ()
    })


/// Whether a merge is ALLOWED is a decision, so it is decided in Dark; the builtin only does the work.
///
/// Two structural gates. Conflicts deliberately do NOT gate: they are auto-resolved by the fold's
/// LWW and recorded, because blocking teaches people to rebase reflexively. "Active" has to mean
/// the same in the check as in the message, which says to merge OR ARCHIVE the children --
/// counting archived children as active makes that advice a dead end.
let private mergeGatesAreDecidedInDark =
  cliTest
    "merge refuses an empty branch, and one with children until they are archived"
    (fun state ->
      task {
        let! _ = runCli state [ "switch"; "gateempty" ]
        let! _ = runCli state [ "switch"; "main" ]
        let! empty = runCli state [ "merge"; "gateempty" ]
        Expect.stringContains
          empty
          "nothing to merge"
          "an empty branch has nothing to give its parent"

        let! _ = runCli state [ "switch"; "gateparent" ]
        let! _ =
          runCli
            state
            [ "fn"; "Tests.Gate.one"; "(x: Int64) : Int64 = Stdlib.Int64.add x 6L" ]
        let! _ = runCli state [ "switch"; "gatechild" ]
        let! _ = runCli state [ "switch"; "main" ]

        let! blocked = runCli state [ "merge"; "gateparent" ]
        Expect.stringContains
          blocked
          "active children"
          "a parent cannot merge out from under its children"

        let! _ = runCli state [ "branch"; "archive"; "gatechild" ]
        let! merged = runCli state [ "merge"; "gateparent" ]
        Expect.stringContains
          merged
          "Merged"
          "and archiving the child clears the gate, rather than repeating the advice"

        let! _ = runCli state [ "branch"; "archive"; "gateempty" ]
        ()
      })


/// `diff` and `log` answer questions, so they answer in JSON too.
///
/// Both render a string with no record behind them, so this is a shape to design rather than a flag
/// to add. The flag is filtered out before the branch reference is read, so it can go on either
/// side: otherwise `dark diff --json foo` looks for a branch literally named "--json".
let private diffAndLogAnswerInJson =
  cliTest "diff and log answer in JSON, with the flag on either side" (fun state ->
    task {
      let! _ = runCli state [ "switch"; "jsonsurface" ]
      let! _ =
        runCli
          state
          [ "fn"; "Tests.JsonS.only"; "(x: Int64) : Int64 = Stdlib.Int64.add x 1L" ]

      // `log` on a branch is the op sequence, oldest first.
      let! logJson = runCli state [ "log"; "--json" ]
      Expect.stringContains
        logJson
        "\"seq\":0"
        "the branch's ops are numbered from the start"
      Expect.stringContains logJson "Tests.JsonS.only" "and name what they touched"

      let! _ = runCli state [ "switch"; "main" ]

      // `log` on main is the commit history, so it answers with what `commits --json` answers.
      let! mainLog = runCli state [ "log"; "--json" ]
      Expect.stringContains mainLog "\"hash\"" "on main it is the commit history"

      let! diffJson = runCli state [ "diff"; "jsonsurface"; "--json" ]
      Expect.stringContains
        diffJson
        "Tests.JsonS.only"
        "diff reports the changed name"
      Expect.stringContains diffJson "\"change\":\"new\"" "and classifies it"

      let! flagFirst = runCli state [ "diff"; "--json"; "jsonsurface" ]
      Expect.equal (flagFirst.Trim()) (diffJson.Trim()) "the flag is not positional"

      let! _ = runCli state [ "branch"; "archive"; "jsonsurface" ]
      ()
    })


/// A commit must not put a reference that cannot resolve into history: commits are what other
/// machines pull. A draft is ALLOWED to be unresolved while you work, though -- writing a caller
/// before its callee is ordinary, and `WipRefresh` re-resolves once the callee lands -- so the
/// forward reference has to commit cleanly.
let private commitRefusesUnresolvedReferences =
  cliTest
    "commit refuses a reference that never resolves, but not a forward one"
    (fun state ->
      task {
        let! _ = runCli state [ "switch"; "main" ]
        let! _ =
          runCli
            state
            [ "fn"
              "Tests.UnresT.bad"
              "(x: Int64) : Int64 = Tests.UnresT.missing x" ]

        let! refused = runCli state [ "commit"; "unresolved"; "-y" ]
        Expect.stringContains refused "don't resolve" "the commit is refused"
        Expect.stringContains
          refused
          "Tests.UnresT.missing"
          "and it names the reference it could not find, which is the whole point"

        // Live-on-write is deliberately permissive, so the gate has an escape hatch, and
        // it has to be typed: `-y` alone must not wave it through.
        let! allowed =
          runCli state [ "commit"; "unresolved"; "--allow-unresolved"; "-y" ]
        Expect.stringContains allowed "commit" "--allow-unresolved records it as-is"

        // A forward reference inside one draft: the caller is authored first and cannot
        // resolve yet, and re-resolution fixes it before commit ever looks.
        let! _ =
          runCli
            state
            [ "fn"
              "Tests.UnresT.caller"
              "(x: Int64) : Int64 = Tests.UnresT.callee x" ]
        let! _ =
          runCli
            state
            [ "fn"
              "Tests.UnresT.callee"
              "(x: Int64) : Int64 = Stdlib.Int64.add x 3L" ]

        let! forward = runCli state [ "commit"; "forward ref"; "-y" ]
        Expect.isFalse
          (forward.Contains "don't resolve")
          $"a forward reference that resolved is not refused, got: {forward}"
        ()
      })


/// `discard` is one verb over two implementations.
///
/// On main it drops the uncommitted ops and RE-FOLDS the store from what survives, since those ops
/// had already folded into `locations`. On a branch there is nothing to re-fold: branch ops are
/// `effective = 0` and never reached main's projections, so deleting the rows IS the removal.
///
/// That MAIN's draft survives has to be asserted on the OP LOG, not on whether main's names still
/// resolve: a branch discard does not re-fold, so deleted main ops would leave `locations` rows
/// outliving them and main's functions still answering. Resolution cannot see that; the op count can.
let private discardOnABranchLeavesMainAlone =
  cliTest
    "discard on a branch drops that branch's draft and leaves main's alone"
    (fun state ->
      task {
        // Something uncommitted on MAIN, which must survive the branch's discard.
        let! _ = runCli state [ "switch"; "main" ]

        // Measured as a DELTA, not an absolute: the store is shared with every other
        // cliTest, so whatever else is uncommitted on main is in this number too.
        let! baseline =
          runCli state [ "eval"; "Darklang.SCM.PackageOps.draftOpCount ()" ]

        let! _ =
          runCli
            state
            [ "fn"
              "Tests.Disc.onMain"
              "(x: Int64) : Int64 = Stdlib.Int64.add x 1L" ]

        // Fully qualified: `SCM.PackageOps.draftOpCount` does not resolve here and comes
        // back as an error STRING, so before-vs-after would compare two identical error
        // messages and pass whatever the code does.
        let! before =
          runCli state [ "eval"; "Darklang.SCM.PackageOps.draftOpCount ()" ]
        Expect.equal
          (int (before.Trim()))
          (int (baseline.Trim()) + 2)
          "authoring one fn on main added exactly its two ops (AddFn + SetName) to the draft"

        let! _ = runCli state [ "switch"; "discardbr" ]
        let! _ =
          runCli
            state
            [ "fn"
              "Tests.Disc.onBranch"
              "(x: Int64) : Int64 = Stdlib.Int64.add x 2L" ]
        let! live = runCli state [ "eval"; "Tests.Disc.onBranch 1L" ]
        Expect.stringContains live "3" "the branch item is live before the discard"

        let! out = runCli state [ "discard"; "-y" ]
        Expect.stringContains
          out
          "discardbr"
          "discard names the branch as its scope, not main"

        let! gone = runCli state [ "eval"; "Tests.Disc.onBranch 1L" ]
        Expect.isFalse (gone.Trim() = "3") $"the branch draft is gone, got: {gone}"

        let! _ = runCli state [ "switch"; "main" ]
        let! after =
          runCli state [ "eval"; "Darklang.SCM.PackageOps.draftOpCount ()" ]
        Expect.equal
          (after.Trim())
          (before.Trim())
          "main's uncommitted ops all survived the branch's discard"

        let! _ = runCli state [ "branch"; "archive"; "discardbr" ]
        ()
      })


let private conflictsBelongToTheBranchTheyHappenedOn =
  cliTest "a conflict is answered on the branch it happened on" (fun state ->
    task {
      let! _ = runCli state [ "switch"; "confbr" ]
      let! branchId = runCli state [ "eval"; "Builtin.scmCurrentBranch ()" ]
      Expect.isFalse (branchId = "") "switch put us on a real branch"

      let! _ = runCli state [ "eval"; recordConflictOn branchId "cnfbranch01" ]
      let! _ = runCli state [ "eval"; recordConflictOn "" "cnfmain0001" ]

      // Each branch has its own list. Dropping the branch filter from `pending`
      // shows both here.
      let! onBranch = runCli state [ "conflicts" ]
      Expect.stringContains
        onBranch
        "cnfbranch01"
        "the branch's conflict is on the branch's list"
      Expect.isFalse
        (onBranch.Contains "cnfmain0001")
        "and main's is not, because answering it from here would write into an overlay"

      // The payoff. A conflict is a property of the STORE, so an ack given on a branch
      // has to count everywhere rather than be refused for being branch-local.
      let! acked = runCli state [ "conflicts"; "ack"; "cnfbranch01" ]
      Expect.stringContains
        acked
        "acked"
        "a conflict can be answered from the branch it is on"

      // The ids you can act on are exactly the ids you were shown, so a lookup by
      // prefix is scoped too.
      let! crossBranch = runCli state [ "conflicts"; "ack"; "cnfmain0001" ]
      Expect.stringContains
        crossBranch
        "no conflict matching"
        "an id copied from main's list does nothing here"

      let! _ = runCli state [ "switch"; "main" ]
      let! onMain = runCli state [ "conflicts" ]
      Expect.stringContains
        onMain
        "cnfmain0001"
        "main still has its own, unanswered"
      Expect.isFalse
        (onMain.Contains "cnfbranch01")
        "and the branch's, which is now acked, is gone from both"

      // Answered, so this test leaves no pending row behind for whatever reads the store next.
      let! _ = runCli state [ "conflicts"; "ack"; "cnfmain0001" ]
      let! _ = runCli state [ "branch"; "archive"; "confbr" ]
      ()
    })


let private branchItemsArePolicyTargets =
  cliTest "a policy verb can name an item that only exists on a branch" (fun state ->
    task {
      let! _ = runCli state [ "switch"; "polbr" ]
      let! _ =
        runCli
          state
          [ "fn"; "Tests.Pol.only"; "(x: Int64) : Int64 = Stdlib.Int64.add x 5L" ]

      // `locations` is main's projection and a branch's SetNames never fold into it, so
      // a read that goes only to that table answers about MAIN while you're on a branch.
      let! pinned =
        runCli state [ "propagate"; "pin"; "Tests.Pol.only"; "on the branch" ]
      Expect.stringContains pinned "pinned" "the branch item is a valid target"

      let! onBranch = runCli state [ "propagate" ]
      Expect.stringContains
        onBranch
        "Tests.Pol.only"
        "and the choice is visible from the branch"

      let! _ = runCli state [ "switch"; "main" ]
      let! onMain = runCli state [ "propagate" ]
      Expect.isFalse
        (onMain.Contains "Tests.Pol.only")
        "and stays branch-local, like every other branch decision"

      let! _ = runCli state [ "branch"; "archive"; "polbr" ]
      ()
    })


let tests =
  testSequenced
  <| testList
    "CliTraces"
    [ // Tracing is ON for every test below, because most of them are about the trace surface itself.
      // It records each call's ARGUMENTS, so an `eval` here pays to write whatever it materialises: a
      // page of sync ops is 2000 records carrying hex-encoded blobs, which takes this list from four
      // minutes to over nine. Assert on counts and identifiers here, not op bodies; anything needing
      // real blobs belongs in a suite that does not trace, like `MultiInstance`.
      test "set trace detail" {
        LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.On
      }
      // Base CLI commands
      testHelpCommand
      everyCommandAnswersHelp
      workbenchViewsRender
      everyScmSectionRenders
      showingACommitDoesNotFetchEveryOp
      workbenchBranchActionsWork
      mergeAndRebaseRefuseOnMain
      contextRowKeepsTheDraftWhenNarrow
      hintRowKeepsTheWayOut
      workbenchHandlesTerminalSizes
      workbenchContextRowSaysWhereYouAre
      everyCommandSurvivesABogusArgument
      missingTargetsAreNamed
      documentedCommandsAreReal
      deprecationIsReversible
      theWorkedExampleWorks
      editsAreVisibleInTheSameProcess
      deprecationTakesEffectInTheSameProcess
      allowHarmfulOverridesTheHalt
      otherBranchAnswersStayCurrent
      branchVerbsTakeTheNameYouSee
      partialCommitTakesOnlyWhatYouNamed
      reviewQueueRoundTrips
      branchItemsArePolicyTargets
      conflictsBelongToTheBranchTheyHappenedOn
      discardOnABranchLeavesMainAlone
      commitRefusesUnresolvedReferences
      diffAndLogAnswerInJson
      mergeGatesAreDecidedInDark
      branchBundleImportIsAllOrNothing
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
      testTracesFindEscapesLikeWildcards
      testTracesTruncatedStillShowsRoot ]
