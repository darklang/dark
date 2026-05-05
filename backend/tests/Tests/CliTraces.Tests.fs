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
    let program : RT.Program = { accountID = System.Guid.Empty; dbs = Map.empty }

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
let private runCli
  (state : RT.ExecutionState)
  (args : string list)
  : Task<string> =
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
      let! result =
        Exe.executeFunction state fnName [] (NEList.singleton argsDval)
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


// ─── helpers for each test ────────────────────────────────────────────────

let private withState (f : RT.ExecutionState -> Task<unit>) : Task<unit> =
  task {
    let! state = buildState ()
    do! f state
  }


// ─── Base CLI command tests ───────────────────────────────────────────────

let private testHelpCommand =
  testTask "help command" {
    do! withState (fun state ->
      task {
        let! output = runCli state [ "help" ]
        System.Console.Error.WriteLine($"[DEBUG help] captured: {output}")
        Expect.stringContains output "Packages:" "category header"
        Expect.stringContains output "SCM:" "SCM header"
        Expect.stringContains output "help" "help command"
        Expect.stringContains output "version" "version command"
        Expect.stringContains output "status" "status command"
      })
  }

let private testVersionCommand =
  testTask "version command" {
    do! withState (fun state ->
      task {
        let! output = runCli state [ "version" ]
        Expect.stringContains output "Darklang CLI" "CLI banner"
        Expect.stringContains output "alpha-" "version prefix"
      })
  }

let private testStatusCommand =
  testTask "status command" {
    do! withState (fun state ->
      task {
        let! output = runCli state [ "status" ]
        Expect.stringContains output "On branch" "branch line"
        Expect.isTrue
          (output.Contains("No uncommitted changes.")
           || output.Contains("Uncommitted changes:"))
          "uncommitted-changes summary"
      })
  }

let private testRunBoolAnd =
  testTask "run Bool.and" {
    do! withState (fun state ->
      task {
        let! output = runCli state [ "run"; "@Stdlib.Bool.and true false" ]
        Expect.equal output "false" "Bool.and result"
      })
  }

let private testRunInt64Add =
  testTask "run Int64.add" {
    do! withState (fun state ->
      task {
        let! output = runCli state [ "run"; "@Stdlib.Int64.add 5L 3L" ]
        Expect.equal output "8" "Int64.add result"
      })
  }

let private testEvalStringLength =
  testTask "eval String.length" {
    do! withState (fun state ->
      task {
        let! output =
          runCli state [ "eval"; "Stdlib.String.length \"hello\"" ]
        Expect.equal output "5" "String.length result"
      })
  }

let private testEvalListLength =
  testTask "eval List.length" {
    do! withState (fun state ->
      task {
        let! output =
          runCli state [ "eval"; "[1L; 2L; 3L] |> Stdlib.List.length" ]
        Expect.equal output "3" "List.length result"
      })
  }

let private testListFunctions =
  testTask "ls Stdlib.List" {
    do! withState (fun state ->
      task {
        let! output = runCli state [ "ls"; "Stdlib.List" ]
        Expect.stringContains output "Functions" "section"
        Expect.stringContains output "head" "head fn"
      })
  }

let private testViewFunction =
  testTask "view Stdlib.List.head" {
    do! withState (fun state ->
      task {
        let! output = runCli state [ "view"; "Stdlib.List.head" ]
        Expect.stringContains output "head" "fn name"
        Expect.stringContains output "Option" "Option in signature"
        Expect.stringContains output "->" "fn signature arrow"
      })
  }

let private testListTypes =
  testTask "ls Stdlib.Option" {
    do! withState (fun state ->
      task {
        let! output = runCli state [ "ls"; "Stdlib.Option" ]
        Expect.stringContains output "Types" "section"
        Expect.stringContains output "Option" "Option type"
      })
  }

let private testHelpForRun =
  testTask "help run" {
    do! withState (fun state ->
      task {
        let! output = runCli state [ "help"; "run" ]
        Expect.stringContains output "run" "command name"
        Expect.isTrue
          (output.Contains("function") || output.Contains("execute"))
          "run-command description"
      })
  }

let private testHelpForLs =
  testTask "help ls" {
    do! withState (fun state ->
      task {
        let! output = runCli state [ "help"; "ls" ]
        Expect.stringContains output "ls" "command name"
        Expect.isTrue
          (output.Contains("list") || output.Contains("List"))
          "ls description"
      })
  }

let private testEvalSimpleExpression =
  testTask "eval simple expression" {
    do! withState (fun state ->
      task {
        let! output = runCli state [ "eval"; "2L + 3L" ]
        Expect.equal output "5" "2L + 3L result"
      })
  }

let private testEvalStringExpression =
  testTask "eval string concat" {
    do! withState (fun state ->
      task {
        let! output = runCli state [ "eval"; "\"hello\" ++ \"world\"" ]
        Expect.equal output "helloworld" "string concat result"
      })
  }


// ─── Trace surface tests ──────────────────────────────────────────────────

let private testTracesHelp =
  testTask "traces help lists subcommand surface" {
    do! withState (fun state ->
      task {
        let! output = runCli state [ "traces"; "help" ]
        for term in
          [ "list"; "view"; "tail"; "follow"; "find"; "hotspots"; "inspect"
            "gen-test"; "values"; "export"; "import"; "replay"; "delete"
            "prune"; "clear"; "--json"; "--with-trace" ] do
          Expect.stringContains output term $"contains {term}"
      })
  }

let private testTracesTailShowsLastEval =
  testTask "traces tail shows last eval" {
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "traces"; "clear" ]
        let! _ = runCli state [ "eval"; "let x = 7L\nx" ]
        let! output = runCli state [ "traces"; "tail" ]
        Expect.stringContains output "Handler: eval" "eval handler line"
        Expect.stringContains output "expression = \"let x = 7L"
                              "recorded input"
      })
  }

let private testTracesClearEmpties =
  testTask "traces clear empties (locks d2591e14c leak fix)" {
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "eval"; "1L + 2L" ]
        let! _ = runCli state [ "traces"; "clear" ]
        let! listOut = runCli state [ "traces"; "list" ]
        Expect.stringContains listOut "No traces found." "post-clear state"
      })
  }

let private testTracesClearAlsoClearsExprValues =
  testTask "traces clear also drops trace_expr_values rows" {
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "traces"; "clear" ]
        let! _ = runCli state [ "eval"; "let x = 99L\nx" ]
        let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
        let tid = parseTraceID listJson

        // Sanity: pre-clear, the trace has the let value recorded.
        let! beforeOut = runCli state [ "traces"; "values"; tid ]
        Expect.stringContains beforeOut "→  99"
                              "let value present pre-clear"

        let! _ = runCli state [ "traces"; "clear" ]
        let! afterOut = runCli state [ "traces"; "values"; tid ]
        // Post-clear: prefix resolution should fail OR empty-state msg;
        // either way `→  99` should not be present.
        Expect.isFalse
          (afterOut.Contains "→  99")
          "expr_values orphaned after clear"
      })
  }

let private testTracesStatsCounts =
  testTask "traces stats shows counts" {
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "traces"; "clear" ]
        let! _ = runCli state [ "eval"; "1L" ]
        let! _ = runCli state [ "eval"; "2L" ]
        let! output = runCli state [ "traces"; "stats" ]
        Expect.stringContains output "total ms" "table header"
        Expect.stringContains output "count" "count column"
        Expect.stringContains output "│ eval" "eval row"
      })
  }

let private testTracesExportJson =
  testTask "traces export <id> JSON schema" {
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "traces"; "clear" ]
        let! _ = runCli state [ "eval"; "let x = 5L\nx" ]
        let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
        let tid = parseTraceID listJson
        let! exportOut = runCli state [ "traces"; "export"; tid ]
        for field in
          [ "\"id\""; "\"handler_desc\""; "\"timestamp\""
            "\"input_value\""; "\"fn_calls\""; "\"expr_values\"" ] do
          Expect.stringContains exportOut field $"export has {field}"
      })
  }

let private testTracesFindByContent =
  testTask "traces find <pattern> by content" {
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "traces"; "clear" ]
        let! _ = runCli state [ "eval"; "\"unique-token-xyz12345\"" ]
        let! _ = runCli state [ "eval"; "1L + 1L" ]
        let! output =
          runCli state [ "traces"; "find"; "unique-token-xyz12345" ]
        Expect.stringContains output "Traces matching" "find banner"
        Expect.stringContains output "eval" "eval handler"
      })
  }

let private testTracesDeleteSingle =
  testTask "traces delete <id> + cascade preserves siblings" {
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "traces"; "clear" ]
        let! _ = runCli state [ "eval"; "let alpha = 11L\nalpha" ]
        let! _ = runCli state [ "eval"; "let beta = 22L\nbeta" ]
        // Latest = beta. Delete it; alpha should remain with values.
        let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
        let betaTid = parseTraceID listJson
        let! delOut = runCli state [ "traces"; "delete"; betaTid ]
        Expect.stringContains delOut "Deleted trace" "delete confirm"

        let! listJson2 = runCli state [ "traces"; "list"; "1"; "--json" ]
        let alphaTid = parseTraceID listJson2
        let! valuesOut = runCli state [ "traces"; "values"; alphaTid ]
        Expect.stringContains valuesOut "→  11" "alpha's values intact"
      })
  }

let private testViewWithTraceAnnotates =
  testTask "view <fn> --with-trace <id> annotates" {
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "traces"; "clear" ]
        let! _ = runCli state [ "eval"; "Stdlib.Int64.clamp 5L 1L 10L" ]
        let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
        let tid = parseTraceID listJson
        let! output =
          runCli state [ "view"; "Stdlib.Int64.clamp"; "--with-trace"; tid ]
        Expect.stringContains output "annotated with" "annotation banner"
        Expect.isFalse
          (output.Contains "annotated with 0 values")
          "non-zero annotations"
        Expect.stringContains output "// = " "inline annotation marker"
      })
  }

let private testTracesPruneKeep =
  testTask "traces prune --keep N + cascade" {
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "traces"; "clear" ]
        let! _ = runCli state [ "eval"; "let a = 1L\na" ]
        let! _ = runCli state [ "eval"; "let b = 2L\nb" ]
        let! _ = runCli state [ "eval"; "let c = 3L\nc" ]
        let! pruneOut =
          runCli state [ "traces"; "prune"; "--keep"; "1" ]
        Expect.stringContains pruneOut "Pruned 2 trace" "prune confirm"

        let! listOut = runCli state [ "traces"; "list" ]
        Expect.stringContains listOut "Recent traces (last 20):" "list banner"
        Expect.isFalse (listOut.Contains "let a") "a pruned"
        Expect.isFalse (listOut.Contains "let b") "b pruned"

        let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
        let tid = parseTraceID listJson
        let! valuesOut = runCli state [ "traces"; "values"; tid ]
        Expect.stringContains valuesOut "→  3" "kept trace's values intact"
      })
  }

let private testTracesReplayDiffMatches =
  testTask "traces replay <id> --diff matches recorded output" {
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "traces"; "clear" ]
        let! _ = runCli state [ "eval"; "1L + 2L" ]
        let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
        let tid = parseTraceID listJson
        let! output = runCli state [ "traces"; "replay"; tid; "--diff" ]
        Expect.stringContains output "Original output: 3" "original"
        Expect.stringContains output "New output: 3" "new"
        Expect.stringContains output "unchanged" "diff result"
      })
  }

let private testTracesInspectErrorsOnBuiltinHandler =
  testTask "traces inspect <eval-id> errors on builtin handler" {
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "traces"; "clear" ]
        let! _ = runCli state [ "eval"; "1L + 2L" ]
        let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
        let tid = parseTraceID listJson
        let! output = runCli state [ "traces"; "inspect"; tid ]
        Expect.stringContains output "is a builtin" "builtin guard"
        Expect.stringContains output "no source to view" "explanation"
      })
  }

let private testTracesValuesAfterLet =
  testTask "traces values <id> lists (exprId, dval) pairs after let" {
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "traces"; "clear" ]
        let! _ = runCli state [ "eval"; "let x = 1L + 2L\nx" ]
        let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
        let tid = parseTraceID listJson
        let! valuesOut = runCli state [ "traces"; "values"; tid ]
        Expect.stringContains valuesOut "→  3" "let RHS value recorded"
      })
  }

let private testTracesRejectsNegativeLimit =
  testTask "negative limit rejected across commands" {
    do! withState (fun state ->
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
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "traces"; "clear" ]
        let! _ = runCli state [ "eval"; "1L + 1L" ]
        let! tailNFirst =
          runCli state [ "traces"; "tail"; "1"; "--route"; "eval" ]
        Expect.stringContains tailNFirst "Trace:" "tail N --route"
        let! tailRouteFirst =
          runCli state [ "traces"; "tail"; "--route"; "eval"; "1" ]
        Expect.stringContains tailRouteFirst "Trace:" "tail --route N"
        let! listJsonFn =
          runCli
            state
            [ "traces"; "list"; "--json"; "--fn"; "add"; "5" ]
        Expect.stringContains listJsonFn "[" "list --json --fn fn N"
      })
  }

let private testTracesFindEscapesLikeWildcards =
  testTask "find escapes SQL LIKE wildcards" {
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "traces"; "clear" ]
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
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "traces"; "clear" ]
        let! _ = runCli state [ "eval"; "1L + 1L" ]
        let cases =
          [ [ "traces"; "tail"; "--route"; "" ],
            "--route pattern must not be empty"
            [ "traces"; "tail"; "--route"; "   " ],
            "--route pattern must not be empty"
            [ "traces"; "list"; "--route"; "" ],
            "--route pattern must not be empty"
            [ "traces"; "list"; "--fn"; "   " ],
            "--fn pattern must not be empty" ]
        for (argv, expected) in cases do
          let! out = runCli state argv
          Expect.stringContains out expected $"{argv} rejected"
      })
  }

let private testTracesArity1Catchalls =
  testTask "arity-1 traces commands print focused usage on extra args" {
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "traces"; "clear" ]
        let! _ = runCli state [ "eval"; "1L + 1L" ]
        let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
        let tid = parseTraceID listJson

        let cases =
          [ [ "traces"; "export"; tid; "--frob" ],
            "Usage: traces export <trace-id>"
            [ "traces"; "inspect"; tid; "--frob" ],
            "Usage: traces inspect <trace-id>"
            [ "traces"; "values"; tid; "--frob" ],
            "Usage: traces values <trace-id>"
            [ "traces"; "delete"; tid; "--frob" ],
            "Usage: traces delete <trace-id>" ]
        for (argv, expected) in cases do
          let! out = runCli state argv
          Expect.stringContains out expected $"{argv} catch-all"
      })
  }

let private testTracesStatsHintHiddenForEvalOnly =
  testTask "stats footer hides --route hint when no HTTP traces" {
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "traces"; "clear" ]
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
    do! withState (fun state ->
      task {
        let! typoOut = runCli state [ "traces"; "nonsense" ]
        Expect.stringContains typoOut "Unknown subcommand: nonsense"
                              "typo flagged"
        let! typoTwoOut = runCli state [ "traces"; "lst" ]
        Expect.stringContains typoTwoOut "Unknown subcommand: lst"
                              "lst flagged"

        let! bareOut = runCli state [ "traces" ]
        Expect.isFalse
          (bareOut.Contains "Unknown subcommand")
          "bare not flagged"
        let! helpOut = runCli state [ "traces"; "help" ]
        Expect.isFalse
          (helpOut.Contains "Unknown subcommand")
          "help not flagged"
      })
  }

let private testTracesFiltersAreCaseInsensitive =
  testTask "list --route is case-insensitive" {
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "traces"; "clear" ]
        let! _ = runCli state [ "eval"; "1L + 1L" ]
        let! listLower =
          runCli state [ "traces"; "list"; "--route"; "eval" ]
        Expect.stringContains listLower "eval" "lower matches"
        let! listUpper =
          runCli state [ "traces"; "list"; "--route"; "EVAL" ]
        Expect.stringContains listUpper "eval" "upper matches"
        Expect.isFalse
          (listUpper.Contains "No traces found")
          "upper still finds"
        let! listMixed =
          runCli state [ "traces"; "list"; "--route"; "Eval" ]
        Expect.stringContains listMixed "eval" "mixed matches"
      })
  }

let private testTracesRejectsEmptyPattern =
  testTask "find / list --fn / list --route reject empty pattern" {
    do! withState (fun state ->
      task {
        let cases =
          [ [ "traces"; "find"; "" ], "find pattern must not be empty"
            [ "traces"; "find"; ""; "--view" ],
            "find pattern must not be empty"
            [ "traces"; "find"; ""; "--json" ],
            "find pattern must not be empty"
            [ "traces"; "list"; "--fn"; "" ],
            "--fn pattern must not be empty"
            [ "traces"; "list"; "--route"; "" ],
            "--route pattern must not be empty" ]
        for (argv, expected) in cases do
          let! out = runCli state argv
          Expect.stringContains out expected $"{argv} rejected"
      })
  }

let private testTracesViewRejectsNegativeSubOptions =
  testTask "view --depth/--slow-ms reject negative" {
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "traces"; "clear" ]
        let! _ = runCli state [ "eval"; "1L + 1L" ]
        let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
        let tid = parseTraceID listJson

        let! depthOut =
          runCli state [ "traces"; "view"; tid; "--depth"; "-1" ]
        Expect.stringContains depthOut "--depth must be ≥ 0" "depth -1"
        let! slowOut =
          runCli state [ "traces"; "view"; tid; "--slow-ms"; "-1" ]
        Expect.stringContains slowOut "--slow-ms must be ≥ 0" "slow-ms -1"
      })
  }

let private testTracesClearAndPruneGrammar =
  testTask "clear/prune singular vs plural phrasing" {
    do! withState (fun state ->
      task {
        let! _ = runCli state [ "traces"; "clear" ]
        let! _ = runCli state [ "eval"; "1L + 1L" ]
        let! clearOne = runCli state [ "traces"; "clear" ]
        let! _ = runCli state [ "eval"; "1L + 1L" ]
        let! _ = runCli state [ "eval"; "2L + 2L" ]
        let! clearTwo = runCli state [ "traces"; "clear" ]
        let! _ = runCli state [ "eval"; "1L + 1L" ]
        let! _ = runCli state [ "eval"; "2L + 2L" ]
        let! pruneNone = runCli state [ "traces"; "prune"; "--keep"; "0" ]
        let! _ = runCli state [ "eval"; "3L + 3L" ]
        let! _ = runCli state [ "eval"; "4L + 4L" ]
        let! pruneOne = runCli state [ "traces"; "prune"; "--keep"; "1" ]

        Expect.stringContains clearOne "Cleared 1 trace." "singular"
        Expect.stringContains clearTwo "Cleared 2 traces." "plural"
        Expect.stringContains pruneNone "none kept" "prune --keep 0"
        Expect.stringContains pruneOne "kept the most-recent"
                              "prune --keep 1"
      })
  }

let private testTracesRejectsFlagAsTraceId =
  testTask "flag-shaped trace-id input rejected as flag" {
    do! withState (fun state ->
      task {
        let cmds =
          [ [ "traces"; "export"; "--frob" ]
            [ "traces"; "inspect"; "--frob" ]
            [ "traces"; "values"; "--frob" ]
            [ "traces"; "replay"; "--frob" ]
            [ "traces"; "delete"; "--frob" ]
            [ "traces"; "gen-test"; "--frob" ] ]
        for argv in cmds do
          let! out = runCli state argv
          Expect.stringContains out "Unknown flag: --frob"
                                $"{argv} rejected"
      })
  }


// Tests use `Console.SetOut` to capture stdout in-process. Console.Out is
// process-global, so running these in parallel would let captures bleed
// across tests. `testSequenced` forces Expecto to run them one at a time.
let tests =
  testSequenced
  <| testList
    "CliTraces"
    [ // Base CLI commands
      testHelpCommand
      testVersionCommand
      testStatusCommand
      testRunBoolAnd
      testRunInt64Add
      testEvalStringLength
      testEvalListLength
      testListFunctions
      testViewFunction
      testListTypes
      testHelpForRun
      testHelpForLs
      testEvalSimpleExpression
      testEvalStringExpression
      // Trace surface
      testTracesHelp
      testTracesTailShowsLastEval
      testTracesValuesAfterLet
      testTracesClearEmpties
      testTracesClearAlsoClearsExprValues
      testTracesStatsCounts
      testTracesExportJson
      testTracesFindByContent
      testTracesDeleteSingle
      testViewWithTraceAnnotates
      testTracesPruneKeep
      testTracesReplayDiffMatches
      testTracesRejectsNegativeLimit
      testTracesRejectsFlagAsTraceId
      testTracesClearAndPruneGrammar
      testTracesViewRejectsNegativeSubOptions
      testTracesRejectsEmptyPattern
      testTracesFiltersAreCaseInsensitive
      testTracesUnknownSubcommandSurfaced
      testTracesStatsHintHiddenForEvalOnly
      testTracesArgOrderingsWork
      testTracesArity1Catchalls
      testTracesRouteEmptyRejection
      testTracesFindEscapesLikeWildcards
      testTracesInspectErrorsOnBuiltinHandler ]
