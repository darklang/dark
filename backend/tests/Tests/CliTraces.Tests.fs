/// The CLI's eval/run surface and the trace store behind it, plus the single
/// composition point for all three CLI integration suites: `tests` below is the one
/// sequenced list the runner sees, in an order that is deliberate (trace detail is
/// flipped on by the first entry, and the slow sweeps run last).
module Tests.CliTraces

open Expecto
open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Fumble
open LibDB.Sqlite

module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module Dval = LibExecution.Dval

open TestUtils.TestUtils

open Tests.CliTestHarness

/// `--local`, so the suite does not depend on reaching GitHub.
///
/// Bare `version` checks for a newer release, and against a firewall that drops rather
/// than refuses it waits ~11s before giving up. What this test is about is the version
/// the binary reports, which `--local` answers from the binary alone.
let private testVersionCommand =
  cliTest "version command" (fun state ->
    task {
      let! output = runCli state [ "version"; "--local" ]
      Expect.stringContains output "Darklang CLI" "CLI banner"
      Expect.stringContains output "alpha-" "version prefix"
      Expect.isFalse
        (output.Contains "update available" || output.Contains "unable to check")
        "and says nothing about updates, since it did not look"

      // A near miss must not silently fall through to the network check, which is the
      // one thing the caller was trying to avoid.
      let! typo = runCli state [ "version"; "--locl" ]
      Expect.stringContains typo "unknown option" "a mistyped flag is named"
      Expect.isFalse (typo.Contains "update available") "and nothing was fetched"
    })

let private testStatusCommand =
  cliTest "status command" (fun state ->
    task {
      let! output = runCli state [ "status" ]
      // Which of "clean: ..." / "draft: N items changed" is left depends on what ran before.
      Expect.isTrue
        (output.Contains("clean:") || output.Contains("draft:"))
        $"status says whether there's uncommitted work, got: {output}"
      // `status` names the branch ALWAYS, main included. "Where am I" is the question `status` is
      // for, and an answer that is silent exactly half the time -- on the default branch, where most
      // people are most of the time -- does not answer it.
      Expect.isTrue
        (output.Contains("On branch"))
        $"status names the branch it is on, main included, got: {output}"
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
       cliTest label (fun state ->
         task {
           let! output = runCli state args
           Expect.equal output expected label
         })))

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
      "List.length", [ "eval"; "[1L, 2L, 3L] |> Stdlib.List.length" ], "3"
      "simple expr", [ "eval"; "2L + 3L" ], "5"
      "string concat", [ "eval"; "\"hello\" ++ \"world\"" ], "helloworld" ]

// ─── Script declaration identity ────────────────────────────────────

/// A script's own types, values and fns are grafted into the package manager
/// keyed by content hash, so two declarations that hash the same collapse into
/// one and calls to either reach whichever survived.
///
/// Collapsing is correct when the declarations really are identical, and wrong
/// when they only look identical because they were hashed before their
/// references resolved: an unresolved reference carries no name, so
/// `f (r: TA) = 7` and `f (r: TB) = 7` serialise the same.
let private testScriptDeclIdentity =
  testCliEquals
    "script declaration identity"
    [ // Both fns are byte-identical apart from a parameter type that is still
      // unresolved when the first pass runs. They must stay two functions.
      "distinct types behind unresolved refs",
      [ "eval"
        "type TA = { a: String }\n\
         type TB = { b: Int }\n\
         let takesA (r: TA) : Int = 7\n\
         let takesB (r: TB) : Int = 7\n\
         (takesA (TA { a = \"x\" })) + (takesB (TB { b = 1 }))" ],
      "14"

      // The other direction: genuinely identical declarations SHOULD share one
      // hash. Collapsing them is content addressing working, not a bug.
      "identical fns share one hash",
      [ "eval"
        "let alpha (x: Int) : Int = x + 1\n\
         let beta (x: Int) : Int = x + 1\n\
         (alpha 1) + (beta 1)" ],
      "4"

      // Hashing after resolution means the hash graph can contain cycles, so
      // mutually recursive declarations have to be hashed as a batch.
      "mutually recursive fns",
      [ "eval"
        "let isEven (n: Int) : Bool = if n == 0 then true else isOdd (n - 1)\n\
         let isOdd (n: Int) : Bool = if n == 0 then false else isEven (n - 1)\n\
         isEven 10" ],
      "true"

      // Content addressing reaches across the script/package boundary: a script
      // type with the same shape as a package type IS that type. This is what a
      // location-keyed identity scheme would have cost.
      "script type unifies with package type",
      [ "eval"
        "type MyErr = | BadFormat\n\
         let f (e: Darklang.Stdlib.Int.ParseError) : Int = 1\n\
         f (MyErr.BadFormat)" ],
      "1" ]


// ─── Runtime error rendering ────────────────────────────────────────

/// A type mismatch against a package declaration names the function, the
/// parameter and both types. Hashes appearing here instead of names is the
/// failure mode that hid a declaration-collision bug for a whole release: the
/// message named two hashes, so it read as a type mismatch rather than as the
/// wrong function being called.
let private testRteNamesPackageDecls =
  cliTest "RTE names package declarations" (fun state ->
    task {
      let! output = runCli state [ "eval"; "Stdlib.List.length \"not a list\"" ]
      Expect.stringContains
        output
        "Darklang.Stdlib.List.length"
        "fn named, not hashed"
      Expect.stringContains output "1st parameter `list`" "parameter named"
      Expect.stringContains output "expects List<_>" "expected type named"
      Expect.stringContains output "but got String" "actual type named"
    })

/// The same message for a script's own declarations. These are never in the
/// store, and the CLI renders the error after the executor holding them is gone,
/// so the pretty-printer's hash-to-name lookup has nothing to find unless the
/// script's names are carried to it. Missing, it prints 64-character hashes, and
/// a declaration collision then reads as an ordinary type mismatch.
let private testRteNamesScriptDecls =
  cliTest "RTE names script declarations" (fun state ->
    task {
      let! output =
        runCli
          state
          [ "eval"
            "type Celsius = { degrees: Int }\n\
             type Fahrenheit = { degrees: Float }\n\
             let describe (t: Celsius) : String = \"ok\"\n\
             describe (Fahrenheit { degrees = 1.0 })" ]
      Expect.stringContains output "describe's 1st parameter `t`" "fn named"
      Expect.stringContains output "expects Celsius" "expected type named"
      Expect.stringContains output "but got Fahrenheit" "actual type named"
      // Bare, not `CliScript.Celsius`: the owner is scaffolding the parser
      // stamped on, and no name can reach the declaration through it.
      Expect.isFalse (output.Contains "CliScript.") "no scaffolding owner"
    })

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

/// A bottom-up `mkdir -p` must not name an existing common ancestor of its
/// target and the protected policy directory.
let private testMkdirRecursiveUnderPolicyAncestor =
  cliTest "mkdir -p under an ancestor of the policy directory" (fun state ->
    task {
      let root =
        System.IO.Path.Combine(
          System.IO.Path.GetTempPath(),
          $"dark-mkdirp-test-{System.Guid.NewGuid()}"
        )
      let policy = System.IO.Path.Combine(root, "policy")
      let target = System.IO.Path.Combine(root, "work", "a", "b")
      System.IO.Directory.CreateDirectory root |> ignore<System.IO.DirectoryInfo>
      let restorePolicyDirectory =
        LibExecution.HostSecurity.policyDirectoryForTesting policy
      let! found =
        LibDB.ProgramTypes.Fn.find
          [ PT.mainBranchId ]
          { owner = "Darklang"
            modules = [ "Stdlib"; "Cli"; "Dir" ]
            name = "createRecursive" }
        |> Ply.toTask
      let hash =
        match found with
        | Some(PT.Hash h) -> h
        | None -> Tests.failtestf "Stdlib.Cli.Dir.createRecursive not found"
      let createRecursive () =
        Exe.executeFunction
          state
          (RT.FQFnName.fqPackage hash)
          []
          (NEList.singleton (RT.DString target))
      let expectOk (label : string) (result : RT.ExecutionResult) =
        match result with
        | Ok(RT.DEnum(_, _, _, "Ok", _)) -> ()
        | other -> Tests.failtestf "%s: expected Ok, got %A" label other
      try
        let! first = createRecursive ()
        expectOk "mkdir -p of missing levels beside the policy directory" first
        Expect.isTrue
          (System.IO.Directory.Exists target)
          "missing levels were created"
        let! again = createRecursive ()
        expectOk "mkdir -p on an existing directory" again
      finally
        restorePolicyDirectory.Dispose()
        if System.IO.Directory.Exists root then
          System.IO.Directory.Delete(root, true)
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
  cliTestWithFreshTraces "traces delete <id> preserves siblings" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "1L + 1L" ]
      let! _ = runCli state [ "eval"; "2L + 2L" ]

      let! latestJson = runCli state [ "traces"; "list"; "1"; "--json" ]
      let latestTid = parseTraceID latestJson

      let! delOut = runCli state [ "traces"; "delete"; latestTid; "--yes" ]
      Expect.stringContains delOut "Deleted trace" "delete confirm"

      let! listAfter = runCli state [ "traces"; "list" ]
      Expect.isFalse
        (listAfter.Contains latestTid)
        "deleted trace ID gone from list"
    })

let private testTracesPruneKeep =
  cliTestWithFreshTraces "traces prune --keep N keeps the most-recent" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "1L" ]
      let! _ = runCli state [ "eval"; "2L" ]
      let! _ = runCli state [ "eval"; "3L" ]

      let! latestJson = runCli state [ "traces"; "list"; "1"; "--json" ]
      let latestTid = parseTraceID latestJson

      let! pruneOut = runCli state [ "traces"; "delete"; "--keep"; "1"; "--yes" ]
      Expect.stringContains pruneOut "Pruned 2 trace" "prune confirm"

      let! listOut = runCli state [ "traces"; "list" ]
      Expect.stringContains listOut "Recent traces (last 20):" "list banner"
      Expect.stringContains listOut latestTid "latest trace kept"
    })

let private testTracesRejectsNegativeLimit =
  cliTest "negative limit rejected across commands" (fun state ->
    task {
      for argv in
        [ [ "traces"; "list"; "-1" ]
          [ "traces"; "stats"; "-1" ]
          [ "traces"; "hotspots"; "-1" ]
          [ "traces"; "find"; "foo"; "-1" ] ] do
        let! out = runCli state argv
        Expect.stringContains out "Limit must be ≥ 1" $"{argv} rejected"
    })

let private testTracesArgOrderingsWork =
  cliTestWithFreshTraces "tail/list flag-order variants both work" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "1L + 1L" ]
      let! tailNFirst = runCli state [ "traces"; "tail"; "1"; "--route"; "eval" ]
      Expect.stringContains tailNFirst "Trace:" "tail N --route"
      let! tailRouteFirst =
        runCli state [ "traces"; "tail"; "--route"; "eval"; "1" ]
      Expect.stringContains tailRouteFirst "Trace:" "tail --route N"
      let! listJsonFn =
        runCli state [ "traces"; "list"; "--json"; "--fn"; "add"; "5" ]
      Expect.stringContains listJsonFn "[" "list --json --fn fn N"
    })

let private testTracesFindEscapesLikeWildcards =
  cliTestWithFreshTraces "find escapes SQL LIKE wildcards" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "1L + 1L" ]
      let! pctOut = runCli state [ "traces"; "find"; "%" ]
      Expect.stringContains pctOut "No traces match '%'." "literal %"
      let! zPctOut = runCli state [ "traces"; "find"; "z%" ]
      Expect.stringContains zPctOut "No traces match 'z%'." "literal z%"
      let! zUscOut = runCli state [ "traces"; "find"; "z_" ]
      Expect.stringContains zUscOut "No traces match 'z_'." "literal z_"
    })

let private testTracesRouteEmptyRejection =
  cliTestWithFreshTraces "tail/list reject empty/whitespace --route" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "1L + 1L" ]
      let cases =
        [ [ "traces"; "tail"; "--route"; "" ], "--route pattern must not be empty"
          [ "traces"; "tail"; "--route"; "   " ],
          "--route pattern must not be empty"
          [ "traces"; "list"; "--route"; "" ], "--route pattern must not be empty"
          [ "traces"; "list"; "--fn"; "   " ], "--fn pattern must not be empty" ]
      for (argv, expected) in cases do
        let! out = runCli state argv
        Expect.stringContains out expected $"{argv} rejected"
    })

let private testTracesArity1Catchalls =
  cliTestWithFreshTraces
    "arity-1 traces commands print focused usage on extra args"
    (fun state ->
      task {
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

let private testTracesStatsHintHiddenForEvalOnly =
  cliTestWithFreshTraces
    "stats footer hides --route hint when no HTTP traces"
    (fun state ->
      task {
        let! _ = runCli state [ "eval"; "1L + 1L" ]
        let! _ = runCli state [ "eval"; "2L + 2L" ]
        let! statsOut = runCli state [ "traces"; "stats" ]
        Expect.stringContains statsOut "Per-handler stats" "table"
        Expect.stringContains statsOut "eval" "eval row"
        Expect.isFalse
          (statsOut.Contains "drill into a route")
          "no route hint for eval-only"
      })

let private testTracesUnknownSubcommandSurfaced =
  cliTest "unknown traces subcommand prints clear error" (fun state ->
    task {
      let! typoOut = runCli state [ "traces"; "nonsense" ]
      Expect.stringContains typoOut "Unknown subcommand: nonsense" "typo flagged"
      let! typoTwoOut = runCli state [ "traces"; "lst" ]
      Expect.stringContains typoTwoOut "Unknown subcommand: lst" "lst flagged"

      let! bareOut = runCli state [ "traces" ]
      Expect.isFalse (bareOut.Contains "Unknown subcommand") "bare not flagged"
      let! helpOut = runCli state [ "traces"; "help" ]
      Expect.isFalse (helpOut.Contains "Unknown subcommand") "help not flagged"
    })

let private testTracesFiltersAreCaseInsensitive =
  cliTestWithFreshTraces "list --route is case-insensitive" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "1L + 1L" ]
      let! listLower = runCli state [ "traces"; "list"; "--route"; "eval" ]
      Expect.stringContains listLower "eval" "lower matches"
      let! listUpper = runCli state [ "traces"; "list"; "--route"; "EVAL" ]
      Expect.stringContains listUpper "eval" "upper matches"
      Expect.isFalse (listUpper.Contains "No traces found") "upper still finds"
      let! listMixed = runCli state [ "traces"; "list"; "--route"; "Eval" ]
      Expect.stringContains listMixed "eval" "mixed matches"
    })

let private testTracesRejectsEmptyPattern =
  cliTest "find / list --fn / list --route reject empty pattern" (fun state ->
    task {
      let cases =
        [ [ "traces"; "find"; "" ], "find pattern must not be empty"
          [ "traces"; "find"; ""; "--view" ], "find pattern must not be empty"
          [ "traces"; "find"; ""; "--json" ], "find pattern must not be empty"
          [ "traces"; "list"; "--fn"; "" ], "--fn pattern must not be empty"
          [ "traces"; "list"; "--route"; "" ], "--route pattern must not be empty" ]
      for (argv, expected) in cases do
        let! out = runCli state argv
        Expect.stringContains out expected $"{argv} rejected"
    })

let private testTracesViewRejectsNegativeSubOptions =
  cliTestWithFreshTraces "view --depth/--slow-ms reject negative" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "1L + 1L" ]
      let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
      let tid = parseTraceID listJson

      let! depthOut = runCli state [ "traces"; "view"; tid; "--depth"; "-1" ]
      Expect.stringContains depthOut "--depth must be ≥ 0" "depth -1"
      let! slowOut = runCli state [ "traces"; "view"; tid; "--slow-ms"; "-1" ]
      Expect.stringContains slowOut "--slow-ms must be ≥ 0" "slow-ms -1"
    })

let private testTracesDeleteGrammar =
  cliTestWithFreshTraces
    "delete --all/--keep singular vs plural phrasing"
    (fun state ->
      task {
        let! _ = runCli state [ "eval"; "1L + 1L" ]
        let! clearOne = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
        let! _ = runCli state [ "eval"; "1L + 1L" ]
        let! _ = runCli state [ "eval"; "2L + 2L" ]
        let! clearTwo = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
        let! _ = runCli state [ "eval"; "1L + 1L" ]
        let! _ = runCli state [ "eval"; "2L + 2L" ]
        let! pruneNone = runCli state [ "traces"; "delete"; "--keep"; "0"; "--yes" ]
        let! _ = runCli state [ "eval"; "3L + 3L" ]
        let! _ = runCli state [ "eval"; "4L + 4L" ]
        let! pruneOne = runCli state [ "traces"; "delete"; "--keep"; "1"; "--yes" ]

        Expect.stringContains clearOne "Cleared 1 trace." "singular"
        Expect.stringContains clearTwo "Cleared 2 traces." "plural"
        Expect.stringContains pruneNone "none kept" "prune --keep 0"
        Expect.stringContains pruneOne "kept the most-recent" "prune --keep 1"
      })

let private testTracesReplayReruns =
  cliTestWithFreshTraces
    "traces replay <id> re-evaluates the recorded eval input"
    (fun state ->
      task {
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

let private testTracesPruneIdempotent =
  cliTestWithFreshTraces
    "traces prune --keep is idempotent under repeated runs"
    (fun state ->
      task {
        for _ in 1..5 do
          let! _ = runCli state [ "eval"; "1L + 2L" ]
          ()

        // Sequential: in-process Console capture isn't safe for
        // concurrent runCli calls. Each prune wraps its four
        // sub-evaluations in one transaction, so "kept" is stable.
        let! _ = runCli state [ "traces"; "delete"; "--keep"; "2"; "--yes" ]
        let! _ = runCli state [ "traces"; "delete"; "--keep"; "2"; "--yes" ]
        let! _ = runCli state [ "traces"; "delete"; "--keep"; "2"; "--yes" ]

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

let private testTracesLargeTraceListSurvives =
  cliTestWithFreshTraces
    "traces list survives a 50-trace store; find still returns banner"
    (fun state ->
      task {
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

let private testTracesViewToleratesCorruptedRow =
  cliTestWithFreshTraces
    "traces view <id> renders the rest of the call tree on a corrupted row"
    (fun state ->
      task {
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

let private testTracesRejectsFlagAsTraceId =
  cliTest "flag-shaped trace-id input rejected as flag" (fun state ->
    task {
      let cmds = [ [ "traces"; "delete"; "--fake-arg" ] ]
      for argv in cmds do
        let! out = runCli state argv
        Expect.stringContains out "Unknown flag: --fake-arg" $"{argv} rejected"
    })

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

/// The four cases that dominate this file's time: about 34 of its 61 seconds.
///
/// `Tests.CliSurface.everyCommandSurvivesABogusArgument` drives every registered command with an argument that means
/// nothing (12s), `Tests.CliScm.reviewQueueRoundTrips` is the only end-to-end cover of `dark review` (11s), and the two
/// commit cases author, resolve and commit real items (6s each). They are the slowest AND among the most
/// valuable here, which is why they are on by default and the lever skips them rather than the reverse:
///
///     DARK_FAST_TESTS=1 ./scripts/run-backend-tests     # ~34s cheaper, and blind to those four
///
/// Ranked on warm runs: the frame-rendering cases look like 22s cold but are 2.5s warm.
let private slowCliTests =
  if System.Environment.GetEnvironmentVariable "DARK_FAST_TESTS" = "1" then
    []
  else
    [ Tests.CliSurface.everyCommandSurvivesABogusArgument
      Tests.CliSurface.everyCommandSurvivesABranch
      Tests.CliScm.editingOnABranchRepointsItsCallers
      Tests.CliScm.discardOnABranchLeavesMainsDraftAlone
      Tests.CliScm.committingOnABranchLeavesMainsDraftUncollapsed
      Tests.CliSurface.everyCommandAnswersWhenBare
      Tests.CliScm.reviewQueueRoundTrips
      Tests.CliScm.partialCommitTakesOnlyWhatYouNamed
      Tests.CliScm.commitRefusesUnresolvedReferences ]

let tests =
  testSequenced
  <| testList
    "CliTraces"
    ([ // Tracing is ON for every test below, because most of them are about the trace surface itself.
       // It records each call's ARGUMENTS, so an `eval` here pays to write whatever it materialises: a
       // page of sync ops is 2000 records carrying hex-encoded blobs, which takes this list from four
       // minutes to over nine. Assert on counts and identifiers here, not op bodies; anything needing
       // real blobs belongs in a suite that does not trace, like `MultiInstance`.
       test "set trace detail" {
         LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.On
       } ]
     @ Tests.CliSurface.tests
     @ Tests.CliScm.tests
     @ [ testVersionCommand
         testStatusCommand
         testRunCases
         testEvalCases
         testScriptDeclIdentity
         testRteNamesPackageDecls
         testRteNamesScriptDecls
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
     @ slowCliTests)
