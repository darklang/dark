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
module Traces = LibDB.Traces
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module Dval = LibExecution.Dval
module P = LibExecution.Permissions

open TestUtils.TestUtils

open Tests.CliTestHarness

module PT = LibExecution.ProgramTypes

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
          (executionState state)
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
          "show"
          "inspect"
          "record"
          "tail"
          "follow"
          "find"
          "hotspots"
          "rerun"
          "delete"
          "--json" ] do
        Expect.stringContains output term $"contains {term}"
    })

let private testTracesTailShowsLastEval =
  cliTestWithFreshTraces "traces tail shows last eval" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "let x = 7L\nx" ]
      let! output = runCli state [ "traces"; "tail" ]
      Expect.stringContains output "entry     eval" "the entry line"
      // All of it, not the table's truncation: a multi-line input prints as its own block.
      Expect.stringContains output "let x = 7L" "recorded input"
    })

let private testTracesDeleteEmpties =
  cliTestWithFreshTraces "traces delete --all empties the list" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "1L + 2L" ]
      let! pre = runCli state [ "traces"; "list" ]
      Expect.stringContains pre "eval" "list non-empty pre-delete"
      let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
      let! post = runCli state [ "traces"; "list" ]
      Expect.stringContains post "nothing has run yet" "list empty post-delete"
    })

let private testTracesStatsCounts =
  cliTestWithFreshTraces "traces stats shows counts" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "1L" ]
      let! _ = runCli state [ "eval"; "2L" ]
      let! output = runCli state [ "traces"; "stats" ]
      Expect.stringContains output "total ms" "table header"
      Expect.stringContains output "traces" "the count column"
      Expect.stringContains output "eval" "the eval row"
    })

let private testTracesFindByContent =
  cliTestWithFreshTraces "traces find <pattern> by content" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "\"unique-token-xyz12345\"" ]
      let! _ = runCli state [ "eval"; "1L + 1L" ]
      let! output = runCli state [ "traces"; "find"; "unique-token-xyz12345" ]
      Expect.stringContains output "what ran" "the run table's header"
      Expect.stringContains output "eval" "eval handler"

      // A pure computation logs no calls, so the only place its value appears is the run's own
      // recorded answer. `find` searches that too, or `find 3` over `1L + 2L` finds nothing.
      let! _ = runCli state [ "eval"; "40L + 2L" ]
      let! byAnswer = runCli state [ "traces"; "find"; "42" ]
      Expect.stringContains
        byAnswer
        "40L + 2L"
        "a pure run is found by what it answered"
    })

let private testTracesDeleteSingle =
  cliTestWithFreshTraces "traces delete <id> preserves siblings" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "1L + 1L" ]
      let! _ = runCli state [ "eval"; "2L + 2L" ]

      let! latestJson = runCli state [ "traces"; "list"; "1"; "--json" ]
      let latestTid = parseTraceID latestJson

      let! delOut = runCli state [ "traces"; "delete"; latestTid; "--yes" ]
      Expect.stringContains
        delOut
        $"deleted {latestTid.Substring(0, 8)}"
        "delete confirm"

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
      Expect.stringContains pruneOut "deleted 2 traces" "prune confirm"

      let! listOut = runCli state [ "traces"; "list" ]
      Expect.stringContains listOut "what ran" "the run table's header"
      Expect.stringContains listOut (latestTid.Substring(0, 8)) "latest trace kept"
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
        Expect.stringContains out "has to be 1 or more" $"{argv} rejected"
    })

let private testTracesArgOrderingsWork =
  cliTestWithFreshTraces "tail/list flag-order variants both work" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "1L + 1L" ]
      let! tailNFirst = runCli state [ "traces"; "tail"; "1"; "--route"; "eval" ]
      Expect.stringContains tailNFirst "status" "tail N --route"
      let! tailRouteFirst =
        runCli state [ "traces"; "tail"; "--route"; "eval"; "1" ]
      Expect.stringContains tailRouteFirst "status" "tail --route N"
      let! listJsonFn =
        runCli state [ "traces"; "list"; "--json"; "--fn"; "add"; "5" ]
      Expect.stringContains listJsonFn "[" "list --json --fn fn N"
    })

let private testTracesFindEscapesLikeWildcards =
  cliTestWithFreshTraces "find escapes SQL LIKE wildcards" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "1L + 1L" ]
      let! pctOut = runCli state [ "traces"; "find"; "%" ]
      Expect.stringContains pctOut "no kept trace matches %" "literal %"
      let! zPctOut = runCli state [ "traces"; "find"; "z%" ]
      Expect.stringContains zPctOut "no kept trace matches z%" "literal z%"
      let! zUscOut = runCli state [ "traces"; "find"; "z_" ]
      Expect.stringContains zUscOut "no kept trace matches z_" "literal z_"
    })

let private testTracesRouteEmptyRejection =
  cliTestWithFreshTraces "tail/list reject empty/whitespace --route" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "1L + 1L" ]
      let cases =
        [ [ "traces"; "tail"; "--route"; "" ],
          "--route takes part of a path or a method"
          [ "traces"; "tail"; "--route"; "   " ],
          "--route takes part of a path or a method"
          [ "traces"; "list"; "--route"; "" ],
          "--route takes part of a path or a method"
          [ "traces"; "list"; "--fn"; "   " ], "--fn takes a function name" ]
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
        Expect.stringContains statsOut "per entry, over the last" "table"
        Expect.stringContains statsOut "eval" "eval row"
        Expect.isFalse
          (statsOut.Contains "drill into a route")
          "no route hint for eval-only"
      })

let private testTracesUnknownSubcommandSurfaced =
  cliTest "unknown traces subcommand prints clear error" (fun state ->
    task {
      let! typoOut = runCli state [ "traces"; "nonsense" ]
      Expect.stringContains typoOut "unknown subcommand: nonsense" "typo flagged"
      let! typoTwoOut = runCli state [ "traces"; "lst" ]
      Expect.stringContains typoTwoOut "unknown subcommand: lst" "lst flagged"

      let! bareOut = runCli state [ "traces" ]
      Expect.isFalse (bareOut.Contains "unknown subcommand") "bare not flagged"
      let! helpOut = runCli state [ "traces"; "help" ]
      Expect.isFalse (helpOut.Contains "unknown subcommand") "help not flagged"
    })

let private testTracesFiltersAreCaseInsensitive =
  cliTestWithFreshTraces "list --route is case-insensitive" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "1L + 1L" ]
      let! listLower = runCli state [ "traces"; "list"; "--route"; "eval" ]
      Expect.stringContains listLower "eval" "lower matches"
      let! listUpper = runCli state [ "traces"; "list"; "--route"; "EVAL" ]
      Expect.stringContains listUpper "eval" "upper matches"
      Expect.isFalse (listUpper.Contains "no kept run") "upper still finds"
      let! listMixed = runCli state [ "traces"; "list"; "--route"; "Eval" ]
      Expect.stringContains listMixed "eval" "mixed matches"
    })

let private testTracesRejectsEmptyPattern =
  cliTest "find / list --fn / list --route reject empty pattern" (fun state ->
    task {
      let cases =
        [ [ "traces"; "find"; "" ], "find takes something to look for"
          [ "traces"; "find"; ""; "--inspect" ], "find takes something to look for"
          // The two spellings this flag has had, each refused by name rather than as an
          // unknown flag.
          [ "traces"; "find"; "x"; "--view" ], "traces find --inspect"
          [ "traces"; "find"; "x"; "--show" ], "traces find --inspect"
          [ "traces"; "find"; ""; "--json" ], "find takes something to look for"
          [ "traces"; "list"; "--fn"; "" ], "--fn takes a function name"
          [ "traces"; "list"; "--route"; "" ],
          "--route takes part of a path or a method" ]
      for (argv, expected) in cases do
        let! out = runCli state argv
        Expect.stringContains out expected $"{argv} rejected"
    })

let private testTracesViewRejectsNegativeSubOptions =
  cliTestWithFreshTraces "details --depth/--slow-ms reject negative" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "1L + 1L" ]
      let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
      let tid = parseTraceID listJson

      let! depthOut = runCli state [ "traces"; "inspect"; tid; "--depth"; "-1" ]
      Expect.stringContains depthOut "--depth is gone" "depth -1"
      let! slowOut = runCli state [ "traces"; "inspect"; tid; "--slow-ms"; "-1" ]
      Expect.stringContains
        slowOut
        "--slow-ms is a number of milliseconds"
        "slow-ms -1"
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

        Expect.stringContains clearOne "cleared 1 trace" "singular"
        Expect.stringContains clearTwo "cleared 2 traces" "plural"
        Expect.stringContains pruneNone "none kept" "prune --keep 0"
        Expect.stringContains pruneOne "kept the most-recent" "prune --keep 1"
      })

let private testTracesReplayReruns =
  cliTestWithFreshTraces
    "traces rerun <id> re-evaluates the recorded eval input"
    (fun state ->
      task {
        let! _ = runCli state [ "eval"; "1L + 2L" ]
        let! listJsonBefore = runCli state [ "traces"; "list"; "1"; "--json" ]
        let tid = parseTraceID listJsonBefore
        let! out = runCli state [ "traces"; "rerun"; tid ]
        Expect.stringContains out $"rerunning {tid.Substring(0, 8)}" "header line"
        Expect.stringContains out "3" "result printed"
        Expect.stringContains out "rerun complete" "completion line"

        // A rerun is a trace of its own, so the count goes 1 -> 2.
        let! listJsonAfter = runCli state [ "traces"; "list"; "10"; "--json" ]
        let traceCount = (listJsonAfter.Split("\"id\":\"")).Length - 1
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

        // The table shortens ids, so count the rows rather than the uuids: every trace row
        // starts with the pin marker (a space, or `*` when pinned) then a hex id prefix, and
        // carries the entry it ran.
        let! listOut = runCli state [ "traces"; "list" ]
        let rowPattern =
          System.Text.RegularExpressions.Regex("^[ *][0-9a-f]{8}\\S*\\s+done\\s")
        let count =
          listOut.Split('\n')
          |> Array.filter (fun l -> rowPattern.IsMatch l)
          |> Array.length
        Expect.equal count 2 "repeated prunes converge on --keep"
      })

/// `pin` and `unpin` go through Dark's own SQL (`Darklang.Tracing.Store.setPinned`), and a
/// Recording, end to end: what `off` and `on` actually store.
///
/// Sequenced and restored, because the setting is process-global.
let private testRecordingSettings =
  testSequenced
  <| testTask "recording off stores nothing, and on stores the run and its calls" {
    do!
      withState (fun state ->
        task {
          let rows (tid : string) : Task<int> =
            task {
              let! n =
                Sql.query
                  "SELECT COUNT(*) AS n FROM trace_fn_calls WHERE trace_id = @t"
                |> Sql.parameters [ "t", Sql.string tid ]
                |> Sql.executeRowAsync (fun read -> read.int "n")
              return n
            }

          // A run with one impure call in it, with recording off and then on.
          let run () =
            runCli state [ "eval"; "Stdlib.printLine \"x\"" ]
            |> Task.map ignore<string>

          try
            // `off`: no row at all.
            LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.Off
            let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
            do! run ()
            let! afterOff = Traces.list 10
            Expect.isEmpty afterOff "off records nothing"

            // `on`: the row with its input and its answer, plus the impure call.
            LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.On
            let! _ = runCli state [ "traces"; "delete"; "--all"; "--yes" ]
            do! run ()
            let! afterOn = Traces.list 10
            match afterOn with
            | [ (only : Traces.Trace) ] ->
              Expect.isSome only.result "on records what the run answered"
              let! n = rows (string only.id)
              Expect.equal n 1 "on records the one impure call"
              let! logged =
                Sql.query
                  "SELECT fn_hash, kind, parent_call_id, ord FROM trace_fn_calls
                   WHERE trace_id = @t"
                |> Sql.parameters [ "t", Sql.string (string only.id) ]
                |> Sql.executeRowAsync (fun read ->
                  read.string "fn_hash",
                  read.string "kind",
                  read.stringOrNone "parent_call_id",
                  read.int64 "ord")
              let (name, kind, parent, ord) = logged
              Expect.equal name "printLine" "and it is the call that had the effect"
              // The log is a sequence, not a tree: every row is flat and has an ordinal.
              Expect.equal kind "builtin" "every logged call is a builtin"
              Expect.isNone parent "nothing is nested"
              Expect.equal ord 0L "and it is the first effect of its process"
            | other ->
              failtest
                $"expected one run with recording on, got {List.length other}"
          finally
            LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.Off
        })
  }


/// The recording setting, read and written through the CLI.
///
/// There are two doors onto one stored key -- `dark traces record on` and
/// `dark config set trace.record on` -- and this asserts they are the same key, since the whole
/// point of having two is that neither is a second source of truth. Sequenced and restored:
/// the write is persistent, which is what makes it worth testing and also what makes it a
/// hazard for every test after it.
let private testRecordingSetting =
  testSequenced
  <| testTask "the recording setting is one key with two doors onto it" {
    do!
      withState (fun state ->
        task {
          let! before = LibDB.Config.get "trace.record"

          try
            let! bad = runCli state [ "traces"; "record"; "loud" ]
            Expect.stringContains
              bad
              "not a recording setting"
              "a word that is neither on nor off is refused"

            let! _ = runCli state [ "traces"; "record"; "on" ]
            let! stored = LibDB.Config.get "trace.record"
            Expect.equal
              stored
              (Some "on")
              "`traces record on` writes the stored key"

            let! shown = runCli state [ "traces"; "record" ]
            Expect.stringContains
              shown
              "recording: on"
              "and reading it back says so"

            let! _ = runCli state [ "config"; "set"; "trace.record"; "off" ]
            let! stored = LibDB.Config.get "trace.record"
            Expect.equal stored (Some "off") "and `config set` writes the same key"

            let! json = runCli state [ "traces"; "record"; "--json" ]
            Expect.stringContains json "\"stored\"" "--json says what is stored"

            let! bogus = runCli state [ "config"; "set"; "trace.record"; "loud" ]
            Expect.stringContains
              bogus
              "on or off"
              "and the config door refuses the same words"
          finally
            // Whatever was there before, including nothing.
            match before with
            | Some v -> (LibDB.Config.set "trace.record" v).Result
            | None ->
              (Sql.query "DELETE FROM config_v0 WHERE key = @key"
               |> Sql.parameters [ "key", Sql.string "trace.record" ]
               |> Sql.executeStatementAsync)
                .Result
            LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.Off
        })
  }


/// "Nothing here" has more than one cause, and the advice under each is different. This pins that
/// they are told apart, because one vague line used to tell someone with recording OFF to go and
/// run the thing again, which is the one piece of advice that cannot work.
let private testEmptyAnswersNameTheirCause =
  testSequenced
  <| testTask "an empty trace answer says WHY it is empty" {
    do!
      withState (fun state ->
        task {
          let! before = LibDB.Config.get "trace.record"

          try
            let! _ = runCli state [ "traces"; "record"; "on" ]
            let! onCase =
              runCli state [ "traces"; "calls"; "Darklang.Stdlib.Uuid.generate" ]
            Expect.stringContains
              onCase
              "recording is on"
              "recording on: it simply has not happened yet"

            let! _ = runCli state [ "traces"; "record"; "off" ]
            let! offCase =
              runCli state [ "traces"; "calls"; "Darklang.Stdlib.Uuid.generate" ]
            Expect.stringContains
              offCase
              "recording is off"
              "recording off: nothing could have been kept"
            Expect.stringContains
              offCase
              "dark traces record on"
              "and the lever is the one that helps, not `run it again`"

            // Setting a setting to what it already is is not a change, and saying what the
            // setting now means reads as though the command did not take.
            let! again = runCli state [ "traces"; "record"; "off" ]
            Expect.stringContains
              again
              "as it already was"
              "the second `record off` says nothing moved"
            Expect.isFalse
              (again.Contains "nothing from here on is kept")
              "and does not describe a change that did not happen"
          finally
            match before with
            | Some v -> (LibDB.Config.set "trace.record" v).Result
            | None ->
              (Sql.query "DELETE FROM config_v0 WHERE key = @key"
               |> Sql.parameters [ "key", Sql.string "trace.record" ]
               |> Sql.executeStatementAsync)
                .Result
            LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.Off
        })
  }


/// `dark view Stdlib.List.map` works, so these have to as well. They did not: the index stores
/// the owner-first name a hash resolves to, so the short form came back as an empty ANSWER about
/// a function with plenty of runs, which is worse than a refusal because it looks like a fact.
let private testTracesTakeTheShortNameForm =
  cliTest
    "the traces verbs take `Stdlib.X` as well as `Darklang.Stdlib.X`"
    (fun state ->
      task {
        let! _ = runCli state [ "traces"; "record"; "on" ]
        let! _ = runCli state [ "eval"; "Stdlib.List.length [1L, 2L]" ]

        let! full =
          runCli state [ "traces"; "calls"; "Darklang.Stdlib.List.length" ]
        let! short = runCli state [ "traces"; "calls"; "Stdlib.List.length" ]
        Expect.isFalse
          (short.Contains "no recorded run")
          "the short form finds the same runs the long one does"
        Expect.equal
          (short.Split('\n').Length)
          (full.Split('\n').Length)
          "and answers with the same table"

        let! shownShort = runCli state [ "traces"; "show"; "Stdlib.List.length" ]
        Expect.isFalse
          (shownShort.Contains "no function named")
          "`show` takes it too"

        // A name that really is not there is still a refusal, not an empty answer.
        let! nope = runCli state [ "traces"; "show"; "Stdlib.List.lenth" ]
        Expect.stringContains nope "no function named" "a typo is still refused"
      })


/// What a command reports to a SHELL, which is the only part of a failure a script can read.
///
/// This exists because `dark run` printed a reason and then exited 0 for a script that raised,
/// a script that was not there, and a script that ended non-zero itself -- so nothing driving
/// Dark could tell a failure from a success, and nothing here could see it, because the harness
/// used to return output and throw the status away.
let private testExitCodes =
  cliTest "a command's exit code says whether it worked" (fun state ->
    task {
      // The suite's working directory is `backend/`, and these only have to be readable by the
      // same process, so the system temp directory is the portable place for them.
      let script (name : string) (body : string) : string =
        let path = System.IO.Path.Combine(System.IO.Path.GetTempPath(), name)
        System.IO.File.WriteAllText(path, body)
        path

      let good = script "exit-ok.dark" "Stdlib.printLine \"fine\"\n"
      let raises = script "exit-raise.dark" "Stdlib.Int64.divide 1L 0L\n"

      try
        let cases =
          [ [ "run"; good ], 0, "a script that ran"
            [ "run"; raises ], 1, "a script that raised"
            [ "run"
              System.IO.Path.Combine(
                System.IO.Path.GetTempPath(),
                "exit-missing.dark"
              ) ],
            1,
            "a script that is not there"
            [ "eval"; "1L + 1L" ], 0, "an expression that answered"
            [ "eval"; "Stdlib.Int64.divide 1L 0L" ], 1, "an expression that raised"

            // Naming something that is not there is a refusal, whatever printed.
            [ "traces"; "inspect"; "zzzzzzzz" ], 1, "a run id nothing matches"
            [ "traces"; "show"; "zzzzzzzz" ],
            1,
            "a name that is neither a fn nor a run"
            // `show` takes a function now, so a run id handed to it is a refusal that says
            // which verb wants one, rather than an answer about a function nobody called.
            [ "traces"; "show"; "abc123ef" ],
            1,
            "a run id where a function name goes"
            [ "traces"; "fork"; "zzzzzzzz" ], 1, "forking a run that is not there"
            [ "ps"; "show"; "zzzzzzzz" ], 1, "a process id nothing matches"
            [ "traces"; "record"; "loud" ],
            1,
            "a recording setting that is not on or off"
            [ "traces"; "nonsense" ], 1, "a subcommand that does not exist"
            [ "traces"; "list"; "-1" ], 1, "a limit that is not a count"

            // An empty ANSWER is not a refusal: the question was asked and answered.
            [ "traces"; "calls"; "Nothing.Ran.Through.This" ],
            0,
            "a function no run went through"
            [ "traces" ], 0, "the listing"
            [ "ps" ], 0, "what is running"
            [ "traces"; "help" ], 0, "the help" ]

        for (args, expected, what) in cases do
          let! (_out, status) = runCliWithStatus state args
          let typed = String.concat " " args
          Expect.equal status expected $"{what}: dark {typed}"
      finally
        for path in [ good; raises ] do
          try
            System.IO.File.Delete path
          with _ ->
            ()
    })


/// Everything `--json` answers, in the shape something that is not a person would read.
let private testTracesJsonShapes =
  cliTestWithFreshTraces "--json answers on details, calls and show" (fun state ->
    task {
      let! _ = runCli state [ "eval"; "Stdlib.printLine \"j\"" ]
      let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
      let tid = parseTraceID listJson
      let short = tid.Substring(0, 8)

      let! logJson = runCli state [ "traces"; "inspect"; short; "--json" ]
      for key in [ "\"trace\""; "\"calls\""; "\"durationMs\""; "\"fn\"" ] do
        Expect.stringContains logJson key $"inspect --json carries {key}"
      // A recorded value is TEXT in JSON, not the encoding it is stored as.
      Expect.isFalse (logJson.Contains "DUnit") "no Dval encoding in the JSON"
      // The tree columns are gone from the Dark type, so they must not reappear in the JSON.
      for gone in [ "parentCallId"; "lambdaExprId" ] do
        Expect.isFalse
          (logJson.Contains gone)
          $"{gone} is not in the shape any more"

      let! callsJson =
        runCli state [ "traces"; "calls"; "Darklang.Stdlib.printLine"; "--json" ]
      Expect.stringContains
        callsJson
        tid
        "calls --json lists the trace that went through it"

      let! valuesJson =
        runCli state [ "traces"; "show"; "Darklang.Stdlib.printLine"; "--json" ]
      for key in [ "\"values\""; "\"problem\""; "\"fn\"" ] do
        Expect.stringContains valuesJson key $"show --json carries {key}"
    })


/// parameter that does not bind is a RUNTIME failure there, not a load one: the command dies
/// with SQLite's "Must add values for the following parameters". Nothing else in the suite runs
/// either verb, so this is the test that keeps that path honest.
let private testTracesPinRoundTrip =
  cliTestWithFreshTraces
    "traces pin / unpin round-trips through the store"
    (fun state ->
      task {
        let! _ = runCli state [ "eval"; "1L + 2L" ]
        let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
        let tid = parseTraceID listJson
        let short = tid.Substring(0, 8)

        let! pinned = runCli state [ "traces"; "pin"; short ]
        Expect.stringContains pinned $"pinned {short}" "pin says so"
        let! shown = runCli state [ "traces"; "inspect"; short ]
        Expect.stringContains shown "pinned" "and `inspect` carries it"

        let! unpinned = runCli state [ "traces"; "unpin"; short ]
        Expect.stringContains unpinned $"unpinned {short}" "unpin says so"

        let! missing = runCli state [ "traces"; "pin"; "zzzzzzzz" ]
        Expect.stringContains
          missing
          "no trace whose id starts with zzzzzzzz"
          "an id nothing matches is refused by name"
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
        Expect.stringContains listOut "what ran" "list returns the run table"
        let! findOut = runCli state [ "traces"; "find"; "3" ]
        // 50 evals of `1L + 2L` all produce DInt64 3.
        Expect.stringContains findOut "what ran" "find returns the run table"
      })

let private testTracesViewToleratesCorruptedRow =
  cliTestWithFreshTraces
    "traces inspect <id> renders the rest of the log on a corrupted row"
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

        let! out = runCli state [ "traces"; "inspect"; tid ]
        Expect.isFalse
          (out.Contains "corrupt-test")
          "corrupt row dropped from the rendered log"
        Expect.stringContains out "Stdlib" "non-corrupt rows still render"
      })

let private testTracesRejectsFlagAsTraceId =
  cliTest "flag-shaped trace-id input rejected as flag" (fun state ->
    task {
      let cmds = [ [ "traces"; "delete"; "--fake-arg" ] ]
      for argv in cmds do
        let! out = runCli state argv
        Expect.stringContains out "unknown flag: --fake-arg" $"{argv} rejected"
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
            // Comfortably over the cap of 20, and IMPURE: only impure calls are recorded, so a
            // pure map over forty elements would log nothing and cap nothing.
            let! evalOut =
              runCli
                state
                [ "eval"
                  "Stdlib.List.length (Stdlib.List.map (Stdlib.List.range 1 40) (fun x -> Stdlib.printLine (Stdlib.Int.toString x)))" ]
            Expect.stringContains evalOut "40" "the eval itself succeeded"
            let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
            let tid = parseTraceID listJson
            let! shown = runCli state [ "traces"; "inspect"; tid ]

            // The marker prints after the log and outside the display cap, so a run with more
            // calls than fit on a screen still says it was cut by the RECORDER.
            Expect.stringContains
              shown
              "trace truncated"
              "the marker says the trace was capped"
            Expect.stringContains
              shown
              "printLine"
              "and the calls it did keep are still rendered"
          finally
            LibDB.Tracing.TraceLimits.resetMaxEventsForTesting ()
            // Leaving detail on would hand full tracing to every later sequenced suite.
            LibDB.Tracing.TraceDetail.setForTesting LibDB.Tracing.TraceDetail.Off
        })
  }

// ─── Permission profiles ─────────────────────────────────────────────────

/// Exercise real CLI dispatch and persisted policies in a temporary host store.
/// The surrounding CliTraces sequence isolates the process-global override.
let private testPermissionProfiles =
  cliTest
    "permission profiles preview, validate and apply through the host"
    (fun state ->
      task {
        let dir =
          System.IO.Path.Combine(
            System.IO.Path.GetTempPath(),
            $"dark-profile-{System.Guid.NewGuid()}"
          )
        System.IO.Directory.CreateDirectory dir |> ignore<System.IO.DirectoryInfo>
        let restore = LibExecution.HostSecurity.policyDirectoryForTesting dir
        try
          // The harness state already manages policies, as the CLI's own control code does.
          let admin = state
          let guest =
            InProcess { executionState state with canManagePolicies = false }
          let initial =
            P.Policy.create
              [ P.Rule.All ]
              [ P.Rule.Effect LibExecution.Effects.Effect.Clock ]
          LibDB.PolicyStore.setInstancePolicy initial
          let root = System.IO.Path.Combine(dir, "project 'quoted'; all")
          let profileArgs name = [ "permissions"; "profile"; name; root ]

          let! listing = runCli admin [ "permissions"; "profile" ]
          for name in [ "default"; "read-only"; "local-dev" ] do
            Expect.stringContains listing name "the preset is listed"
            let args =
              if name = "default" then
                [ "permissions"; "profile"; name ]
              else
                profileArgs name
            let! preview = runCli admin args
            Expect.stringContains preview "Preview only" "no implicit approval"
            Expect.stringContains
              preview
              "including denies"
              "replacement is explicit"
            Expect.equal
              (LibDB.PolicyStore.instancePolicy ())
              initial
              "preview preserves policy"

          for args in
            [ [ "--yes" ]
              [ "unknown"; "--yes" ]
              [ "default"; "extra"; "--yes" ]
              [ "default"; "--yess" ]
              [ "read-only"; "--yes" ]
              [ "local-dev"; "relative"; "--yes" ]
              [ "local-dev"; "~/project"; "--yes" ]
              [ "local-dev"; "*"; "--yes" ]
              [ "local-dev"; root; "extra"; "--yes" ] ] do
            let! output = runCli admin ([ "permissions"; "profile" ] @ args)
            Expect.isFalse
              (output.Contains "applied profile")
              "bad input cannot apply"
            Expect.isNonEmpty output "bad input gets a diagnostic"
            Expect.equal
              (LibDB.PolicyStore.instancePolicy ())
              initial
              "bad input preserves policy"

          let allowed request =
            match request with
            | Ok request ->
              P.Policy.allows request (LibDB.PolicyStore.instancePolicy ())
            | Error reason -> failtest reason
          let checkSharedLimits () =
            Expect.isFalse (allowed (P.Request.native "test")) "no native authority"
            Expect.isFalse
              (allowed (P.Request.processSpawn "/bin/echo" []))
              "no process authority"
            Expect.isFalse
              (allowed (P.Request.env P.AccessKind.Read "HOME"))
              "no env access"
            Expect.isTrue
              (allowed (P.Request.http "GET" "https://example.com/x"))
              "HTTPS GET"
            Expect.isFalse
              (allowed (P.Request.http "POST" "https://example.com/x"))
              "no POST"
            Expect.isFalse
              (allowed (P.Request.http "GET" "http://example.com/x"))
              "HTTPS only"
            Expect.isFalse
              (allowed (P.Request.http "GET" "https://example.com:8443/x"))
              "port 443 only"

          let! applied = runCli admin (profileArgs "read-only" @ [ "--yes" ])
          Expect.stringContains
            applied
            "applied profile read-only"
            "explicit application"
          Expect.isTrue
            (allowed (P.Request.file P.AccessKind.Read (root + "/input")))
            "quoted root is kept as one rule field"
          Expect.isFalse
            (allowed (P.Request.file P.AccessKind.Read (root + "-other/input")))
            "a sibling prefix is outside the root"
          Expect.isFalse
            (allowed (P.Request.file P.AccessKind.Write (root + "/output")))
            "no writes"
          Expect.isFalse (allowed (P.Request.httpServer 9090)) "no serving"
          checkSharedLimits ()

          let! _ = runCli admin (profileArgs "local-dev" @ [ "--yes" ])
          Expect.isTrue
            (allowed (P.Request.file P.AccessKind.Write (root + "/output")))
            "project writes"
          Expect.isFalse
            (allowed (P.Request.file P.AccessKind.Write (dir + "/outside")))
            "scoped writes"
          Expect.isTrue (allowed (P.Request.httpServer 9090)) "HTTP serving"
          checkSharedLimits ()

          let! _ = runCli admin [ "permissions"; "profile"; "default"; "--yes" ]
          let rules policy =
            let allow, deny = P.Policy.rules policy
            Set.ofList allow, Set.ofList deny
          Expect.equal
            (rules (LibDB.PolicyStore.instancePolicy ()))
            (rules P.Policy.defaultInstance)
            "default agrees with the host seed and replaces previous grants and denies"

          let beforeGuest = LibDB.PolicyStore.instancePolicy ()
          let! denied = runCliCatching guest (profileArgs "local-dev" @ [ "--yes" ])
          Expect.isError denied "guest callers cannot apply a profile"
          Expect.equal
            (LibDB.PolicyStore.instancePolicy ())
            beforeGuest
            "host-only writer"
        finally
          restore.Dispose()
          System.IO.Directory.Delete(dir, true)
      })

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
    [ Tests.CliSurface.everyCommandWorksWithValidArguments
      Tests.CliSurface.everyCommandSurvivesABogusArgument
      Tests.CliSurface.everyCommandSurvivesABranch
      Tests.CliScm.editingOnABranchRepointsItsCallers
      Tests.CliScm.discardOnABranchLeavesMainsDraftAlone
      Tests.CliScm.committingOnABranchLeavesMainsDraftUncollapsed
      Tests.CliSurface.everyCommandAnswersWhenBare
      Tests.CliScm.reviewQueueRoundTrips
      Tests.CliScm.partialCommitTakesOnlyWhatYouNamed
      Tests.CliScm.commitRefusesUnresolvedReferences ]

/// Tracing costs more than anything else these tests do: it records every call's ARGUMENTS,
/// so an `eval` pays to write whatever it materialises, and a page of sync ops is 2000
/// records carrying hex-encoded blobs.
///
/// So it is on only for the tests that are ABOUT tracing, and each of those carries the
/// setting itself, in `cliTestWithFreshTraces`. Do not put it back as a test at the head of
/// this list: `--shard` partitions by test, so the toggle lands on one node and the tests it
/// enabled land on another.
///
/// Assert on counts and identifiers here, not on op bodies; anything needing real blobs
/// belongs in a suite that does not trace, like `MultiInstance`.

let tests =
  testSequenced
  <| testList
    "CliTraces"
    (Tests.CliSurface.tests
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
         testHelpForLs ]
     @ [ // Trace surface
         testTracesHelp
         testTracesTailShowsLastEval
         testTracesDeleteEmpties
         testTracesStatsCounts
         testTracesFindByContent
         testTracesDeleteSingle
         testTracesPruneKeep
         testTracesReplayReruns
         testTracesPruneIdempotent
         testTracesPinRoundTrip
         testRecordingSettings
         testRecordingSetting
         testEmptyAnswersNameTheirCause
         testTracesTakeTheShortNameForm
         testTracesJsonShapes
         testExitCodes
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
         testTracesTruncatedStillShowsRoot
         testPermissionProfiles ]
     @ slowCliTests)
