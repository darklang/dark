/// The CLI cases that reach past what a command prints -- the recorder's settings and limits,
/// the trace tables, the host's policy and process registry -- plus the single composition
/// point for all three CLI integration suites: `tests` below is the one list the runner sees.
/// Its in-process cases run one at a time, in an order that is deliberate (the slow sweeps run
/// last); the cases on CLI instances run beside the rest of the suite.
///
/// What a person sees at a shell from `eval`, `run` and `traces` is tested in Dark, in
/// `packages/darklang/tests/cli/evalAndRun.dark` and `traces.dark`.
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

let private testTerminalPrinterFallback =
  cliTest "terminal values survive a missing pretty-printer" (fun target ->
    task {
      let state = executionState target
      let printer = RT.Hash(LibExecution.PackageRefs.Fn.Cli.renderValue ())
      let state =
        { state with
            fns =
              { state.fns with
                  package =
                    fun hash ->
                      if hash = printer then Ply None else state.fns.package hash } }
      let! output = Exe.dvalToReprForTerminal state 80 false [] (Dval.bigInt 42I)
      Expect.stringStarts
        output
        "<pretty-print failed for dval:"
        "fallback is shown"
      Expect.stringContains output "FnNotFound" "the printer failure is included"
      Expect.stringContains output "42" "the original value is still available"
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
          PT.BranchId.Main
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

// ─── Trace store ──────────────────────────────────────────────────────────

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
            runCli state [ "eval"; "Stdlib.printLine(\"x\")" ]
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


/// `ps show` names a pid in its own usage line, and a one-shot `ps` prints pids rather than ids
/// a person can use, so a pid from the machine table has to be something `show` takes.
let private testPsShowTakesAPid =
  cliTest "ps show takes a pid from the machine table" (fun state ->
    task {
      let dir =
        System.IO.Path.Combine(
          System.IO.Path.GetTempPath(),
          $"dark-ps-show-{System.Guid.NewGuid()}"
        )
      System.IO.Directory.CreateDirectory dir |> ignore<System.IO.DirectoryInfo>
      LibExecution.HostRegistry.setDirectory dir
      try
        let command = "dark serve Demo.router --port 9095 --live"
        LibExecution.HostRegistry.register "dark serve :9095" command "main"
        let pid = string System.Environment.ProcessId
        let! (out, status) = runCliWithStatus state [ "ps"; "show"; pid ]
        Expect.equal status 0 $"a registered pid is shown: {out}"
        Expect.stringContains out command "with its whole command line"
        let! (_out, missing) = runCliWithStatus state [ "ps"; "show"; "999999" ]
        Expect.equal missing 1 "a pid nothing has is still a refusal"
      finally
        LibExecution.HostRegistry.setDirectory ""
        System.IO.Directory.Delete(dir, true)
    })

/// What is typed for a trace id is matched as text. A quote used to end the SQL string the id
/// was spliced into, so the command died with an SQLite syntax error; `%` and `_` would match
/// any id if they were read as wildcards, and with one run in the store that would pin it.
let private testTracesIdPrefixIsPlainText =
  cliTestWithFreshTraces
    "a quote or a wildcard in a trace id is just text"
    (fun state ->
      task {
        let! _ = runCli state [ "eval"; "1 + 2" ]
        for typed in [ "a'b"; "'"; "%"; "_" ] do
          let! out = runCli state [ "traces"; "pin"; typed ]
          Expect.stringContains
            out
            $"no trace whose id starts with {typed}"
            $"{typed} is refused by name"
      })


let private testTracesLargeTraceListSurvives =
  cliTestWithFreshTraces
    "traces list survives a 50-trace store; find still returns banner"
    (fun state ->
      task {
        // Not the multi-MB stress case, but enough to OOM or time out.
        //
        // A builtin call rather than `1L + 2L`, because `find` searches recorded fn-call args and
        // results and an operator on two builtin numerics no longer leaves one: it is answered
        // directly whether or not tracing is on, so that the number a program gives does not
        // depend on whether anything is watching.
        for _ in 1..50 do
          let! _ = runCli state [ "eval"; "Stdlib.String.length(\"abc\")" ]
          ()
        let! listOut = runCli state [ "traces"; "list"; "20" ]
        Expect.stringContains listOut "what ran" "list returns the run table"
        let! findOut = runCli state [ "traces"; "find"; "3" ]
        // 50 calls of `String.length "abc"` all record a result of 3.
        Expect.stringContains findOut "what ran" "find returns the run table"
      })

let private testTracesViewToleratesCorruptedRow =
  cliTestWithFreshTraces
    "traces inspect <id> renders the rest of the log on a corrupted row"
    (fun state ->
      task {
        // An EFFECTFUL call, because only impure calls are recorded: with a pure eval this
        // trace has no rows at all, and the render assertion below passed on the echoed
        // input rather than on anything rendered.
        let! _ = runCli state [ "eval"; "Stdlib.printLine(\"kept\")" ]
        let! listJson = runCli state [ "traces"; "list"; "1"; "--json" ]
        let tid = parseTraceID listJson

        // Inject a corrupt fn_call row: bytes that aren't a valid
        // binary-serialized RT.Dval. The eval's own rows stay
        // valid; the bad one must be skipped, not abort the render.
        // The result goes where every result goes, the trace's blobs, so the row is read
        // and fails to decode rather than never being joined at all.
        let corruptBytes = [| 0x00uy; 0x01uy; 0x02uy |]
        let _ =
          Sql.executeTransactionSync
            [ "INSERT INTO trace_blobs (trace_id, hash, bytes)
               VALUES (@traceId, 'corrupt-hash', @badResult)",
              [ [ "traceId", Sql.string tid; "badResult", Sql.bytes corruptBytes ] ]
              "INSERT INTO trace_fn_calls
                (trace_id, call_id, parent_call_id, kind, fn_hash,
                 lambda_expr_id, args, result, duration_ms)
               VALUES
                (@traceId, 'corrupt-test', NULL, 'fn', 'corrupt',
                 NULL, @badArgs, 'corrupt-hash', 0)",
              [ [ "traceId", Sql.string tid; "badArgs", Sql.bytes corruptBytes ] ] ]

        let! out = runCli state [ "traces"; "inspect"; tid ]
        Expect.isFalse
          (out.Contains "corrupt-test")
          "corrupt row dropped from the rendered log"
        // `->` is the row separator and appears nowhere else in this output; "Stdlib" was
        // satisfied by the `input` line whether or not a single row rendered.
        Expect.stringContains out "->" "non-corrupt rows still render"
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
                  "Stdlib.List.length(Stdlib.List.map(Stdlib.List.range(1, 40), fun x -> Stdlib.printLine(Stdlib.toString(x))))" ]
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

/// The cases that dominate this file's time.
///
/// `Tests.CliSurface.everyCommandRefusesAWordItDoesNotKnow` and its flag sibling drive every registered
/// command with something it does not take (12s between them), and the two commit cases author, resolve
/// and commit real items (6s each). They are the slowest AND among the most valuable here, which is why
/// they are on by default and the lever skips them rather than the reverse:
///
///     DARK_FAST_TESTS=1 ./scripts/run-backend-tests     # cheaper, and blind to these
///
/// Ranked on warm runs: the frame-rendering cases look like 22s cold but are 2.5s warm.
let private slowCliTests =
  if System.Environment.GetEnvironmentVariable "DARK_FAST_TESTS" = "1" then
    []
  else
    [ Tests.CliSurface.everyCommandWorksWithValidArguments
      Tests.CliSurface.everyCommandRefusesAWordItDoesNotKnow
      Tests.CliSurface.everyCommandRefusesAFlagItDoesNotHave
      Tests.CliSurface.everyCommandSurvivesABranch
      Tests.CliScm.editingOnABranchRepointsItsCallers
      Tests.CliScm.discardOnABranchLeavesMainsDraftAlone
      Tests.CliScm.committingOnABranchLeavesMainsDraftUncollapsed
      Tests.CliSurface.everyCommandAnswersWhenBare
      Tests.CliScm.partialCommitTakesOnlyWhatYouNamed
      Tests.CliScm.commitRefusesUnresolvedReferences ]

/// Tracing costs more than anything else these tests do: it records every call's ARGUMENTS,
/// so an `eval` pays to write whatever it materialises, and a page of ops is 2000
/// records carrying hex-encoded blobs.
///
/// So it is on only for the tests that are ABOUT tracing, and each of those carries the
/// setting itself, in `cliTestWithFreshTraces`. Do not put it back as a test at the head of
/// this list: `--shard` partitions by test, so the toggle lands on one node and the tests it
/// enabled land on another.
///
/// Assert on counts and identifiers here, not on op bodies; anything needing real blobs
/// belongs in a suite that does not trace, like `MultiInstance`.
/// Not sequenced as a whole: the in-process cases sequence themselves (`cliTest`), and the
/// ones on instances run beside the rest of the suite.
let tests =
  testList
    "CliTraces"
    (Tests.CliSurface.tests
     @ Tests.CliScm.tests
     @ [ testTerminalPrinterFallback; testMkdirRecursiveUnderPolicyAncestor ]
     @ [ // Trace store
         testRecordingSettings
         testTracesIdPrefixIsPlainText
         testPsShowTakesAPid
         testTracesLargeTraceListSurvives
         testTracesViewToleratesCorruptedRow
         testTracesTruncatedStillShowsRoot
         testPermissionProfiles ]
     @ slowCliTests)
