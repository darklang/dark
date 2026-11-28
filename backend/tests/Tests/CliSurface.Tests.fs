/// The CLI's command surface as a whole: the registry-driven sweeps (every command
/// bare, with --help, with a bogus argument, standing on a branch) and the docs' claims
/// checked against the registry.
/// Run order and sequencing live in CliTraces.Tests.fs, which composes this list.
module Tests.CliSurface

open Expecto
open System.Threading.Tasks

open Prelude
open Fumble
open LibDB.Sqlite

module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module Dval = LibExecution.Dval

open TestUtils.TestUtils

open Tests.CliTestHarness

module CliDsl = Tests.CliDsl


let private storedPermissionsAreIsolated =
  testSequenced
  <| testTask "CLI tests isolate stored permissions even after a failure" {
    let originalDirectory = LibExecution.HostSecurity.policyDirectory ()
    let mutable initial = LibDB.PolicyStore.empty
    try
      do!
        withState (fun _ ->
          task {
            initial <- LibDB.PolicyStore.get ()
            LibDB.PolicyStore.setInstancePolicy
              LibExecution.Permissions.Policy.denyAll
            let approved =
              LibDB.PolicyStore.recordApproval
                None
                "Tests.PolicyIsolation.root"
                (LibDB.PolicyStore.reviewToken (LibDB.PolicyStore.get ()))
                { hash = "test-policy-root"
                  policy = LibExecution.Permissions.Policy.denyAll
                  fingerprint = "test-fingerprint"
                  explicitRules = false }
            Expect.equal approved (Ok()) "the first test stored an approval"
            Exception.raiseInternal "simulated test failure" []
          })
      failtest "the first test should have raised"
    with e when e.Message = "simulated test failure" ->
      ()
    Expect.equal
      (LibExecution.HostSecurity.policyDirectory ())
      originalDirectory
      "the failed test restored the policy directory"
    do!
      withState (fun _ ->
        task {
          Expect.equal
            (LibDB.PolicyStore.get ())
            initial
            "the next test gets fresh instance permissions and no old approvals"
        })
  }


let private reusesCompiledFunctions =
  cliTest "the CLI harness reuses compiled package functions" (fun target ->
    task {
      let state = executionState target
      let hash = RT.Hash(LibExecution.PackageRefs.Fn.Cli.executeCliCommand ())
      let first = state.fns.package hash
      let second = state.fns.package hash
      match first, second with
      | Some first, Some second ->
        Expect.isTrue
          (System.Object.ReferenceEquals(first, second))
          "fetching a function again must reuse its compiled instructions"
      | _ -> Tests.failtest "the CLI entry point must be available"
    })

let private timeoutBoundsSynchronousWork =
  testSequenced
  <| testTask "the CLI timeout bounds work before its first await" {
    let release =
      TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)
    let stopped =
      TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)
    try
      let! result =
        runWithTimeout (System.TimeSpan.FromMilliseconds 100.0) (fun () ->
          try
            // A bounded wait keeps a broken timeout from hanging this regression.
            release.Task.Wait(System.TimeSpan.FromSeconds 5.0) |> ignore<bool>
            Task.FromResult 42
          finally
            stopped.SetResult())
      Expect.isNone result "the deadline wins while the synchronous work is blocked"
    finally
      release.SetResult()
    do! stopped.Task.WaitAsync(System.TimeSpan.FromSeconds 5.0)
  }

let private timeoutPreservesCapture =
  testSequenced
  <| testTask "the CLI timeout preserves results and captured output" {
    Expect.isTrue (NonBlockingConsole.startCapture ()) "capture starts"
    try
      let! result =
        runWithTimeout (System.TimeSpan.FromSeconds 5.0) (fun () ->
          print "captured by the caller"
          Task.FromResult 42)
      Expect.equal result (Some 42) "the result crosses the worker boundary"
      Expect.equal
        ((NonBlockingConsole.stopCapture ()).Trim())
        "captured by the caller"
        "the worker inherits the caller's output capture"
    finally
      NonBlockingConsole.stopCapture () |> ignore<string>
  }

/// stderr shares stdout's queue and capture window. Written straight to the console, an error could
/// print above the output that led to it and escaped the workbench's in-frame capture.
let private stderrIsCapturedInOrder =
  cliTest "stderr is captured apart from stdout, and in order with it" (fun target ->
    task {
      let program =
        "let _ = Stdlib.printLine(\"first\") in let _ = Stdlib.printErrorLine(\"second\") in let _ = Stdlib.printLine(\"third\") in 4"
        + ""
        + ""

      let! (out, err, status) = runCliStreams target [ "eval"; program ]
      Expect.equal status 0 "the eval succeeded"
      Expect.equal out "first\nthird\n4" "stdout holds the answer and nothing else"
      Expect.equal err "second" "stderr holds the one line written to it"

      let! both = runCli target [ "eval"; program ]
      Expect.equal
        both
        "first\nsecond\nthird\n4"
        "runCli keeps both streams, in order"
    })

let private stdinThatIsNotUtf8IsRefused =
  Tests.CliInstance.instanceTest
    "stdin that is not UTF-8 is refused, not replaced"
    (fun i ->
      task {
        let notUtf8 = Some [| 0xFFuy; 0x00uy; 0xFEuy |]
        let! (code, out, _) =
          Tests.CliInstance.runRawWithBytes
            i
            [ "eval"; "Stdlib.Cli.Stdin.readAll()" ]
            notUtf8
        Expect.equal code 0 "readAll answers"
        Expect.equal
          ((CliDsl.plain out).Trim())
          "Error(\"stdin is not valid UTF-8: FF\")"
          "an Error naming the first bad byte"

        let! (code, out, err) =
          Tests.CliInstance.runRawWithBytes i [ "eval"; "-" ] notUtf8
        Expect.equal code 1 "eval - refuses"
        Expect.equal out "" "no answer"
        Expect.stringContains err "stdin is not valid UTF-8: FF" "says why"
      })

/// `ps` and `top` read a process's name and command line from /proc; the process reads its own.
let private processTitleSaysWhatRuns =
  Tests.CliInstance.instanceTest "a process's title says what it runs" (fun i ->
    task {
      if System.OperatingSystem.IsLinux() then
        let! (code, _, err) =
          Tests.CliInstance.runRaw
            i
            [ "permissions"; "allow"; "file"; "read"; "/proc" ]
        Expect.equal code 0 $"allowing /proc: {err}"
        let read (file : string) =
          task {
            let! (_, out, err) =
              Tests.CliInstance.runRaw
                i
                [ "eval"; $"Stdlib.Cli.File.readText(\"/proc/self/{file}\")" ]
            return out + err
          }
        let! comm = read "comm"
        Expect.stringContains comm "dark eval" "the name"
        let! cmdline = read "cmdline"
        Expect.stringContains
          cmdline
          "dark eval Stdlib.Cli.File.readText"
          "the command line names `dark`, not the binary's path"
    })

/// `dark run ... | head` ends when `head` does: once nobody reads stdout, the run stops, quietly.
let private runStopsWhenStdoutCloses =
  Tests.CliInstance.instanceTest "a run stops when its stdout closes" (fun i ->
    task {
      if not (System.OperatingSystem.IsWindows()) then
        // A line every 20 ms, for a minute.
        let script = System.IO.Path.Combine(i.dir, "ticks.dark")
        System.IO.File.WriteAllText(
          script,
          "let tick(n: Int64): Int64 =\n"
          + "  Stdlib.printLine(Stdlib.display(n))\n"
          + "  Stdlib.Cli.Posix.sleep(20.0)\n"
          + "  if n >= 3000 then 0 else tick(n + 1)\n"
          + "tick(1)\n"
        )
        let p = Tests.CliInstance.startUndrained i [ "run"; script ]
        try
          let! first = p.StandardOutput.ReadLineAsync()
          Expect.equal first "1" "the run prints"
          p.StandardOutput.Close()
          Expect.isTrue
            (p.WaitForExit 30000)
            "it stops soon after its stdout closes"
          Expect.equal p.ExitCode 141 "and exits as a process SIGPIPE ended"
          Expect.equal (p.StandardError.ReadToEnd()) "" "saying nothing"
        finally
          Tests.CliInstance.stop p
    })

/// `Stdout.writeBytes` writes every byte value as it is, in order with the text around it.
let private stdoutBytesArriveUnchanged =
  Tests.CliInstance.instanceTest "bytes written to stdout arrive unchanged" (fun i ->
    task {
      let everyByte = [| for b in 0..255 -> byte b |]
      let script = System.IO.Path.Combine(i.dir, "bytes.dark")
      System.IO.File.WriteAllText(
        script,
        "let _ = Stdlib.print(\"<\")\n"
        + "let _ =\n"
        + "  Stdlib.Cli.Stdout.writeBytes(\n"
        + $"    Stdlib.Base64.decode(\"{System.Convert.ToBase64String everyByte}\")\n"
        + "    |> Builtin.unwrap)\n"
        + "let _ = Stdlib.print(\">\")\n"
        + "0\n"
      )
      let p = Tests.CliInstance.startUndrained i [ "run"; script ]
      try
        use written = new System.IO.MemoryStream()
        do! p.StandardOutput.BaseStream.CopyToAsync written
        Expect.isTrue (p.WaitForExit 30000) "the run ends"
        Expect.equal p.ExitCode 0 $"and succeeds: {p.StandardError.ReadToEnd()}"
        Expect.equal
          (written.ToArray())
          (Array.concat [ [| byte '<' |]; everyByte; [| byte '>' |] ])
          "every byte value arrives as it was written, between the text around it"
      finally
        Tests.CliInstance.stop p
    })

let private testHelpCommand =
  instanceTest "help command" (fun state ->
    task {
      let! output = runCli state [ "help" ]
      Expect.stringContains output "Packages:" "category header"
      Expect.stringContains output "Changes:" "changes header"
      Expect.stringContains output "Branches:" "branches header"
      Expect.stringContains output "help" "help command"
      Expect.stringContains output "version" "version command"
      Expect.stringContains output "status" "status command"
    })

/// Every registered command name, read out of `dark help`.
///
/// `help` prints each as `  name (alias, ...) - description`, so a name is the first token of any
/// indented line containing " - ". Parsing the human output is deliberate: a command that stops
/// appearing on the surface a person sees has stopped existing.
let private registeredCommands (state : Target) : Task<List<string>> =
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

/// Displaying a commit's ops must not fetch all of them.
///
/// The seed commit holds about twelve thousand. Taking the whole list and keeping the first handful
/// deserializes every blob to discard almost all of them, which in the workbench is a freeze on a
/// keypress. Asserted on the OUTPUT, not a stopwatch: if the cap goes, so does "showing the first 50".
let private showingACommitDoesNotFetchEveryOp =
  instanceTest
    "showing a big commit's ops is capped, not fetched whole"
    (fun state ->
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


// ─── Refusals ──────────────────────────────────────────────────────────────────
//
// A command REFUSES what it was not built to take, and refusing means exiting non-zero: a refusal
// that exits 0 is success to a shell, a CI step or an agent, and `dark discard --only-this -y`
// discarded the whole draft because the junk was ignored. Each test below states one part of
// that, by name. Where a command departs from it, the departure is written down beside the test
// with what the command does instead, and that is asserted too, or said plainly not to run.

/// An empty grant is not a grant, and must not report that it is.
let private permissionsRefusesAnEmptyRule =
  instanceTest
    "`permissions allow` with no rule is refused rather than reported as allowed"
    (fun state ->
      task {
        let! refused = runCli state [ "permissions"; "allow"; "" ]

        Expect.stringContains
          refused
          "invalid permission rule"
          "it should say what was wrong with it"

        Expect.isFalse
          (refused.Contains "allowed ")
          "and must not report a rule it did not add"
      })

/// Several rules in one call, `;`-separated. One that is not a rule saves none of them.
let private permissionsAllowTakesSeveralRules =
  instanceTest
    "`permissions allow` takes several rules, and saves none when one is not a rule"
    (fun state ->
      task {
        let! refused, code =
          runCliWithExit
            state
            [ "permissions"; "allow"; "http-server 127.0.0.1 8123; zzznope" ]
        Expect.equal code 1L $"a rule that is not one fails the call:\n{refused}"
        Expect.stringContains
          refused
          "invalid permission rule: zzznope"
          "it names the one that is not a rule"
        let! listed = runCli state [ "permissions"; "list" ]
        Expect.isFalse
          (listed.Contains "http-server")
          $"and the good rule beside it was not saved:\n{listed}"

        let! _ =
          runCli
            state
            [ "permissions"; "allow"; "http-server 127.0.0.1 8123; file read /tmp" ]
        let! listed = runCli state [ "permissions"; "list" ]
        Expect.stringContains
          listed
          "http-server"
          $"the first rule is saved:\n{listed}"
        Expect.stringContains listed "file read '/tmp'" $"and the second:\n{listed}"
      })

/// Every `dark <word>` the in-CLI docs mention has to be a real command.
///
/// A doc that confidently describes a command that is not there is worse than no doc, and nothing
/// else checks. Narrow by design: only the WORD after `dark`, which is what is verifiable
/// mechanically. It cannot tell you the prose is wrong, only that the commands are real.
let private docTopicsToCheck =
  [ "scm"
    "for-ai"
    "for-ai packages"
    "for-ai tests"
    "for-ai permissions"
    "for-ai traces"
    "for-ai scm"
    "for-ai syntax"
    "cli"
    "processes"
    "live"
    "http-server" ]

let private documentedCommandsAreReal =
  instanceTest "every command the docs mention exists" (fun state ->
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
        let! doc = runCli state ("docs" :: List.ofArray (topic.Split(' ')))

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

let private unwrapErrorsAreReadable =
  instanceTest "eval checks unwrap operands and runs valid unwraps" (fun state ->
    task {
      let! rejected, exitCode =
        runCliWithExit state [ "eval"; "(fun value -> Some(value? + 1))(4)" ]
      Expect.equal exitCode 1L "invalid unwrap fails eval"
      Expect.stringContains
        rejected
        "`?` needs an Option or Result"
        "names the ? and its required operand"
      Expect.isFalse
        (rejected.Contains "Encountered a Runtime Error")
        "the error renderer itself succeeds"
      let! accepted =
        runCli state [ "eval"; "(fun value -> Some(value? + 1))(Some(4))" ]
      Expect.stringContains accepted "Some(5)" "valid unwrap still executes"
    })


/// Missing old approval code must not interfere with the current function.
let private missingApprovalVersionDoesNotBlockCurrentCode =
  cliTest
    "an approval of a missing version does not block current code"
    (fun state ->
      task {
        let name = "Tests.HonestApproval.stale"
        let! _ = runCli state [ "fn"; "/" + name; "(x: Int64) : Int64 = x + 6143" ]
        let! approved, approvalStatus =
          runCliWithStatus
            state
            [ "permissions"; "approve"; name; "deny all"; "--yes" ]
        Expect.equal approvalStatus 0 approved

        // Move the approval to a version this store never held: what a store reset leaves behind.
        // CLI commands restore the logged-in account; the host state may be anonymous.
        let! viewer =
          evalUnder (executionState state) "(Darklang.Cli.initState()).accountID"
        let account =
          match viewer with
          | RT.DEnum(_, _, _, "Some", [ RT.DUuid id ]) -> Some id
          | RT.DEnum(_, _, _, "None", []) -> None
          | other -> failtestf "unexpected CLI account: %A" other
        let current = LibDB.PolicyStore.approvedVersion account name
        Expect.isSome current "the approval landed"
        let gone = String.replicate 64 "e"
        let moved =
          LibDB.PolicyStore.recordApproval
            account
            name
            (LibDB.PolicyStore.reviewToken (LibDB.PolicyStore.get ()))
            { hash = gone
              policy = LibExecution.Permissions.Policy.denyAll
              fingerprint = "missing-version"
              explicitRules = false }
        Expect.isOk moved "the approval now names a version the store does not have"

        let! ran, status = runCliWithStatus state [ "eval"; name + "(1)" ]
        let! _ = runCli state [ "permissions"; "unapprove"; name ]
        Expect.equal status 0 ran
        Expect.equal (ran.Trim()) "6144" "the current body runs"
      })


/// A policy file that cannot be trusted is a refusal naming the file, not an internal error.
///
/// It printed the sentence twice, once as `Internal error` with an F# frame and once as an
/// uncaught exception with a Dark call stack, and it named `~/.darklang/policy`, which is not
/// where the file is when the store lives somewhere else.
let private damagedPolicyIsARefusal =
  instanceTest
    "a damaged policy file refuses plainly, naming where it is"
    (fun target ->
      task {
        let dir =
          match target with
          | Instance i -> i.dir
          | InProcess _ -> Tests.failtestf "needs its own instance"
        let file = System.IO.Path.Combine(dir, "policy", "policies.bin")
        System.IO.File.WriteAllText(file, "not a policy file at all")

        let! said, exitCode =
          runCliWithExit
            target
            [ "permissions"; "allow"; "http"; "GET"; "https://example.com:443/" ]
        Expect.equal exitCode 1L $"it refuses:\n{said}"
        Expect.stringContains
          said
          "The policy file is damaged"
          $"says what is wrong:\n{said}"
        Expect.stringContains
          said
          file
          $"and names the file where it actually is:\n{said}"
        Expect.isFalse
          (said.Contains "Internal error")
          $"not as an internal error:\n{said}"
        Expect.isFalse
          (said.Contains "Call stack")
          $"and with no call stack:\n{said}"
      })


/// A rule ending in `/*` covers the paths under it, end to end: what `permissions allow` saves is
/// what a request is checked against.
///
/// `https://api.github.com:443/*` printed "policy updated" and was saved with the literal path
/// `/*`, which no request has, so the next GET to `/repos/darklang/dark` was refused anyway.
let private httpRuleWithTrailingStarMatches =
  instanceTest "an http rule ending in /* allows the paths under it" (fun target ->
    task {
      let dir =
        match target with
        | Instance i -> i.dir
        | InProcess _ -> Tests.failtestf "needs its own instance"

      let! said, exitCode =
        runCliWithExit
          target
          [ "permissions"; "allow"; "http"; "GET"; "https://api.github.com:443/*" ]
      Expect.equal exitCode 0L $"the rule is saved:\n{said}"

      let file = System.IO.Path.Combine(dir, "policy", "policies.bin")
      let policy =
        (LibDB.PolicyStore.fromBytes (System.IO.File.ReadAllBytes file)).instance
      let allows (url : string) =
        match LibExecution.Permissions.Request.http "GET" url with
        | Ok request -> LibExecution.Permissions.Policy.allows request policy
        | Error error -> Tests.failtestf "%s" error

      Expect.isTrue
        (allows "https://api.github.com:443/repos/darklang/dark")
        "the URL the agent fetched is allowed"
      Expect.isFalse
        (allows "https://example.com/repos/darklang/dark")
        "and another host is not"
    })


/// A doc-only edit through `dark import` lands, as it does through `dark fn`.
///
/// Docs are not part of the content hash, so an edit that changes only a `///` line leaves every
/// item on the hash it had, and the module path, which never wrote the `UpdateDoc` the single-item
/// commands do, saved nothing while printing the same "Defined" line as a real save. Two items, one
/// doc changed, because a module saves a batch and one item's words must not land on the other.
let private moduleDocOnlyEditLands =
  instanceTest
    "a doc-only edit through dark import lands on the item it was made to"
    (fun state ->
      task {
        let write (text : string) =
          let file = System.IO.Path.GetTempFileName()
          System.IO.File.WriteAllText(file, text)
          file
        let source (docA : string) =
          $"/// {docA}\nlet docA(x: Int64): Int64 = x + 7741\n\n/// The second item's words, untouched.\nlet docB(x: Int64): Int64 = x + 7742\n"

        let! _ =
          runCli
            state
            [ "import"; write (source "First wording."); "/Tests.HonestDocs" ]
        let! saved =
          runCli
            state
            [ "import"; write (source "Second wording."); "/Tests.HonestDocs" ]

        let! viewA = runCli state [ "view"; "Tests.HonestDocs.docA" ]
        Expect.stringContains
          viewA
          "Second wording."
          $"the edited doc landed:\n{viewA}\n{saved}"

        let! viewB = runCli state [ "view"; "Tests.HonestDocs.docB" ]
        Expect.stringContains
          viewB
          "The second item's words, untouched."
          $"and the other item kept its own:\n{viewB}"
        Expect.isFalse
          (viewB.Contains "Second wording.")
          $"rather than taking the first's:\n{viewB}"
      })


/// In the run order CliTraces.Tests.fs composes; sequencing lives there too.
/// An answer that is not UTF-8 is a no, not a crash. Only a terminal is asked, so `script` gives
/// `discard` one, with CI's "nobody is here" switches off.
let nonUtf8AnswerIsANo =
  CliInstance.instanceTest "an answer that is not UTF-8 is a no" (fun i ->
    task {
      let! (code, _, err) = CliInstance.runRaw i [ "val"; "Tests.Utf8Probe.x"; "1" ]
      Expect.equal code 0 $"the draft has something to drop: {err}"
      let! (code, out, err) =
        CliInstance.runRawWithBytes
          { i with cli = "/usr/bin/env" }
          [ "-u"
            "CI"
            "-u"
            "DARK_NO_INPUT"
            "script"
            "-qec"
            $"{i.cli} discard"
            "/dev/null" ]
          (Some [| 0xFFuy; byte '\n' |])
      Expect.equal code 1 $"a no exits 1: {out}{err}"
      Expect.stringContains out "Drop these? [y/N]" "it was asked"
      Expect.stringContains out "kept." "and kept the draft"
      let! status = CliInstance.run i [ "status" ]
      Expect.stringContains
        status
        "draft: 1 item changed"
        "the draft is still there"
    })


let tests : List<Test> =
  [ unwrapErrorsAreReadable
    nonUtf8AnswerIsANo
    missingApprovalVersionDoesNotBlockCurrentCode
    damagedPolicyIsARefusal
    httpRuleWithTrailingStarMatches
    moduleDocOnlyEditLands
    storedPermissionsAreIsolated
    reusesCompiledFunctions
    timeoutBoundsSynchronousWork
    timeoutPreservesCapture
    stderrIsCapturedInOrder
    stdinThatIsNotUtf8IsRefused
    processTitleSaysWhatRuns
    runStopsWhenStdoutCloses
    stdoutBytesArriveUnchanged
    testHelpCommand
    showingACommitDoesNotFetchEveryOp
    permissionsRefusesAnEmptyRule
    permissionsAllowTakesSeveralRules
    documentedCommandsAreReal ]
