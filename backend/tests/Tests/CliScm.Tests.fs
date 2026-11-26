/// The SCM verbs through the CLI: branch/commit/merge/discard/conflicts/review,
/// their refusals, their exit codes, and what each leaves in the draft and the log.
/// Run order and sequencing live in CliTraces.Tests.fs, which composes this list.
module Tests.CliScm

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
open Tests.CliDsl

/// A draft holding a deprecation can be committed by BOTH paths, and each fails differently without
/// this. `--include=` has to read the `Reference` a `Deprecate` carries as a reference and not as a
/// `Hash`, or it dies at runtime. And `commit --json` must not decide "nothing to commit" from the
/// changed-NAMES list, which a deprecation is never in: an agent then cannot commit one at all, and it
/// ships in the seed as somebody's mystery op.
let private aDeprecationCommitsByEitherPath =
  instanceTest
    "a deprecation-only draft commits, by --include= and by --json"
    (fun state ->
      task {
        do! discardAll state
        do! fn state "Tests.DepCommit.a" "() : Int64 = 41"
        do! fn state "Tests.DepCommit.b" "() : Int64 = 42"
        do! commit state "two fns"

        let! _ = runCli state [ "delete"; "fn"; "Tests.DepCommit.a"; "-y" ]
        let! selected =
          runCli state [ "commit"; "just a"; "--include=Tests.DepCommit.a"; "-y" ]
        Expect.isFalse
          (Option.isSome (looksLikeARuntimeFailure selected))
          $"--include= over a draft holding a Deprecate does not crash: {selected}"

        let! _ = runCli state [ "delete"; "fn"; "Tests.DepCommit.b"; "-y" ]
        do!
          shows
            state
            [ "commit"; "just b"; "--json"; "-y" ]
            "\"committed\":true"
            "and --json commits it"

        do! shows state [ "status" ] "clean" "leaving nothing behind"
      })

/// Archiving a branch records it as a commit of its own and leaves main's draft untouched: nothing
/// it does may be swept up later under a message about something else.
let private archivingABranchCommitsItsEvent =
  cliTestOnMain
    "archiving a branch commits the event, leaving a genuinely clean tree"
    (fun state ->
      task {
        do! discardAll state
        do! switch state "archcommit"
        do! switch state "main"
        let! archived = runCli state [ "branch"; "archive"; "archcommit"; "-y" ]
        Expect.stringContains
          archived
          "archived branch"
          $"the archive went through: {archived}"

        let! status = runCli state [ "status" ]
        Expect.stringContains status "clean" $"and the tree is clean: {status}"

        // The real assertion. "clean" is what `status` prints when the draft is empty AND when it
        // cannot see the archived branch's ops at all, so ask the draft directly.
        let pending =
          Sql.query
            "SELECT count(*) AS n FROM package_ops
           WHERE commit_hash IS NULL AND branch_id = '00000000-0000-0000-0000-000000000001'"
          |> Sql.executeRow (fun read -> read.int64 "n")
        Expect.equal
          pending
          0L
          "and the draft is empty, which is what 'clean' was claiming"

        // The marker is housekeeping, hidden from the default listing; `--all` shows it.
        do!
          shows
            state
            [ "commits"; "3"; "--all" ]
            "archived branch \"archcommit\""
            "the archive is its own commit"
      })

/// `fn` takes a file as well as text, like `module`. A definition is never one token, so one
/// argument that names a file that exists is the file.
let private fnReadsItsDefinitionFromAFile =
  instanceTest "fn takes a source file, the way module does" (fun state ->
    task {
      let file =
        System.IO.Path.Combine(
          System.IO.Path.GetTempPath(),
          "dark-fn-from-file.dark"
        )
      System.IO.File.WriteAllText(
        file,
        "(n: Int64) : Int64 =\n  if n < 2 then n\n  else n * 2\n"
      )

      let! saved = runCli state [ "fn"; "Tests.FnFile.twice"; file ]
      Expect.isFalse
        (saved.Contains "Runtime Error" || saved.Contains "Parse error")
        $"the file is read as the definition: {saved}"
      do!
        shows
          state
          [ "eval"; "Tests.FnFile.twice(21)" ]
          "42"
          "and the fn it defined runs"

      let! _ = runCli state [ "discard"; "--yes" ]
      ()
    })

/// A name that does not resolve fails before any call is made, so its stack is empty and the host
/// prints no stack header over it. One line for a one-line mistake; a real stack, from a fn that
/// failed inside another, still prints.
let private aMissingNameIsOneLineNotAStackHeader =
  instanceTest
    "eval of a name that does not exist prints the error and no empty call stack"
    (fun state ->
      task {
        let! missing = runCli state [ "eval"; "Tests.Nope.missing(1)" ]
        Expect.stringContains missing "not found" $"the error itself: {missing}"
        Expect.isFalse
          (missing.Contains "Call stack")
          $"and no stack header over an empty stack: {missing}"

        let! _ = runCli state [ "fn"; "Tests.Stk.boom"; "() : Int64 = 1 / 0" ]
        let! deep = runCli state [ "eval"; "Tests.Stk.boom()" ]
        Expect.stringContains
          deep
          "\n  in Stdlib.Int64.Divide.divide\n  called from Stk.boom"
          $"a failure inside a call still shows where: {deep}"
        do! discardAll state
        ()
      })

/// Scripts and agents branch on the exit, so failure has to BE one, not prose. The full sweep of
/// error paths is not wired yet; the two a script hits first are, and this pins them.
let private failuresExitNonzero =
  instanceTest
    "an unknown command and a failed eval exit nonzero; a good command exits zero"
    (fun state ->
      task {
        let! (_, ok) = runCliWithExit state [ "status" ]
        Expect.equal ok 0L "a command that worked exits 0"
        let! (unknownOut, unknown) = runCliWithExit state [ "zzznotacommand" ]
        Expect.stringContains unknownOut "Unknown command" "and says so"
        Expect.equal unknown 1L "an unknown command exits 1"
        let! (evalOut, failed) =
          runCliWithExit state [ "eval"; "Tests.Nope.zzz(1)" ]
        Expect.stringContains evalOut "not found" "the error is still printed"
        Expect.equal failed 1L "a failed eval exits 1"
      })

/// A branch discard deletes the rows its ops wrote along with the ops, so nothing the branch binds is
/// left pointing at an op that is gone, and `status`'s store health stays quiet.
let private aBranchDiscardLeavesNoDanglingTags =
  cliTestOnMain
    "discarding a branch draft leaves no binding pointing at a missing op"
    (fun state ->
      task {
        do! switch state "dangletags"
        do! fn state "Tests.Dangle.f" "() : Int64 = 5"
        let! _ =
          runCli state [ "fn"; "Tests.Dangle.g"; "(): Int64 = Tests.Dangle.f()" ]
        do! discardAll state

        let dangling =
          Sql.query
            "SELECT count(*) AS n FROM locations l
           WHERE l.branch_id <> '00000000-0000-0000-0000-000000000001'
             AND NOT EXISTS (SELECT 1 FROM package_ops p
                             WHERE p.id = l.op_id AND p.branch_id = l.branch_id)"
          |> Sql.executeRow (fun read -> read.int64 "n")
        Expect.equal dangling 0L "every surviving binding points at a real op"

        let! status = runCli state [ "status" ]
        Expect.isFalse
          (status.Contains "store problems")
          $"and store health stays quiet: {status}"

        do! archiveBranches state [ "dangletags" ]
      })

/// What a person sees after a save that failed, on a fresh install: it says so with a failing exit,
/// it stored nothing, and the next commit of something else is not held up by it.
let private failedValueSaveLeavesNothingBehind (onBranch : bool) =
  CliInstance.instanceTest
    (if onBranch then
       "a value that fails on a branch is not saved, and unrelated work on the branch commits"
     else
       "a value that fails is not saved, and unrelated work commits")
    (fun i ->
      task {
        if onBranch then
          let! (made, code) =
            CliInstance.runWithExit i [ "branch"; "create"; "honesty" ]
          Expect.equal code 0L $"the branch exists: {made}"

        let! (saved, code) =
          CliInstance.runWithExit
            i
            [ "val"
              "Tests.SaveHonesty.broken"
              "Stdlib.List.head(noSuchSaveHonestyName)" ]
        Expect.equal code 1L $"a failed save exits non-zero: {saved}"

        let! _ =
          CliInstance.run i [ "fn"; "Tests.SaveHonesty.fine"; "() : Int64 = 5150" ]
        let! committed = CliInstance.run i [ "commit"; "unrelated work"; "-y" ]
        Expect.isFalse
          (committed.Contains "cannot commit")
          $"unrelated work commits, rather than being refused over the failed save: {committed}"
        Expect.stringContains
          committed
          "Tests.SaveHonesty.fine"
          "and it is the unrelated work"

        // Wrapped to the terminal's width and coloured per line, so a phrase can break across
        // lines with escape codes in between.
        let unwrapped =
          System.Text.RegularExpressions.Regex.Replace(
            System.Text.RegularExpressions.Regex.Replace(
              saved,
              "\u001b\\[[0-9;]*m",
              ""
            ),
            "\\s+",
            " "
          )
        Expect.stringContains
          unwrapped
          "Nothing was saved"
          "and says nothing was saved"

        let! read = CliInstance.run i [ "eval"; "Tests.SaveHonesty.broken" ]
        Expect.stringContains read "not found" $"the value was never stored: {read}"
      })

/// A policy file in a format this build can't read grants nothing. The CLI says so, or every
/// refusal looks like one the person chose.
let private oldPolicyFormatIsNamed =
  CliInstance.instanceTest
    "a policy file in an old format is named at startup"
    (fun i ->
      task {
        let! (quiet, _) = CliInstance.runWithExit i [ "eval"; "1 + 1" ]
        Expect.isFalse
          (quiet.Contains "older Dark")
          $"a current policy file brings no warning: {quiet}"

        let older = LibDB.PolicyStore.formatVersion - 1
        let bytes = LibDB.PolicyStore.toBytes LibDB.PolicyStore.empty
        use stream = new System.IO.MemoryStream(bytes)
        use reader = new System.IO.BinaryReader(stream)
        LibSerialization.Binary.Serializers.Common.String.read reader
        |> ignore<string>
        use writer = new System.IO.BinaryWriter(stream)
        writer.Write older
        writer.Flush()
        System.IO.File.WriteAllBytes(
          System.IO.Path.Combine(i.dir, "policy", "policies.bin"),
          bytes
        )

        let! (warned, _) = CliInstance.runWithExit i [ "eval"; "1 + 1" ]
        Expect.stringContains
          warned
          $"is from an older Dark (format {older};"
          "the warning names the file's format"
        Expect.stringContains
          warned
          "dark permissions profile default --yes"
          "and the command that starts again from the defaults"

        let! (applied, code) =
          CliInstance.runWithExit i [ "permissions"; "profile"; "default"; "--yes" ]
        Expect.equal code 0L $"the profile applies over the old file: {applied}"
        Expect.isTrue
          (System.IO.File.Exists(
            System.IO.Path.Combine(i.dir, "policy", $"policies.bin.format-{older}")
          ))
          $"the old file is kept: {applied}"

        let! (after, _) = CliInstance.runWithExit i [ "eval"; "1 + 1" ]
        Expect.isFalse
          (after.Contains "older Dark")
          $"and the warning is gone: {after}"
      })

let private deprecationIsReversible =
  instanceTest "delete can be undone" (fun state ->
    task {
      do! author state "Tests.Undep.item" "(x: Int64) : Int64 = x + 4242"

      let! _ =
        runCli state [ "delete"; "fn"; "Tests.Undep.item"; "-m"; "t"; "--yes" ]
      let! hidden = runCli state [ "ls"; "Tests.Undep" ]
      Expect.stringContains
        hidden
        "No visible items in /Tests.Undep."
        "a deleted item leaves the module listing empty"

      do!
        shows
          state
          [ "undeprecate"; "Tests.Undep.item" ]
          "Undeprecated"
          "undeprecate reports what it did"

      do! shows state [ "ls"; "Tests.Undep" ] "  item" "the item appears again"

      do! shows state [ "eval"; "Tests.Undep.item(1)" ] "4243" "and it still runs"
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

    execSql
      $"DELETE FROM package_ops WHERE commit_hash IN
          (SELECT hash FROM commits WHERE message IN {messages})"

    execSql $"DELETE FROM commits WHERE message IN {messages}"

    execSql
      "DELETE FROM package_ops WHERE commit_hash IS NULL
         AND branch_id = '00000000-0000-0000-0000-000000000001'
         AND id IN (SELECT DISTINCT l.op_id FROM locations l
                    WHERE l.owner = 'Ux' AND l.modules = 'Money')"

    execSql "DELETE FROM locations WHERE owner = 'Ux' AND modules = 'Money'"

  }

let private theWorkedExampleWorks =
  cliTestOnMain "the worked example in `docs scm` does what it says" (fun state ->
    task {
      do! resetWorkedExample ()
      do! switch state "main"

      // Commit whatever else is uncommitted: the example claims counts ("2 items
      // changed"), which are only about the example if the draft starts empty.
      // Committing is non-destructive; discarding would take other tests' work with it.
      let _ = LibDB.Inserts.commitAllAsBaseline "worked-example setup"

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
      let! _ = runCli state [ "fn"; "Tests.Cache.v"; "(x: Int64) : Int64 = x + 1" ]

      do! shows state [ "eval"; "Tests.Cache.v(0)" ] "1" "the first version runs"

      let! _ = runCli state [ "fn"; "Tests.Cache.v"; "(x: Int64) : Int64 = x + 2" ]

      do!
        shows
          state
          [ "eval"; "Tests.Cache.v(0)" ]
          "2"
          "and so does the edit, without restarting"
    })

let private deprecationTakesEffectInTheSameProcess =
  cliTest "marking a fn harmful takes effect without restarting" (fun state ->
    task {
      let! _ = runCli state [ "fn"; "Tests.Harm.f"; "(x: Int64) : Int64 = x + 31" ]

      do! shows state [ "eval"; "Tests.Harm.f(0)" ] "31" "it runs to begin with"

      let! _ =
        runCli
          state
          [ "deprecate"
            "fn"
            "Tests.Harm.f"
            "--block-execution"
            "-m"
            "t"
            "--yes" ]

      // The harmful set is cached for the life of the process, so a REPL session that
      // marks something dangerous and keeps running is where it has to be dropped.
      do!
        shows
          state
          [ "eval"; "Tests.Harm.f(0)" ]
          "Harmful"
          "and it halts as soon as it's marked"

      let! _ = runCli state [ "undeprecate"; "Tests.Harm.f" ]

      do!
        shows
          state
          [ "eval"; "Tests.Harm.f(0)" ]
          "31"
          "and runs again as soon as it's unmarked"
    })

// The `--allow-harmful` pair of tests lived here and CANNOT work here, which is worth stating rather
// than leaving as a gap someone re-adds.
//
// `runCli` builds an RT-level `ExecutionState`, and that default is permissive BY DESIGN ("tests run
// permissive" in `Execution.createState`). The harmful gate is narrowed by the CLI HOST, per entry
// point, so in this harness the halt never fires and the test asserts a halt that cannot happen.
//
// It is tested where it can be: `harmfulGate` in `packages/darklang/tests/cli/deprecate.dark`, which drives
// a real CLI process and asserts BOTH directions -- a fn marked harmful refuses to run, and
// `--allow-harmful` runs it anyway.

/// Every `--json` surface emits parseable JSON.
///
/// `--json` is the agent-facing half of this CLI, and a broken one fails in a way a human never
/// sees: the pretty output is fine while the machine output is a stack trace. A `String` field in a
/// JSON record lets an unconverted id reach the boundary, which is what typing branch ids is meant
/// to rule out.
///
/// The refusal sweeps in `CliSurface.Tests.fs` do not reach this: `--json` is not a bogus argument.
let private everyJsonSurfaceParses =
  instanceTest "every --json surface emits parseable JSON" (fun state ->
    task {
      do! fn state "Tests.Json.probe" "() : Int64 = 1"

      let surfaces =
        [ [ "status" ]
          [ "commit" ]
          [ "commits" ]
          [ "branches" ]
          [ "conflicts" ]
          [ "constraints" ]
          [ "deps"; "Tests.Json.probe" ] ]

      for argv in surfaces do
        let! output = runCli state (argv @ [ "--json" ])
        let body = output.Trim().Split('\n') |> Array.last

        // Parsing is the assertion. A command that printed an error instead of JSON fails here, which
        // is the failure mode worth catching: it looks fine to a person reading the terminal.
        try
          System.Text.Json.JsonDocument.Parse body
          |> ignore<System.Text.Json.JsonDocument>
        with e ->
          let cmd = String.concat " " argv

          Expect.isTrue
            false
            $"`dark {cmd} --json` did not emit JSON: {e.Message}. Got: {body}"

      return ()
    })

/// `dark edit` changes an item without retyping it, in both the shapes it has to serve.
///
/// The two shapes exist because the two callers want opposite things: a person wants $EDITOR, a
/// script wants no terminal at all.
///
/// `--raw` is the other half: it reads an item back out without a trailer or a `let f ` prefix, so a
/// script never screen-scrapes the display.
let private editChangesAnItemWithoutRetypingIt =
  instanceTest
    "edit round-trips an item through --raw, and leaves its siblings alone"
    (fun state ->
      task {
        do! author state "Tests.Edit.target" "(x: Int64) : Int64 = x * 2"
        do! fn state "Tests.Edit.sibling" "() : Int64 = 9"

        // `--raw` must be exactly what the authoring path accepts: a whole declaration, no trailer, no
        // capabilities line, no highlighting.
        let! raw = runCli state [ "view"; "Tests.Edit.target"; "--raw" ]
        Expect.stringContains raw "let target" "raw output is a whole declaration"
        Expect.isFalse (raw.Contains "capabilities:") "with no capabilities trailer"

        // The scriptable shape: no editor, no terminal.
        let dir = System.IO.Path.GetTempPath()
        let file = System.IO.Path.Combine(dir, "dark-edit-test.dark")
        System.IO.File.WriteAllText(file, raw.Replace("x * 2", "x * 5"))

        do!
          shows
            state
            [ "edit"; "Tests.Edit.target"; file ]
            "Updated Tests.Edit.target"
            "names the updated declaration"

        do!
          shows state [ "eval"; "Tests.Edit.target(3)" ] "15" "the edit took effect"

        // The module's other declarations are not collateral.
        do!
          shows
            state
            [ "eval"; "Tests.Edit.sibling()" ]
            "9"
            "the sibling in the same module is untouched"

        // And an edit PROPAGATES. `edit` lands through `dark module`, whose op scan picks out `SetName`
        // in a `filterMap`, where a pattern of the wrong arity is a silent no-match: the edit looks fine
        // and no dependent follows.
        do!
          author
            state
            "Tests.Edit.caller"
            "(): Int64 = (Tests.Edit.target(1)) + 100"

        do!
          shows
            state
            [ "eval"; "Tests.Edit.caller()" ]
            "105"
            "the caller sees the current target"

        System.IO.File.WriteAllText(file, raw.Replace("x * 2", "x * 9"))
        let! _ = runCli state [ "edit"; "Tests.Edit.target"; file ]

        do!
          shows
            state
            [ "eval"; "Tests.Edit.caller()" ]
            "109"
            "and follows the edit, rather than staying on the old version"

        // Refusals a person actually hits.
        do!
          shows
            state
            [ "edit" ]
            "`dark edit` needs an argument"
            "a bare `edit` says how to use it"

        do!
          shows
            state
            [ "edit"; "Tests.Edit" ]
            "is a module"
            "a module is refused, and named as the reason"

        do!
          shows
            state
            [ "edit"; "Tests.Edit.nope" ]
            "Cannot edit"
            "a name that isn't there is refused"

        System.IO.File.Delete file
        return ()
      })

/// One NAME holds one item, whatever KIND it is. Binding a fn over a name that held a value replaces
/// it rather than leaving both live.
///
/// The failure it guards against is two live bindings for one name, differing only by kind. Nothing
/// downstream expects that: `ls` would list the name twice, and which one a caller resolves would come
/// down to query order.
let private aNameHoldsOneItemWhateverItsKind =
  cliTest "binding a fn over a name that held a value replaces it" (fun state ->
    task {
      do! value state "Tests.Displace.thing" "42"
      do!
        shows
          state
          [ "eval"; "Tests.Displace.thing" ]
          "42"
          "the value is what the name means"

      // A second value alongside it, so the assertion below is about DISPLACEMENT and not about the
      // module having no values at all.
      do! value state "Tests.Displace.other" "9"

      do! fn state "Tests.Displace.thing" "() : Int64 = 7"

      do!
        shows
          state
          [ "eval"; "Tests.Displace.thing()" ]
          "7"
          "the name now means the fn"

      let! listing = runCli state [ "ls"; "Tests.Displace" ]
      Expect.stringContains listing "thing" "the name is still there"
      Expect.stringContains listing "other" "and the untouched value still is too"

      // The value section must hold `other` and NOT `thing`: one live binding per name, so the
      // displaced value is gone from the listing rather than sitting beside the fn.
      //
      // Sliced by "the lines under Values: until the next section header", not by a fixed pair
      // of headers: the listing's section ORDER is a UI decision.
      let valuesSection =
        let lines = listing.Split '\n'
        match lines |> Array.tryFindIndex (fun l -> l.Contains "Values:") with
        | None -> ""
        | Some i ->
          lines[i + 1 ..]
          |> Array.takeWhile (fun l -> not (l.TrimEnd().EndsWith ":"))
          |> String.concat "\n"

      Expect.stringContains
        valuesSection
        "other"
        "the surviving value is listed as a value"
      Expect.isFalse
        (valuesSection.Contains "thing")
        "the displaced value is NOT still listed beside the fn that replaced it"

      return ()
    })

/// Record a pending conflict directly, through the same F# a merge or a rebase records with. The
/// candidates are empty because nothing here reads them; what is under test is which branch the row
/// lands on and which one an answer reaches.
let private recordConflict
  (branchId : LibExecution.ProgramTypes.BranchId)
  (id : string)
  (owner : string)
  (modules : string)
  (name : string)
  (autoResolvedTo : string)
  : unit =
  LibDB.ScmActions.recordConflicts
    branchId
    [ { LibDB.ScmDetect.Conflict.id = id
        owner = owner
        modules = modules
        name = name
        itemType = "fn"
        part = ""
        kind = "same-name-different-hash"
        candidates = []
        autoResolvedTo = autoResolvedTo
        reason = "test"
        status = "pending"
        resolvedBy = "" } ]


/// Run <param f> with the SCM action stopped at <param step> (`ScmActions.Testing.failAt`), which is
/// what a store error there looks like to the CLI.
let private withActionStoppedAt (step : string) (f : unit -> Task<'a>) : Task<'a> =
  task {
    LibDB.ScmActions.Testing.failAt.Value <- step
    try
      return! f ()
    finally
      LibDB.ScmActions.Testing.failAt.Value <- null
  }

/// `commit --json -y` is how an agent commits, so a commit the store refuses must not exit 0 with
/// `committed: false`: `dark commit --json -y && dark push` would carry on as if it had worked. It has
/// to exit 1 and say why on stderr, with stdout still the JSON alone.
let private aFailedJsonCommitExitsNonzero =
  cliTestOnMain
    "commit --json -y that the store refuses exits 1 and says why"
    (fun state ->
      task {
        do! discardAll state
        do! fn state "Tests.JsonCommitFails.f" "() : Int64 = 12"

        let! (out, err, status) =
          withActionStoppedAt "commit.row" (fun () ->
            runCliStreams state [ "commit"; "json commit fails"; "--json"; "-y" ])
        Expect.equal
          status
          1
          $"a commit that did not happen exits 1, got: {out} / {err}"
        Expect.stringContains err "stopped at commit.row" "and stderr says why"
        use doc = System.Text.Json.JsonDocument.Parse out
        Expect.isFalse
          (doc.RootElement.GetProperty("committed").GetBoolean())
          $"and stdout is still the JSON, saying nothing was committed: {out}"
        do! shows state [ "status" ] "1 item changed" "the draft is still there"

        do! discardAll state
      })

/// Acking a conflict is a write, and so is settling one by override. When the write fails, both
/// answer with the failure, not "acked" or success.
let private aConflictCloseThatFailsIsReported =
  cliTestOnMain
    "an ack or an override whose close fails says so and exits 1"
    (fun state ->
      task {
        do! discardAll state
        let mainId = LibExecution.ProgramTypes.BranchId.Main

        // ack: no candidates needed.
        recordConflict mainId "ackfail001" "Tests" "CloseFails" "acked" "aaa"
        let! (ackOut, ackStatus) =
          withActionStoppedAt "conflict.status" (fun () ->
            runCliWithStatus state [ "conflicts"; "ack"; "ackfail001" ])
        Expect.equal ackStatus 1 $"a failed ack exits 1, got: {ackOut}"
        Expect.isFalse
          (ackOut.Contains "the auto-pick stands")
          $"and does not claim it worked: {ackOut}"
        let! (jsonOut, jsonStatus) =
          withActionStoppedAt "conflict.status" (fun () ->
            runCliWithStatus state [ "conflicts"; "ack"; "ackfail001"; "--json" ])
        Expect.equal jsonStatus 1 $"and so does the --json form, got: {jsonOut}"
        do!
          shows
            state
            [ "conflicts"; "list" ]
            "ackfail001"
            "the conflict is still pending"
        do! run state [ "conflicts"; "ack"; "ackfail001" ]
        do!
          lacks
            state
            [ "conflicts"; "list" ]
            "ackfail001"
            "and acking again closes it"

        // override: the candidate it takes must be a real version, so author one.
        do! fn state "Tests.CloseFails.f" "() : Int64 = 13"
        let! (LibExecution.ProgramTypes.Hash real) =
          findPackageFn "Tests.CloseFails.f"
        let other = String.replicate 64 "0"
        LibDB.ScmActions.recordConflicts
          mainId
          [ { LibDB.ScmDetect.Conflict.id = "ovrfail001"
              owner = "Tests"
              modules = "CloseFails"
              name = "f"
              itemType = "fn"
              part = ""
              kind = "same-name-different-hash"
              candidates =
                [ { side = "local"
                    hash = real
                    removed = false
                    text = ""
                    originTs = "2026-01-01T00:00:00.000Z"
                    author = "" }
                  { side = "incoming"
                    hash = other
                    removed = false
                    text = ""
                    originTs = "2026-01-02T00:00:00.000Z"
                    author = "" } ]
              autoResolvedTo = other
              reason = "test"
              status = "pending"
              resolvedBy = "" } ]
        let! (ovrOut, ovrStatus) =
          withActionStoppedAt "conflict.status" (fun () ->
            runCliWithStatus state [ "conflicts"; "override"; "ovrfail001"; "A" ])
        Expect.equal
          ovrStatus
          1
          $"an override whose close fails exits 1, got: {ovrOut}"
        Expect.stringContains ovrOut "override failed" "and says so"
        // Folding the `Decision` op also closes the record, so the state after the failure is not what
        // is asserted; that the failure is reported, and that settling again finishes, is.
        let status () =
          task {
            let rows =
              Sql.query "SELECT status FROM conflicts WHERE id = 'ovrfail001'"
              |> Sql.executeRows (fun read -> read.string "status")
            return String.concat "," rows
          }
        let! (againOut, againStatus) =
          runCliWithStatus state [ "conflicts"; "override"; "ovrfail001"; "A" ]
        Expect.equal againStatus 0 $"settling again finishes it, got: {againOut}"
        let! afterRetry = status ()
        Expect.equal afterRetry "overridden" "and closes it"

        Sql.query "DELETE FROM conflicts WHERE modules = 'CloseFails'"
        |> Sql.executeStatement
        do! discardAll state
      })


/// `--include=<caller>` on a branch committed the caller and left its dependency's NAME in the
/// draft, so a reader downstream could run the caller but could not find what it called.
let aPartialCommitTakesItsDependencysName =
  instanceTest
    "a partial commit on a branch carries the names it depends on"
    (fun state ->
      task {
        do! start state
        // The dependency's content already exists, but its branch-local name
        // below still has to come along. Do not depend on seed/test run order.
        do! fn state "Tests.Partial2.committed" "() : Int64 = 5"
        do! commit state "dependency content already committed"
        do! switch state "partialbr"
        do! fn state "Tests.Partial2.dep" "() : Int64 = 5"
        do! fn state "Tests.Partial2.caller" "(): Int64 = Tests.Partial2.dep() + 1"

        do! commitOnly state "caller" "Tests.Partial2.caller"
        do! evals state "Tests.Partial2.caller()" "6" "the caller runs here"
        do!
          clean
            state
            "and nothing of it is left uncommitted: the dependency came along"
        do! onMain state
      })


/// An alias saved with the same code as another declaration shares its `AddFn`. When a later save
/// resolves that code's forward reference, the draft refresh rewrites the `AddFn` and must keep the
/// alias's body in the log: a re-fold of the log then still binds the alias to a body, and a
/// dependents read that needs it finds it.
let private aRefreshedDraftKeepsAnAliasBody =
  instanceTest "a draft refresh keeps the body an alias still binds" (fun state ->
    task {
      do! fn state "Tests.Alias.helper" "(): Int64 = 40"
      let body = "(): Int64 = Tests.Alias.helper() + Tests.Alias.later()"
      do! fn state "Tests.Alias.f" body
      do! fn state "Tests.Alias.g" body
      do! fn state "Tests.Alias.later" "(): Int64 = 2"
      // records the callers' reference names, which a dependents read checks against their bodies
      do! run state [ "rename"; "Tests.Alias.helper"; "Tests.Alias.helper2" ]

      let dir =
        match state with
        | Instance i -> i.dir
        | InProcess _ -> failtest "needs an instance's store"
      use conn =
        new Microsoft.Data.Sqlite.SqliteConnection(
          $"""Data Source={System.IO.Path.Combine(dir, "data.db")};Pooling=False"""
        )
      conn.Open()
      use cmd = conn.CreateCommand()
      // what a schema change does: drop the projections and fold the log again on the next start
      cmd.CommandText <-
        (LibDB.Seed.projectionTables
         |> List.map (fun t -> $"DELETE FROM {t};")
         |> String.concat "")
        + LibDB.AppliedOps.reset
      cmd.ExecuteNonQuery() |> ignore<int>
      conn.Close()

      let! (out, err, status) =
        runCliStreams state [ "deps"; "usedby"; "Tests.Alias.helper2" ]
      Expect.equal status 0 $"the dependents read succeeds, got: {out} / {err}"
      Expect.stringContains (plain out) "f, g" "both callers are found"
      do! evals state "Tests.Alias.g()" "42" "the alias runs after the re-fold"
      let! status = runCliPlain state [ "status" ]
      Expect.isFalse
        (status.Contains "no content")
        $"no name is bound to a missing body, got: {status}"
    })


/// `deps usedby` that cannot read a caller says so and exits 1, rather than raising. A second
/// indexed name for the callee makes the caller's names ambiguous, so the read needs the caller's
/// stored body, and the body is gone.
let private aDependentsReadThatFailsIsReported =
  damagedTest "deps usedby reports a caller it cannot read" (fun state ->
    task {
      do! fn state "Tests.DepFail.callee" "(): Int64 = 1"
      do! fn state "Tests.DepFail.caller" "(): Int64 = Tests.DepFail.callee() + 1"
      do! commit state "dep fail"
      do! switch state "depfail"

      let dir =
        match state with
        | Instance i -> i.dir
        | InProcess _ -> failtest "needs an instance's store"
      let db = System.IO.Path.Combine(dir, "data.db")
      use conn =
        new Microsoft.Data.Sqlite.SqliteConnection(
          $"Data Source={db};Pooling=False"
        )
      conn.Open()
      use cmd = conn.CreateCommand()
      cmd.CommandText <-
        "WITH caller AS (SELECT item_hash FROM locations
           WHERE owner = 'Tests' AND modules = 'DepFail' AND name = 'caller' AND item_hash <> '')
         INSERT INTO package_dependencies
           SELECT item_hash, depends_on_hash, depends_on_item_type, depends_on_owner,
                  depends_on_modules, 'calleeAlias'
           FROM package_dependencies
           WHERE item_hash IN (SELECT item_hash FROM caller) AND depends_on_name = 'callee';
         DELETE FROM package_functions WHERE hash IN
           (SELECT item_hash FROM locations
            WHERE owner = 'Tests' AND modules = 'DepFail' AND name = 'caller')"
      cmd.ExecuteNonQuery() |> ignore<int>
      conn.Close()

      let why =
        "Couldn't read the dependents of Tests.DepFail.callee: Cannot read reference names: stored declaration is missing"

      for args in
        [ [ "deps"; "usedby"; "Tests.DepFail.callee" ]
          [ "deps"; "usedby"; "Tests.DepFail.callee"; "--deep" ]
          [ "deps"; "Tests.DepFail.callee" ]
          [ "deps"; "Tests.DepFail.callee"; "--json" ] ] do
        let! (out, err, status) = runCliStreams state args
        let ran = String.concat " " args
        Expect.equal status 1 $"{ran} fails, got: {out} / {err}"
        Expect.stringContains (plain err) why $"{ran} names what it couldn't read"
        Expect.isFalse
          (err.Contains "Internal error" || err.Contains "Uncaught exception")
          $"{ran} reports rather than raising, got: {err}"
    })


/// A dependency review that cannot read what it needs is reported, rather than raising. Here a
/// second indexed name for the callee makes the caller's names ambiguous, so the review reads the
/// caller's stored body, and the body is gone.
let private aDependencyReviewThatFailsIsReported =
  damagedTest "constraints reports a dependency review that fails" (fun state ->
    task {
      do! fn state "Tests.DepFail.callee" "(): Int64 = 1"
      do! fn state "Tests.DepFail.caller" "(): Int64 = Tests.DepFail.callee() + 1"
      do! commit state "dep fail"
      do! switch state "depfail"

      let dir =
        match state with
        | Instance i -> i.dir
        | InProcess _ -> failtest "needs an instance's store"
      let db = System.IO.Path.Combine(dir, "data.db")
      use conn =
        new Microsoft.Data.Sqlite.SqliteConnection(
          $"Data Source={db};Pooling=False"
        )
      conn.Open()
      use cmd = conn.CreateCommand()
      cmd.CommandText <-
        "WITH caller AS (SELECT item_hash FROM locations
           WHERE owner = 'Tests' AND modules = 'DepFail' AND name = 'caller' AND item_hash <> '')
         INSERT INTO package_dependencies
           SELECT item_hash, depends_on_hash, depends_on_item_type, depends_on_owner,
                  depends_on_modules, 'calleeAlias'
           FROM package_dependencies
           WHERE item_hash IN (SELECT item_hash FROM caller) AND depends_on_name = 'callee';
         DELETE FROM package_functions WHERE hash IN
           (SELECT item_hash FROM locations
            WHERE owner = 'Tests' AND modules = 'DepFail' AND name = 'caller')"
      cmd.ExecuteNonQuery() |> ignore<int>
      conn.Close()

      let why = "Cannot read reference names: stored declaration is missing"

      let! (out, err, status) = runCliStreams state [ "constraints" ]
      Expect.equal status 0 $"constraints still answers, got: {out} / {err}"
      Expect.stringContains
        err
        $"couldn't check everything: {why}"
        "naming the check it couldn't run"
      Expect.stringContains
        (plain err)
        "nothing found by the checks that DID run."
        "and not claiming there is nothing to find"

      let! (out, _, status) = runCliStreams state [ "constraints"; "--json" ]
      Expect.equal status 0 $"constraints --json answers, got: {out}"
      use doc = System.Text.Json.JsonDocument.Parse out
      Expect.equal
        [ for b in doc.RootElement.GetProperty("blocked").EnumerateArray() ->
            b.GetString() ]
        [ why ]
        "with the reason under blocked"
    })


/// A store whose deprecation query fails says what SQLite said and exits 1.
let private aFailingDeprecationQueryIsAStoreError =
  damagedTest "a deprecation query that fails names the SQL failure" (fun state ->
    task {
      let dir =
        match state with
        | Instance i -> i.dir
        | InProcess _ -> failtest "needs an instance's store"
      let db = System.IO.Path.Combine(dir, "data.db")
      use conn =
        new Microsoft.Data.Sqlite.SqliteConnection(
          $"Data Source={db};Pooling=False"
        )
      conn.Open()
      use cmd = conn.CreateCommand()
      cmd.CommandText <- "ALTER TABLE deprecations RENAME TO deprecationsgone"
      cmd.ExecuteNonQuery() |> ignore<int>
      conn.Close()

      let! (out, status) = runCliWithStatus state [ "eval"; "1 + 1" ]
      Expect.equal status 1 $"the command fails, got: {out}"
      Expect.stringContains out "SQL query failed" "as a failed query"
      Expect.stringContains
        out
        "no such table: deprecations"
        "naming what SQLite said"
    })


let tests : List<Test> =
  [ failedValueSaveLeavesNothingBehind false
    failedValueSaveLeavesNothingBehind true
    oldPolicyFormatIsNamed
    deprecationIsReversible
    theWorkedExampleWorks
    editsAreVisibleInTheSameProcess
    deprecationTakesEffectInTheSameProcess
    aDeprecationCommitsByEitherPath
    archivingABranchCommitsItsEvent
    fnReadsItsDefinitionFromAFile
    aMissingNameIsOneLineNotAStackHeader
    failuresExitNonzero
    aBranchDiscardLeavesNoDanglingTags
    aNameHoldsOneItemWhateverItsKind
    editChangesAnItemWithoutRetypingIt
    everyJsonSurfaceParses
    aPartialCommitTakesItsDependencysName
    aFailedJsonCommitExitsNonzero
    aConflictCloseThatFailsIsReported
    aRefreshedDraftKeepsAnAliasBody
    aDependentsReadThatFailsIsReported
    aDependencyReviewThatFailsIsReported
    aFailingDeprecationQueryIsAStoreError ]
