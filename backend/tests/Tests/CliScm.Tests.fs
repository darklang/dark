/// The SCM verbs through the CLI: branch/commit/merge/discard/conflicts/review,
/// their refusals, their exit codes, and what each leaves in the draft and the log.
/// Run order and sequencing live in CliTraces.Tests.fs, which composes this list.
module Tests.CliScm

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
open Tests.CliDsl

/// Code the CLI runs cannot point the unguarded HTTP transport at whatever it likes.
///
/// `httpGetUnsafeBytes` turns the SSRF guards off so a relay behind loopback or a tailnet is reachable at
/// all. It is in the general builtin set, so a function pulled from a peer could call it -- and did: A
/// authors a function that calls it, syncs, B pulls and runs it, and it reads a service on B's loopback.
///
/// What stops that now is the caller gate rather than an origin allowlist: every frame must be bundled
/// first-party code. An expression typed at `eval` is not, which is the same position pulled code is in.
let private unguardedTransportRefusesGuestCallers =
  cliTest
    "the unguarded transport refuses a caller that is not bundled code"
    (fun state ->
      task {
        let! out =
          runCli
            state
            [ "eval"
              "match Builtin.httpGetUnsafeBytes \"http://127.0.0.1:9/x\" with | Ok _ -> \"REACHED\" | Error e -> e" ]

        Expect.stringContains
          out
          "restricted to trusted first-party"
          $"the refusal says why, got: {out}"
        Expect.isFalse (out.Contains "REACHED") "and nothing was fetched"
        return ()
      })

/// `dark diff` with no branch named answers about the DRAFT.
let private bareDiffShowsTheDraft =
  cliTest "diff with no branch shows the draft" (fun state ->
    task {
      let! _ = runCli state [ "discard"; "--yes" ]
      let! clean = runCli state [ "diff" ]
      Expect.stringContains clean "clean" $"a clean tree says so, got: {clean}"
      Expect.isFalse (clean.Contains "usage:") "and does not answer with usage"

      do! fn state "Tests.BareDiff.item" "() : Int64 = 3L"
      let! withDraft = runCli state [ "diff" ]
      Expect.stringContains withDraft "CHANGED" "a draft is listed as changes"
      Expect.stringContains
        withDraft
        "Tests.BareDiff.item"
        $"and names what changed, got: {withDraft}"

      let! _ = runCli state [ "discard"; "--yes" ]
      return ()
    })

/// A subcommand typed without its arguments must not leave state behind.
let private bareSubcommandDoesNotBecomeABranch =
  cliTest "a branch subcommand with no arguments creates nothing" (fun state ->
    task {
      let! before = runCli state [ "branches" ]

      do!
        shows
          state
          [ "branch"; "rename" ]
          "missing its arguments"
          "a subcommand with no arguments says so"

      let! after = runCli state [ "branches" ]
      Expect.equal after before "and the branch list is untouched"

      // A genuine near-miss still gets the typo message rather than this one.
      do!
        shows
          state
          [ "branch"; "renam" ]
          "Did you mean"
          "a typo is still named as a typo"

      return ()
    })

/// A commit id is derived from what the commit HOLDS and where it sits, not only from who typed what and
/// when, and the graph is a chain rather than a flat list.
let private commitsChainToTheirParent =
  cliTest "a commit names the one it follows" (fun state ->
    task {
      // `commit` prints `commit <8 hex> -- n op(s).`; `show` prints 16. So the short id from the first
      // commit is a prefix of what the second one reports as its parent.
      // Anchored on the word `commit`, not "the first 8 hex characters anywhere": the output also lists
      // what changed, and a name can be all hex by coincidence.
      let idFrom (output : string) : string =
        let words =
          output.Split([| ' '; '\n'; '\r' |]) |> Array.filter (fun w -> w <> "")
        words
        |> Array.tryFindIndex (fun w -> w = "commit")
        |> Option.bind (fun i -> Array.tryItem (i + 1) words)
        |> Option.defaultValue ""

      do! fn state "Tests.Chain.one" "() : Int64 = 1L"
      let! first = runCli state [ "commit"; "chain one"; "-y" ]
      let firstId = idFrom first
      Expect.isFalse (firstId = "") $"the first commit printed an id, got: {first}"

      do! fn state "Tests.Chain.two" "() : Int64 = 2L"
      let! second = runCli state [ "commit"; "chain two"; "-y" ]
      let secondId = idFrom second
      Expect.isFalse
        (secondId = "")
        $"the second commit printed an id, got: {second}"
      Expect.notEqual
        secondId
        firstId
        "two commits of different work get different ids"

      let! shown = runCli state [ "show"; secondId ]
      Expect.stringContains
        shown
        "follows"
        "the second commit says it follows something"
      Expect.stringContains
        shown
        firstId
        $"and the thing it follows is the first commit, got: {shown}"

      let! _ = runCli state [ "discard"; "--yes" ]
      return ()
    })

/// Editing something on a BRANCH repoints what calls it, there and then.
///
/// Propagation runs off "what versions has this name had", and the branch-blind form of that lookup
/// reads `locations` -- main's projection, which a branch never writes to. On a branch it then finds
/// no earlier version, concludes nothing needs repointing, and leaves every caller on the version you
/// just edited past: the caller keeps returning the old answer while the callee plainly says otherwise.
///
/// Asserted by RUNNING the caller rather than by reading the draft: what a repoint is for is that the
/// thing above you gets the new answer.
let editingOnABranchRepointsItsCallers =
  cliTestOnMain "editing an item on a branch repoints what calls it" (fun state ->
    task {
      do!
        shows
          state
          [ "switch"; "cli-propagate-branch" ]
          "cli-propagate-branch"
          "the test is on the branch"

      do! fn state "Tests.BranchProp.base" "() : Int64 = 1L"
      do!
        author
          state
          "Tests.BranchProp.caller"
          "() : Int64 = Stdlib.Int64.multiply (Tests.BranchProp.base ()) 7L"

      do!
        shows
          state
          [ "eval"; "Tests.BranchProp.caller ()" ]
          "7"
          "the caller runs against the first version"

      do! fn state "Tests.BranchProp.base" "() : Int64 = 2L"
      let! after = runCli state [ "eval"; "Tests.BranchProp.caller ()" ]

      do! shows state [ "switch"; "main" ] "main" "and it put the run back on main"

      Expect.stringContains
        after
        "14"
        "the caller was repointed onto the edit, on the branch"
    })

/// A branch records what put each of its bindings there (`op_branches.source`), so on a branch `status`
/// can split what you typed from what followed, and `pin` un-stages the BRANCH's repoint. Without the
/// column `pin` on a branch finds MAIN's staged repoint and drops it -- a pin issued on a branch
/// reverting main -- and `status` on a branch reports main's followers as the branch's.
let private aBranchKnowsWhatFollowed =
  cliTestOnMain
    "status and pin on a branch act on the branch's own followers, not main's"
    (fun state ->
      task {
        // On main: a base, a caller, committed. Then an edit to the base on MAIN, uncommitted, so main's
        // draft holds a staged repoint of its own for the pin on the branch to leave alone.
        do! switch state "main"
        let! _ =
          runCli state [ "fn"; "Tests.BranchFollow.base"; "() : Int64 = 1001L" ]
        do!
          author
            state
            "Tests.BranchFollow.caller"
            "() : Int64 = Stdlib.Int64.multiply (Tests.BranchFollow.base ()) 7L"
        do! commit state "branchfollow v1"
        // Other tests leave followers in main's draft too, so main is asserted RELATIVE to itself.
        let followedIn (status : string) =
          let m =
            System.Text.RegularExpressions.Regex.Match(status, @"(\d+) followed")
          if m.Success then int m.Groups[1].Value else 0
        let! mainBefore = runCli state [ "status" ]
        let! _ =
          runCli state [ "fn"; "Tests.BranchFollow.base"; "() : Int64 = 1003L" ]
        let! mainStatus = runCli state [ "status" ]
        Expect.equal
          (followedIn mainStatus)
          (followedIn mainBefore + 1)
          $"main's draft gained its own follower: {mainStatus}"

        // On a branch: edit the base; the caller follows, on the branch.
        do! switch state "follow-branch"
        let! _ =
          runCli state [ "fn"; "Tests.BranchFollow.base"; "() : Int64 = 1002L" ]
        let! after = runCli state [ "eval"; "Tests.BranchFollow.caller ()" ]
        Expect.stringContains
          after
          "7014"
          "the caller followed the edit, on the branch"
        do!
          shows
            state
            [ "status" ]
            "1 followed"
            "the branch reports its own follower, from its own record"

        // pin on the branch drops the BRANCH's staged repoint: the caller is back on v1's base.
        let! pinned =
          runCli state [ "propagate"; "pin"; "Tests.BranchFollow.caller" ]
        Expect.stringContains
          pinned
          "dropped the staged repoint"
          $"the branch's repoint was un-staged: {pinned}"
        // The branch's own repoint is gone. What the caller resolves to now is what main's live projection
        // says (an overlay over main, draft included), which is main's staged repoint, 21; the branch's
        // 14 is what must be gone.
        let! back = runCli state [ "eval"; "Tests.BranchFollow.caller ()" ]
        Expect.isFalse
          (back.Contains "7014")
          $"the branch no longer holds its repoint: {back}"

        // Main's draft is exactly as it was: the edit and ITS follower.
        do! switch state "main"
        let! mainAfter = runCli state [ "status" ]
        Expect.equal
          (followedIn mainAfter)
          (followedIn mainStatus)
          $"main's own staged repoint is untouched: {mainAfter}"
        do!
          shows
            state
            [ "eval"; "Tests.BranchFollow.caller ()" ]
            "7021"
            "and main's caller still follows main's edit"

        do! discardAll state
        do! archiveBranches state [ "follow-branch" ]
      })

/// A full branch id this store does not hold is REFUSED. Only a name nobody has starts a branch, so
/// `dark switch <a peer's uuid>` cannot quietly mint one named after the uuid and read as success.
let private switchRefusesAForeignId =
  cliTestOnMain
    "switch refuses a uuid this store does not have, rather than starting a branch named after it"
    (fun state ->
      task {
        do! switch state "main"
        let foreign = string (System.Guid.NewGuid())
        do!
          shows
            state
            [ "switch"; foreign ]
            "no branch with id"
            "the switch is refused, saying why"
        do! shows state [ "branch" ] "on main" "and the process stayed where it was"
        let! listed = runCli state [ "branches" ]
        Expect.isFalse
          (listed.Contains foreign)
          "no branch was started under the id's name"
      })

/// A branch that names content main already held (same body, another name) holds only the `SetName`:
/// the `Add` is main's, since an op is identified by what it adds. The bundle must carry that Add
/// anyway, or the receiver, whose main may not have it, gets a name pointing at nothing.
let private aBundleCarriesTheContentItsNamesPointAt =
  cliTestOnMain
    "a branch bundle carries the Add for a body the branch borrowed from main"
    (fun state ->
      task {
        do! fn state "Tests.Borrow.onMain" "() : Int64 = 5005L"
        do! switch state "borrowbr"
        let! _ =
          runCli state [ "fn"; "Tests.Borrow.onBranch"; "() : Int64 = 5005L" ]
        let! log = runCli state [ "log"; "borrowbr" ]
        Expect.isFalse
          (log.Contains "AddFn")
          $"the branch holds only the SetName: {log}"

        let exported = $"{LibConfig.Config.runDir}/bundle-borrow.json"
        let! _ = runCli state [ "branch"; "export"; "borrowbr"; exported ]
        let json = System.IO.File.ReadAllText exported
        let ops =
          System.Text.RegularExpressions.Regex.Matches(json, "\"blobHex\"").Count
        Expect.equal
          ops
          2
          $"the bundle carries the Add as well as the SetName: {json.Substring(0, min 200 json.Length)}"

        do! archiveBranches state [ "borrowbr" ]
      })

/// A merge into main commits what it landed. The branch's ops arrive uncommitted (a pulled branch, or
/// one authored and never committed) and the merge event is authored uncommitted, so without this they
/// sit in main's draft afterwards, reading as "1 item changed" that nobody edited, until the next
/// unrelated commit sweeps them up under its message.
let private aMergeCommitsWhatItLands =
  cliTestOnMain
    "merging a branch leaves main's draft as it was, under a merge commit"
    (fun state ->
      task {
        do! discardAll state
        do! switch state "mergecommit"
        do! fn state "Tests.MergeCommit.f" "() : Int64 = 6006L"
        // Committed on the branch first: merge refuses uncommitted work now, so that it arrives
        // in the parent under a message somebody wrote. What this test is about is the OTHER
        // draft -- main's, which must survive the merge untouched.
        do! commit state "mergecommit work"
        let! merged = runCli state [ "merge"; "mergecommit"; "-y" ]
        Expect.stringContains merged "Merged" $"the merge went through: {merged}"

        do!
          shows
            state
            [ "status" ]
            "clean"
            "main's draft holds nothing the merge landed"
        do!
          shows
            state
            [ "commits"; "3" ]
            "merged branch \"mergecommit\""
            "and a merge commit names it"
        do!
          shows
            state
            [ "eval"; "Tests.MergeCommit.f ()" ]
            "6006"
            "and the work is live on main"

        // The merge deleted the branch's tags, so `log` and `diff` read the merge event
        // instead of reporting a branch that never held anything.
        let! log = runCli state [ "log"; "mergecommit" ]
        Expect.stringContains
          log
          "merged from branch \"mergecommit\""
          $"log of a merged branch reads its merge event: {log}"
        Expect.stringContains log "AddFn f" $"and lists the ops it carried: {log}"
        let! diff = runCli state [ "diff"; "mergecommit" ]
        Expect.stringContains
          diff
          "is merged"
          $"diff of a merged branch says so rather than 'changes nothing': {diff}"
      })

/// An `Unbind` on a branch takes a main name away on that branch, shows in the draft as a removal, and
/// takes it off main when the branch merges. Authored through `SCM.PackageOps.add`, since no verb writes
/// one yet; the importer will, once the reload becomes a diff.
let private anUnbindRemovesANameThroughTheCli =
  cliTestOnMain
    "an unbind removes a name on the branch, then on main when the branch merges"
    (fun state ->
      task {
        do! discardAll state
        do! fn state "Tests.Gone.f" "() : Int64 = 1L"
        do! commit state "gone soon"
        do! switch state "gonebr"
        let! added =
          runCli
            state
            [ "eval"
              "Darklang.SCM.PackageOps.add (Builtin.scmCurrentBranch ()) [ Darklang.LanguageTools.ProgramTypes.PackageOp.Unbind(Darklang.LanguageTools.ProgramTypes.PackageLocation { owner = \"Tests\"; modules = [\"Gone\"]; name = \"f\" }, Stdlib.Option.Option.None) ]" ]
        Expect.stringContains
          added
          "Ok"
          $"the unbind was authored on the branch: {added}"

        // `Ok` with "not found", not `| Error _ -> true`: a runtime crash would otherwise read as
        // "the name is gone", which is the assertion passing for the one reason it must not.
        do!
          shows
            state
            [ "eval"; "Tests.Gone.f ()" ]
            "not found"
            "the name is gone on the branch"
        do! shows state [ "status" ] "1 item removed" "status names the removal"
        let! review = runCli state [ "commit"; "remove f"; "-y" ]
        Expect.stringContains
          review
          "REMOVED (1)"
          $"commit's review lists it: {review}"
        Expect.stringContains review "Tests.Gone.f" $"by name: {review}"

        do! switch state "main"
        do!
          shows
            state
            [ "eval"; "Tests.Gone.f ()" ]
            "1"
            "main still has it before the merge"
        do! shows state [ "merge"; "gonebr"; "-y" ] "Merged" "the branch merges"
        do!
          shows
            state
            [ "eval"; "Tests.Gone.f ()" ]
            "not found"
            "and main no longer has the name"
        do!
          shows
            state
            [ "commits"; "3" ]
            "remove f"
            "the removal's commit travelled with the merge"
      })

/// A draft holding a deprecation can be committed by BOTH paths, and each fails differently without
/// this. `--include=` has to read the `Reference` a `Deprecate` carries as a reference and not as a
/// `Hash`, or it dies at runtime. And `commit --json` must not decide "nothing to commit" from the
/// changed-NAMES list, which a deprecation is never in: an agent then cannot commit one at all, and it
/// ships in the seed as somebody's mystery op.
let private aDeprecationCommitsByEitherPath =
  cliTestOnMain
    "a deprecation-only draft commits, by --include= and by --json"
    (fun state ->
      task {
        do! discardAll state
        do! fn state "Tests.DepCommit.a" "() : Int64 = 41L"
        do! fn state "Tests.DepCommit.b" "() : Int64 = 42L"
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

/// Archiving a branch commits the event it authors. A `BranchEvent` binds no name, so `status` counts
/// nothing and reads clean while the op sits in main's draft, waiting for the next unrelated commit to
/// sweep it up under a message about something else. It also fails the seed guard, which is where it
/// surfaces: a release build refuses over an archive nobody knew was pending.
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
        let! pending =
          Sql.query
            "SELECT count(*) AS n FROM package_ops
           WHERE commit_hash IS NULL AND effective = 1
             AND id NOT IN (SELECT op_id FROM op_branches)"
          |> Sql.executeRowAsync (fun read -> read.int64 "n")
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

/// A bundle carries the sender's COMMITS, not just their ops.
///
/// Without them a pulled branch arrives wholly uncommitted, and the puller's next `commit` sweeps the
/// author's finished work under the puller's message -- the same op filed under a different commit on
/// each machine.
/// `module` took a file and `fn` took text, so the multi-line fn a person had in a file went through
/// a heredoc or not at all (an agent on two machines lost a round to it). A definition is never one
/// token, so one argument that names a file that exists is the file.
let private fnReadsItsDefinitionFromAFile =
  cliTest "fn takes a source file, the way module does" (fun state ->
    task {
      let file =
        System.IO.Path.Combine(
          System.IO.Path.GetTempPath(),
          "dark-fn-from-file.dark"
        )
      System.IO.File.WriteAllText(
        file,
        "(n: Int64) : Int64 =\n  if n < 2L then n\n  else n * 2L\n"
      )

      let! saved = runCli state [ "fn"; "Tests.FnFile.twice"; file ]
      Expect.isFalse
        (saved.Contains "Runtime Error" || saved.Contains "Parse error")
        $"the file is read as the definition: {saved}"
      do!
        shows
          state
          [ "eval"; "Tests.FnFile.twice 21L" ]
          "42"
          "and the fn it defined runs"

      let! _ = runCli state [ "discard"; "--yes" ]
      ()
    })

/// A branch that never touched `f` still calls `f`. When main moves `f`, the branch's callers keep the
/// old version by hash, `rebase` had nothing to reconcile (it compared names the branch TOUCHED), and no
/// constraint said so: the branch ran main's old code silently. Seen on two machines. Now the branch
/// reports the stale usage, and `rebase` repoints it, which is what "accept the parent's changes" means.
let private aBranchLearnsThatMainMovedItsDependency =
  cliTestOnMain
    "a branch's callers of a fn main moved are reported, and rebase repoints them"
    (fun state ->
      task {
        do! discardAll state
        do! fn state "Tests.Moved.dep" "() : Int64 = 1L"
        do! commit state "dep v1"
        do! switch state "movedbr"
        let! _ =
          runCli
            state
            [ "fn"; "Tests.Moved.caller"; "() : Int64 = Tests.Moved.dep ()" ]
        do! commit state "caller"
        do! switch state "main"
        do! fn state "Tests.Moved.dep" "() : Int64 = 2L"
        do! commit state "dep v2"
        do! switch state "movedbr"

        do!
          shows
            state
            [ "eval"; "Tests.Moved.caller ()" ]
            "1"
            "the branch's caller still runs the old dep"
        let! constraints = runCli state [ "constraints" ]
        Expect.stringContains
          constraints
          "Tests.Moved.caller"
          $"and the branch says so: {constraints}"

        let! rebased = runCli state [ "rebase" ]
        Expect.stringContains
          rebased
          "Tests.Moved.caller"
          $"rebase names what it repointed: {rebased}"
        // As a repoint, not as a divergence: nobody on the branch touched the caller, and "your
        // branch's versions win by recency" is advice about a fight that is not happening.
        Expect.stringContains
          rebased
          "now point at the parent's version"
          $"and calls it a repoint: {rebased}"
        Expect.isFalse
          (rebased.Contains "you also touched")
          $"not a divergence: {rebased}"
        do!
          shows
            state
            [ "eval"; "Tests.Moved.caller ()" ]
            "2"
            "and the caller follows main's dep now"

        do! discardAll state
        do! archiveBranches state [ "movedbr" ]
      })

/// `status` printed one constraint count, and on a store with standing constraints it read as "your
/// edit broke N things" (an agent authoring on two machines said exactly that). The count now says
/// how many touch a name this draft binds; a clean tree calls the rest what they are, standing.
let private statusSeparatesTheDraftsConstraintsFromStandingOnes =
  cliTestOnMain
    "status says which constraints this draft caused and which stood already"
    (fun state ->
      task {
        do! discardAll state
        let! _ =
          runCli state [ "fn"; "Tests.F9Kind.dep"; "(x: Int64) : Int64 = x + 1L" ]
        let! _ =
          runCli
            state
            [ "fn"; "Tests.F9Kind.use"; "(x: Int64) : Int64 = Tests.F9Kind.dep x" ]
        do! commit state "f9 base"

        // A kind change cannot be repointed, so the caller is left behind: a constraint THIS draft made.
        do! value state "Tests.F9Kind.dep" "5L"
        do!
          shows
            state
            [ "status" ]
            "1 from this draft"
            "the constraint the draft caused is attributed to it"

        do! commit state "f9 kind"
        do!
          shows
            state
            [ "status" ]
            "standing constraint"
            "on a clean tree it is standing, not outstanding against you"

        do! fn state "Tests.F9Kind.other" "() : Int64 = 1L"
        do!
          shows
            state
            [ "status" ]
            "none from this draft"
            "an unrelated draft is not blamed for it"

        // Back to a fn: the cascade repoints the caller and THIS constraint is gone, so the store is
        // left as it was found. Asked by name: the shared store carries other tests' constraints.
        let! _ =
          runCli state [ "fn"; "Tests.F9Kind.dep"; "(x: Int64) : Int64 = x + 3L" ]
        do! commit state "f9 restored"
        let! after = runCli state [ "constraints" ]
        Expect.isFalse
          (after.Contains "F9Kind")
          $"restoring the kind clears it: {after}"
      })

/// A name that does not resolve fails before any call is made, so its stack is empty and the host
/// prints no stack header over it. One line for a one-line mistake; a real stack, from a fn that
/// failed inside another, still prints.
let private aMissingNameIsOneLineNotAStackHeader =
  cliTest
    "eval of a name that does not exist prints the error and no empty call stack"
    (fun state ->
      task {
        let! missing = runCli state [ "eval"; "Tests.Nope.missing 1L" ]
        Expect.stringContains missing "not found" $"the error itself: {missing}"
        Expect.isFalse
          (missing.Contains "Call-stack")
          $"and no stack header over an empty stack: {missing}"

        let! _ =
          runCli
            state
            [ "fn"; "Tests.Stk.boom"; "() : Int64 = Stdlib.Int64.divide 1L 0L" ]
        let! deep = runCli state [ "eval"; "Tests.Stk.boom ()" ]
        Expect.stringContains
          deep
          "Call-stack"
          $"a failure inside a call still shows where: {deep}"
        Expect.stringContains deep "Stk.boom" $"naming the frame: {deep}"
        do! discardAll state
        ()
      })

/// A branch pulled without its parent lists a parent this store has never registered. The bare id
/// read as corruption on the machine that saw it; the list now says what it is and how to fix it.
let private anUnknownParentIsNamedAsSuch =
  cliTest
    "a branch whose parent is not in this store says so in the list"
    (fun state ->
      task {
        let! _ =
          runCli
            state
            [ "eval"
              "Darklang.SCM.Branch.create \"orphan13\" (Stdlib.Uuid.parse \"11111111-2222-3333-4444-555555555555\" |> Builtin.unwrap) |> Stdlib.Option.isSome" ]
        let! listed = runCli state [ "branches" ]
        Expect.stringContains listed "orphan13 (" $"the orphan is listed: {listed}"
        Expect.stringContains
          listed
          "11111111 (a branch not in this store"
          $"and its parent is explained rather than shown as an id: {listed}"
        do! archiveBranches state [ "orphan13" ]
      })

/// Scripts and agents branch on the exit, so failure has to BE one, not prose. The full sweep of
/// error paths is not wired yet; the two a script hits first are, and this pins them.
let private failuresExitNonzero =
  cliTest
    "an unknown command and a failed eval exit nonzero; a good command exits zero"
    (fun state ->
      task {
        let! (_, ok) = runCliWithExit state [ "status" ]
        Expect.equal ok 0L "a command that worked exits 0"
        let! (unknownOut, unknown) = runCliWithExit state [ "zzznotacommand" ]
        Expect.stringContains unknownOut "Unknown command" "and says so"
        Expect.equal unknown 1L "an unknown command exits 1"
        let! (evalOut, failed) =
          runCliWithExit state [ "eval"; "Tests.Nope.zzz 1L" ]
        Expect.stringContains evalOut "not found" "the error is still printed"
        Expect.equal failed 1L "a failed eval exits 1"
      })

/// The docs promise review-then-ask, and a pipe has nobody to ask: without `-y`, nothing commits.
/// And a commit's message is the one sentence other machines get, so an empty one is refused
/// rather than recorded forever.
let private commitAsksAndNeedsAMessage =
  cliTestOnMain
    "commit on a piped stdin stays a draft, and an empty message is refused"
    (fun state ->
      task {
        do! discardAll state
        do! fn state "Tests.CommitGate.f" "() : Int64 = 11L"

        // No -y: the harness's stdin is a pipe, so the prompt cannot be answered and the answer is no.
        do!
          shows
            state
            [ "commit"; "gated" ]
            "left as a draft"
            "nothing commits without an answer"
        do! shows state [ "status" ] "1 item changed" "and the draft is still there"

        do!
          shows
            state
            [ "commit"; "-y" ]
            "a commit needs a message"
            "an empty message is refused, -y or not"

        do!
          shows
            state
            [ "commit"; "gated for real"; "-y" ]
            "-- 2 ops"
            "with a message and -y it lands"
      })

/// `--as` takes one value and `--why` swallows the rest of the line; neither is a target id.
let private constraintFlagsAreNotTargets =
  cliTest "constraint flag values are not read as finding ids" (fun state ->
    task {
      let! asOut =
        runCli state [ "constraints"; "resolve"; "zzzzzzzz"; "--as"; "repoint" ]
      Expect.stringContains asOut "zzzzzzzz" $"the target is reported: {asOut}"
      Expect.isFalse
        (asOut.Contains "matching \"repoint\"")
        $"and the --as value is not: {asOut}"

      let! whyOut = runCli state [ "ack"; "zzzzzzzz"; "--why"; "not"; "now" ]
      Expect.isFalse
        (whyOut.Contains "matching \"not\"" || whyOut.Contains "matching \"now\"")
        $"an unquoted --why reason is not read as ids: {whyOut}"
    })

/// A branch discard captures its op ids before deleting, so the untag can no longer miss the rows
/// the delete removed -- which left tags pointing at nothing, and `status`'s store health calling
/// the store damaged at exactly the moment trust was thinnest.
let private aBranchDiscardLeavesNoDanglingTags =
  cliTestOnMain
    "discarding a branch draft leaves no tag pointing at a missing op"
    (fun state ->
      task {
        do! switch state "dangletags"
        do! fn state "Tests.Dangle.f" "() : Int64 = 5L"
        let! _ =
          runCli state [ "fn"; "Tests.Dangle.g"; "() : Int64 = Tests.Dangle.f ()" ]
        do! discardAll state

        let! dangling =
          Sql.query
            "SELECT count(*) AS n FROM op_branches ob
           WHERE NOT EXISTS (SELECT 1 FROM package_ops p WHERE p.id = ob.op_id)"
          |> Sql.executeRowAsync (fun read -> read.int64 "n")
        Expect.equal dangling 0L "every surviving tag points at a real op"

        let! status = runCli state [ "status" ]
        Expect.isFalse
          (status.Contains "store problems")
          $"and store health stays quiet: {status}"

        do! archiveBranches state [ "dangletags" ]
      })

/// Archiving the branch you stand on moves you to its PARENT -- read before the archive drops the
/// row from the listing, or a branch-of-a-branch landed you on main.
let private archivingAChildLandsOnItsParent =
  cliTestOnMain
    "archiving a child branch from atop it lands on the parent, not main"
    (fun state ->
      task {
        do! switch state "archparent"
        do! switch state "archchild"
        do!
          shows
            state
            [ "branch"; "archive"; "archchild"; "-y" ]
            "now on branch \"archparent\""
            "the child's parent, not main"
        do! archiveBranches state [ "archparent" ]
      })

let private aBundleCarriesItsCommits =
  cliTestOnMain
    "a branch bundle arrives with the author's commits, not as a draft"
    (fun state ->
      task {
        do! switch state "committedbr"
        let! shown = runCli state [ "eval"; "Builtin.scmCurrentBranch ()" ]
        let sourceId =
          System.Text.RegularExpressions.Regex.Match(shown, @"[0-9a-f-]{36}").Value
        do! fn state "Tests.Carry.done" "() : Int64 = 7007L"
        do! commit state "the author's own message"
        do! switch state "main"

        let exported = $"{LibConfig.Config.runDir}/bundle-commits.json"
        let! _ = runCli state [ "branch"; "export"; "committedbr"; exported ]
        let json = System.IO.File.ReadAllText exported
        Expect.stringContains
          json
          "the author's own message"
          "the bundle carries the commit"
        let freshId = "1e51f3a2-9c4d-4b7a-8f61-2d3e4c5b6a71"
        let retargeted =
          json.Replace(sourceId, freshId).Replace("committedbr", "arrivedbr")
        let path = $"{LibConfig.Config.runDir}/bundle-commits-arrived.json"
        System.IO.File.WriteAllText(path, retargeted)

        do! shows state [ "branch"; "import"; path ] "imported branch" "it imports"
        // `switch`, not `--branch`: the harness runs the Dark entry only, and `--branch` moves the
        // process in the F# entry, so a draft read here would be main's.
        do! switch state "arrivedbr"
        do!
          shows
            state
            [ "status" ]
            "clean"
            "nothing on the arrived branch is a draft"
        do!
          shows
            state
            [ "commits"; "3" ]
            "the author's own message"
            "and the author's commit is there"

        do! archiveBranches state [ "arrivedbr"; "committedbr" ]
      })

/// `discard` on a branch drops the BRANCH's work and leaves main's draft alone.
///
/// Worth asserting because the command is destructive and the two drafts are kept apart by a `WHERE`
/// clause rather than by anything the type system checks: main's draft is the uncommitted ops NOT tagged
/// to a branch, so a query that forgot the tag would take both, and the way you would find out is that
/// work you never mentioned had gone.
let discardOnABranchLeavesMainsDraftAlone =
  cliTestOnMain
    "discard on a branch drops the branch's work, not main's draft"
    (fun state ->
      task {
        do! fn state "Tests.DiscardIso.onMain" "() : Int64 = 1L"

        do!
          shows
            state
            [ "switch"; "cli-discard-branch" ]
            "cli-discard-branch"
            "the test is on the branch"

        let! _ =
          runCli state [ "fn"; "Tests.DiscardIso.onBranch"; "() : Int64 = 2L" ]
        let! discarded = runCli state [ "discard"; "--all"; "--yes" ]
        Expect.stringContains discarded "onBranch" "it discarded the branch's item"
        Expect.isFalse
          (discarded.Contains "onMain")
          "and did not touch the one on main"

        do!
          shows
            state
            [ "eval"; "Tests.DiscardIso.onBranch ()" ]
            "not found"
            "the branch's item really went"

        do!
          shows state [ "switch"; "main" ] "main" "and it put the run back on main"

        do!
          shows
            state
            [ "eval"; "Tests.DiscardIso.onMain ()" ]
            "1"
            "main's draft survived the branch's discard"

        let! _ = runCli state [ "discard"; "--all"; "--yes" ]
        ()
      })

/// Committing on a branch does not collapse main's draft.
///
/// Commit collapses the draft's superseded namings, keeping the last binding per name -- and "the draft"
/// means MAIN's, the uncommitted ops not tagged to a branch. A branch's ops are branch-pending and are
/// collapsed when they merge. Nothing in the types says which one `collapse` is about; it is a guard on
/// the current branch, and if that guard went, committing anything anywhere would quietly rewrite work
/// sitting on main.
let committingOnABranchLeavesMainsDraftUncollapsed =
  cliTestOnMain "committing on a branch does not collapse main's draft" (fun state ->
    task {
      // Two versions of one name: four draft ops that a collapse would reduce.
      do! fn state "Tests.CollapseIso.item" "() : Int64 = 1L"
      do! fn state "Tests.CollapseIso.item" "() : Int64 = 2L"

      let! beforeJson = runCli state [ "status"; "--json" ]
      let draftOps (json : string) =
        let marker = "\"draftOps\":"
        let i = json.IndexOf marker
        if i < 0 then
          Tests.failtestf "no draftOps in status --json: %s" json
        else
          let rest = json.Substring(i + marker.Length)
          let upToComma = rest.Split(',') |> Array.head
          let digits = upToComma.Split('}') |> Array.head
          digits.Trim()

      let before = draftOps beforeJson

      do!
        shows
          state
          [ "switch"; "cli-collapse-branch" ]
          "cli-collapse-branch"
          "the test is on the branch"

      let! _ =
        runCli state [ "fn"; "Tests.CollapseIso.onBranch"; "() : Int64 = 9L" ]
      do! commit state "branch commit"

      do! shows state [ "switch"; "main" ] "main" "and it put the run back on main"

      let! afterJson = runCli state [ "status"; "--json" ]

      Expect.equal
        (draftOps afterJson)
        before
        "the branch's commit left main's draft exactly as it was"

      let! _ = runCli state [ "discard"; "--all"; "--yes" ]
      ()
    })

/// A commit refuses a draft with a definite at-rest type error, and `--allow-type-errors` is the way past.
///
/// The save path PRINTS "commit will refuse it until fixed", so the gate has to exist or the
/// checker's report promises something the tool does not do, which is worse than saying nothing.
/// `-y` deliberately does not wave it through, same as `--allow-unresolved`.
let private commitRefusesDefiniteTypeErrors =
  cliTest
    "commit refuses a definite type error, and --allow-type-errors takes it"
    (fun state ->
      task {
        // A one-field enum case given two arguments. Definite, and cheap to state.
        let source =
          "type Wrapped = Wrap of (Int * String)\n\n"
          + "let bad (value: Wrapped) : Wrapped =\n"
          + "  match value with\n"
          + "  | Wrap((number, text)) -> Wrapped.Wrap(number, text)\n"

        let file =
          System.IO.Path.Combine(
            System.IO.Path.GetTempPath(),
            "dark-arity-gate.dark"
          )
        System.IO.File.WriteAllText(file, source)

        do!
          shows
            state
            [ "module"; "Tests.ArityGate"; file ]
            "At-rest type check failed"
            "saving reports the error rather than hiding it"

        let! refused = runCli state [ "commit"; "should not land"; "-y" ]
        Expect.stringContains refused "cannot commit" "the commit is refused"
        Expect.stringContains
          refused
          "expects 1 field"
          "and says which error, not just that there was one"

        let! taken =
          runCli state [ "commit"; "taking it"; "-y"; "--allow-type-errors" ]
        // The op count, not "commit": "nothing to commit -- your draft is empty" also contains "commit ".
        Expect.stringContains
          taken
          "-- 4 ops"
          $"the typed override takes it: {taken}"

        // The refusal names its item, and FIXING an error unblocks a later commit without the
        // override: the gate checks the draft's LIVE items, so a superseded broken version (kept in
        // the draft as history) no longer refuses forever.
        Expect.stringContains
          refused
          "Tests.ArityGate.bad"
          $"the refusal names the failing item: {refused}"
        do!
          author
            state
            "Tests.ArityGate.bad2"
            "(x: Int64) : Int64 = Stdlib.String.length x"
        do! shows state [ "commit"; "nope"; "-y" ] "cannot commit" "broken refuses"
        let! _ =
          runCli state [ "fn"; "Tests.ArityGate.bad2"; "(x: Int64) : Int64 = x" ]
        let! fixed_ = runCli state [ "commit"; "fixed, no override"; "-y" ]
        Expect.isFalse
          (fixed_.Contains "cannot commit")
          $"a fixed draft commits without --allow-type-errors: {fixed_}"
      })

let private deprecationIsReversible =
  cliTest "delete can be undone" (fun state ->
    task {
      do!
        author
          state
          "Tests.Undep.item"
          "(x: Int64) : Int64 = Stdlib.Int64.add x 4242L"

      let! _ =
        runCli state [ "delete"; "fn"; "Tests.Undep.item"; "-m"; "t"; "--yes" ]
      let! hidden = runCli state [ "ls"; "Tests.Undep" ]
      Expect.isFalse (hidden.Contains "item") "a deleted item is hidden"

      do!
        shows
          state
          [ "undeprecate"; "Tests.Undep.item" ]
          "Undeprecated"
          "undeprecate reports what it did"

      do! shows state [ "ls"; "Tests.Undep" ] "item" "and it is back on the shelf"

      do! shows state [ "eval"; "Tests.Undep.item 1L" ] "4243" "and it still runs"
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
      execSql
        $"DELETE FROM package_ops WHERE commit_hash IN
            (SELECT hash FROM commits WHERE message IN {messages})"

    do! execSql $"DELETE FROM commits WHERE message IN {messages}"

    do!
      execSql
        "DELETE FROM package_ops WHERE commit_hash IS NULL
           AND id NOT IN (SELECT op_id FROM op_branches)
           AND id IN (SELECT DISTINCT l.op_id FROM locations l
                      WHERE l.owner = 'Ux' AND l.modules = 'Money')"

    do! execSql "DELETE FROM locations WHERE owner = 'Ux' AND modules = 'Money'"

    do!
      execSql
        "DELETE FROM propagation_policy WHERE owner = 'Ux' AND modules = 'Money'"
  }

let private theWorkedExampleWorks =
  cliTestOnMain "the worked example in `docs scm` does what it says" (fun state ->
    task {
      do! resetWorkedExample ()
      do! switch state "main"

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

      do! shows state [ "eval"; "Tests.Cache.v 0L" ] "1" "the first version runs"

      let! _ =
        runCli
          state
          [ "fn"; "Tests.Cache.v"; "(x: Int64) : Int64 = Stdlib.Int64.add x 2L" ]

      do!
        shows
          state
          [ "eval"; "Tests.Cache.v 0L" ]
          "2"
          "and so does the edit, without restarting"
    })

let private deprecationTakesEffectInTheSameProcess =
  cliTest "marking a fn harmful takes effect without restarting" (fun state ->
    task {
      let! _ =
        runCli
          state
          [ "fn"; "Tests.Harm.f"; "(x: Int64) : Int64 = Stdlib.Int64.add x 31L" ]

      do! shows state [ "eval"; "Tests.Harm.f 0L" ] "31" "it runs to begin with"

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
      do!
        shows
          state
          [ "eval"; "Tests.Harm.f 0L" ]
          "Harmful"
          "and it halts as soon as it's marked"

      let! _ = runCli state [ "undeprecate"; "Tests.Harm.f" ]

      do!
        shows
          state
          [ "eval"; "Tests.Harm.f 0L" ]
          "31"
          "and runs again as soon as it's unmarked"
    })

/// The `--allow-harmful` pair of tests lived here and CANNOT work here, which is worth stating rather
/// than leaving as a gap someone re-adds.
///
/// `runCli` builds an RT-level `ExecutionState`, and that default is permissive BY DESIGN ("tests run
/// permissive" in `Execution.createState`). The harmful gate is narrowed by the CLI HOST, per entry
/// point, so in this harness the halt never fires and the test asserts a halt that cannot happen.
///
/// It is tested where it can be: "Harmful Gate" in `packages/darklang/cli/tests/tests.dark`, which drives
/// a real CLI process and asserts BOTH directions -- a fn marked harmful refuses to run, and
/// `--allow-harmful` runs it anyway.
/// `commit --include=` turns part of a draft into history: an unnamed item stays in the draft, a
/// named item's uncommitted dependency comes WITH it (a commit referencing uncommitted content
/// would be internally inconsistent), and a name the draft does not hold is refused.
let partialCommitTakesOnlyWhatYouNamed =
  cliTestOnMain
    "commit --include= takes the named items plus their dependencies"
    (fun state ->
      task {
        do! switch state "main"
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
        // The refusal fires BEFORE the review renders, and names both the missing name and where
        // to look -- showing the whole draft over a selection that commits none of it was the one
        // dishonesty a review screen cannot afford.
        Expect.stringContains
          refused
          "--include names things not in your draft: Tests.Pc.absent"
          "an unknown name is refused"

        do!
          shows
            state
            [ "status" ]
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
        do! shows state [ "status" ] "1 item" "the unnamed item stayed in the draft"

        do!
          shows
            state
            [ "eval"; "Tests.Pc.user 0L" ]
            "2"
            "the committed item still evaluates"

        // A name edited twice in one draft has two namings, and `--include=` takes both: leaving the
        // earlier one behind would describe a version nobody has. (The deleted `scm-partial-commit.dark`
        // refused this case; the model now commits both.)
        do! fn state "Tests.Pc.twice" "() : Int64 = 3001L"
        do! fn state "Tests.Pc.twice" "() : Int64 = 3002L"
        let! both =
          runCli state [ "commit"; "twice"; "--include=Tests.Pc.twice"; "-y" ]
        Expect.stringContains both "commit" $"the twice-edited name commits: {both}"
        do!
          shows
            state
            [ "status" ]
            "1 item"
            "and only the unnamed item is still in the draft"
        do!
          shows
            state
            [ "eval"; "Tests.Pc.twice ()" ]
            "3002"
            "and the committed version is the later edit"

        let! _ = runCli state [ "discard"; "--yes" ]
        return ()
      })

/// A branch verb takes the name you can see, not the id it resolves to: every branch has a uuid
/// behind it, and a verb handed the name printed by `dark branches` must not treat it as an id.
let private branchVerbsTakeTheNameYouSee =
  cliTestOnMain "every branch verb accepts the name the listing prints" (fun state ->
    task {
      do! switch state "verbname"
      let! _ =
        runCli
          state
          [ "fn"; "Tests.Vn.one"; "(x: Int64) : Int64 = Stdlib.Int64.add x 1L" ]
      do! switch state "main"

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

      do!
        shows
          state
          [ "diff"; "notabranch" ]
          "no branch"
          "a name we don't have is still an error"
    })

/// A review queue is a branch you name, so the verbs have to resolve it like any other: `review
/// import` stages under a minted id, and the typed queue name is not that id.
let reviewQueueRoundTrips =
  cliTestOnMain
    "a review queue can be inspected and approved by the name you gave it"
    (fun state ->
      task {
        do! switch state "rqsrc"
        let! _ =
          runCli
            state
            [ "fn"; "Tests.Rq.one"; "(x: Int64) : Int64 = Stdlib.Int64.add x 5L" ]
        do! switch state "main"

        let path = $"{LibConfig.Config.runDir}/rq-test-bundle.json"
        let! _ = runCli state [ "sync"; "export"; path ]

        // Asserted POSITIVELY. These three were `isFalse (contains "no review queue")`, which any other
        // failure text passed, including a runtime error, and this is the only end-to-end cover of
        // `dark review`.
        let! staged = runCli state [ "review"; "import"; path; "rqueue" ]
        Expect.stringContains
          staged
          "rqueue"
          $"the queue is created by name: {staged}"
        Expect.isFalse
          (Option.isSome (looksLikeARuntimeFailure staged))
          $"and importing it did not fail: {staged}"

        let! shown = runCli state [ "review"; "rqueue" ]
        Expect.isFalse
          (Option.isSome (looksLikeARuntimeFailure shown))
          $"and is inspectable by that name: {shown}"

        let! approved = runCli state [ "review"; "approve"; "rqueue" ]
        Expect.isFalse
          (Option.isSome (looksLikeARuntimeFailure approved))
          $"and approvable by it, which is the whole workflow: {approved}"
      })

let private otherBranchAnswersStayCurrent =
  cliTestOnMain
    "asking about a branch you're not on gives a current answer"
    (fun state ->
      task {
        do! switch state "cachebr"
        let! _ =
          runCli
            state
            [ "fn"; "Tests.Br.one"; "(x: Int64) : Int64 = Stdlib.Int64.add x 11L" ]

        do! switch state "main"

        // Populates the memo of "ops for a branch I'm not on".
        do! shows state [ "diff"; "cachebr" ] "one" "the first item shows up"

        do! switch state "cachebr"
        let! _ =
          runCli
            state
            [ "fn"; "Tests.Br.two"; "(x: Int64) : Int64 = Stdlib.Int64.add x 22L" ]

        do! switch state "main"

        // The branch moved while we weren't on it, and a memo loaded once and never
        // dropped answers with the branch as it was -- right before you decide to merge.
        do!
          shows state [ "diff"; "cachebr" ] "two" "and so does what was added since"

        do! archiveBranches state [ "cachebr" ]
      })

/// `dark propagate follow` must not destroy a name that happens to share a hash.
///
/// `SCM.Propagation.rebind` catches a followed name up by emitting a bare `SetName`. The fold read a
/// standalone SetName as a RENAME and deprecated every other location on that hash, so catching `x` up
/// silently unlisted `y` when the two had identical bodies -- one command, no sync, and `status` reported
/// "1 followed" without mentioning that a name was gone.
///
/// Identical bodies being one item is routine here, which is what made this reachable rather than exotic.
let private followingDoesNotDestroyASharedName =
  cliTest
    "catching a pinned name up does not unlist a name sharing its hash"
    (fun state ->
      task {
        do! fn state "Tests.Follow.dep" "() : Int64 = 1L"

        // Identical bodies, so x and y are ONE content-addressed item sharing a hash.
        let body = "() : Int64 = Stdlib.Int64.add (Tests.Follow.dep ()) 100L"
        do! fn state "Tests.Follow.x" body
        do! fn state "Tests.Follow.y" body

        // Pin x, then move dep so only y follows and the two names diverge.
        let! _ = runCli state [ "propagate"; "pin"; "Tests.Follow.x"; "held" ]
        do! fn state "Tests.Follow.dep" "() : Int64 = 2L"

        do!
          shows
            state
            [ "eval"; "Tests.Follow.y ()" ]
            "102"
            "y followed the moved dependency"

        // Catch x up. It rebinds to the hash y already holds.
        let! _ =
          runCli state [ "propagate"; "follow"; "Tests.Follow.x"; "caught up" ]

        do!
          shows
            state
            [ "eval"; "Tests.Follow.x ()" ]
            "102"
            "x caught up, which is what follow is for"

        let! yAfter = runCli state [ "eval"; "Tests.Follow.y ()" ]
        Expect.stringContains
          yAfter
          "102"
          "and y still exists, having done nothing wrong"
        Expect.isFalse
          (yAfter.Contains "not found")
          "catching one name up must not delete another that shares its hash"

        return ()
      })

/// Every `--json` surface emits parseable JSON.
///
/// `--json` is the agent-facing half of this CLI, and a broken one fails in a way a human never sees:
/// the pretty output is fine while the machine output is a stack trace. `dark branches --json` died with
/// "Expected String for field `id`, but got <Uuid>" -- an id reaching a JSON boundary without being
/// converted, which is exactly what typing branch ids was meant to make impossible and what a `String`
/// field in a JSON record quietly reintroduces.
///
/// `everyCommandSurvivesABogusArgument` does not reach this: `--json` is not a bogus argument.
let private everyJsonSurfaceParses =
  cliTest "every --json surface emits parseable JSON" (fun state ->
    task {
      do! fn state "Tests.Json.probe" "() : Int64 = 1L"

      let surfaces =
        [ [ "status" ]
          [ "commit" ]
          [ "commits" ]
          [ "branches" ]
          [ "conflicts" ]
          [ "constraints" ]
          [ "propagate" ]
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
/// Every authoring command took a whole definition, so changing one line of a forty-line function meant
/// typing the other thirty-nine. That is the single thing that stopped this being usable as a daily
/// driver by a person, and it barely registered for an agent, which regenerates whole definitions
/// anyway. The two shapes exist because the two callers want opposite things: a person wants $EDITOR, a
/// script wants no terminal at all.
///
/// `--raw` is the other half. Without it, reading an item back out meant stripping a trailer and a
/// `let f ` prefix with `sed`, which is a screen-scrape: it works until the display changes.
let private editChangesAnItemWithoutRetypingIt =
  cliTest
    "edit round-trips an item through --raw, and leaves its siblings alone"
    (fun state ->
      task {
        do!
          author
            state
            "Tests.Edit.target"
            "(x: Int64) : Int64 = Stdlib.Int64.multiply x 2L"
        do! fn state "Tests.Edit.sibling" "() : Int64 = 9L"

        // `--raw` must be exactly what the authoring path accepts: a whole declaration, no trailer, no
        // capabilities line, no highlighting.
        let! raw = runCli state [ "view"; "Tests.Edit.target"; "--raw" ]
        Expect.stringContains raw "let target" "raw output is a whole declaration"
        Expect.isFalse (raw.Contains "capabilities:") "with no capabilities trailer"

        // The scriptable shape: no editor, no terminal.
        let dir = System.IO.Path.GetTempPath()
        let file = System.IO.Path.Combine(dir, "dark-edit-test.dark")
        System.IO.File.WriteAllText(
          file,
          raw.Replace("multiply x 2L", "multiply x 5L")
        )

        do!
          shows
            state
            [ "edit"; "Tests.Edit.target"; file ]
            "Defined 1 declaration"
            "one declaration, said in the singular"

        do!
          shows state [ "eval"; "Tests.Edit.target 3L" ] "15" "the edit took effect"

        // The module's other declarations are not collateral.
        do!
          shows
            state
            [ "eval"; "Tests.Edit.sibling ()" ]
            "9"
            "the sibling in the same module is untouched"

        // And an edit PROPAGATES. `edit` lands through `dark module`, whose op scan matched `SetName`
        // with two fields against a three-field case: a silent no-match in a `filterMap`, so nothing was
        // ever reported as updated and no dependent followed. The edit itself looked completely fine.
        do!
          author
            state
            "Tests.Edit.caller"
            "() : Int64 = Stdlib.Int64.add (Tests.Edit.target 1L) 100L"

        do!
          shows
            state
            [ "eval"; "Tests.Edit.caller ()" ]
            "105"
            "the caller sees the current target"

        System.IO.File.WriteAllText(
          file,
          raw.Replace("multiply x 2L", "multiply x 9L")
        )
        let! _ = runCli state [ "edit"; "Tests.Edit.target"; file ]

        do!
          shows
            state
            [ "eval"; "Tests.Edit.caller ()" ]
            "109"
            "and follows the edit, rather than staying on the old version"

        // Refusals a person actually hits.
        do!
          shows
            state
            [ "edit" ]
            "usage: dark edit"
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
      do! value state "Tests.Displace.thing" "42L"
      do!
        shows
          state
          [ "eval"; "Tests.Displace.thing" ]
          "42"
          "the value is what the name means"

      // A second value alongside it, so the assertion below is about DISPLACEMENT and not about the
      // module having no values at all.
      do! value state "Tests.Displace.other" "9L"

      do! fn state "Tests.Displace.thing" "() : Int64 = 7L"

      do!
        shows
          state
          [ "eval"; "Tests.Displace.thing ()" ]
          "7"
          "the name now means the fn"

      let! listing = runCli state [ "ls"; "Tests.Displace" ]
      Expect.stringContains listing "thing" "the name is still there"
      Expect.stringContains listing "other" "and the untouched value still is too"

      // The value section must hold `other` and NOT `thing`: one live binding per name, so the
      // displaced value is gone from the listing rather than sitting beside the fn.
      let valuesSection =
        let i = listing.IndexOf "Values:"
        let j = listing.IndexOf "Functions:"
        if i >= 0 && j > i then listing.Substring(i, j - i) else ""

      Expect.stringContains
        valuesSection
        "other"
        "the surviving value is listed as a value"
      Expect.isFalse
        (valuesSection.Contains "thing")
        "the displaced value is NOT still listed beside the fn that replaced it"

      return ()
    })

/// A branch off a branch sees its whole ancestry, and nothing sees its descendants.
///
/// Nested branches are not exotic: `dark branch new` parents the new branch to the one you are standing
/// on, so you get a chain by not thinking about it. They are the shape that exercises `chainBindings`,
/// whose recursive walk up the parent chain is the overlay logic everything else trusts, and nothing
/// covered them.
let private branchChainSeesItsAncestry =
  cliTestOnMain
    "a branch off a branch sees its parent's COMMITTED work, and main sees neither"
    (fun state ->
      task {
        do! switch state "chainOne"
        do! fn state "Tests.Chain.one" "() : Int64 = 11L"

        // Created while standing on chainOne, so its parent is chainOne rather than main.
        let! _ = runCli state [ "branch"; "new"; "chainTwo" ]
        do! fn state "Tests.Chain.two" "() : Int64 = 22L"

        // The parent's edit is still WIP, so the child does not see it yet: a parent's draft is its
        // own until committed, all the way up the chain.
        do!
          shows
            state
            [ "eval"; "Tests.Chain.one ()" ]
            "not found"
            "a parent's WIP does not leak down"

        // Committed on the parent, the child sees the whole chain.
        do! switch state "chainOne"
        do! commit state "chain one"
        do! switch state "chainTwo"
        do! shows state [ "eval"; "Tests.Chain.two ()" ] "22" "its own work"
        do!
          shows
            state
            [ "eval"; "Tests.Chain.one ()" ]
            "11"
            "and its parent's committed work, through the chain"

        // The parent does NOT see its child's.
        do! switch state "chainOne"
        do!
          shows
            state
            [ "eval"; "Tests.Chain.one ()" ]
            "11"
            "the parent still sees its own work"
        do!
          shows
            state
            [ "eval"; "Tests.Chain.two ()" ]
            "not found"
            "and not the child's, which would be work leaking DOWN the chain"

        // Main sees neither.
        do! switch state "main"
        do!
          shows
            state
            [ "eval"; "Tests.Chain.one ()" ]
            "not found"
            "main sees no branch work"

        do! archiveBranches state [ "chainTwo"; "chainOne" ]
      })

/// The reads that answered about main from a branch, each with its own scenario. `dark ops` on a branch
/// off a branch omitted the intermediate branch; `undo` on a branch stepped onto a version main authored
/// AFTER the fork; `deps` on a branch listed main's version of a caller the branch had re-authored.
let private branchReadsAnswerAboutTheBranch =
  cliTestOnMain
    "ops, undo and deps answer about the branch you stand on, chain included"
    (fun state ->
      task {
        // A name main has, at v1, then a branch forks it and edits it.
        do! switch state "main"
        do! fn state "Tests.BranchRead.base" "() : Int64 = 1L"
        // A caller main has too, so the branch's re-authoring of it below leaves main's version behind.
        do!
          author
            state
            "Tests.BranchRead.caller"
            "() : Int64 = Tests.BranchRead.base ()"
        do! commit state "branchread v1"
        do! switch state "brOne"
        do! fn state "Tests.BranchRead.base" "() : Int64 = 2L"
        // The caller re-authored on the branch, and a branch off the branch with one op of its own.
        do!
          author
            state
            "Tests.BranchRead.caller"
            "() : Int64 = Stdlib.Int64.add (Tests.BranchRead.base ()) 0L"
        let! _ = runCli state [ "branch"; "new"; "brTwo" ]
        do! fn state "Tests.BranchRead.deep" "() : Int64 = 3L"

        // ops from the deepest branch lists the intermediate branch's op, not only its own and main's.
        let! ops = runCli state [ "ops" ]
        Expect.stringContains
          ops
          "Tests.BranchRead.caller"
          "the intermediate branch's op is listed from its child"
        Expect.stringContains
          ops
          "Tests.BranchRead.deep"
          "alongside the child's own"

        // Meanwhile main moves the same name on to v3, which the branch never had.
        do! switch state "main"
        do! fn state "Tests.BranchRead.base" "() : Int64 = 3L"
        do! commit state "branchread v3"

        // undo on the branch steps back to the fork's version, v1, and not onto main's v3.
        do! switch state "brOne"
        do!
          shows
            state
            [ "eval"; "Tests.BranchRead.base ()" ]
            "2"
            "the branch is at its own edit"
        let! _ = runCli state [ "undo"; "Tests.BranchRead.base" ]
        do!
          shows
            state
            [ "eval"; "Tests.BranchRead.base ()" ]
            "1"
            "undo steps to the version the branch forked from"

        // deps on the branch: the branch re-authored `caller`, so main has no version of it to list twice.
        do! fn state "Tests.BranchRead.base" "() : Int64 = 2L"
        do!
          shows
            state
            [ "deps"; "usedby"; "Tests.BranchRead.base" ]
            "Found 1 dependents"
            "the caller is listed once, the branch's version, not main's as well"

        do! archiveBranches state [ "brTwo"; "brOne" ]
      })

/// `dark merge` and `dark rebase` with no argument mean the branch you are standing on.
let private bareMergeAndRebaseMeanThisBranch =
  cliTestOnMain
    "merge and rebase with no argument mean the branch you're on"
    (fun state ->
      task {
        // On main there is nothing a bare form could mean, so it still refuses.
        let! onMain = runCli state [ "rebase" ]
        Expect.stringContains
          onMain
          "usage: dark rebase"
          "on main a bare rebase refuses"
        Expect.stringContains
          onMain
          "no branch to rebase"
          "and says why, rather than only how"

        do! switch state "barebr"
        let! _ =
          runCli
            state
            [ "fn"; "Tests.Bare.one"; "(x: Int64) : Int64 = Stdlib.Int64.add x 1L" ]
        do! commit state "bare branch work"

        // The dry runs are the safe half to assert on: they name the branch without moving anything.
        do!
          shows
            state
            [ "rebase"; "--dry-run" ]
            "barebr"
            "a bare rebase resolves to the branch you're on"

        do! shows state [ "merge"; "--dry-run" ] "barebr" "and so does a bare merge"

        // A named branch that does not exist is still a different answer from the bare form.
        do!
          shows
            state
            [ "merge"; "nosuchbranch" ]
            "no branch"
            "a named branch is still looked up"

        do! archiveBranches state [ "barebr" ]
      })

/// "Who calls this" must answer about the BRANCH you are standing on.
///
/// `SCM.Deps.dependentsOf` joined `package_dependencies` to `locations` inside the query. A branch's
/// items never fold into `locations` -- that is what makes a branch an overlay rather than a copy -- so
/// the join dropped every branch-authored caller and returned main's list, which looks complete. The
/// edge was there the whole time; only the name to call it by was missing.
///
/// It is the question you ask BEFORE changing something, so the wrong answer is the dangerous direction:
/// it says nothing on your branch depends on this.
let private dependentsSeeTheBranchYouAreOn =
  cliTestOnMain
    "who calls this counts the callers on your branch, not just main's"
    (fun state ->
      task {
        do!
          author
            state
            "Tests.Dep.target"
            "(x: Int64) : Int64 = Stdlib.Int64.add x 1L"
        do!
          author
            state
            "Tests.Dep.mainCaller"
            "(x: Int64) : Int64 = Stdlib.Int64.add (Tests.Dep.target x) 10L"

        do!
          shows
            state
            [ "deps"; "Tests.Dep.target" ]
            "mainCaller"
            "main's caller shows on main"

        // Committed, so the branch can see it: main's draft is invisible from a branch.
        do! commit state "dep target"
        do! switch state "depbr"
        do!
          author
            state
            "Tests.Dep.branchCaller"
            "(x: Int64) : Int64 = Stdlib.Int64.add (Tests.Dep.target x) 99L"

        let! onBranch = runCli state [ "deps"; "Tests.Dep.target" ]
        Expect.stringContains
          onBranch
          "branchCaller"
          "the caller authored on this branch is a dependent here"
        Expect.stringContains
          onBranch
          "mainCaller"
          "and main's caller has not stopped being one"

        // Standing on main is unchanged: a branch's callers are that branch's business.
        do! switch state "main"
        let! onMain = runCli state [ "deps"; "Tests.Dep.target" ]
        Expect.stringContains onMain "mainCaller" "main still sees its own caller"
        Expect.isFalse
          (onMain.Contains "branchCaller")
          "and does NOT see one that only exists on a branch"

        do! archiveBranches state [ "depbr" ]
      })

/// A `record` call as an eval expression. The candidates are empty because nothing
/// here reads them; what is under test is which branch the row lands on.
let private recordConflictOn (branchIdExpr : string) (id : string) : string =
  $"""Darklang.SCM.Conflicts.record ({branchIdExpr}) [Darklang.SCM.Conflicts.Conflict {{ id = "{id}"; owner = "Zz"; modules = "Confl"; name = "f"; itemType = "fn"; part = ""; kind = "same-name-different-hash"; candidates = []; autoResolvedTo = "bbb"; reason = "test"; status = "pending"; resolvedBy = "" }}]"""

/// A branch bundle carrying an op this build cannot decode is imported anyway: the readable ops land,
/// the unreadable one is stored raw and inert for a later build, and a note says so. That is what main
/// sync does with such ops. Refusing the bundle left the branch absent altogether, which a branch three
/// ops short still beats. A bundle that is not even well-formed (a blob that is not hex) is a different
/// thing and is still refused. Built from a REAL export plus one such record.
let private branchBundleKeepsWhatItCannotRead =
  cliTestOnMain
    "one undecodable op is stored inert and the rest of the bundle imports"
    (fun state ->
      task {
        do! switch state "bundlebr"
        let! shown = runCli state [ "eval"; "Builtin.scmCurrentBranch ()" ]
        // `eval` prints `<Uuid: ...>`; the id is what the bundle carries.
        let sourceId =
          System.Text.RegularExpressions.Regex.Match(shown, @"[0-9a-f-]{36}").Value
        Expect.equal
          sourceId.Length
          36
          $"the branch id was read off eval's output: {shown}"
        do!
          author
            state
            "Tests.Bundle.only"
            "(x: Int64) : Int64 = Stdlib.Int64.add x 1L"
        do! switch state "main"

        let exported = $"{LibConfig.Config.runDir}/bundle-partial.json"
        let! _ = runCli state [ "branch"; "export"; "bundlebr"; exported ]
        let json = System.IO.File.ReadAllText exported

        // Retarget at a branch this store lacks, so "was it registered" is answerable.
        let freshId = "0e51f3a2-9c4d-4b7a-8f61-2d3e4c5b6a70"
        let retargeted =
          json.Replace(sourceId, freshId).Replace("bundlebr", "importedbr")

        // Valid hex, not a valid op: the deserializer rejects it. Appended rather than substituted, so
        // every real op in the bundle stays valid: the partial case.
        let bad =
          """,{"blobHex":"ff3907","commit":"","id":"7c9e6679-7425-40de-944b-e07fc1f90ae7","ts":"2026-01-01T00:00:00.000Z"}"""
        // The serializer emits fields alphabetically, so `parent` follows `ops` and the
        // ops array does not end the document. Splice at the array's own close.
        let marker = """],"parent":"""
        let cut = retargeted.IndexOf marker
        Expect.isGreaterThan
          cut
          0
          "the exported bundle has an ops array followed by parent"
        let partial = retargeted.Substring(0, cut) + bad + retargeted.Substring(cut)

        let partialPath = $"{LibConfig.Config.runDir}/bundle-partial-bad.json"
        System.IO.File.WriteAllText(partialPath, partial)

        do!
          shows
            state
            [ "branch"; "import"; partialPath ]
            "imported branch"
            "the bundle imports"

        let! listed = runCli state [ "branches" ]
        Expect.isTrue
          (listed.Contains "importedbr")
          $"and the branch exists: {listed}"
        // `switch`, not `--branch`: the flag is resolved and consumed in `Cli.fs` before Dark runs, so
        // through this harness (which calls the Dark entry point directly) it moves nothing.
        do! switch state "importedbr"
        do!
          shows
            state
            [ "eval"; "Tests.Bundle.only 1L" ]
            "2"
            "and its readable work runs"
        do! switch state "main"

        // The unreadable op is here, inert, on the branch: kept for a build that can read it.
        let! kept =
          Sql.query
            "SELECT count(*) AS n FROM package_ops p JOIN op_branches ob ON ob.op_id = p.id
           WHERE p.id = '7c9e6679-7425-40de-944b-e07fc1f90ae7' AND p.effective = 0"
          |> Sql.executeRowAsync (fun read -> read.int64 "n")
        Expect.equal kept 1L "the unreadable op is stored inert on the branch"

        do! archiveBranches state [ "importedbr"; "bundlebr" ]
      })

/// Whether a merge is ALLOWED is a decision, so it is decided in Dark; the builtin only does the work.
///
/// Two structural gates. Conflicts deliberately do NOT gate: they are auto-resolved by the fold's
/// LWW and recorded, because blocking teaches people to rebase reflexively. "Active" has to mean
/// the same in the check as in the message, which says to merge OR ARCHIVE the children --
/// counting archived children as active makes that advice a dead end.
let private mergeGatesAreDecidedInDark =
  cliTestOnMain
    "merge refuses an empty branch, and one with children until they are archived"
    (fun state ->
      task {
        do! switch state "gateempty"
        do! switch state "main"
        do!
          shows
            state
            [ "merge"; "gateempty" ]
            "nothing to merge"
            "an empty branch has nothing to give its parent"

        do! switch state "gateparent"
        let! _ =
          runCli
            state
            [ "fn"; "Tests.Gate.one"; "(x: Int64) : Int64 = Stdlib.Int64.add x 6L" ]
        // Committed, so the refusal under test is the CHILDREN one and not the uncommitted-work
        // one -- both are real, and a test that cannot tell them apart proves neither.
        do! commit state "gate work"
        do! switch state "gatechild"
        do! switch state "main"

        do!
          shows
            state
            [ "merge"; "gateparent" ]
            "active children"
            "a parent cannot merge out from under its children"

        let! _ = runCli state [ "branch"; "archive"; "gatechild"; "-y" ]
        do!
          shows
            state
            [ "merge"; "gateparent"; "-y" ]
            "Merged"
            "and archiving the child clears the gate, rather than repeating the advice"

        do! archiveBranches state [ "gateempty" ]
      })

/// `diff` and `log` answer questions, so they answer in JSON too.
///
/// Both render a string with no record behind them, so this is a shape to design rather than a flag
/// to add. The flag is filtered out before the branch reference is read, so it can go on either
/// side: otherwise `dark diff --json foo` looks for a branch literally named "--json".
let private diffAndLogAnswerInJson =
  cliTestOnMain
    "diff and log answer in JSON, with the flag on either side"
    (fun state ->
      task {
        do! switch state "jsonsurface"
        do!
          author
            state
            "Tests.JsonS.only"
            "(x: Int64) : Int64 = Stdlib.Int64.add x 1L"

        // `log` on a branch is the op sequence, oldest first.
        let! logJson = runCli state [ "log"; "--json" ]
        Expect.stringContains
          logJson
          "\"seq\":0"
          "the branch's ops are numbered from the start"
        Expect.stringContains
          logJson
          "Tests.JsonS.only"
          "and name what they touched"

        do! switch state "main"

        // `log` on main is the commit history, so it answers with what `commits --json` answers.
        do!
          shows
            state
            [ "log"; "--json" ]
            "\"hash\""
            "on main it is the commit history"

        let! diffJson = runCli state [ "diff"; "jsonsurface"; "--json" ]
        Expect.stringContains
          diffJson
          "Tests.JsonS.only"
          "diff reports the changed name"
        Expect.stringContains diffJson "\"change\":\"new\"" "and classifies it"

        let! flagFirst = runCli state [ "diff"; "--json"; "jsonsurface" ]
        Expect.equal
          (flagFirst.Trim())
          (diffJson.Trim())
          "the flag is not positional"

        do! archiveBranches state [ "jsonsurface" ]
      })

/// A commit must not put a reference that cannot resolve into history: commits are what other
/// machines pull. A draft is ALLOWED to be unresolved while you work, though -- writing a caller
/// before its callee is ordinary, and `WipRefresh` re-resolves once the callee lands -- so the
/// forward reference has to commit cleanly.
let commitRefusesUnresolvedReferences =
  cliTestOnMain
    "commit refuses a reference that never resolves, but not a forward one"
    (fun state ->
      task {
        do! switch state "main"
        do!
          author
            state
            "Tests.UnresT.bad"
            "(x: Int64) : Int64 = Tests.UnresT.missing x"

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
        // The op count, not `commit`: the refusal this flag exists to get past is
        // "cannot commit: these reference names that don't resolve", which contains "commit",
        // so the obvious assertion passed whether the flag worked or not.
        Expect.stringContains
          allowed
          "-- 2 ops"
          $"--allow-unresolved records it as-is: {allowed}"

        // A forward reference inside one draft: the caller is authored first and cannot
        // resolve yet, and re-resolution fixes it before commit ever looks.
        do!
          author
            state
            "Tests.UnresT.caller"
            "(x: Int64) : Int64 = Tests.UnresT.callee x"
        do!
          author
            state
            "Tests.UnresT.callee"
            "(x: Int64) : Int64 = Stdlib.Int64.add x 3L"

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
  cliTestOnMain
    "discard on a branch drops that branch's draft and leaves main's alone"
    (fun state ->
      task {
        // Something uncommitted on MAIN, which must survive the branch's discard.
        do! switch state "main"

        // Measured as a DELTA, not an absolute: the store is shared with every other
        // cliTest, so whatever else is uncommitted on main is in this number too.
        let! baseline =
          runCli state [ "eval"; "Darklang.SCM.PackageOps.draftOpCount ()" ]

        // A body no other test uses. Content-addressing means an identical body is the SAME item and its
        // `AddFn` dedups, so a shared body would add one op here rather than two and the delta below
        // would be measuring how many other tests happened to write the same function.
        do!
          author
            state
            "Tests.Disc.onMain"
            "(x: Int64) : Int64 = Stdlib.Int64.add x 90210L"

        // Fully qualified: `SCM.PackageOps.draftOpCount` does not resolve here and comes
        // back as an error STRING, so before-vs-after would compare two identical error
        // messages and pass whatever the code does.
        let! before =
          runCli state [ "eval"; "Darklang.SCM.PackageOps.draftOpCount ()" ]
        Expect.equal
          (int (before.Trim()))
          (int (baseline.Trim()) + 2)
          "authoring one fn on main added exactly its two ops (AddFn + SetName) to the draft"

        do! switch state "discardbr"
        do!
          author
            state
            "Tests.Disc.onBranch"
            "(x: Int64) : Int64 = Stdlib.Int64.add x 90211L"
        do!
          shows
            state
            [ "eval"; "Tests.Disc.onBranch 1L" ]
            "90212"
            "the branch item is live before the discard"

        do!
          shows
            state
            [ "discard"; "-y" ]
            "discardbr"
            "discard names the branch as its scope, not main"

        let! gone = runCli state [ "eval"; "Tests.Disc.onBranch 1L" ]
        Expect.isFalse
          (gone.Trim() = "90212")
          $"the branch draft is gone, got: {gone}"

        do! switch state "main"
        let! after =
          runCli state [ "eval"; "Darklang.SCM.PackageOps.draftOpCount ()" ]
        Expect.equal
          (after.Trim())
          (before.Trim())
          "main's uncommitted ops all survived the branch's discard"

        do! archiveBranches state [ "discardbr" ]
      })

/// Committing on a branch commits THAT BRANCH's ops.
///
/// The commit ROW was written before the ops were stamped, so a failed branch commit also left a commit
/// naming nothing behind.
let private committingOnABranchCommitsItsOps =
  cliTestOnMain "committing on a branch commits that branch's ops" (fun state ->
    task {
      do! switch state "commitbr"
      do! fn state "Tests.CommitBr.item" "() : Int64 = 7L"

      let! out = runCli state [ "commit"; "on the branch"; "-y" ]
      Expect.stringContains out "commit" "the branch commit reports a commit"
      Expect.isFalse
        (out.Contains "Runtime Error")
        $"and does not throw on the way, got: {out}"

      // The point: the branch's own commit, not the store's. Counting `Commits.recent` here would pass
      // against the bug, since main's commits are in that number too.
      let! onBranch =
        runCli
          state
          [ "eval"
            "Stdlib.List.length (Darklang.SCM.PackageOps.commitsOnBranch (Darklang.SCM.PackageOps.currentBranch ()) 100L)" ]
      Expect.isTrue
        ((int (onBranch.Trim())) >= 1)
        $"the branch has a commit of its own, got: {onBranch}"

      // And the draft is empty afterwards, which is what "committed" means.
      let! draft =
        runCli state [ "eval"; "Darklang.SCM.PackageOps.draftOpCount ()" ]
      Expect.equal (draft.Trim()) "0" "the branch's draft is empty once committed"

      do! archiveBranches state [ "commitbr" ]
    })

let private conflictsBelongToTheBranchTheyHappenedOn =
  cliTestOnMain "a conflict is answered on the branch it happened on" (fun state ->
    task {
      do! switch state "confbr"
      let! branchId = runCli state [ "eval"; "Builtin.scmCurrentBranch ()" ]
      Expect.isFalse (branchId = "") "switch put us on a real branch"

      // Ask Dark for the branch rather than round-tripping the id through a printed string: `eval` runs
      // ON the branch, so `scmCurrentBranch ()` IS it. Parsing the printed form is what broke this --
      // it prints `<Uuid: ...>`, not a bare uuid, so the parse failed, the fallback wrote the conflict
      // onto MAIN, and the only symptom was an empty list two assertions later.
      let! _ =
        runCli
          state
          [ "eval"; recordConflictOn "Builtin.scmCurrentBranch ()" "cnfbranch01" ]
      let! _ =
        runCli
          state
          [ "eval"
            recordConflictOn "Darklang.SCM.Branch.mainBranchId" "cnfmain0001" ]

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
      do!
        shows
          state
          [ "conflicts"; "ack"; "cnfmain0001" ]
          "no conflict matching"
          "an id copied from main's list does nothing here"

      do! switch state "main"
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
      do! archiveBranches state [ "confbr" ]
    })

let private branchItemsArePolicyTargets =
  cliTestOnMain
    "a policy verb can name an item that only exists on a branch"
    (fun state ->
      task {
        do! switch state "polbr"
        let! _ =
          runCli
            state
            [ "fn"; "Tests.Pol.only"; "(x: Int64) : Int64 = Stdlib.Int64.add x 5L" ]

        // `locations` is main's projection and a branch's SetNames never fold into it, so
        // a read that goes only to that table answers about MAIN while you're on a branch.
        let! pinned =
          runCli state [ "propagate"; "pin"; "Tests.Pol.only"; "on the branch" ]
        Expect.stringContains pinned "pinned" "the branch item is a valid target"

        do!
          shows
            state
            [ "propagate" ]
            "Tests.Pol.only"
            "and the choice is visible from the branch"

        do! switch state "main"
        let! onMain = runCli state [ "propagate" ]
        Expect.isFalse
          (onMain.Contains "Tests.Pol.only")
          "and stays branch-local, like every other branch decision"

        do! archiveBranches state [ "polbr" ]
      })



/// Main's uncommitted draft never leaks into a branch's view: a draft-born name does not resolve
/// there, and a draft edit over a committed version resolves to the committed one. The branch is an
/// overlay on COMMITTED main; `dark commit` is what publishes work downstream.
let private aBranchNeverSeesMainsDraft =
  cliTestOnMain
    "a branch resolves through committed main, not main's draft"
    (fun state ->
      task {
        do! fn state "Tests.DraftMask.f" "() : Int64 = 1L"

        do! shows state [ "switch"; "dmask" ] "dmask" "switched"
        do!
          shows
            state
            [ "eval"; "Tests.DraftMask.f ()" ]
            "not found"
            "a draft-born name is invisible from a branch"

        do! switch state "main"
        do! commit state "draft mask v1"
        do! switch state "dmask"
        let! committed = runCli state [ "eval"; "Tests.DraftMask.f ()" ]
        Expect.stringContains committed "1" "committed work is visible"

        do! switch state "main"
        do! fn state "Tests.DraftMask.f" "() : Int64 = 2L"
        do!
          shows
            state
            [ "eval"; "Tests.DraftMask.f ()" ]
            "2"
            "main runs its own draft"
        do! switch state "dmask"
        do!
          shows
            state
            [ "eval"; "Tests.DraftMask.f ()" ]
            "1"
            "the branch stays on the committed version"

        do! switch state "main"
        do! discardAll state
        do! archiveBranches state [ "dmask" ]
      })

/// Editing a name whose live version a COLLEAGUE authored prints a one-line heads-up naming them
/// and the commit it arrived in; editing your own version says nothing. The check reads the live
/// binding's commit author before the save lands.
let private editingAColleaguesVersionSaysSo =
  cliTestOnMain
    "an edit over a colleague's version gets a heads-up; over your own, silence"
    (fun state ->
      task {
        do! fn state "Tests.PeerNote.f" "() : Int64 = 1L"
        do! commit state "peer note v1"

        let! ownEdit = runCli state [ "fn"; "Tests.PeerNote.f"; "() : Int64 = 2L" ]
        Expect.isFalse
          (ownEdit.Contains "note:")
          "editing your own version prints no heads-up"
        let! _ = runCli state [ "discard"; "Tests.PeerNote.f"; "-y" ]

        // The same binding, now wearing a colleague's name, as a sync would leave it.
        do!
          execSql
            "UPDATE commits SET author = 'colleague' WHERE message = 'peer note v1'"

        let! peerEdit = runCli state [ "fn"; "Tests.PeerNote.f"; "() : Int64 = 3L" ]
        Expect.stringContains
          peerEdit
          "note: colleague changed this"
          "the heads-up names them"
        Expect.stringContains peerEdit "peer note v1" "and the commit it arrived in"
        let! _ = runCli state [ "discard"; "Tests.PeerNote.f"; "-y" ]
        ()
      })

/// `dark commits` hides housekeeping (event-only commits: a branch archive or merge marker with no
/// authored work) behind `--all`, and says how many it hid.
let private commitsHideHousekeeping =
  cliTestOnMain
    "commits hides archive markers by default; --all shows them"
    (fun state ->
      task {
        do! switch state "hkeep"
        do! fn state "Tests.HKeep.f" "() : Int64 = 1L"
        do! archiveBranches state [ "hkeep" ]

        let! plain = runCli state [ "commits" ]
        Expect.isFalse
          (plain.Contains "archived branch \"hkeep\"")
          "the archive marker is hidden by default"
        Expect.stringContains
          plain
          "housekeeping"
          "and the listing says it hid something"

        do!
          shows
            state
            [ "commits"; "--all" ]
            "archived branch \"hkeep\""
            "--all shows the marker"
        ()
      })


/// A relay-hosted store holds client-pushed ops at effective=0, untagged and uncommitted.
/// `discard` must not touch them: they are data this store holds for someone else, and the
/// draft it reports must be the draft it drops.
let private discardSparesInertOps =
  cliTestOnMain "discard leaves relay-hosted (inert) ops alone" (fun state ->
    task {
      let! ops =
        parsePackageOps
          """module InertHold

let held (x: Int64) : Int64 = x + 41L"""
      let inertIds =
        ops |> List.map (fun op -> string (LibDB.Inserts.computeOpHash op))
      for op in ops do
        let id = LibDB.Inserts.computeOpHash op
        do!
          execSqlP
            "INSERT OR IGNORE INTO package_ops (id, op_blob, applied, effective, origin_ts)
               VALUES (@id, @blob, 1, 0, @ts)"
            [ "id", Sql.uuid id
              "blob",
              Sql.bytes (
                LibSerialization.Binary.Serialization.PT.PackageOp.serialize id op
              )
              "ts", Sql.string "2026-01-01T00:00:00.000Z" ]

      do! shows state [ "status" ] "clean" "inert ops are not the draft"

      do! discardAll state

      let! survived =
        countSql
          $"""SELECT COUNT(*) as n FROM package_ops
                WHERE id IN ({inertIds |> List.map (fun i -> $"'{i}'") |> String.concat ", "})"""
          []
      Expect.equal
        survived
        (int64 (List.length inertIds))
        "discard left the hosted ops in place"

      do!
        execSql
          $"""DELETE FROM package_ops WHERE id IN ({inertIds |> List.map (fun i -> $"'{i}'") |> String.concat ", "})"""
    })

/// Content-addressed ops are one row, so two branches authoring identical source share ops.
/// Merging one must still commit what it landed -- the shared op is main's now -- and the
/// sibling's later archive must not make it reappear as draft work nobody typed.
let private mergeCommitsWhatASiblingStillTags =
  cliTestOnMain
    "a merge commits shared ops even while a sibling still tags them"
    (fun state ->
      task {
        do! switch state "shared1"
        do! fn state "Tests.SharedLand.f" "() : Int64 = 99L"
        do! commit state "shared1 work"
        do! switch state "main"
        do! switch state "shared2"
        do! fn state "Tests.SharedLand.f" "() : Int64 = 99L"
        do! commit state "shared2 work"
        do! switch state "main"

        do! shows state [ "merge"; "shared1"; "-y" ] "erged" "the merge went through"

        // The shared content op is tagged by shared2 still; it must carry the merge's commit.
        let! unstamped =
          countSql
            "SELECT COUNT(*) as n FROM package_ops p
             WHERE p.commit_hash IS NULL AND p.effective = 1
               AND p.id IN (SELECT op_id FROM op_branches ob
                            JOIN branches b ON b.id = ob.branch_id WHERE b.name = 'shared2')"
            []
        Expect.equal
          unstamped
          0L
          "every landed op the sibling tags is stamped by the merge"

        do! shows state [ "status" ] "clean" "the merge left main clean"

        let! _ = runCli state [ "branch"; "archive"; "shared2"; "-y" ]
        do!
          shows
            state
            [ "status" ]
            "clean"
            "the sibling's archive does not resurrect the op as draft work"
      })

/// In the run order CliTraces.Tests.fs composes; sequencing lives there too.
/// Ocean's review, #1: `discard <name>` on a branch dropped every op for that name, committed
/// Ops are content-addressed, so re-binding a name to a hash it held before is byte-identical to
/// the op that first bound it: it dedupes, folds nothing, and the revert silently does not happen
/// while the CLI reports success. `Decision`/`Override` is the op that means "this binding again,
/// and I mean it" -- it is what `propagate pin` already authors for exactly this reason.
let revertingToAnEarlierVersionTakesEffect =
  cliTestOnMain "going back to an earlier version actually goes back" (fun state ->
    task {
      do! start state
      do! switch state "revertbr"
      do! fn state "Tests.Revert.b" "() : Int64 = 611L"
      do! commit state "611"
      do! fn state "Tests.Revert.b" "() : Int64 = 622L"
      do! commit state "622"
      do! evals state "Tests.Revert.b ()" "622" "the second version is live"

      do! fn state "Tests.Revert.b" "() : Int64 = 611L"
      do!
        evals
          state
          "Tests.Revert.b ()"
          "611"
          "and going back to the first takes effect"
      do! dirty state "the revert is a change, so the draft holds it"

      do! commit state "back to 611"
      do! evals state "Tests.Revert.b ()" "611" "and it survives the commit"
      do! onMain state
    })

/// The same on main, where authoring is live-on-write.
let revertingOnMainTakesEffect =
  cliTestOnMain "going back to an earlier version works on main too" (fun state ->
    task {
      do! start state
      do! fn state "Tests.RevertMain.f" "() : Int64 = 1L"
      do! commit state "one"
      do! fn state "Tests.RevertMain.f" "() : Int64 = 2L"
      do! commit state "two"
      do! fn state "Tests.RevertMain.f" "() : Int64 = 1L"
      do! evals state "Tests.RevertMain.f ()" "1" "back to the first version"
      do! discardAll state
    })


/// `--include=<caller>` on a branch committed the caller and left its dependency's NAME in the
/// draft, so a reader downstream could run the caller but could not find what it called.
let aPartialCommitTakesItsDependencysName =
  cliTestOnMain
    "a partial commit on a branch carries the names it depends on"
    (fun state ->
      task {
        do! start state
        do! switch state "partialbr"
        do! fn state "Tests.Partial2.dep" "() : Int64 = 5L"
        do!
          fn state "Tests.Partial2.caller" "() : Int64 = Tests.Partial2.dep () + 1L"

        do! commitOnly state "caller" "Tests.Partial2.caller"
        do! evals state "Tests.Partial2.caller ()" "6" "the caller runs here"
        do!
          clean
            state
            "and nothing of it is left uncommitted: the dependency came along"
        do! onMain state
      })


/// Commit B, then A, then C: C's parent must be A, the commit that actually precedes it. Taking
/// "the newest commit" by arrival instead put A on nobody's chain.
let commitsFollowTheCommitBeforeThem =
  cliTestOnMain
    "each commit follows the one before it, whatever order they were made in"
    (fun state ->
      task {
        do! start state
        do! fn state "Tests.Chain.a" "() : Int64 = 1L"
        do! fn state "Tests.Chain.b" "() : Int64 = 2L"
        do! commitOnly state "B" "Tests.Chain.b"
        do! commitOnly state "A" "Tests.Chain.a"

        let! afterA = runCliPlain state [ "commits"; "1" ]
        let aHash =
          System.Text.RegularExpressions.Regex.Match(afterA, @"[0-9a-f]{8}").Value

        do! fn state "Tests.Chain.c" "() : Int64 = 3L"
        do! commit state "C"

        let! newest = runCliPlain state [ "commits"; "1" ]
        let cHash =
          System.Text.RegularExpressions.Regex.Match(newest, @"[0-9a-f]{8}").Value

        let! shown = runCliPlain state [ "show"; cHash ]
        Expect.stringContains
          shown
          aHash
          $"C ({cHash}) follows A ({aHash}), the commit before it, got: {shown}"
        do! discardAll state
      })


/// A pin made on a parent branch APPLIES on a child (resolution walks the chain), but `propagate`
/// listed only the child's own rows, so the child was told nothing was pinned while being governed
/// by one.
let aParentsPinIsListedOnTheChild =
  cliTestOnMain "a child branch lists the pins that govern it" (fun state ->
    task {
      do! start state
      do! switch state "pinpar"
      do! fn state "Tests.Pin2.base" "() : Int64 = 10L"
      do! fn state "Tests.Pin2.caller" "() : Int64 = Tests.Pin2.base () + 1L"
      do! pin state "Tests.Pin2.caller"
      do! commit state "par"

      do! switch state "pinkid"
      do!
        shows
          state
          [ "propagate" ]
          "Tests.Pin2.caller"
          "the child lists the pin it inherits"

      // And the pin is really in force here, which is what makes the listing's silence a lie.
      do! fn state "Tests.Pin2.base" "() : Int64 = 20L"
      do!
        evals
          state
          "Tests.Pin2.caller ()"
          "11"
          "the inherited pin held the caller back"
      do! onMain state
    })


/// `conflicts override <name>` took the OLDEST row for that name, resolved or not, while the
/// listing shows only pending ones. With one name conflicted twice, answering by name acted on the
/// settled conflict and left the open one open, reporting success either way.
let overrideByNameAnswersThePendingConflict =
  cliTestOnMain "answering a conflict by name answers the open one" (fun state ->
    task {
      do! start state
      do! switch state "confl2"

      let record (id : string) (hash : string) =
        $"""Darklang.SCM.Conflicts.record (Builtin.scmCurrentBranch ()) [Darklang.SCM.Conflicts.Conflict {{ id = "{id}"; owner = "Tests"; modules = "Confl2"; name = "same"; itemType = "fn"; part = ""; kind = "same-name-different-hash"; candidates = []; autoResolvedTo = "{hash}"; reason = "test"; status = "pending"; resolvedBy = "" }}]"""

      do! run state [ "eval"; record "r2first001" "aaa" ]
      do! run state [ "conflicts"; "ack"; "r2first001" ]
      do! run state [ "eval"; record "r2second02" "bbb" ]

      // By NAME: the settled one must not be what answers.
      let! answered = runCliPlain state [ "conflicts"; "ack"; "Tests.Confl2.same" ]
      Expect.stringContains
        answered
        "r2second02"
        $"the open conflict is the one answered, got: {answered}"

      let! left = runCliPlain state [ "conflicts" ]
      Expect.isFalse
        (left.Contains "r2second02")
        $"and it is closed afterwards, got: {left}"
      do! onMain state
    })


let tests : List<Test> =
  [ commitRefusesDefiniteTypeErrors
    deprecationIsReversible
    theWorkedExampleWorks
    editsAreVisibleInTheSameProcess
    deprecationTakesEffectInTheSameProcess
    otherBranchAnswersStayCurrent
    branchVerbsTakeTheNameYouSee
    branchItemsArePolicyTargets
    committingOnABranchCommitsItsOps
    conflictsBelongToTheBranchTheyHappenedOn
    discardOnABranchLeavesMainAlone
    diffAndLogAnswerInJson
    mergeGatesAreDecidedInDark
    branchBundleKeepsWhatItCannotRead
    bareDiffShowsTheDraft
    unguardedTransportRefusesGuestCallers
    bareSubcommandDoesNotBecomeABranch
    dependentsSeeTheBranchYouAreOn
    bareMergeAndRebaseMeanThisBranch
    branchChainSeesItsAncestry
    branchReadsAnswerAboutTheBranch
    aBranchKnowsWhatFollowed
    switchRefusesAForeignId
    aBundleCarriesTheContentItsNamesPointAt
    aMergeCommitsWhatItLands
    aBundleCarriesItsCommits
    anUnbindRemovesANameThroughTheCli
    aDeprecationCommitsByEitherPath
    archivingABranchCommitsItsEvent
    aBranchLearnsThatMainMovedItsDependency
    fnReadsItsDefinitionFromAFile
    statusSeparatesTheDraftsConstraintsFromStandingOnes
    aMissingNameIsOneLineNotAStackHeader
    anUnknownParentIsNamedAsSuch
    failuresExitNonzero
    commitAsksAndNeedsAMessage
    constraintFlagsAreNotTargets
    aBranchDiscardLeavesNoDanglingTags
    archivingAChildLandsOnItsParent
    aNameHoldsOneItemWhateverItsKind
    editChangesAnItemWithoutRetypingIt
    everyJsonSurfaceParses
    followingDoesNotDestroyASharedName
    aBranchNeverSeesMainsDraft
    discardSparesInertOps
    mergeCommitsWhatASiblingStillTags
    editingAColleaguesVersionSaysSo
    commitsHideHousekeeping
    commitsChainToTheirParent
    revertingToAnEarlierVersionTakesEffect
    revertingOnMainTakesEffect
    aPartialCommitTakesItsDependencysName
    commitsFollowTheCommitBeforeThem
    aParentsPinIsListedOnTheChild
    overrideByNameAnswersThePendingConflict ]
