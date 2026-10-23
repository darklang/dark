# later/

Code that is not part of the product right now.

Everything in here was moved out of the live tree during the molt, with its original repo path
preserved underneath, so `git mv later/<path> <path>` puts a WHOLE FILE back exactly where it was.
That is most of the directory but not all of it: 117 files are fragments carved out of files that
are still live, which come back by hand, and a few things were DELETED rather than moved, for which
only `git log` has the content. Both exceptions are spelled out below. Nothing in here is built,
loaded, tested or linted:

- the F# projects are out of `backend/fsdark.sln`
- the `.dark` files are out of `packages/`, so the package reload never sees them
- the scripts are out of `scripts/`, so no gate and no CI step runs them
- `backend/tests/Tests/Builtin.Tests.fs` prunes `later/` from its own corpus scan, and
  `scripts/linting/yamllinter` prunes it from its walk

It is allowed to rot. If a file in here stops compiling against the tree that moved on without
it, that is expected and is not a bug to fix. The intent for most of it is to come back as a
downloadable unit rather than to be merged back into the main tree.

This file is written for whoever picks something up later, which in practice means an agent. It
says what is here, how it is organised, what putting a piece back actually involves, and the
traps that are specific to this directory rather than to the code in it.

---

## What is in here

476 files are tracked, and 4,426 files sit under `vscode-extension/` on disk of which only 65
are tracked: 4,096 of the untracked ones are its `node_modules`, which came along for the ride
because the directory moved as a directory. Tracked, by area:

    packages/             295 files   Dark code. The big one.
    backend/               86 files   F#: three LibCloud modules, the Wasm project, the builtins
                                      whose only Dark wrapper left, and carved fragments.
    vscode-extension/      65 files   the VS Code extension's own source.
    scripts/               13 files   the sync gates, the two relay hand tools, the demo script,
                                      and two superseded test harnesses.
    docs/                   5 files   three are fragments of pages that are still live.
    experiments/            4 files
    user-code/              3 files
    .vscode/                2 files   the extension's launch and task configs.
    deploy/                 2 files   Dockerfile.matter and fly.toml, the relay's deployment.
    README.md                         this file.

Every number above is a snapshot and goes stale the next time something is carved in here.
`git ls-files later | wc -l` is the total, the same piped through
`sed 's|^later/||' | awk -F/ 'NF==1{print "ROOT"} NF>1{print $1}' | sort | uniq -c` is the table,
and `git grep -l 'RESTORE:' -- later | wc -l` is the fragment count, since a fragment is exactly
a file carrying a `RESTORE:` header and a whole file is not.

Under `packages/darklang/`, by subject: `sync/` (the wire codec, the import planner, the hosted
relay), `cli/` (the commands and views that went with it), `scm/` (bundles, and the carved
attribution helpers), `opml/`, `vscode/`, `languageServerProtocol/` and `lsp-extensions/`,
`modelContextProtocol/` and `internal-mcp-server/`, `llm/`, `discord.dark`, `tailscale.dark`,
`wip/`, `tests/` (see below), plus `packages/stachu/` and `packages/feriel/` entire.

### `packages/darklang/tests/` and the package-test port

The execution testfiles under `backend/testfiles/execution/` became package TESTS while this
branch was open: items in the store, declared `test name = body`, run by `dark test run`. That
changes what this directory holds for anything test-shaped, so three things are worth knowing.

The suites for molted subjects are here at their NEW paths, because that is where a `git mv` has
to put them back: `tests/scm/wire.dark`, `tests/scm/matterRoutes.dark`,
`tests/scm/syncSeenEverything.dark`, `tests/cli/tailscale.dark`, `tests/cli/appServiceSafety.dark`,
`tests/languageServerProtocol/position.dark`, `tests/stdlib/db.dark`, and
`packages/stachu/tests/` (three files).

Their pre-port versions are NOT here. An earlier round of this molt moved the old
`backend/testfiles/execution/**` copies, and those were dropped once the port landed: restoring a
file for a runner that no longer exists is worse than having nothing. The one exception is
`backend/testfiles/execution/llm/usage.dark`, which upstream deleted rather than ported, so its
assertions exist nowhere else; its own header says the runner is gone and what porting it means.

The fragments carved out of ported tests sit beside the ported file, not the old one:
`tests/cli/outliner.dark.formats`, `tests/stdlib/altJson.dark.integer-callers`,
`tests/languageTools/atRestTypeChecker.dark.lsp-diagnostics`.

## Two kinds of thing in here, and only one of them is easy

**A whole file at its original path.** `git mv` restores it and there is nothing to read. Most of
the directory is this.

**A fragment**, at `later/<original path>.<topic>`: a piece carved out of a file that is still
live, because what left was interleaved with something that stays. There are 103 of them. One
(`later/backend/src/user-db-loading`) holds two separate carves, so 104 in all, and four carry a
`MOVED:` header instead of a `RESTORE:` one. Nineteen do not sit beside a live file at
all: they have synthetic extensionless paths (`later/packages/darklang/commit-author-param`,
`later/backend/src/user-db-loading`) because they cover several source files at once, so looking
for a carve by its source path will not always find it. Grep the whole directory instead. Every
fragment opens with a header in this shape:

    // RESTORE: packages/darklang/cli/registry.dark
    // WAS: the command rows for push/pull/sync/connect, in Registry.allCommands
    // ANCHOR: between the ("backups", ...) row and the ("builtins", ...) row
    // ALSO: later/packages/darklang/cli/registry.dark.sync-accepts carries the
    //       accepts* functions these rows name
    // WHEN: the sync molt, 2026-10-12

`RESTORE` is the file it came out of. `WAS` says what it is. `ANCHOR` says where it went, by
naming what sat on either side of it rather than by line number, since line numbers rot on the
first edit either side. `ALSO` cross-references the other fragments that have to come back in the
same sitting, because a carve usually leaves pieces in two or three files. `WHEN` dates it and
names the round, which is the thing to grep on.

    grep -rnE '(RESTORE|MOVED):' later/      every fragment, with its origin
    grep -rln 'WHEN: the sync molt' later/   one round's fragments

Two things to know about those two commands, because both mislead on their own.

`RESTORE:` alone misses four files, which use `MOVED:` instead: `keywordDescription.dark`,
`parser/canvas.dark`, `stdlib/cli/gunzip.dark` and `stdlib/cli/ui/spinner.dark`. Hence the
alternation above.

`WHEN:` is a weak index. 80 fragments say "the sync molt", and that round swept up sync, the AI
view, the apps framework, gated views and the outliner, so grepping it hands you five unrelated
subsystems. Worse, four pairs that genuinely must come back together are split ACROSS rounds, so
the round is neither necessary nor sufficient: trust `ALSO:` over `WHEN:`, and read every `ALSO:`
chain transitively rather than one level deep.

There are more header kinds than the five above, and the extra ones carry the decisions rather
than the mechanics: `NOTE:` (12 of them), `TODO:` (7), `CONFIGURATION:` (3), `CLEANUP:` (3),
`HELPER:` and `FUTURE:`. Read a fragment's whole header block, not the five fields you expected.

## Putting something back

The order that works:

1. **Collect the whole topic first.** Grep `WHEN:` for the round and read every `ALSO:` line
   before touching anything. A feature carved out of five files comes back into five files, and
   restoring four of them leaves the tree in a state where the build is green and the feature is
   broken, which is the worst of the three outcomes.
2. **Whole files first, by `git mv`.** That restores the content and the history together.
3. **Then the fragments, by hand.** Expect to port rather than to apply. A `.dark` fragment calls
   functions at hashes that no longer exist, and an F# fragment may name a type that has since
   been renamed or widened. Read the anchor, find the place, write the code that belongs there
   now.
4. **Re-add F# projects to `backend/fsdark.sln`**, the `ProjectReference`s that named them, AND
   the `<Compile Include>` lines. The molt removed five `ProjectReference`s to `LibCloud` plus its
   `Project` entry in the sln, and 17 `<Compile Include>` lines out of projects that STAYED:
   `Builtins.Matter` (Libs/DB.fs, Libs/Account.fs), `LibDB` (UserDB.fs), `LibExecution`
   (UnguardedOrigins.fs, RTQueryCompiler.fs), `LibSerialization`
   (DvalReprInternalQueryable.fs), `Prelude` (Lazy.fs), `LibCloud` (Account.fs, Serialize.fs,
   Toplevels.fs) and `Tests` (Toplevels.Tests.fs, CliSyncSurface.Tests.fs,
   DvalReprInternalQueryable.Tests.fs, OpTransport.Tests.fs, MultiInstanceHarness.fs,
   MultiInstance.Tests.fs, MultiInstanceDark.Tests.fs). No fragment records those lines, so this
   step is the one to do from this list rather than from a header. Seven of the 17 are TEST files,
   which is the dangerous half: `git mv` them back without the fsproj edit and they are never
   compiled and never run, so the suite goes green over tests that do not exist.
5. **Build, then run the whole suite.** Dark resolves names when a call EXECUTES, so a green build
   says almost nothing about restored `.dark` code. The suite is the first thing that sees a call
   site, a match arm, a record construction or an arity that does not line up.

## Traps specific to this directory

- **A builtin that comes back needs its Dark wrapper in the same change.**
  `backend/tests/Tests/Builtin.Tests.fs` asserts that every builtin has exactly one `Builtin.x`
  reference in `packages/`, and that every builtin has at least one. 38 builtins left in this molt
  because their only wrapper did; restore one without its wrapper and that test fails, correctly.
  All 38 are declared under `later/`, each with its wrapper, and nothing under `later/` names a
  builtin that is gone, so this half of the directory is self-consistent.
- **`LibDB/Releases.fs` has NO steps, and the schema is one editable file.** An earlier version of
  this README said there were two steps that `DROP TABLE`d the sync tables and `commits.author`,
  and that a restorer should re-declare tables in a new numbered file "because a merged schema file
  is frozen". Both halves were wrong, and wrong in the direction that makes a restorer skip work:
  - `steps` is `[]`, by deliberate reset. Nothing dropped anything from any existing store. A store
    older than this commit is simply unsupported and `dark` tells you to delete it, which is why
    carrying the old steps would have been pretending to support an upgrade that does not work.
    So the sync tables and `commits.author` are absent from a FRESH store because they are absent
    from the schema, and present in an old store that this build will not open.
  - the fourteen numbered schema files are gone, collapsed into
    `backend/migrations/schema/00-initial-schema.sql`, and the frozen-once-merged rule went with
    them. `AGENTS.md` has the rule that replaced it: a new table goes in a stamped file beside it
    or straight into the initial schema, and a new COLUMN needs both a declaration there and a
    `Releases.fs` step, because a schema file cannot `ALTER`.
  Four fragments still describe the old world and say so in their own headers; see the next trap.
- **A commit's id no longer includes an author.** `mintHash` hashes message, stamp, ops and
  parent. Commits made before the molt keep the ids they have. If attribution comes back, it comes
  back as a column that is not in the hash, or every existing commit id changes.
- **Code in here calls things the live tree no longer has, and Dark will not warn you.** This is
  the trap that matters most, because a name resolves when the call EXECUTES: the restored file
  builds clean, the suite can be green, and it raises the first time somebody reaches the line.
  Worse, the files that need this warning most are WHOLE FILES, which carry no header at all, so
  nothing in the file itself tells you. The full list, and it is a list rather than one case:

      SCM.PackageOps.identity          called by sync/autopush.dark, sync/setup.dark,
      SCM.PackageOps.storedIdentity      sync/syncPushPull.dark, sync/syncStatus.dark and
                                         languageTools/lsp-server/branches.dark. Carried by the
                                         fragments packageOps.dark.identity and packageOps.dark.sync,
                                         which those five files do not reference.
      SCM.PackageOps.branchDiff        called by sync/sync.dark:204
      SCM.PackageOps.branchDiffLines   called by lsp-server/branches.dark:291. Both were a string
                                         layer over branchDiffEntries, which is live and returns the
                                         same information as records; cli/scm/scmjson.dark and
                                         cli/scm/branchHistory.dark show how to render it.
      SCM.PackageOps.opBindsAName      called by sync/packageOpsSync.dark:50. Carried, with its
                                         builtin, by the four *.binds-a-name fragments.
      SCM.PackageOps.getCommitNamedOps called by lsp-server/branches.dark:445. Carried, with its
                                         builtin, by the three *.commit-named-ops fragments.
      vApps, vAI                       view indices, called by workbench/keys-view.dark.sync,
                                         nav.dark.apps and ai-and-apps.views. vDevices IS
                                         re-declared by wiring.sync; these two are declared nowhere.
      renderCalendar, calendarHints    named by the ViewDef in workbench/gated-views, whose own
                                         NOTE says the Calendar view was deleted rather than kept.
                                         The fragment contradicts itself; the bodies are in git only.
      AiSection, aiSectionIndex,       named by views-misc.dark.ai and nav.dark.apps. No fragment
      State.ai, ReturnPoint.aiSection    carries their declarations; re-author from git log.
      type AccountID                   removed from backend/src/Prelude/Prelude.fs, used by
                                         later/backend/src/LibCloud/Account.fs and Toplevels.fs.
                                         F#, so the compiler stops you. Loud, not silent.

  Two more are ARITY changes, which fail the same silent way: `itemsForView` takes 4 parameters now
  and `nav.dark.apps` passes 5 (the extra one was `state.ai.section`); `SCM.PackageOps.commitDraft`
  takes 2 and `lsp-server/branches.dark:390` passes 3 (`author` first). Both raise `Apply`.

  Three more arrived with the package-test port, and the first of them is the only one in here
  that fails LOUDLY, which is worth knowing because it is the cheapest kind:

      => sqlerror "msg"                a test expectation form that no longer PARSES. The
                                         user-DB query compiler produced those errors and left
                                         with the DBs, so the parser has one expected-error form
                                         (`=> raises`) and `PT.ExpectedError` has one case. Used by
                                         tests/stdlib/db.dark and the fragment
                                         CliPackages.Tests.fs.sql-error-view. A parse error on load,
                                         not a surprise at run time.
      PT.PackageTest.testDBs           the field that carried a test's private DB schemas, and the
                                         per-run minting and row cleanup in PackageTests.fs with
                                         it. `[<DB>]` still parses inside a test and still lowers
                                         to a `TDB`, so tests/stdlib/db.dark LOADS and then finds
                                         no database. Restoring it means the field, the serializer,
                                         `Canonical.fs`, both halves of the PT/Dark bridge and the
                                         Dark mirror in languageTools/programTypes.dark, in
                                         lockstep: a Dark record built with a field its declaration
                                         lacks raises where the INPUT is built, not where the code
                                         runs. The fragment PackageTests.Tests.fs.isolated-dbs
                                         lists the F# side.
      RT.Program, program.dbs          the runtime's user-DB set, now gone from ExecutionState
                                         along with the `VarNotFound` branch that turned a name
                                         into a `DDB`. Read by
                                         later/backend/src/Builtins/Builtins.Matter/Libs/DB.fs and
                                         described by later/backend/src/user-db-loading. F#, so the
                                         compiler stops you.

  And one dangling PackageRef, which only ever showed up as a warning: `PackageRefs.Type.Cli.script`
  named `Cli.Scripts.Script` after the `scripts` command moved here, so every package reload printed
  `Warning: 1 PackageRefs items not found`. Nothing in F# read it. It is removed now, and
  `cli/registry.dark.scripts` carries the line to put back.
- **`later/backend/src/LibCloud/LibCloud.fsproj` lists the wrong files.** It names `Config.fs` and
  `File.fs`, which the molt did not move here at all: they stayed, rehomed as
  `backend/src/LibConfig/Dirs.fs` and `File.fs`. It names none of `Account.fs`, `Serialize.fs` or
  `Toplevels.fs`, which are the three files actually sitting beside it. Restoring LibCloud fails at
  MSBuild on a missing file and then compiles nothing. Fix the fsproj before anything else.
- **Several fragment headers assert that something STAYED when it did not.** They were written
  during a round that a later round undid, and they read as "this part is already done, skip it",
  which is the one error shape that leaves a green build and a broken feature. Known cases, all in
  `NOTE:` or `ANCHOR:` lines: `sync-tables.sql` (a DROP step that does not exist; `op_owners`
  "stayed"), `09-user-data.sql.user-space` (same, a named step that does not exist),
  `conflicts.dark.sync-bases` ("still in the schema"), `branches.dark.relay` ("still in
  03-branches.sql", a deleted file), `packageOps.dark.sync` ("storedIdentity and identity
  STAYED"), `branch.dark.relay-pushpull` ("export/import stayed in the live tree"),
  `packageOpsBundles.dark.relay` (anchors between two functions "which stay"), and
  `.vscode/launch-extension.json` ("the file is gone from `.vscode/` entirely", when it is live and
  merely emptied, so following it clobbers a live file). Verify a header's claim about the live tree
  against the live tree. The headers are reliable about what they CARRY and unreliable about what
  surrounds it.
- **A restored gate needs more than its file**, and a restored CI job needs more than its script.
  `scripts/testing/gates` lost eight gate names (`setup`, `relay-routes`, `sync-hostile-relay`,
  `sync-multi-instance`, `lsp-branches`, `lsp-typing`, `lsp-concurrent`, `hosted-db`), their
  `GATES=` and `CI_GATES=` entries, their one-line descriptions and the `. _gates-sync` source
  line. `.circleci/config.yml` lost the `publish-vs-code-extension` job and its workflow entry. No
  fragment carries any of that, so `git mv` gives you a file nothing sources and a script nothing
  calls.
- **Some things were DELETED, not moved here.** `backend/src/Prelude/Lazy.fs`,
  `backend/src/LibCloud/Config.fs` and `File.fs` (which became `LibConfig/Dirs.fs` and `File.fs`,
  so their content survives in a different shape), `backend/static/favicon-32x32.png` (a
  byte-identical copy of `backend/testfiles/data/favicon-32x32.png`, which stays), and the
  thirteen numbered schema files (folded into `00-initial-schema.sql`). Only `git log` has the
  first of those as it stood.
- **The restored VS Code launch and task configs point into `later/`.** Both name
  `${workspaceRoot}/vscode-extension`, which is now `later/vscode-extension`. Rewrite the paths or
  move the extension back; neither fragment says so.
- **`packages/stachu/` and `packages/feriel/` are personal trees**, not product code. They are in
  here because nothing in the product references them, not because anybody decided about them.
- **The VS Code extension's `node_modules` moved with it** but were never tracked, so they are on
  this disk and in nobody else's checkout. Delete them and re-install if it comes back.

## Finding out what left and why

The commit that created this directory is the record: its message says what moved, what the core
is, and what the molt cost in lines, package items, builtins, commands and reload time. There is
no separate notes file here on purpose, and a pointer to one would rot.

For a question this file cannot answer, the shape that works is
`git log --diff-filter=R --follow -- later/<path>` for one file's move, and the molt commit's own
message for the reasoning.
