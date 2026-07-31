# Darklang Monorepo

Darklang is a language, editor and infrastructure in one. This repo holds the F# runtime
(`backend/`) and the Darklang standard library and CLI, written in Darklang itself
(`packages/`).

## Start the dev container first

Everything runs inside this clone's dev container. Nothing works until it's up:

    scripts/dev/start

Safe to re-run; it's a no-op if the container is already going. The first run pulls and
builds, so expect a few minutes. It also picks this clone's host ports and prints them.

Then read `./scripts/run-cli docs for-ai` for language syntax, naming and workflow. Always
read that one.

## Several clones at once

We keep several clones checked out side by side, one per task or agent, each with its own
container. Nothing is shared between them: containers are matched to clones by directory,
each gets its own `backend/Build` volume, and each gets its own block of host ports.

`./scripts/*` all auto-enter this clone's container (see
`scripts/devcontainer/_assert-in-container`). If there's no container for the clone you're
in, they stop and tell you, rather than running in someone else's. Don't reach for
`docker exec` to work around that; run `scripts/dev/start` instead.

Scripts starting with `_` are meant to be called by other scripts. The rest are for you.

## Ports

Inside the container, 9090-9099 are reserved for ad-hoc `darklang serve`. That's the same
in every clone.

On the host, each clone gets its own block: the first clone up gets 9090-9099, the next
9100-9109, and so on. `scripts/dev/start` prints yours. To get one later:

    scripts/dev/host-port        # host port for container 9090
    scripts/dev/host-port 9091   # ...for 9091

## Builds

Auto-rebuilds on save; don't manually rebuild. A `.dark` change takes about 30 seconds
(the whole package set reloads, not just your file). A `.fs` change is about 80. Watch:

    rundir/logs/packages.log        # .dark reload
    rundir/logs/build-server.log    # F# build

## Tests

    ./scripts/run-backend-tests

Filtering is Expecto's, and the three flags do different things:

    --filter <path>            slash-separated, matched from the ROOT
    --filter-test-list <sub>   substring, matches test *lists*
    --filter-test-case <sub>   substring, matches test *cases*
    --list-tests               print every test name

`--filter` is the one that surprises people. It matches from the root of the tree, and
everything lives under `testList "tests"`, so `--filter LibExecution` matches nothing while
`--filter tests/LibExecution` works. The other two are substring matches, which is why they
feel more forgiving. When in doubt, `--list-tests` and grep.

Never run two `run-backend-tests` at once, even in different clones: they share
`test-data.db`, a fixed port, and a `killall -9 Tests`.

A full run takes a few minutes. It logs to `rundir/logs/fsharp-tests.log`; poll that for
"errored in" to catch failures without waiting for the end.

## Directories

    backend/src/          # F# source
      LibExecution/       # execution engine
      LibParser/          # parser
      LibDB/              # package DB, branches, SCM ops, user DB, SQLite
                          # plumbing (LibDB.Sqlite), tracing recorder
      Builtins/           # Cli, CliHost, Http.Client, Http.Server, Language,
                          # Matter, Pure, Random, Time
    packages/darklang/    # .dark files
      cli/                # CLI code
      scm/                # SCM library (branch, rebase, merge, packageOps)
      stdlib/             # standard library
    rundir/logs/          # log files
    scripts/              # build and dev scripts

## Logs (rundir/logs/)

    cli.log              # CLI runtime issues
    lsp.log              # LSP input/output
    packages.log         # .dark loading from disk
    build-server.log     # F# build issues
    migrations.log       # migrations, if recently changed

## Adding a builtin (F#)

Pick the right `Builtins` subproject: `Pure` (pure stdlib), `Matter` (DB, package store,
traces), `Language` (reflection, parser, language tools), `Http.Server`, `Http.Client`,
`CliHost` (eval, script entry), `Cli` (file, terminal, other side effects), `Time`,
`Random`. Add the fn to the `fns` list in the relevant `Libs/<module>.fs`, save, wait for
the rebuild.

Return structured Dark values (enums, records), not pre-rendered strings or string tags.
Dark-side code formats for display. Don't build `"superseded-by:<hash>"` in F# for a Dark
caller to parse; return the `DeprecationKind` enum. F#-side stringify is only right when
the value genuinely has no Dark-side type, which is rare.

## Adding a CLI command (Darklang)

1. `packages/darklang/cli/<name>.dark`
2. Implement `execute`, `help`, `complete`
3. Register in `Registry.allCommands` in `cli/registry.dark`

## SCM / branches

    LibDB/Branches.fs         # branch CRUD, getBranchChain
    LibDB/Rebase.fs           # conflict detection, rebase
    LibDB/Merge.fs            # merge into parent
    LibDB/Inserts.fs          # ops take branchId
    LibDB/Queries.fs          # branch-aware SQL
    LibDB/PackageManager.fs   # pt(branchId) constructs PM
    Builtins/Builtins.Matter/Libs/PM/{Branches,Rebase,Merge}.fs

`PackageManager.pt` takes a branchId and pre-computes the branch chain for name resolution.
Items are global (content-addressed); locations (name bindings) are branch-scoped.

## Gotchas

**PackageRefs stale hash.** `backend/src/LibExecution/package-ref-hashes.txt` isn't in git.
Empty is tolerated; non-empty with a missing key crashes at startup with "PackageRefs: X
hash not found". After adding a ref:
`> backend/src/LibExecution/package-ref-hashes.txt && ./scripts/build/reload-packages`

**Name resolution in test files.** `backend/testfiles/` is parsed with owner "Tests", so
`Darklang.*` names need full qualification or the `Stdlib.` shortcut. `Stdlib.Json.ParseError.toString`
and `Darklang.SCM.Branch.mainBranchId` resolve; `SCM.Branch.mainBranchId` doesn't. Impl:
`backend/src/LibParser/NameResolver.fs` and `packages/darklang/languageTools/nameResolver.dark`.

**Enum construction across modules.** Name the DU type, not just the module:
`ProgramTypes.Reference.PackageFn hash` works, `ProgramTypes.PackageFn hash` doesn't. In a
match arm the bare case is fine, since the matched value's type resolves it.

**Cross-module pipes.** Dark parses pipes greedily, so
`Stdlib.List.length xs |> Stdlib.Int.toString` raises "Pipe: LongIdent". Parenthesize the
left side.

**Nested functions.** Only the fully annotated form works: `let helper (x: Int) : Int = ...`.
It desugars to a local lambda, can close over outer bindings and can call itself, but mutual
recursion between nested functions isn't supported. There's no `rec` keyword; top-level
functions are self-recursive already.

**Record update takes no type tag.** `{ state with field = v }` is right.
`MyType { state with field = v }` looks like F# but parses as function application.

**Value bindings take no type annotation.** `let xs = [...]`, not `let xs : List<String> = [...]`,
which fails with "Value annotations are not supported". Function bindings do take them, and
nested functions require them.

## Interactive CLI testing

The interactive CLI (`run-cli` with no args) needs a real TTY. Use `expect`:

    ./scripts/run-in-docker expect scripts/testing/test-interactive.expect

Telemetry lands in `rundir/logs/telemetry.jsonl`. Full guide: `docs interactive-testing`.

## Debugging

    Builtin.debug "label" value   # prints DEBUG: label: <repr> to stdout
    eval <expr>                   # test small pieces

## Style

`///` for doc comments on types, DU cases and fns, in both F# and Dark. `//` for inline
notes. 85 columns.

## Keep this file honest

If you learn something that cost 30+ minutes and isn't written down here or in
`docs for-ai`, add a short entry before you move on. These compound.

## Elsewhere

    ~/vaults/Darklang Dev    # team notes
    wip.darklang.com         # website WIP
    blog.darklang.com        # blog
