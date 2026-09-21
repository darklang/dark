# Live programming: running things follow your edits

A process that stays up (a TUI, `dark serve`, a daemon) keeps running the code you
had when it started, unless something tells it the store changed and it resolves its
entry names again. This is that something, and the rules a host follows once it has
heard.

Status, 2026-09-20: both demos run in the container, and the leftovers below are in. The signal, what changed, affects,
last-good, a `serve` that follows edits, the `Node` tree with its terminal and page
renderers, `Host.await` over a shim, the host loop behind `dark apps view`, the
workbench on the same loop, a daemon runner, and `live.autopush` are in. Since then:
Ctrl-S saves a view's model as a `val` and `--resume` starts from it; `package-stats`
and `sync status` are trees; the TUI windows a tall tree and right-aligns numbers;
autopush runs from the workbench's and the LSP's saves too; `serve --dev` reloads an
open page.

---

## The idea in one paragraph

Reload is name resolution, not code replacement. An edit never overwrites a version:
it binds a name to a new hash, and the old hash stays in the store. So a host goes
live by dropping its name caches, resolving its entry name again and calling whatever
hash that is now. In-flight work keeps the hashes it started with. A fn reference held
as a value (a closure, a router handed to a server at start) keeps its hash until
someone resolves the name again, which is why `serve` holds the router's LOCATION and
resolves per request, and why a daemon that holds a value needs a restart until the
scheduler's budget yield lands.

## The signal (how a process learns)

`LibDB.Sqlite.DataVersion.current ()`, `Builtin.storeVersion`, `Stdlib.Live.version`.

`PRAGMA data_version` is a per-connection counter that moves when a commit lands
through any OTHER connection, in this process or another. LibDB opens a pooled
connection per query, so the pragma is asked on one connection held for the life of
the process (one per store, since tests swap stores and back). The number itself means
nothing; "same as last time" or "moved" does, and only within one process.

A config write or a trace write moves it too. `Stdlib.Live.poll` tells those apart from
an edit by asking the op log what landed (below), and answers `None` for them.

## What changed (`Stdlib.Live.poll`, `SCM.PackageOps.idsLandedSince`)

A `Watch` is a branch, the last counter seen, the newest insert time (`created_at`,
second resolution) reported so far, and the ids already reported at that second.
`poll` returns the next `Watch` and, when something landed, a `Change`: the ops, the
names they bound or unbound (`touched`), or `unknown` when it will not describe them
(more than 500 ops in one poll, which a host treats as "everything changed").

Why insert time and not the origin stamp or the rowid: the origin stamp is a logical
clock that runs ahead after a big batch (a package reload mints thousands of stamps a
millisecond apart), so an op authored in the next process can carry a stamp EARLIER
than the log's newest and never sort as "after" it; a peer's ops arrive with the peer's
clock. A rowid is reused after a delete. Insert time only moves forward on this store.

What insert time cannot do is tell a new op from an old one handed back by a draft
rewrite: `WipRefresh` deletes and re-inserts main's draft (with a fresh insert time and
the same ids) when a forward reference resolves. That is rare; when it happens the poll
sees a flood and reports `unknown`, and the host re-runs everything. Correct, not
precise. A rewrite that preserved `created_at` for unchanged ops (it already preserves
`origin_ts`) would make it precise; that is a `LibDB/Inserts.fs` change and is not done.

The ids at the newest second are held only while that second is still the current one.
A reload lands thousands of ops in one second; reading them back on every poll for as
long as the watch lived cost 400 ms a poll, and nothing can land in a second that has
passed.

`poll` drops the name caches (`LibDB.Caching.invalidateAll`, via
`SCM.Draft.invalidateCaches`) BEFORE reporting a change, so the caller's next name
lookup sees the store as it is. That is the whole reload.

## Which caches drop, which survive

| cache | keyed by | on a change |
|---|---|---|
| `LibDB.PackageManager.pt.findFn` / `findType` / `findValue` | location | dropped by `invalidateAll`; the next lookup reads the new binding |
| `LibDB.PackageManager.ptForBranch` overlay (`otherBranchOps`) | branch id | dropped by `invalidateAll` |
| `LibDB.PackageManager.harmfulCache` | none (a set of hashes) | dropped by `invalidateAll` |
| `LibDB.PackageManager.rt.getFn` / `getType` / `getValue`, `pt.getFn` ... | hash | dropped by `invalidateAll` too, harmlessly: a hash's body never changes, the entry refills on the next call |
| `ExecutionState.packageFnCallCache` | hash | survives; correct, a hash's call data never changes |
| `ExecutionState.lambdaInstrCache`, `VMState.lambdaInstrDataCache` | lambda id | survives; per execution |
| `RuntimeTypes.Types` `DeclCache` | type hash | survives |
| `Interpreter` package-policy memo | hash, per call data | survives; keyed on the policy fn's identity |

The rule: name-keyed answers drop, hash-keyed answers stay. The test
`tests/Interpreter/Reload` pins the consequence: a fn reference held before an edit
runs the old body afterwards, a fresh lookup of the name runs the new one.

## Does it reach what I'm showing (`Stdlib.Live.affects`)

`affects change entry` is true when `entry` is one of the touched names or depends,
transitively, on one of them. Reverse edges from `package_dependencies`, matched by
NAME (`SCM.Deps.dependentsOf`), so a caller still pointing at the previous version of
a name is found, which is exactly the caller a live host is. Over-approximates
(a dependent through a branch that never runs is still a dependent) and never misses.
An `unknown` change affects everything; a walk past 2,000 names answers true.

## Which version to run (`Stdlib.Live.LastGood`)

`LastGood` holds an entry LOCATION, the hash the host is on, and, when the newest
version is not that hash, the at-rest report that says why. `refresh branchId landed
lg` looks the name up again and:

- checks at rest (`AtRestTypeChecker.checkPackageOps`, against the store's closure) the
  newest version when its hash differs from the one held, AND every declaration in
  `landed` (the change's ops);
- on `Failed`, keeps the previous hash and carries the report;
- otherwise adopts the newest hash.

The second half is what catches a broken CALLEE. Propagation repoints the entry at the
new callee, and the entry's own body still checks clean against the callee's unchanged
signature; checking the entry alone would adopt a version that fails on its first call.
So a broken save never blanks a screen or a site: the previous version is still in the
store, and it still runs, on the callees it was built against.

`Live.applicable lg` is the callable for the held hash (`Builtin.applicableByHash`, the
by-hash twin of `applicableByName`). `Live.diagnostic lg` is the one line for a band or
a log.

## `serve` follows edits

`dark serve <router>` resolves the router by name at start (the "no such fn" check and
the bind-time approval root), then hands `Stdlib.HttpServer.serveLive` a
`Stdlib.Live.Router.State` and `Router.step`. Per request the server runs `step` over
the state it keeps between requests; `step` polls, and when the change reaches the
router it refreshes `LastGood` and answers with the handler for this request. The
decision is Dark's; F# only holds the state between requests, because a Dark value
cannot outlive the call that made it. Two requests in flight may both run the step;
it is idempotent (a poll and at most one check), so the race costs a repeated check.

The router's hash is the approval root, so the guest state is re-derived when the hash
moves. A newly broken version is said once on stdout (`live: <entry>: still on the
last good version. The newest has a type error: <why>`), and the fix too (`live: now
serving the new version of <entry>`); the wire keeps getting the last good one. `--no-live` pins the
version resolved at start.

Under the scheduler `serve` is a process that holds its thread on the listener; each
request runs on a thread-pool thread with no scheduler current, so the per-request
`Router.step` keeps polling for itself rather than reading the scheduler's queue. (The
rebase plan wanted that poll replaced by "the latest change the scheduler has seen";
requests are not processes yet, so the poll stays until the re-entry-removal step makes
them one.)

`serve --dev` adds the browser half: `GET /__live` is an event stream that holds the
connection, compares the router's hash every half second to the one the page was served
from, says `reload` once it moved, and ends; every HTML response carries a six-line
script that listens and reloads. Not for production: one comparison per open tab per
half second, and a script in every page.

Test: `tests/CliWorkspace/live/serve follows edits and keeps the last good version`.

## The `Node` tree

A view is `Model -> Stdlib.Cli.UI.Node.Node<'msg>`: `Text`, `Styled`, `Row`, `Column`,
`Table`, `ListOf`, `Input`, `Button`, `Link`, `Region`, `Band`, `Rows` (an escape hatch for
views written as lines), `Empty`. Data, not drawing, except `Input.edited`, which has to
turn the new text into the app's message. `Node.measure` is the natural size;
`Node.toSpans tree region focus` paints it into a `Layout.Region` for the terminal (a
`Column` stacks by natural rows and clips, a `Row` lays out by natural width, a `Region`
fixes a box); `Html.render` writes the same tree as markup with `dark-` classes, `Button`
and `Input` as forms posting the message. Focus is an index into `Node.focusable`,
followed by id across frames.

## The host loop

Every TUI host is written against one contract, provided by a shim today and by the
scheduler's event queue later:

```
let rec loop model =
  match Host.await [Key; StoreChanged] with
  | StoreChanged c when Live.affects c.locations entry ->
      let model = reload entry model      // last-good rule applies
      loop (render model)
  | Key k -> loop (render (update model k))
  | _ -> loop model
```

`Stdlib.Host.await [Key; StoreChanged; Timer ms]` returns the first that fired:
`Key of KeyRead` (the runtime's read: a key, a paste, a burst with its repeat count),
`StoreChanged of Live.Change`, `Timer`. It is `Builtin.hostAwait` (`docs/processes.md`:
under the scheduler the calling process parks on the event queue, fed by a reader
thread, timers and a store poll over `LibDB.Sqlite.DataVersion`; outside a scheduler the
thread is held, polling) plus one thing the runtime cannot do: say which ops landed. The
poll posts that the store MOVED; `await` describes it with `Live.poll` from a watch the
runtime keeps in one slot (`Builtin.hostWatchGet/Set`, since `await` takes no state and a
Dark value does not outlive the call that made it), and a move that carried no op (a
config write) is absorbed and the wait goes on. `Host.begin ()` starts the watch before
a loop resolves its entries, so an edit during startup is not missed. The shim that
stood in for the scheduler on 2026-09-19 (a Dark loop over `stdinReadKeyTimeout`) is
gone; the loops did not change when it went.

The loop itself is `cli/apps/host.dark`. A `View` is three fns by name (`init : Unit ->
'model`, `update : 'model -> Host.Event<'msg> -> 'model`, `render : 'model -> Node`), on an
`App` as `Target.Views`, or any module with those three (`dark apps view My.Module`).
One turn: a store change that reaches the view's entries (`Live.affects`) re-resolves
them through `LastGood` and re-renders, with a toast naming what moved; a key goes to
the focused `Input` or `Button`, else to `update`. `render` failing at rest or at run
keeps the last frame with the reason in a `Band` under it. The model is the only app
state and survives every swap in memory; Ctrl-S snapshots it as a `val` under the render
fn's module (`<Module>.Sessions.s<stamp>`, through the REPL's capture, the one path that
knows which values have a literal form) and `dark apps view ... --resume <val>` starts
from it. A tree taller than the pane is windowed: PageUp/PageDown move the window and
drop the focus, Tab pulls the focused node into view. `Live.call` (over `Builtin.applicableTryApply`)
is how an RTE from user code becomes a value, and it runs the fn as the approval root
of its call, as `serve` does the router: a fresh version of a view is not asked to be
approved again before it may print.

The CLI's own loop (`cli/loop.dark`) waits through the same `await`, and hands a
`StoreChanged` to the current page (`SubApp.onStoreChanged`, `Component.onStoreChanged`).
The workbench re-reads its item list and the SCM picture and says what moved in the
footer, so an edit from the LSP, an agent or a pull shows up in the detail pane while
you look at it.

Tests: `tests/CliWorkspace/live` drives the loop one turn at a time as a process on a
scheduler the test owns (`CliTestHarness.loopDriver`, `stepOn`): `pushKey` posts a key
to its queue as the reader thread would, `pushTick` posts a store change as the poll
would. The H2 guarantee the loop rests on (a parked process resumed after an edit
finishes on the old hash; a fresh one gets the new) is `tests/scheduler`'s.

The workbench's `Processes` pane (`P` from Apps) is the process table (`Stdlib.Exec.list`)
as a tree, with the selected process's stack (`Stdlib.Exec.inspect`) beside it: live's
first consumer of the scheduler's data. Every workbench, `dark apps view` and daemon is
a process; a slow `render` budget-yields, and keys typed during it are read after it,
not lost: the runtime reads them as one chunk (its paste path), and the host loop hands
`update` one key event per character, each carrying its own `char` and the chunk's key
and modifiers, unless an `Input` has focus, which wants the chunk whole.

A save that puts a name back on a version it held before lands as a decision (a second
`SetName` would be the op that already exists); `Live.touchedBy` counts it, the CLI says
"Put ... back on an earlier version", and the dependents follow.

## Daemons

A daemon that follows edits is a step, `step : 's -> 's`, run by `Cli.Apps.Runner`:
once per interval, waiting through `Host.await [StoreChanged; Timer]` in between, so an
edit that reaches the step is resolved through `LastGood` before the next tick and a
broken version is skipped with the reason in the log. The heartbeat example is on it
(`apps.heartbeat.step` points it at a step of your own). A daemon written as one
long-running fn cannot follow anything, scheduler or not: its frames call callees by
hash, and only a name lookup (`Live`, `applicableByName`) sees a new binding, so the
budget yield changes nothing for it. `dark apps` says `behind` when its entrypoint's
hash moved since it started, and `dark apps restart <slug>` is the answer. (The rebase
plan expected `behind` and `restart` to go; they stay, for this reason.)

## Prod follows a branch (demo 2)

Host side: `dark --branch <b> serve <router>` and a pull loop (`dark apps enable sync`
with `sync.branches all`, interval `apps.sync.intervalMs`). Local side: `dark config set
live.autopush on`; every `fn`/`type`/`val`/`module` save then commits the draft under
`auto: <n> ops, <first name>` and pushes (main with `push`, a branch with `branch push`).
Type errors are committed on purpose: the host keeps its last good version and says why
in its log, which is the same answer you get locally.

`scripts/testing/_demo2-live.sh` walks it in the container on main: a relay, A with
autopush, B pulling every two seconds and serving. Measured: A's save is B's page three
seconds later; the broken save leaves B on the last good page with `live: Demo.Site.router:
still on the last good version. The newest has a type error: ...` in its serve log; the
fix follows. B pulls from a shell
loop rather than the auto-sync daemon: in this container the daemon dies on its first
tick because its `eval` guest is refused the relay transport (`httpGetUnsafeBytes is
restricted to trusted first-party code`), a pre-existing gap outside this work.

## The demos

```
demo 1, TUI follows edits
  terminal A:  dark apps view stats
  terminal B:  edit Stats.render (workbench, LSP, or an agent), save
  A repaints within ~200 ms.
  terminal B:  save a version with a type error
  A keeps the last frame; a band under it shows the diagnostic.
  terminal B:  fix it
  A repaints, band gone.

demo 2, prod follows a branch
  fly host:    dark --branch stachu/site apps enable sync
               dark --branch stachu/site serve Site.router --port 8080
  local:       dark config set live.autopush on
               edit Site.page, save
  ~5 s later:  curl https://darklang-dev.fly.dev/  -> new page
  local:       save a broken Site.page
               curl -> still the last good page; the host log has the diagnostic
```

Demo 2's local half works today: `dark serve Tests.LiveProbe.router --port 9095` in
one terminal, `dark fn Tests.LiveProbe.page '(): String = "v2"'` in another, and the
next `curl` has it; `dark fn Tests.LiveProbe.page '(): String = 3'` leaves the page on
v2 and prints the diagnostic in the serve terminal.
