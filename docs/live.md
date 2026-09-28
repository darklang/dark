# Live programming: running things follow your edits

A process that stays up (a TUI, `dark serve`, a daemon) keeps running the code you
had when it started, unless something tells it the store changed and it resolves its
entry names again. This is that something, and the rules a host follows once it has
heard.

`dark docs live` is the short, user-facing version of this document: a live
view (`apps view <Module> --live`), a `serve --live`, a host on another machine
following a branch, live values in the editor. This is the design and the
rules a host follows.

---

## The idea in one paragraph

Reload is name resolution, not code replacement. An edit never overwrites a version:
it binds a name to a new hash, and the old hash stays in the store. So a host goes
live by dropping its name caches, resolving its entry name again and calling whatever
hash that is now. In-flight work keeps the hashes it started with. A fn reference held
as a value (a closure, a router handed to a server at start) keeps its hash until
someone resolves the name again, which is why `serve` holds the router's LOCATION and
resolves per request, and why a daemon written as one long-running fn needs a restart
(Daemons, below).

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
moves. A newly broken version is said once on stdout (`[live] <entry>: still on the
last good version; the newest has a type error: <why>`), and the fix too (`[live] now
on the new version of <entry>`), beside the `[HttpServer] ...` request lines; the
wire keeps getting the last good one. Without `--live` the version resolved at start pins the
version resolved at start. What a request answers when its handler is slow, raises or
is stopped (504, 500, 503, and `http.requestTimeoutMs`) is in `docs/processes.md`, "An
HTTP request is a process".

Under the scheduler `serve` is a process that holds its thread on the listener, and
each request is a process of its own (`Scheduler.SpawnApply` from the thread that took
the request, with the server's process as its parent, so `dark ps` inside a handler shows
the request under the server). The per-request `Router.step` still polls for itself
rather than reading the scheduler's queue: the store-change source posts to processes
that are parked on `Host.await`, and a request process is never parked on it, so there
is nothing for it to read. Making the poll an event would mean a builtin that hands a
process the scheduler's latest store generation; the poll is one `PRAGMA data_version`
on a held connection, so that trade is not worth its builtin yet.

`--live` also brings the browser half: `GET /__live` is an event stream that holds the
connection, compares the router's hash every half second to the one the page was served
from (the page's own listener says which, `/__live?from=<hash>`, so an edit that lands
between the response and the connect is still reported), says `reload` once it moved (`[live] page told to reload` in the log; the wait
itself is not logged as a request), and ends; every HTML response carries a short
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

Every TUI host is written against one contract, the scheduler's event queue behind
`Stdlib.Host.await`:

```
let rec loop model =
  match Host.await [Key; StoreChanged] with
  | StoreChanged c when Live.affects c.locations entry ->
      let model = reload entry model      // last-good rule applies
      loop (render model)
  | Key k -> loop (render (update model k))
  | _ -> loop model
```

`Stdlib.Host.await [Key; StoreChanged; Timer ms; ExecDone id]` returns the first that
fired: `Key of KeyRead` (the runtime's read: a key, a paste, a burst with its repeat
count), `StoreChanged of Live.Change`, `Timer`, `ExecDone of id` (a process finished; ask
`Exec.await` how). It is `Builtin.hostAwait` (`docs/processes.md`: under the scheduler
the calling process parks on the event queue, fed by a reader thread, timers and a
store poll over `LibDB.Sqlite.DataVersion`; outside a scheduler the thread is held,
polling) plus one thing the runtime cannot do: say which ops landed. The poll posts that
the store MOVED; `await` describes it with `Live.poll` from a watch the runtime keeps in
one slot (`Builtin.hostWatchGet/Set`, since `await` takes no state and a Dark value does
not outlive the call that made it), and a move that carried no op (a config write) is
absorbed and the wait goes on. `Host.begin ()` starts the watch before a loop resolves
its entries, so an edit during startup is not missed.

A view of something that moves on its own (`ps --watch`) sets `View.every = Some ms`;
the host then adds `Timer ms` to every wait and hands the view an `Event.Tick`, which its
`update` must match. A view with `every = None` never sees a tick.

The loop itself is `cli/apps/host.dark`. A `View` is three fns by name (`init : Unit ->
'model`, `update : 'model -> Host.Event<'msg> -> 'model`, `render : 'model -> Node`), on an
`App` as `Target.Views`, or any module with those three (`dark apps view My.Module --live`).
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
would. The guarantee the loop rests on (a parked process resumed after an edit finishes
on the hash it started with; a fresh one gets the new) is `tests/scheduler`'s
`editDoesNotReachAParkedProcess`.

The workbench's `Processes` pane (`P` from Apps) is the process table (`Stdlib.Exec.list`)
as a tree, with the selected process's stack (`Stdlib.Exec.inspect`) beside it: live's
first consumer of the scheduler's data. Every workbench, `dark apps view` and daemon is
a process; a slow `render` budget-yields, and keys typed during it are read after it,
not lost: the runtime reads them as one chunk (its paste path), and the host loop hands
`update` one key event per character, each carrying its own `char` and the chunk's key
and modifiers, unless an `Input` has focus, which wants the chunk whole.

A save that puts a name back on a version it held before lands as a decision (a second
`SetName` would be the op that already exists); `Live.touchedBy` counts it, the CLI says
"Put ... back to an earlier version", and the dependents follow.

## Daemons

A daemon that follows edits is a step, `step : 's -> 's`, run by `Cli.Apps.Runner`:
once per interval, waiting through `Host.await [StoreChanged; Timer]` in between, so an
edit that reaches the step is resolved through `LastGood` before the next tick and a
broken version is skipped with the reason in the log. The heartbeat example is on it
(`apps.heartbeat.step` points it at a step of your own). A daemon written as one
long-running fn cannot follow anything, scheduler or not: its frames call callees by
hash, and only a name lookup (`Live`, `applicableByName`) sees a new binding, so the
budget yield changes nothing for it. `dark apps` says `behind` when its entrypoint's
hash moved since it started, and `dark apps restart <slug>` is the answer.

A daemon's pidfile and log go under `~/.darklang/run`; when that cannot be written (a
`~/.darklang` some other user created first, no home), `Stdlib.Cli.Daemon.runDir` falls
back to the instance's rundir, then `/tmp/darklang-run`, probing with a real write so the
launcher and the daemon agree on the answer.

## Prod follows a branch (demo 2)

Host side: `dark --branch <b> serve <router>` and a pull loop (`dark apps enable sync`
with `sync.branches all`, interval `apps.sync.intervalMs`). Local side: `dark config set
live.autopush on`; every `fn`/`type`/`val`/`module` save then commits the draft under
`auto: <n> ops, <first name>` and pushes (main with `push`, a branch with `branch push`).
Type errors are committed on purpose: the host keeps its last good version and says why
in its log, which is the same answer you get locally.

`scripts/testing/_demo2-live.sh` walks it in the container: a relay, A with autopush, B on
the auto-sync daemon (`apps start sync`, every two seconds) and `serve`. Measured: A's save
is B's page two to five seconds later; the broken save leaves B on the last good page with
`[live] Demo.Site.router: still on the last good version; the newest has a type error:
...` in its serve log; the fix follows. `--branch` walks the branch variant: A authors on
a branch `site` with autopush (each save is a `branch push`), B has `sync.branches all`
and serves `--branch site`; same three beats, same timings.

Three facts on the host side make that walk work. A daemon is `dark apps daemon-main
<slug>`, a command in the CLI's own state, so it has the host's capabilities (an `eval` is
a guest and could not use its own sync transport); the entrypoint is applied in that
frame, not through `Live.apply`, which would make it a guest root again. A branch's own
ops are inert (`applied = 0`) and tagged onto the branch in the transaction that stores
them, and `landedWhere`/`idsLandedSince` count a tagged op as landed once it is visible,
so a branch watch sees the branch's saves. An op that lands from outside (the daemon's
pull) reaches a long-lived branch process through the same invalidation a poll does
(`LibDB.PackageManager`, the second `Caching.register`). Tests: `tests/CliWorkspace/live`,
"a poll on a branch reports the branch's own saves" and "serve --branch follows edits made
on the branch".

## The agent channel

An agent authors through the same `addAuthored` a person's save goes through, on a
branch you watch, so nothing in the loop above is agent-specific: the store-change event
fires, `affects` picks the views it touched, a broken intermediate state (the agent's
normal case) keeps the last good frame, `dark diff` on the branch is the review. Two
things are for the agent's side, both in `Stdlib.Live` and neither needs the agent
harness.

`Live.show view` points every host loop on this instance at a view (a module with
`init`, `update` and `render`, what `dark apps view <Module.Path>` takes): it writes
`live.show` in the store's config (`config_v0`, never synced), which moves the store
counter, so an open `dark apps view` wakes and switches on its next turn, and a bare
`dark apps view` opens on it. The watch carries what `live.show` said at its last poll,
so a poll that finds the counter moved and nothing landed can tell a show from a trace
write and report only the show, as a change that touched nothing. A setting rather than
an event on the queue, on purpose: an event reaches the loops that are running now, a
setting also reaches the one you open next, and there is no session to scope it to until
sessions persist.

`Live.observe branchId view` is the view without a terminal: `{ report; render; rte }`.
Each of `init` and `render` is taken at its newest version that passes its at-rest
checks (the store's history is the memory, `versionsNewestFirst`; nothing is kept
between calls), `init` runs for the model, `render` for the tree, and the tree goes
through `Stdlib.Cli.UI.Text.render`, the third renderer beside the terminal and HTML:
plain lines, a table as its rows, `- ` before list items, `[ go ]` for a button, `!
boom` for a band that is not merely informative. `report` is the newest version's
at-rest report when it failed and an older version is what rendered; `rte` is the
runtime error when the newest passing version raised, with the version before it as the
picture when one renders. So after each edit the agent reads the same two things you
would see: the frame and what is wrong with the newest code.

An agent harness gets two tools, `observe(view)` and `show(view)`, and calls `observe`
after every save; the trace of that call is a recorded execution, so `Live.Values.replay`
adds per-expression values to the frame. Not wired: the harness is not in this branch.
Test: `tests/CliWorkspace/live/observe renders a view headless and show points a host at
it`.

## Live values

Beside each call in a function, the value it produced the last time the function ran.
Not from the recording: from running the current code again on the recorded inputs. So an
edit to a callee shows up in the caller's values without anyone calling it again, and a
value beside a call is always the value of the code you are looking at.

`Stdlib.Live.Values.replay branchId location` is the whole of it. It finds the newest recorded
run that went through the function (`trace_fns`, the names-only index), replays THAT WHOLE RUN
with every impure call answered from its own log and none performed, and returns `Values`:
`byExpr`, the value of every call keyed by the id of the `EApply` that made it -- calls inside
callees too, under their own ids -- and `problem`, the reason the replay stopped early if it did,
with the values up to that point still in `byExpr`. `None` means no recorded run went through
the function, which is not an error: there is nothing to show yet.

Two consequences worth stating, because they are the point. Because the whole run is replayed
rather than one function called, **nothing is performed**: a handler that charges a card does not
charge it again while you look at it. And because the pure code is recomputed rather than read
back, **the values follow an edit**: change a callee, look again, and the numbers move.

The runtime side is one instruction: `PT2RT` emits `TraceExpr(exprId, reg)` after every
call, and the interpreter hands the register's value to `tracing.storeExprResult`, which
is a no-op everywhere except under a preview's tracer (and is skipped outright when
`skipTracing` is set, so the normal path pays a branch and nothing else). The RECORDER never
sets it: a recorded run stores no per-expression values, because the replay recomputes them.

Where they show:

- The printer. `PrettyPrinter.ProgramTypes.Context.liveValues` (expr id -> rendered
  text) makes `packageFn` write `// = value` after a call that ends a line: a `let`'s
  right-hand side, a statement, a match arm's body, the body's last expression. Not
  inside an argument or an interpolated string, where a comment would break the code.
  The annotation is zero columns wide for layout (a `Styled` with an empty middle), so
  the code breaks exactly as it does without the values.
- The workbench. The Matter view's detail pane refreshes the values whenever the
  selected function changes (`refreshLiveValues`) and prints with them. `dark traces show
  <fn> --watch` is the same thing as a panel of its own, with the runs to pick from and
  up/down to move between them (`cli/traceWatch.dark`).
- The LSP. `textDocument/inlayHint` answers one hint per annotated line, placed at the
  end of the document's line with the same text (`LspServer.InlayHints`). A document
  edited away from the printed form gets fewer hints, never a wrong one. After a
  `fileSystem/write` lands ops, the server sends `workspace/inlayHint/refresh`.

All four go through `Live.Values.replay`, so none of them can drift into showing something the
others do not, and any recorded run will do: there is no second setting to turn on first.

The cost to know about: a hint request replays once per function in the document that has a
recorded run, and there is no cache shared between them. A big file with many recorded functions
pays many replays. Each performs nothing, so it is CPU and not risk.

## The demos

```
demo 1, TUI follows edits
  terminal A:  dark apps view stats
  terminal B:  edit Stats.render (workbench, LSP, or an agent), save
  A repaints before the save has finished printing (about 460 ms end to end,
  of which the live half is 8 ms; the rest is the authoring command).
  terminal B:  save a version with a type error
  A keeps the last frame; a band under it shows the diagnostic.
  terminal B:  fix it
  A repaints, band gone.

demo 2, prod follows a branch
  host:        dark --branch <b> apps enable sync
               dark --branch <b> serve Site.router --port 8080
  local:       dark config set live.autopush on
               edit Site.page, save
  ~2 s later:  curl http://<host>:8080/  -> new page
  local:       save a broken Site.page
               curl -> still the last good page; the host log has the diagnostic
```

The local half of demo 2, by hand: `dark fn Demo.page '(): String = "v1"'`, `dark fn
Demo.router '(req: Stdlib.Http.Request): Stdlib.Http.Response =
Stdlib.Http.responseWithText (Demo.page ()) 200'`, `dark serve Demo.router --port 9095`;
then `dark fn Demo.page '(): String = "v2"'` in another terminal and the next `curl` has
it; `dark fn Demo.page '(): String = 3'` leaves the page on v2 and prints the diagnostic
in the serve terminal.
