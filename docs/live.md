# Live programming: running things follow your edits

A process that stays up (a TUI, `dark serve`, a daemon) keeps running the code you
had when it started, unless something tells it the store changed and it resolves its
entry names again. This is that something, and the rules a host follows once it has
heard.

Status, 2026-09-19: the signal, the "what changed" query, the affects walk, last-good
and a `serve` that follows edits are in. The TUI host loop, the `Node` tree and the
renderers are next; the demo transcripts at the end say what "done" is.

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
moves. A newly broken version is said once on stdout (`live: <entry>: newest version
not applied: <why>`); the wire keeps getting the last good one. `--no-live` pins the
version resolved at start.

Test: `tests/CliWorkspace/live/serve follows edits and keeps the last good version`.

## The host loop (next)

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

Under the shim, `Host.await` is a Dark loop over a key read with a timeout and
`Live.poll`; it holds the OS thread while waiting. The loop does not know that, and
nothing outside `stdlib/host.dark` mentions timers, threads or a queue, so replacing the
shim is a deletion.

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
