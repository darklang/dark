# Processes and the scheduler

A running computation is a value the runtime can step, park, resume and
inspect; one thread runs many of them, and a group of worker threads (one per
core) runs many more. Reads run concurrently on their own and writes keep
their order; `Exec.spawn`/`await` run chosen work in the background. A run is
a TRACE: one row, kept with the log of what it did to the world, suspended by
Ctrl-C, resumed or forked by replaying that log, and read back beside the code
that made it. A lambda a builtin applies is a frame on the process's own
stack; a builtin that needs the host names the operation and the loop performs
it. `dark docs processes` is the short, user-facing version of this document.

In one paragraph: a process is a `VMState` plus the `ExecutionState` it runs
under plus a status. A scheduler steps a process until it finishes, has to
wait for something, or spends its instruction budget. A waiting process is
parked on the task it waits for; when that completes, an event lands on the
scheduler's queue and the scheduler thread resumes the process. A preempted
process goes to the back of the line. Keys, timers and store changes arrive
on the same queue, so `readKey` parks instead of holding the thread. A
scheduler is one thread; a process spawned on a worker scheduler runs on
another core for its whole life, sharing nothing with its neighbours but the
state's concurrent caches.

---

## The three shapes that changed

Everything below rests on three additions to `LibExecution/RuntimeTypes.fs`, and they are worth
reading first because every later section assumes them.

**A VM carries a budget and what it is waiting on.**

```fsharp
 type VMState =
   { ...
+    /// Instructions left in this slice. -1 when nothing is scheduling this VM,
+    /// and -1 never reaches zero, so an unscheduled run is never preempted.
+    mutable budget : int64
+    /// Reads handed out as promises and not settled yet.
+    mutable pendingReads : ResizeArray<Promise>
+    /// The host operation this VM is waiting on, so `ps` can name the wait.
+    mutable hostInflight : Operation option }
```

**A value can be a read that has not landed.**

```fsharp
 type Dval =
   | DInt64 of int64
   ...
+  | DPromise of Promise

+and Promise =
+  { /// The read itself. Settling waits on this.
+    Task : Task<Dval>
+    /// The frame that made it, and the builtin that made it: what a failure is
+    /// reported against, at the point someone looks at the value.
+    frame : CallFrame
+    fn : FQFnName }
```

A `DPromise` only ever sits at the top of a register, as a frame's result, or as a builtin's
return value, so no builtin body is ever handed one. Forcing one is `forceOperand`, and every
opcode that needs a real value goes through it.

**The tracer answers a third question.** It already said "record this call" and "what is this
call's ordinal"; it now also answers "what should this effectful call DO":

```fsharp
 type Tracing =
   { ...
+    /// A resume: the log's answer for this call, by (process, ordinal).
+    replayEffect : int64 -> ReplayStep
+    /// A preview: the log's answer by (name, arguments), or `ValueNone` when the log
+    /// cannot answer, in which case the preview stops rather than performing anything.
+    previewEffect : Option<string -> Dval[] -> ReplayStep voption>
+    /// Which package functions a run went through, names only, for `traces calls`.
+    noteFunction : Hash -> unit }

+and ReplayStep =
+  /// Hand back the recorded value.
+  | Serve of Dval
+  /// Perform this one for real and go on replaying: a spawn, whose recorded
+  /// result names a process that no longer exists, or an environment read,
+  /// whose result was never stored.
+  | PerformOnce
+  /// The log has run out; this call and everything after it is live.
+  | PerformOnwards
```

`previewEffect` is `None` on an ordinary run, so the cost on the hot path is one null test and
no allocation.

---

## What a process is

`LibExecution/Scheduler.fs`:

- `Process`: `vm`, `exeState`, `entry` (the function it was spawned on, or
  `EntryExpr`), `parent`, `started`, `status`, `instructionsTaken` (what its
  slices have spent of the budget, the `steps` column in `ps`), `slices` (how many times the
  budget was refilled), the completion F# callers await, and the park state.
- `Status`: `Runnable | Parked of Parked | Done of Dval | Failed of rte * stack`.
- `Parked`: what a parked process waits for, for `ps`. `OnHost op` (a host
  operation the loop is performing: a file read, an HTTP request, a process
  run), `OnBuiltin name` (a `sleep`, the script an `eval` runs),
  `OnPackageFn hash`, `OnLambda`, `OnRareOpcode` (the interpreter waiting on
  the store), `OnEvent specs` (a `Host.await`), `OnProcess id` (an
  `Exec.await`).

`VMState` is self-contained (its own frames, frame pool, caches); `budget` is
the one field the scheduler reads and writes.

## The step

`Interpreter.executeSync exeState vm : StepOutcome` is the scheduler's whole
view of the interpreter:

- `StepDone dv`: the root frame returned.
- `StepBudget`: the budget ran out with work left. Nothing is half done; the
  counter sits on the instruction that has not run.
- `StepAwait (wait, resume)`: something has to be waited for. `wait` is the
  builtin's or package call's task; `resume` writes its result into the frame's
  register (or, for a request made after the wait, pushes the callable's
  frame) and is run on the scheduler thread when the process's turn comes.
  For the rare opcodes and the deferred return-type check, `wait` is a task
  that advances the VM itself as it completes, and `resume` does nothing.

There is one loop. `executeSync` runs frames until something has to be waited
for and answers a `StepOutcome`; `awaitOf` turns the wait into `(wait,
resume)` at the bail site. `executeSync` is that loop; an unscheduled run
(`execute`: tests, the LSP, a host running a function itself) is
`driveToEnd`, fifteen lines that await each `wait` in place and step again.
Its budget is negative, so it never sees `StepBudget`. A wait is decided
once, where it happens, and the scheduler and the plain run differ only in
who waits.

## The budget

`runSyncInstructions` counts `vm.budget` down by one per instruction and stops
at zero; `runFrame` reports that as `FrameBudget`. The default quantum is
10,000 instructions (`Scheduler.defaultQuantum`), refilled before every slice.
A negative budget means unlimited, which is what every VM nobody schedules runs
with. (A callable a builtin applies is a frame of the same VM, so it is
preempted like anything else.) The per-instruction check is within noise on
`interp-arith`, `interp-list` and `eval-listheavy` (`docs/perf/history.md`).

## The one rule

Only a process's own scheduler thread steps it. Everything else (the reader
thread, timer callbacks, the store poll, Ply continuations, other schedulers)
only posts to the queue. `Step` checks the thread id and raises if it is ever
wrong.

The one exception is documented at `StepOutcome`: a rare opcode's deferred
completion writes the VM it belongs to, on whatever thread completes it. The
process is parked meanwhile and nothing looks at its VM until the completion
has posted, so it is exclusive, not shared.

## The event queue and its sources

`LibExecution/HostEvents.fs`. One `Queue` per scheduler; one console and one
store per OS process, so the reader thread and the store poll are
process-wide (`HostEvents.Shared`) and deliver to whichever queue asked:

- `Key of KeyRead`: from the stdin reader thread. It starts on the first
  `Key` subscription and reads one key per request, delivered to the queue
  that requested it (requests from several schedulers are served oldest
  first, one read in flight), so a run that never waits on a key never
  touches the console, and nothing eats keys meant for a `readLine` after a
  TUI has quit. Redirected stdin never starts it: `readKey` answers Escape at
  once, as it always did.
- `Timer id`: a one-shot `System.Threading.Timer` armed per `Timer ms` spec,
  disposed when something else satisfies the subscription; a late fire posts
  an id nobody wants and is dropped.
- `StoreChanged`: a poll of `PRAGMA data_version` every `Scheduler.storePollMs`
  (200 ms) on a connection of its own (the pragma answers per connection),
  posted to every queue watching. It carries nothing: which ops landed is
  Dark's question, answered by `Stdlib.Live.poll` inside `Stdlib.Host.await`
  before the loop sees the event. Latched per process: a process that
  subscribes after a change it has not been told about is woken at once, so
  a change during a render is not lost.
- `Completed pid`: internal; the parked task finished.
- `ExecDone pid`: a process finished, well or badly, for Dark subscribers
  (`Exec.await` says how). Posted to the schedulers with a subscriber for it,
  which may not be the one that ran it; a subscription to a process already
  over, or forgotten, is answered at once.
- `Wake`: nothing to route; the loop, blocked with nothing runnable, looks
  again (a spawn from another thread, a `Stop`).

The sources `LibExecution` cannot provide itself (the console, the store) are
installed by the host: `Stdin.fs` installs the key source, `Cli.fs` the store
version.

## `Host.await`, the contract

```
module Darklang.Stdlib.Host
type EventSpec = Key | StoreChanged | Timer of ms: Int64 | ExecDone of id: Uuid
type Event = Key of KeyRead | StoreChanged of Stdlib.Live.Change | Timer | ExecDone of id: Uuid
let await (specs: List<EventSpec>) : Event
```

Under the scheduler the calling process parks on the first spec to fire.
Outside one (a plain `execute`) it blocks the thread, polling. A
`StoreChanged` from the runtime carries nothing; `await` asks
`Stdlib.Live.poll` what landed and hands the loop the `Change`, and a move
that carried no op (a config write) is absorbed. `Builtin.hostAwait` declares
`{Stdin; PackageRead}` statically, the union of what any spec could need.

`readKey ()` is unchanged for users; under the scheduler its body is "park on
`[Key]`, return the key".

## Cores: workers

A `Scheduler` is one loop on one thread. `Scheduler.Workers` is a group: the
root plus N more schedulers, each looping on a background thread of its own
(`dark-worker-<i>`), started the first time anything asks for them. N is
`exec.workers` in the store's config (`dark config set exec.workers 4`), or
one per core, which is the default nobody needs to set; `Cli.fs` reads it
into `Scheduler.defaultWorkers` before the root starts. No environment
variable shadows it.

- `root.SpawnOn(...)` spawns on the least loaded worker (fewest runnable or
  parked processes); the process runs there for its whole life. `Spawn`
  keeps it on the calling scheduler.
- `Await` is the process's completion task and works from anywhere. `ps` and
  `kill` from any scheduler in the group see and reach every process in it.
- The Http server's request handlers and `Exec.spawn` are what use the
  workers; the CLI's root and each `eval` expression stay on the root. A CLI
  run that never spawns on a worker never starts the threads.
- Four CPU-bound processes on four workers finish in about 0.6 of one
  thread's time, not 0.25: the interpreter allocates per value and the
  allocator's scaling is the ceiling, so the allocation work in
  `docs/perf/roadmap.md` is also the multi-core work (the numbers:
  `docs/perf/history.md`). The test bounds the ratio at 0.8.

## Entry points

- `Cli.fs` `main`: the entry function is the root process of a fresh scheduler
  that runs on the main thread until it finishes. (`DARK_SCHEDULER=off`, in
  `Cli.fs`, runs the function as a plain unscheduled `execute`; a bisect
  switch for whoever is asking whether an oddity is the scheduler's, not a
  setting.)
- `cliParseAndExecuteScript`: each expression is a child process of the CLI's,
  awaited in order. So a script budget-yields, a `readKey` in it parks, and
  `ps` lists it. Daemons are launched as `eval` and ride the same path.
- `execute` everywhere else, unchanged.

## What a process shares and what it owns

The audit of `ExecutionState`, field by field, for two processes on two
threads under one state. The short answer: the interpreter already ran on
many threads with one state (the Http server's handlers, the parallel test
suite), and the caches are concurrent for that, so a process copies almost
nothing.

Shared by reference, safe as is:

- `lambdaInstrCache`, `packageFnCallCache`: `ConcurrentDictionary`, keyed by
  a lambda's expression id and a package fn's content hash, holding immutable
  compiled data. Shared on purpose: a lambda created in one process is
  callable from another (an `eval` expression's lambda from an HTTP handler,
  say), which a copy-on-spawn would break. The one mutable inside,
  `PackageFnCallData.policy`, is a memo of an immutable record whose owner is
  compared by reference; two writers racing both write a correct answer.
- `Types.find`'s declaration cache: a `ConditionalWeakTable` of
  `ConcurrentDictionary`, per `Types` instance.
- `builtins`, `types`, `fns`, `values`, `blobs`, `program`, `access`,
  `packagePolicy`, `isBundledPackageFn`, `accountID`, `branchId`, the flags,
  `reportException`, `notify`: immutable, or functions over the package
  manager, whose own caches are content-keyed and concurrent.
- The SQLite connection: not on the state at all; every statement takes a
  pooled connection (`Pooling=true`), so there is nothing per thread to keep.

Shared, and written under a lock:

- `deniedRequests`, `permissionWarnings`: the host installs a list, runs the
  script, reads the list. A child's denial has to land in the parent's list,
  so they are shared and the two appends (`PermissionCheck.raiseDenial`,
  `recordPermissionViolation`) lock. Denials are rare; the lock is never hot.
- `test`: the test context's counters. Tests only; left as they are.

One process's own:

- `tracing`: the recorder keeps a call stack to pair frame entries with
  exits, and one stack cannot hold two processes' frames. At spawn the
  scheduler asks the parent's tracer for a per-process view
  (`Tracing.forProcess pid`, `Scheduler.stateForProcess`): the same event
  list, the process's own stack, and every event stamped with the process id
  and a `seq` across the whole trace. `noTracing` answers itself, so an
  untraced spawn copies nothing. See "Traces" below.
- `VMState`: never shared; that was already the rule.

So `stateForProcess` is one record copy when tracing is on and the parent's
state itself when it is off.

## Reads are concurrent

The user-facing rule, in one paragraph: a call whose effects are all reads
(a file, env, db, package or trace read; an HTTP GET or HEAD) that has to
wait does not stop your program. You get its result back at
once, as a read still in flight, and the program runs on; the first thing
that looks at the value waits for it. Every write (`print`, `File.write`, a
POST, a db write) runs when it is reached, in program order. So

```
let pages = List.map urls (fun u -> HttpClient.get u [])  // every GET in flight, at once
Stdlib.printLine "fetching"                              // a write: runs now
let first = List.head pages                              // looks at the list: waits for all
Cli.FileSystem.writeFile out first.body                  // in order
```

`Stdlib.await x` forces a read now rather than at its first use; it is the
identity function, since calling anything with the value is what forces it,
and it works on a list of reads as well as one. (`Exec.await h` is the one
for a handle; the two share the word and not the module, since one function
cannot be typed as both `'a -> 'a` and `Handle<'a> -> 'a`.)

How it works (`Interpreter.Promises`, `RuntimeTypes.Promise`):

- At the builtin call site, when the builtin's `Ply` is not finished and the
  call is deferrable, the register gets a `DPromise` (the task, the builtin's
  name and the frame's execution point) instead of the process parking. A
  call is deferrable when every call of the builtin is a read
  (`Effects.readsOnly`: its effects are all reads, or it is the HTTP
  client's GET and HEAD builtin, `httpClientRead`, which `Effects.fs` names
  since `http` stays one word in the permission language and a policy grants
  a URL, not a method; `HttpClient.get`, `head`, and `request "GET"` go
  through it). `Clock` and `Random` are not read effects either: reading
  them never waits, and `sleep`, the one clock call that does, is a wait the
  program means to take. A read that finishes synchronously (most file and db
  reads in this runtime) is never a promise; only a real wait is.
- A promise is only ever at the top level of a register, a frame's result, or
  a builtin's returned value. Every instruction that inspects, stores or
  passes a value forces it first: `Apply` forces the callee and every
  argument (so no builtin body ever sees one), record, enum, list, tuple, dict and string construction force
  their parts, a closure forces what it closes over, `if`, `||`, `&&`, match
  and let patterns force what they look at, and the end of a run forces its
  result. A bare `let x = ...` copies without looking, which is what keeps a
  read in flight across the statements after it. Returning a promise from a
  function is fine (a wrapper handing back its builtin's result); the return
  type check lets it through, since the builtin's own return type was checked
  when the value was made or is when it lands (`TypeChecker.tryUnifySync`,
  `Dval.toValueType` says `Unknown`).
- Forcing: the instruction does not run; `Promises.settle` replaces a landed
  promise with its value and the instruction runs again at once, or, for one
  still in flight, the frame stops with the counter on the instruction
  (`FrameAwaitForce`) and the process parks on the task, exactly as it parks
  on a builtin. Under a plain `execute` the task loop awaits it.
- A read that failed raises at the force point, with the read's own error and
  the frames it was called from added below the stack (`vm.nestedCallStack`),
  so the report names both sites. A denial is raised at the call, before
  anything is in flight: the ambient effect check runs before the body. A
  read nothing ever looks at has no force point, so the end of the run is
  its force point: a run does not end until every read it made has landed
  (`VMState.pendingReads`, checked at the root frame's return), and the
  first that failed fails the run there, naming the read. The rule a JS
  unhandled rejection follows: `let _ = HttpClient.get bad []` and code that
  never reads it is a failed run, not a silent one. Reads that finish on the
  calling thread raise at the call; only a real wait is deferred.
- `List.map` is promise-aware (`mappedListOrPromise` in `Builtins.Pure`'s
  `List.fs`): a lambda that returns a read in flight hands it back rather
  than being forced at the end of its run, and the map's result is one
  promise for the whole list, landing when every element has. Everything else
  that applies a lambda gets the lambda's result forced. So `List.map urls
  (fun u -> HttpClient.get u [])` is where the reads fan out; the same lambda
  under `List.filter` would run them one by one.
- The bound: at most `Promises.maxInflight` reads in flight per OS process,
  256, fixed rather than a setting (past it the program is saturating
  whatever it reads from). Past it a read is awaited in program order, so a
  map over a hundred thousand urls does not open a hundred thousand sockets.
- Tracing: the builtin's result is recorded when it lands (the recording is
  inside the builtin's own `Ply`); the trace's `seq` is completion order. A
  builtin that combined reads (`List.map`) records its value when the
  combination lands. Under tracing an awaiting builtin's arguments are copied
  before the wait, since the frame's argument buffer is reused once the frame
  runs on.
- Cost when nothing is in flight: one type test per operand on the
  instructions above. The check lives in `tryUnifySync`, not in
  `finishBuiltin`: a `match` on the result there costs 200 bytes per builtin
  call (the `uply` arm's closure is built on every call).

Measured: three reads under `List.map` are all in flight before anything
waits, and the statement after the map runs while they are; two reads in
program order with a write between them: the write runs before either lands;
a failed read raises at `await` with "after the call" already run; the bound
holds (`Scheduler.Tests.fs`, the reads group).

## `Exec.spawn`, `await`, `awaitWithin`, `select`, `cancel`

`Exec.spawn f` starts `f ()` as a process of its own on a worker (the least
loaded), under the access the caller had at the spawn, like a closure, and
hands back a `Handle<'a>`; `Exec.await h` is the value it finished with, or
its error raised again with the child's frames kept below the caller's;
`Exec.awaitWithin ms h` is `None` after `ms` milliseconds and leaves the
process running; `Exec.select hs` is the first to finish with its value.
`Exec.cancel h` asks it to stop (cancel and kill, and what reaches children:
the "`dark ps`" section). `spawn` carries the
`Concurrency` effect, ambient and allowed by the default instance policy: a
spawned process can do nothing the spawner could not. An install whose policy
was seeded before this effect existed needs `dark permissions allow
concurrency` once. `List.parallelMap` is `spawn` per element then `await` in
order, for work that computes; reads run concurrently under plain `List.map`
already.

From a run nobody scheduled (a test's `execute`, the LSP, an HTTP handler)
`spawn` uses a process-wide scheduler with workers of its own
(`Scheduler.CurrentOrShared`), started on first use, and `await` blocks that
thread on the completion as any builtin wait would.

## No host re-entry: a builtin asks, the interpreter applies

A builtin that takes a callable does not run it: it asks
(`Interpreter.requestApply`) and the interpreter pushes the callable's frame
on the process's own stack, so `ps` sees it, the budget preempts it, and a
read in it does not hold the .NET stack. (`Execution.executeApplicable`, a
nested VM on the host stack, remains for the callers in the Edges section.)

- The builtin's body calls `requestApply vm applicable arg moreArgs next` and
  returns what it returns (a placeholder). The interpreter, at the call site,
  sees the request (`VMState.pendingNext` and the three slots beside it: no
  record, so a chain of a thousand applications allocates nothing for them),
  pushes the callable's frame in the same VM from the calling frame, with
  `next` on it (`CallFrame.continuation`), and stops the drain as it would for
  any pushed frame. A builtin or package function passed as the callable is
  called through the ordinary paths and its result driven straight on; a
  partial application answers the applied lambda, as `Apply` does.
- When that frame returns, its result does not go into the caller's register:
  `returnFromFrame` hands it to `next`, and `drive` looks at what `next`
  answered. A further request (the next element) pushes the next frame at
  once; a value ends the chain, into the register the `Apply` named, with the
  frame's counter moved past it, and `finish` records the builtin's result in
  the trace, since the builtin's own return was the placeholder; a wait (a
  `next` that awaits) parks the process on it (`FrameAwaitContinuation`) and
  drives on when it lands.
- `Interpreter.withValue` is for a continuation that has to look at the
  callable's result (a predicate, a key, a fold's accumulator): it waits for a
  read still in flight first, through the same wait. `List.map` and its kin
  carry the result along unlooked-at, so a read in a mapped lambda stays in
  flight and the list comes back as one promise.
- A request is usually the body's first move, and the call site sees it.
  A body that had to wait first (a stream pulling from the network, then
  applying its transform) may still ask: its wait lands where its result
  would have gone into the register, and that landing (`landBuiltin`, in
  all three loops) pushes the frame instead and picks up what records the
  chain's result from the VM (`pendingFinish`). Not for a read: its wait
  would have been handed back as a promise with the request inside it, so
  that raises `requestApply after the first await of a read`. A continuation
  may await and then request; that is `drive`'s ordinary path.
- The frame runs under the builtin's applying access narrowed by what the
  callable captured, exactly as `Apply` narrows a frame's. Errors inside the
  lambda propagate through the process's own frames, so the stack names the
  lambda without `nestedCallStack`.

The list builtins that take a callable: `List.map`, `indexedMap`,
`map2shortest`, `fold`, `filter`, `filterMap`, `findFirst`, `any`, `sortBy`
(`Builtins.Pure/Libs/List.fs`), each with one continuation over two mutable
cells rather than a closure per element. `Dict`, `Option`, `Result` and
`String` take no callable in F# (they are Dark).

Streams (`Stream.unfold`, `map`, `filter`; `Builtins.Pure/Libs/Stream.fs`):
a transform node holds its callable, not a closure over it (`StreamImpl.
Unfold/Mapped/Filtered`), and a pull is a step machine (`Stream.pull`):
`Pulled` an element, `Apply` this callable to this element and continue, or
`Wait` on native IO and continue. The pulling builtin (`next`, `toList`,
`toBlob`) drives it: an `Apply` is a `requestApply`, so the transform runs
as a frame of the pulling process, its answer forced (`withValue`) and
handed back to the pull; a `Wait` is waited for, and a request after it is
the landing case above. The builder's active access is folded into the
callable once, when the transform is built (`narrowedBy`), and the frame push
narrows the puller's access by it, as `Apply` narrows any frame's. So a
narrow producer's transform stays narrow under a wide consumer, and the
deferred-execution matrix in `PermissionsGate` still holds. F# code that
owns a native stream (the HTTP client's body, tests) pulls with
`Stream.readNext`, which drives `Wait` and raises on `Apply`: a stream that
runs Dark code is pulled from a Dark process.

Still through `Execution.executeApplicable`, on purpose: `Router.step` per
request in `HttpServer.fs` (a poll and at most one check, on the thread that
took the request, before the handler is spawned) and the `onListening`
callback; `LiveValues.fs`, which runs a function for inspection, not as part
of a program.

Tests (`Scheduler.Tests.fs`, the frames group): a process parked inside
`List.map f` or a stream transform shows the lambda's frame in `ps` and
resumes; a tight loop in a mapped lambda is preempted; an error in one names
the lambda's frame; a transform over a stream whose source waits on the host
before every element runs as a frame, scheduled and unscheduled. The cost is
within noise on the gate and the bench (`docs/perf/history.md`).

## Host operations are requests: a builtin names, the loop performs

A builtin that touches the OS names the operation (`Interpreter.requestHost
vm op next`) and the loop performs it; nothing is checked or awaited inside
the body (in `Builtins.Cli`: `File`,
`Directory`, `Environment`, `Execution`, `Posix`; in the HTTP client: the
guest request and stream open, and the sync transport's GET and POST):

- The body puts the `Host.Operation` and a continuation on the VM
  (`VMState.pendingHostOp`, `pendingHostNext`; no record, same as an apply
  request) and returns a placeholder. Right after the body returns,
  `invokeBuiltin` sees the request and performs it through the one checked
  boundary (`PermissionCheck.performHostWithAccess`, under the body's access),
  then hands the outcome to `next`; a continuation may name another operation,
  or ask for an apply, and is driven the same way (`performRequested`). The
  body itself is a value again: no builder, nothing awaited inside it.
- A synchronous operation (every file, directory, environment and libc call:
  microseconds, and a pool hop would cost more than the wait) completes on the
  spot and nothing parks. One that waits (an HTTP request; a process run or a
  round of process IO, which `Host.blocking` moves to the pool so a `sleep 10`
  in one process does not stall a scheduler's others) parks the process as any
  wait does, and the VM records the operation (`hostInflight`) so `ps` says
  `the host: process-run /bin/bash` rather than the builtin's name.
- A body that had to wait before it could name the operation (`File.write` of
  a persisted blob reads the bytes from the store first) names it from the
  continuation of that wait, and the landing performs it. Rare; the
  ephemeral-blob case, which is nearly every write, names it at once.
- Denials and rejections raise at the call: the check runs on the loop's
  thread, before anything is performed, under the same access the body ran
  with.

Two OS-facing calls still perform from inside the body, on purpose.
`httpGetUnsafeBytesStart` starts a sync GET and hands back a handle for
`httpAwaitBytes` to collect: the point is not to wait, so it has no
continuation to give the loop. The HTTP server's bind is performed under the
child guest state's access, not the calling frame's, and `serve` then runs
its listener in the same body; the bind is synchronous, so nothing parks
there anyway.

The host boundary (`Host.perform`: resolve, check, execute, audit) is called
from one line in the loop for every OS-facing builtin. An operation is a value
the host answers, which is what lets `ps` name the wait.

## An HTTP request is a process

`serve` spawns each request's handler as a process on a worker, with the
server's process as its parent: `ps` shows it under the server with its own
frames, the budget can preempt it, a read inside it is a value in flight, and
a slow handler never holds up another (a request that sleeps 800 ms sits
beside three that answer at once; the batch takes one slow request, not four).
The handler's outcome is the response:

- It returns a `Http.Response`: that is the response.
- It raises: 500, the body says `The handler failed: <the error>`.
- It runs past the request timeout: the server cancels it (politely, so
  what it has on the host completes and its children stop with it) and
  answers 504, `The handler ran for more than N ms and was cancelled`. The
  limit is the store's `http.requestTimeoutMs`, read once when `serve`
  starts; 30 s unset; 0 means no limit.
- It is stopped from outside (`dark ps cancel`/`kill` on the request's
  process): 503, `The request was stopped: <reason>`.
- The router has no usable version (a live `serve` whose newest router
  fails its checks and has no last good one): 503, `Service Unavailable`.

A finished leaf process (a request, a spawned read) skips the group-wide
scan for children: `childCounts` says whether it ever had any. That scan
was most of a request's cost as a process.

## Traces

`trace_fn_calls` rows carry `process_id`, `seq` and `ord`
(`migrations/schema/08-traces.sql`; existing stores get the columns from
`LibDB/Releases.fs`, with `''`, `0` and `-1` for old rows). `seq` is assigned
as calls complete, under the tracer's lock, across every process writing the
trace: all the rows in `seq` order are the interleaving. `ord` is an effectful
builtin call's ordinal among its process's effectful calls, taken when the
call is made (`Tracing.nextEffect`), so a read that lands late keeps its
place; one process's rows in `ord` order are its log, and what a replay keys
on. `Tracing.FnCall` in Dark carries `processId : Option<Uuid>` and `seq`. A
run nobody scheduled writes `''`.

Recording is on or off, and off is what a shipped binary does until somebody
asks (`TraceDetail.Off | On`). On, a run keeps its own row -- what it was, what
it was given, what it answered, how long it took -- plus every impure call in
order, builtins with a non-empty `callEffects`, each with its arguments, its
result, its ordinal and its duration. That is the classic rule for what an
effect is, and it is the smallest log a run can be resumed, forked or
previewed from, so there is no middle setting to pick: anything less than the
log is a row you can read and nothing you can do.

Three ways to ask for it, narrowest first, and each is the one that beats the
one after it:

- `dark --trace <command>`, or `--no-trace`, for one command.
- `trace.record` in the store's config, written by `dark traces record on` or
  `dark config set trace.record on`, for this instance until changed. The host
  reads it at startup (`TraceDetail.configure`).
- `DARK_CONFIG_TRACE_DETAIL=on` for a whole environment. Our dev containers
  and CI set it, so a clone records and a gate can assert on what a command
  did. A stored setting beats it, because the environment is a container-wide
  default and the stored one is a decision somebody made in this store.

**Pure calls are not stored, at any setting.** Recording every frame and
lambda as well costs 300x the bytes (0.59 MB against 0.002 MB for the same ten
thousand calls) and buys one thing: a call tree for profiling. A night of
ordinary work with that on left 15.8 GB in `trace_fn_calls`. Nothing a person
does with a recorded run needs it: the preview (`traces show`) re-runs the
pure code against the recorded impure answers, so a pure value is recomputed
rather than stored -- which is also why it follows an edit to a pure function,
and a stored value would not. Storing them for profiling is worth its own
feature, with its own switch and its own retention, rather than a third
setting here.

What that buys back, beyond the disk: the interpreter keeps its fast paths and
its per-frame bookkeeping stays off in a recorded run, because nothing about a
frame is recorded (`skipTracing` is always true for the recorder). A trace is
a SEQUENCE of impure calls, not a tree of frames, so `parent_call_id`,
`lambda_expr_id` and `kind` are written flat.

The log is thin enough to leave on because retention keeps the tables
bounded: after a store, the oldest traces past `trace.keep` (200 unset) or
`trace.maxMb` (256 unset) of logged args and results go, except one a running,
suspended or pinned run needs, and the newest run for each entry.

Two secrets are taken out of a row before it is written (`Tracing.Redact`),
and nothing else is. A request header named `authorization`, `cookie`,
`set-cookie`, `x-api-key` or `proxy-authorization` is stored as
`[redacted]`, whatever case it was written in: an argument can be redacted
freely, since a replay serves results, not arguments. An environment read's
secret IS its result, so the result is not stored at all, and a replay runs
that one call again for real instead of serving a value it does not have
(`ReplayStep.PerformOnce`, decided by the builtin's name in the row) and
goes on replaying everything else. So a resumed run reads the environment of
the machine resuming it, which is also the honest answer on another machine.

Everything else the effects were given and returned is in the log as it is: a
key file's bytes a run read, a response body, what a run printed. A secret you
do not want on disk is one to keep out of an effect, or run with `--no-trace`.
Redaction covers everything stored, because the impure calls are everything
stored.

## A trace is a run

One row (`LibDB.Traces`, the `traces` table): what was run (`eval`,
`run <file>`, `GET /path`), its input, its status (`running`, `done`,
`failed`, `suspended`), whether it is pinned, and, for a fork, the run and the
position it branched from. Its calls are `trace_fn_calls` under the same id.
`dark traces` lists them; `traces details|show|resume|fork|pin|rerun|delete`.
`Darklang.Tracing.Store` is the Dark side.

- Ctrl-C during a traced run: the CLI's handler cancels what the run spawned
  and gives it a quarter of a second to land (`Cli.fs`,
  `stopChildrenPolitely`), stores the log as it stands, marks the run
  suspended, prints the resume command and leaves
  (`installSuspendOnInterrupt`; `Traces.Foreground`). Cancelling first is
  what makes the log's end mean something: a child mid-write finishes the
  write and stops at its next turn, rather than being cut by process exit
  with half of it done and none of it logged. A TUI reading keys takes Ctrl-C
  as input and never gets here. A run the suspend took out of the foreground
  stores nothing more if it goes on (a test's does; the CLI's has exited).
- `resume`: `armResume` then the same input through the ordinary `eval` or
  `run` path; the script runner takes the armed resume in place of a fresh
  tracer (`Tracing.createReplayTracer`). Every effectful call whose
  `(process, ordinal)` the log has is answered from it, and not performed: a
  replayed `printLine` is echoed dimmed, so the person resuming sees where
  the run had got to without the world seeing it twice. Three kinds of call
  are not answered from the log:

  - A handle the old process owned and this one cannot have (an OS
    subprocess, an open HTTP stream) stops the resume at that step, naming
    it, and leaves the run as it was (its status and its log).
  - `Exec.spawn` (and `spawnDetached`, `cancel`, `kill`) is performed again:
    serving the old handle would name a process nothing answers to, so the
    resume really spawns, and the new child replays the recorded child's own
    rows through the matching below. A run that spawned resumes like any
    other, children included.
  - An environment read has no result in the log (it is the secret that is
    kept out of it), so it is read again from the environment of the machine
    resuming the run.

  Either of the last two performs the call and goes on replaying everything
  else (`ReplayStep.PerformOnce`; the set is `Tracing.Redact.performAgain`). A logged file read whose file has changed
  since the run was recorded warns and continues on what it read then. The
  first ordinal a process asks for that the log lacks ends that process's
  replay for good, so nothing later in the log can be handed to it after a
  live call; from there the run is live, still recording, and the stored
  trace ends up as the replayed prefix plus what ran after. The recorded
  process ids are the recorded run's; a resumed run's processes are matched
  to them in the order they first appear in the log, which is the order a
  script's expressions start in, and the order a parent spawns its children.
  A run nobody scheduled (a plain `execute`, as in the test harness) records
  and replays under `Guid.Empty`, which is seeded directly and is NOT in the
  list the matching pops from: leaving it in hands the first spawned child
  the root's rows, and the child's own log is never reached.
- `fork`: a new run with the same input, holding a copy of the parent's rows
  with `seq` below the position (the whole log with no `--at`), suspended;
  resume it and it diverges where the log ends. Cutting
  by `seq` can leave a process's later ordinals without earlier ones, which
  the rule above turns into "live from the first hole".
- Replay after a package edit: the input is re-parsed, so names resolve to
  the new code, and the effects come from the log: the new pure code runs
  against the old I/O. Live values (`docs/live.md`) are this, one call at a
  time.
- Three builtins declared with no effects read a host fact live and are not
  recorded, by design: `cliTerminalColorEnabled` (the terminal's colour
  support), `interpreterStatsEnableDetailedTiming` and `interpreterStatsGet`
  (dev instrumentation). A replay in another terminal renders for that
  terminal. Every other pure builtin answers the same twice on the same
  inputs; Dark's dict is an ordered map, so enumeration is deterministic.
  `uuidGenerate` declares `Random` and is in the log.

Tested in `CliRuns.Tests.fs`: record and resume, fork at a position, suspend
mid-way and resume, replay after an edit, a spawn resuming with its child's
own log, retention (the count cap sparing a suspended run, the byte cap
sparing the newest, the newest of each entry surviving the count cap), the
echo and the refusal.

## A run's id

A plain random UUID, and `dark traces` prints the shortest prefix that tells the listed runs
apart: eight characters, unless two of the listed ids collide there.

Random, with no structure in front, because an id is something a person TYPES -- `traces
resume`, `traces details`, `traces fork` all take one -- and a short prefix has to be unique.
Anything ordered in front (a timestamp, say) makes two runs from the same moment agree for a
dozen characters and every short id ambiguous. Nothing needs order out of the id: SQLite sorts
by a column, and every listing orders by `timestamp` or `rowid`.

Not content-addressed, unlike an op id or a commit id, and deliberately. An id has to exist
when the run STARTS, before there is a log to hash; and content-addressing pays when two
parties independently produce the same thing, which is true of an edit and false of a run --
two runs of the same input an hour apart are different events. Syncing runs needs the id to
travel with the row, which it does, and a namespace two instances cannot collide in, which a
random UUID gives.

## Preview: looking at a run

A resume takes a run forward. A PREVIEW looks at one, and the difference is the whole design:
a preview never performs an effect.

`dark traces show <fn> [<run>]` replays a recorded run with every effectful call answered
from that run's log, collects the value of every expression on the way, and prints the
function you asked about with `// = value` beside each call. `dark traces calls <fn>` is the
list of runs to choose from.

This is classic's Preview (`classic-dark/backend/src/LibExecution/Interpreter.fs`, the
`realOrPreview = Preview` arms), with two things taken from it deliberately:

- **An impure call is answered or nothing happens.** Classic returned `DIncomplete` for a call
  the trace could not answer, and let it propagate; we have no such value, so the preview stops
  there and the view says which call stopped it. Pure code runs for real in both, because it is
  cheap and deterministic.
- **The key is `(name, arguments)`, not a position.** A resume keys on `(process, ordinal)`,
  which keeps order and tells two identical calls apart. A view cannot: add a call in the
  middle and every ordinal after it shifts, so an ordinal-keyed view would go blank from there.
  Classic keyed on `(fnname, id, hash)` for the same reason, and a name-and-arguments key still
  answers every call you did not touch. Last write wins.

The pieces:

- `RT.Tracing.previewEffect`, consulted in `invokeBuiltin` before the ordinal replay. `None` on
  an ordinary run, so the cost there is one null test and no allocation.
- `Tracing.createPreviewTracer`: the lookup table, the value collector, and `forProcess`
  returning another preview tracer -- the CLI spawns each expression as its own process, and
  handing back the default there would hand back a tracer that performs effects.
- `cliPreviewRun`: one builtin that loads the log, replays, and hands back the values. No armed
  mode and no shared slot, so two previews at once cannot take each other's log.
- Two ways in, because a run has two shapes. An `eval` or a `run <file>` replays its source. A
  served request's input is a record, so the row carries `entry_hash`, the handler that served
  it, and the preview applies that handler to the recorded request.
- `trace_fns`: which functions a run went through, names only, written while recording. Without it, "which runs went through this sub-router" is
  unanswerable, because a package call is never recorded.

What a preview does not do: it does not write, it is not a run, and it does not echo a logged
print (that echo belongs to a resume, where somebody is taking the run forward).

**Everything that shows a value beside code comes through here.** `dark traces show`, the
workbench's gutter and the LSP's inlay hints all call `Live.Values.replay`, which is the
preview with the newest run that went through the function. There is no second mechanism; the
one that used to re-run a single function on its recorded arguments, and perform its effects
for real, is deleted (`docs/live.md`, "Live values").

## A run on another machine

Not built, and deliberately not. A half-answer to "move a run" is worse than
none: a text bundle of the rows carries no code, no blobs and no argv, so it
moves the runs whose log happens to be self-contained and quietly goes live
early for the rest. The real version is part of synchronising runs (a route on
the relay, or the sync transport carrying rows), and it should arrive with the
rest of that design rather than ahead of it.
## `dark ps`

`Stdlib.Exec.list/inspect/cancel` over `Builtin.execList/execInspect/execCancel`
(`Builtins.Language/Libs/Exec.fs`), and `Cli.Ps.killById`, the one wrapper of
`execKill` (the escape hatch is the CLI's, not the stdlib's); rendered by
`cli/ps.dark` as a tree, a process
under the one that spawned it. Rows are copies;
nothing hands Dark a reference into a running VM. A process on a worker is
snapshotted from another thread: its call stack is read best-effort (a frame
popped under the read comes back as no frames, never a fault). One group per
OS process, so `dark ps` from a shell is the CLI alone; from inside an `eval`
it is the CLI parked on `cliEvaluateExpression` plus the expression's process,
with `ps show` giving both call stacks.

Two ways to stop a process, one asymmetry: `cancel` (`Exec.cancel`, `ps
cancel`) lets what the process is doing on the host complete before it stops
at its next turn, while `kill` (`ps kill`) gives a parked process that turn
at once and abandons what it waited for, which is the escape hatch for a
process stuck in a call that never returns. Both set a reason the process
fails with ("cancelled", "killed from ps"); a wait on events
(`Host.await`, `readKey`) is cut by either, since nothing is in flight
there; a running process finishes its slice first, so a short program that
stops itself completes. Both reach the process's undetached children, now
and again when it finishes (`Scheduler.Stop`, `StopChildrenOf`), as hard or
as politely as the parent's stop was; a child spawned with
`Exec.spawnDetached` is left alone. Tested: cancel lets a gate land, then
stops; children die with the parent unless detached; `awaitWithin` times out
and the child is cancelled after; a kill of a parent reaches a child stuck on
the host at once (`Scheduler.Tests.fs`, the cancellation group).

## Every Dark process on the machine

`dark ps` from any shell prints this instance's own processes first, then the
other Dark processes on the box: a `serve`, a daemon, another terminal's TUI.
Each CLI writes one file under
`<rundir>/run/ps/<pid>.json` at startup (pid, the title a system monitor
shows, the command line, the branch, when it started) and removes it at exit;
a reader drops any whose pid is gone, which is what survives a crash. The
registry is the outer ring, one row per OS process; what is inside another
process (its own Dark process tree) is its own, and no instance can read
another's. Reaching into a row is a signal: `ps cancel
<pid>` sends INT, the Ctrl-C path that suspends a traced run; `ps kill <pid>`
sends KILL. `ps --watch` repaints both tables every half second with a cursor
(`c`, `k`, Escape), and the workbench's Processes pane lists the machine's
rows under its own.

What a daemon is, in these terms: one OS process (`dark apps daemon-main
<slug>`, so its title is `dark <slug>`), whose root process is the daemon's
step loop; anything it spawns is a child in its own table. The registry
outlives the CLI that started the daemon, since the daemon writes its own row.

## The scheduling policy, in Dark

Which runnable process a scheduler steps next is round robin: the one that
has waited longest. That is F#, and the default. An expert setting, for
someone studying scheduling rather than using Dark: a store can name a Dark
function instead:

    dark config set exec.policy Darklang.Stdlib.Exec.Policy.youngestFirst

The function takes the runnable
processes of the scheduler that is asking, as `List<Stdlib.Exec.Summary>`,
oldest first, and answers `Option<Uuid>`: the one to step, or `None` for no
preference. `Stdlib.Exec.Policy` ships `roundRobin`, `youngestFirst` and
`leastRunFirst`; a policy of your own is any function of that shape.

What holds it honest:

- It is asked only when there is a choice, two or more runnable on the same
  scheduler, between slices. One process at a time never pays for it, and
  neither does a process alone on its worker.
- It runs on the scheduler's own thread, outside the scheduler's lock, as an
  unscheduled run of its own, untraced. It may call `Exec.list`. It should be
  quick and should not wait: the whole scheduler waits with it.
- An answer that names no runnable process, or a failure, counts as no
  preference for that turn, and the first failure is said once on stderr. A
  name that does not resolve is said at start and ignored.
- The scheduler still owns the budget and the parking: a policy chooses among
  the runnable, it cannot keep a process past its slice or wake a parked one.

Each scheduler asks for itself; with workers, that is per core.

## Replay, effect by effect

The rule is uniform: every call with a declared effect has its result in the execution's
log, and `dark traces resume` or `fork` hands the logged result back without
performing the effect, until the log runs out and the run goes live. Below, "from the
log" means exactly that: the recorded result is handed back and the effect is not
performed again. "Re-perform" means do it again; "refuse" means the resume stops there
and says so.

| effect kind | what a resume does | why (and what goes wrong otherwise) |
|---|---|---|
| stdout, stderr writes | from the log; in a fresh terminal, echo the logged output dimmed (display, not a re-perform; off in the same terminal) | the world saw it once; silent serve leaves a person mid-conversation with no scrollback |
| readLine, readKey | from the log | the input was given once; re-asking blocks on a key already pressed |
| file read | from the log; warn if the file's mtime is newer than the log | the run decided on those bytes; re-reading diverges silently |
| file write, append, delete, rename, chmod, mkdir, symlink | from the log | re-performing appends twice or deletes the wrong generation |
| directory list, stat, cwd, readlink | from the log | a snapshot the run reasoned about |
| HTTP GET, HEAD | from the log | idempotent, but the response is what the run acted on; re-fetching costs a call per resume and can diverge |
| HTTP POST, PUT, DELETE, PATCH | from the log | sent once; twice charges the card twice |
| HTTP stream open | from the log for the open; refuse past the first unlogged chunk | later chunks were never logged; serving blindly hangs, re-performing opens a second connection |
| DB read | from the log | as file read |
| DB write | from the log | a replayed insert inserts twice |
| process run, exec | from the log | output is logged; re-running is a second `rm` or deploy |
| process spawn, IO, terminate | from the log; refuse past a handle that was alive at suspend | the subprocess died with the old run; nothing to serve |
| environment read | from the log | the value the run saw; another machine's env differs |
| environment write, chdir | from the log, then re-apply to the resuming process before going live | the live suffix assumes the env the old process set |
| clock | from the log | time must not jump backwards then forwards inside one run |
| random | from the log | a fork that re-rolls is not a fork |
| sleep, timers | from the log, returning at once | the wait already happened |
| package store read | from the log | replay-after-an-edit depends on it: prefix on the old results, live suffix on the current store |
| package store write | from the log | landing an op twice is a duplicate or a conflict |
| sync push, pull | from the log; a resume never pushes or pulls on its own | the push is on the server; a mid-replay pull lands ops the run never saw |
| permission prompt | not logged; re-checked every call, never replayed | a fork must not inherit an approval nobody gave it |
| trace read, write | from the log; the live suffix records itself | a trace write during replay would write into the log being replayed |
| native (FFI) | refuse unless declared pure | nothing is known about what it did |

The principle that falls out: reads are answered from the log; writes are never
re-performed; non-deterministic sources (clock, random, input) are answered from it so a
resume and a fork see the same past; and the run's own process state that died with the
old process (cwd, env it set, spawned subprocess handles, open streams) is the
exception: re-apply what can be re-applied, refuse to cross what cannot.

All three open questions the table raised were decided (2026-09-21) and are built: a
resume echoes the logged output dimmed, or marked `[replayed]` when output is
redirected; a call whose result was a live handle this run cannot have (an OS
subprocess, an open HTTP stream) stops the resume at that step, naming it, and leaves
the run as it was (an `Exec.spawn` is performed again instead, 2026-09-22, and the child
replays its own rows); a logged file read whose file has changed since the recording
warns and continues on what it read then.

---

## Edges

What is deliberately not here, and where the seams are:

- `LiveValues.fs` applies a callable through `executeApplicable`, on a VM of
  its own: it runs a function for inspection, not as part of a program.
- A policy chooses which runnable process to step, not where a spawn lands:
  `Exec.spawn` goes to the least loaded worker, in F#.
- A resume matches recorded processes to new ones by start order; a run that
  spawned from Dark may not line up. `resume` is the CLI's, since it runs the
  input through the CLI's own paths; `Exec.fork` from Dark exists.
- `ps show` says how many reads a process has in flight, not which, and shows
  the call stack, not registers.
- A builtin's signature is still `Ply<Dval>`. Store-facing builtins (`DB`,
  the package manager, traces, the CLI host's script runner)
  await SQLite through `LibDB`, which has no asynchronous I/O, so they
  complete on the calling thread and the loop sees finished values rather
  than parking; a request form for them would be a store-operation type over
  some sixty queries that buys nothing while the store is in-process. A pure
  builtin that reads a persisted blob still waits for the store inside
  `uply`; an ephemeral one answers without a builder.
- `sleep` parks on its timer task, not on a `Timer` event; `ps` says `sleep`.
- `Event.ExecDone` carries only the id (a Dark enum cannot hold an untyped
  value); `Exec.await` is how a value comes back.
- Runs moved to another machine are files; a relay route would be its own
  change on the relay's own deploy.
- The stdin reader thread has not been checked on Windows.

---

## Working on this

Where things are and what bites, for whoever changes the scheduler or the live side next. The
rest of this file is what the system does; this section is how to move around in it.

### Where the bodies are

- `LibExecution/Scheduler.fs`: `Process` (the record; `stopReason`/`stopHard`/`detached`
  are the cancellation state), `Scheduler.Step` (the one place a VM is stepped; the
  thread check is there), `Finish` (also the parent-to-children cascade),
  `Stop`/`Cancel`/`Kill`/`StopChildrenOf`, `Describe` (what `ps` says a parked process
  waits on: `hostInflight` first, then the instruction under the counter), `Dispatch`
  (events to processes), `Workers` (the group: root plus one scheduler per worker
  thread; `SpawnOn` picks the least loaded), `CurrentOrShared` (the process-wide
  scheduler a run nobody scheduled uses). `Scheduler.Current`/`CurrentProcess` are
  `AsyncLocal`s: a fresh thread sees `None` and takes the no-scheduler path.
- `LibExecution/HostEvents.fs`: the queue (`Queue.Post`/`Take`/`ArmTimer`), the sources
  (`sources.readKey`/`storeVersion`, installed from `Cli/Cli.fs` and
  `Builtins.Cli/Libs/Stdin.fs`), `Shared.requestKey`/`watchStore` (one reader thread and
  one store poll per OS process, demand-driven).
- `LibExecution/Interpreter.fs`: `executeSync` is the loop, `awaitOf` turns a bail into
  a `StepOutcome`, `driveToEnd` is the unscheduled run; `invokeBuiltin` (the effect
  check, the replay lookup, `performRequested` for host requests, the read deferral
  through `Promises.deferrable`/`tryMake`, `finishBuiltin`);
  `requestApply`/`beginRequest`/`drive`/`landBuiltin` are the apply-request protocol (a
  builtin asks the loop to apply a callable as a frame);
  `requestHost`/`performRequested` are the host-request protocol (a builtin names a
  `Host.Operation`, the loop performs it under `vm.activeAccess` and drives the
  continuation). `Promises` (`maxInflight`, `settle`) is the read-in-flight machinery.
  The budget is `vm.budget`, counted down in `runSyncInstructions`.
- `LibExecution/Effects.fs`: `isRead` (which effects overlap), `readsOnly` (the
  per-builtin exception: `httpClientRead`), `isScoped` (which effects are checked by the
  body's exact request rather than the ambient gate).
- `LibExecution/Host/Host.fs`: `perform` (resolve, check, execute, audit), `blocking` (a
  process run or process IO goes to the pool so the scheduler thread is free); only the
  HTTP arms are asynchronous, every file and libc call completes on the calling thread.
- `LibDB/Tracing.fs` (`traceEffects`, `nextEffect`, `replayEffect`,
  `createReplayTracer`) and `LibDB/Executions.fs` (`Replay.arm`): the effect-only log
  and the armed replay `dark traces resume` uses; `Builtins.CliHost/Libs/Cli.fs` is where
  the CLI arms it and runs the input through its own `eval`/`run` paths.
- `Builtins.Language/Libs/Exec.fs`: the ps and Exec builtins (`execList`, `execInspect`,
  `execSpawn`, `execSpawnDetached`, `execAwait`, `execAwaitWithin`, `execSelect`,
  `execCancel`, `execKill`), `summaryToDT`/`parkedToDT` (the RT-to-Dark mirror of
  `ProcessSummary`), and the policy chooser (the Dark policy called from the F# loop).
- Dark: `stdlib/exec.dark` (the user surface and `ParkedOn`/`Summary`),
  `stdlib/await.dark` (`Stdlib.await`/`awaitAll`), `stdlib/execPolicy.dark`,
  `stdlib/hostAwait.dark`, `cli/ps.dark` (`treeRows` is the tree; the workbench's
  Processes pane in `cli/workbench/views-misc.dark` paints the same rows),
  `cli/exec.dark`, `stdlib/execution.dark`.
- `Cli/Cli.fs`: `main` through the scheduler,
  `execSettings`/`workerCount`/`installPolicy`, `installStoreVersionSource`, the Ctrl-C
  suspend, `DARK_SCHEDULER=off`.

### Rules that bite

- Only a process's own scheduler thread steps it; everything else posts to its queue.
  `Step` raises if the thread is wrong. A Ply continuation that touches a VM is the race
  you will never reproduce on purpose.
- A `PackageRefs` change (a type F# reads by hash: `Stdlib.Exec.*`, the
  `Stdlib.HttpClient` fns) means: build (the hashes file regenerates; if the CLI dies
  with "hash not found", empty `backend/src/LibExecution/package-ref-hashes.txt` and
  build again), then `scripts/run-local-exec export-seed rundir/seed.db` before any
  `--optimize` build, or the published build refuses with "cannot produce this binary's
  package refs". This happened on nearly every landing.
- Inside the source tree every binary, saved perf snapshots included, reads the current
  `package-ref-hashes.txt` (`PackageRefs.loadHashes` prefers the source tree over the
  embedded copy). So after a `PackageRefs` change an older saved binary cannot run on
  its own fixture: it looks up new hashes in an old store. Not fixed. Workaround used:
  run both arms on the new fixture when the workloads do not touch the changed type; the
  real fix is for `bench bin save` to pin the hashes file beside the snapshot and for
  `run_once` to point the binary at it.
- `scripts/perf/bench` restores "the dev store" at the end of an `ab`, which is whatever
  `rundir/data.db` was when it started. If you copied a fixture over `data.db` by hand
  first, that is what it puts back, and the next `gate` run dies with a hash error. Keep
  a copy of `data.db` in `rundir/discard/` before hand experiments.
- Dark embedded in F# tests (`Scheduler.Tests.fs`): a multi-statement program with a
  self-recursive nested fn needs the parenthesised, indented form; a program with `let`s
  and no nested fn needs the column-zero form (`let h = ...` on its own line, the
  expression on the next); mixing them gives `VariableNotFound`. The closing triple
  quote goes on its own line when the program ends in a string literal. Test states are
  allow-all: `executionStateFor pmPT false Map.empty`.
- Prelude shadows `List.find` to return an `Option`; `| null ->` does not compile
  against a DU (use `obj.ReferenceEquals`); a member that calls another member needs
  `this.`, not `_.`.
- `Builtin.Tests` enforces one Dark wrapper per builtin. `ps kill` calls
  `Builtin.execKill` directly from `cli/ps.dark` (precedent: `cli/clear.dark`), which is
  that builtin's one caller; `Exec.cancel` is the stdlib verb. A handle is built from an
  id as `Stdlib.Exec.Handle { id = ... }`.
- A process run (`Stdlib.Cli.execute`) parks now (`Host.blocking`); a file or libc call
  never does. A test process that parks on the host: run `sleep 1`; one that parks
  forever: `Builtin.testGateWait n`, released by `Gates.release n` from F#.
- Never wait on a suite with a `pgrep -f` that matches itself; run it in the foreground
  or wait on the `Tests` pid. Two suites in one clone destroy each other.

### Half-decisions and rough edges

- The builtin signature is still `Ply<Dval>`. A finished `Ply` is a free struct, so this
  costs nothing; the request protocols ride the VM (`pendingNext`, `pendingHostOp`)
  rather than a return type. Changing the ABI of every builtin was judged not worth it.
  The store-facing builtins (`DB`, `PM`, `Traces`, the CLI host's script runner) await
  SQLite, which never actually waits.
- The host's answer comes back through the task the loop parks on (the scheduler's
  `Completed` post), not as a `Response` event of its own. If a `Response` event is ever
  wanted (a remote host), `performRequested` is the one place to change.
- `httpGetUnsafeBytesStart` performs inline on purpose (it exists not to wait); the HTTP
  server's bind performs inline under the child guest's access (`HttpServer.fs`, live's
  file).
- `Exec.awaitWithin` uses `Task.Delay` with a cancellation source, not the queue's
  `Timer` event; same effect, one less path through the scheduler.
- A cancel of a process parked on a task that never completes waits forever; that is
  what `ps kill` is for, said in the ps help. A stop reason is a string on the process
  ("cancelled", "stopped by ps kill", "its parent finished") and the awaiter sees it as
  an `UncaughtException` with that message.
- `ps` shows the tree from the `parent` field; a process whose parent has been forgotten
  (the finished-process cap, 64 per scheduler) becomes a root. `Summary` does not carry
  `detached`; add it if the view needs it (a `PackageRefs` change, so the seed dance).
- `Effects.readsOnly` is a name table with one entry. If a second builtin needs it, that
  is the moment to consider a real `HttpRead` effect and what it costs the policy
  language.
- `exec.maxInflight` is a `mutable` because one test lowers it; it is not a config key.
- The perf gate budget went 9.74 to 9.82 MB (its own commit, reason in
  `docs/perf/history.md`). Two traps: the `scripts/dev/build --optimize` binary reads
  about 0.3 MB higher on the gate than CI's `scripts/build/build-release-cli-exes.sh`,
  and a store that just ran the suite reads 2.5% high. Build with the CI script and
  `reload-packages` before trusting a published gate number.

### How the bench and the gate were run

- Save a snapshot after a Debug build: `scripts/perf/bench bin save <name>` (it copies
  `backend/Build/out/Cli/Debug/net10.0`; `--optimize` builds go elsewhere, so an A/B of
  two snapshots is Debug against Debug, which is what every number on this branch is).
  Snapshots live in `rundir/perf/bin/` (`after-oneloop`, `after-host-requests`,
  `after-http-requests`, `after-pure-nobuilder` are the Ply-out series; `integ-pre-loop`
  is the tip before it).
- Save a store fixture matching the snapshot: `scripts/perf/bench fixture save <name>`
  writes `rundir/perf/fixture-<name>.db` (`fixture` alone is `fixture.db`). Then
  `scripts/perf/bench ab <A> <B> <scenario> --fixture-a <name> --fixture-b <name>`; the
  scenarios are `scripts/perf/bench scenarios` (`interp-arith`, `interp-list`,
  `eval-map1000`, `eval-stream`, ...). Fifteen interleaved pairs; read the median paired
  difference and the pairs count, not the medians. Raw outputs from this series are in
  `rundir/perf-plyout/` and `rundir/perf-followups/` (not in git).
- A workload of your own: a `.dark` file in `rundir/perf-workloads/` in the shape of
  `arith.dark` (warm, `Builtin.interpreterStatsReset`, time with `Builtin.timeNowMs`,
  print `elapsed_ms=`), run by hand with `scripts/run-in-docker
  /home/dark/app/rundir/perf/bin/<name>/Cli --no-trace run
  rundir/perf-workloads/<file>.dark`, interleaved A/B in a shell loop. That is how the
  blob number was taken (`blobcalls.dark` is there).
- The gate: `scripts/perf/gate` prints MB; the exact bytes are the `totalAllocatedBytes`
  of the `gc.stats` line in `rundir/logs/telemetry.jsonl` after a run. Three runs spread
  about 70 KB on 9.5 MB. `scripts/perf/gate --published` needs a fresh seed and a binary
  from `scripts/build/build-release-cli-exes.sh` (the CI path; `--optimize` reads 0.3 MB
  higher).
- Bench the box only when it is quiet; `bench ab` refuses under load unless `--allow-noise`.

### Tests, and which are slow

- `scripts/dev/build --optimize --test` is the full published suite, about 5m45s, 11,444
  tests; every landing ran it. It needs the seed export first after a `PackageRefs`
  change.
- Groups worth knowing (`./scripts/run-backend-tests --filter <group>`, seconds after
  the reload): `tests/scheduler` (36 tests, about 10 s, `testSequenced` because gates,
  trace and the fake key source are process-wide), `tests/LibExecution` (6,734 testfile
  cases, about 40 s, the stdlib), `tests/HttpClient` (4 s), `tests/CliWorkspace` (16 s,
  the workbench and live), `tests/MultiInstance` (10 s, sync), `tests/builtin` (the
  one-wrapper rule), `tests/Interpreter/PermissionsGate`, `tests/permissions`,
  `tests/PermissionEscape`, `tests/blob`. `--groups` lists everything with counts.
- Test-only builtins in `TestUtils/LibTest.fs`: `testGateWait n`, `testTrace s`,
  `testRead n`; `Gates.release`/`reset`, `Trace.take` from F#.

### For the next items

- The perf table (main vs branch): build main in a sibling clone with the same config
  (Debug for `bin save`, or a published build on both sides and pass paths to `bench
  ab`, which accepts a path in place of a snapshot name); the two stores must each match
  their binary (`--fixture-a`/`--fixture-b`, or `none` for a published binary's own
  seed). For the HTTP row, a local server: `scripts/perf/http` starts one; N GETs in a
  `List.map` on main run one by one (main has no scheduler), so the ratio is roughly N.
  Startup: `dark eval 1L` wall clock, twenty runs each, interleaved. `List.parallelMap`
  on one worker: `dark config set exec.workers 1`.
- `serve` end to end: `HttpServer.fs` `executeHandler` spawns each request with
  `SpawnApply` and the serve process as parent, so a request is already a process a `ps
  kill` reaches; a timeout is `Scheduler.AwaitWithin` on the handler's completion then
  `Cancel` (the same member `Exec.awaitWithin` uses); the slow-handler test is one
  request that sleeps beside three that do not; `scripts/perf/http` is the before/after.
  Per-request state is `perRequestStateFor`.
- Process titles: `Cli/Cli.fs` `main` knows the command after parsing; write
  `/proc/self/comm` (15 bytes) and, for the cmdline, overwrite the argv memory through
  `prctl(PR_SET_MM_ARG_START/END)` or by writing into the original argv area (P/Invoke).
  Plain host code called once at startup is the right shape; `tests/hostBoundary` scans
  for OS-touching IL, so put it in `Host/` and see what the scan says.
- The live process view: the registry should be a directory of files under the rundir
  (one per OS process, written at startup with pid, title, entry, branch, started,
  removed at exit), read by `dark ps` from any shell, stale entries dropped when `kill
  -0 pid` fails; cross-process cancel is a signal (`SIGINT`, the CLI's Ctrl-C path,
  which already suspends an execution) and cross-process kill is `SIGKILL`; the store's
  `PRAGMA data_version` poll is not the right carrier for this. `ps --watch` is a host
  loop over `Host.await [Key; Timer 500]` painting `Cli.Ps.treeRows` through the `Node`
  tree; the workbench pane already paints those rows.
- Limits: `exec.maxInstructions` is a comparison against `p.instructionsTaken` (what the
  slices spent of the budget, `quantum - vm.budget` per slice) in `Scheduler.Step`
  before the slice; bytes are `GC.GetAllocatedBytesForCurrentThread()` bracketing the
  slice on the scheduler thread (one thread per scheduler, so the delta is that
  process's) accumulated on the process; a process over its cap is `Finish`ed with a
  plain reason like a cancel. `ps` gets two columns.
- Suspended runs across machines: the execution is rows in the trace tables
  (`LibDB/Traces.fs`, the schema in `migrations/schema/08-traces.sql`); `dark traces
  push` is a bundle of those rows through the sync transport (`httpPostUnsafeBytes` to
  the relay), `pull` the reverse, then `resume` as today. Check the log's size on a real
  run first; a run that read a big file has the bytes in the log.
- The tracing cap is built (`TraceRetention`: `trace.keep`, `trace.maxMb`, the suspended,
  pinned and newest-per-entry exemptions), the redaction is built, and so is pinning
  (`dark traces pin|unpin`, the `pinned` column).
- Replay refinements: all in `invokeBuiltin`'s `replayed` arm (the `Stdout` echo is a
  `match fn.callEffects` there), and in `Tracing.createReplayTracer` for what the log
  cannot reproduce (an open stream, a live subprocess handle, a `Native` call), which
  should fail the resume naming the step's ordinal, and a `FileRead` whose file's mtime
  moved since the log, which should warn.

---

---

## Decisions, indexed

One line each, with the date they were taken. The design they came out of is this doc
and `docs/live.md`.

| id | decision |
|---|---|
| H1 | hosts learn of a store change by polling `PRAGMA data_version` |
| H2 | reload = re-resolve names at the next call; in-flight finishes on old code; a fn held as a value keeps its hash, so daemons need a restart until the scheduler's budget yield |
| H3 | which views re-run: transitive dependents from `package_dependencies`; re-run-all as fallback |
| H4 | a view is `Model -> Node`; one `Node<'msg>` tree; `Ui.Tui.render`, `Ui.Html.render` |
| H5 | model held in memory across a swap; `:save` snapshots it as a `val` |
| H6 | broken edit: keep the last good hash, error in a band; same for RTEs |
| H7 | `serve` resolves the router per request; SSE browser refresh only if time |
| H8 | prod host follows a chosen branch (`dark --branch <name> serve ...`, which exists); new auto-commit-and-push mode |
| H9 | live values from traces: after record/replay lands, not on the demo path |
| H10 | agent channel: untouched until the agent harness merges |
| reframe | target is two demos, phases disposable; `Node` tree and remote move into the first stretch |

Scheduler:

| id | decision |
|---|---|
| S1+S2 | process = wrapped `VMState`, F# cooperative scheduler first; then remove host re-entry until all state is explicit and portable ("a then c") |
| S3 | Ply leaves the interpreter at the end: awaits park on `Operation`, resume with `Response` |
| S4 | reads (calls whose effects are all read-effects) run concurrently and force on use; writes keep program order; `demand x` forces now; `Exec.spawn`/`await` for deliberate background work |
| S5 | instruction-budget yields (BEAM reductions), measured on the perf bench |
| S6 | `readKey` parks on an event queue fed by a reader thread; users still write `readKey ()` |
| S7 | an execution is record/replay: entry + inputs + effectful-call log; resume after Ctrl-C = replay then live; fork = shared log prefix + parent pointer; classic's store-only-impure rule (`callEffects` non-empty) |
| S8 | persistence = the log; no frame serialisation |
| S9 | cores now, via per-process `ExecutionState` copies (the Http server's trick) |
| S10 | live and scheduler in parallel, two clones, merge at the end |
| naming | the durable record/replay thing is an `Execution`; Dark module `Stdlib.Exec` (`spawn`, `await`, `select`, `demand`); CLI `dark exec list/show/resume/fork` for durable ones, `dark ps` for what is running now; `Process` stays an F#-internal name; `Stdlib.Cli.Process` (OS subprocess) untouched |
| S4 verbs | three verbs: `demand x` forces a not-ready value; `Exec.spawn f` starts an explicit execution and returns a handle; `Exec.await h` waits on the handle |
| follow-up order | cores, implicit reads, record/replay, re-entry removal, Ply out, policy. Cores first (Stachu, 2026-09-19: "do the full thing, don't half-ass it"): the cache surgery before the visible reads win. Record/replay before re-entry removal because its replay tests then guard the rewrites |
| syntax (2026-09-21) | the force word is `await` (`demand` renamed); one Dark stop verb `Exec.cancel h` (children die with their parent unless detached), the CLI keeps `dark ps cancel` (polite, cleanup) and `dark ps kill` (escape hatch for a process stuck in a native call); `List.parallelMap` stays; two CLI nouns stay (`dark ps` = running now, `dark exec` = suspended runs); the `concurrency` permission stays a real effect. Trims: `DARK_EXEC_WORKERS` gone, `exec.workers` defaults to the cores, `exec.maxInflight` a fixed default, `DARK_SCHEDULER=off` and `exec.policy` out of user docs, the HTTP read hint goes with Http-as-read, `Ps.printAll` goes with the live process view |
| one branch (2026-09-21) | one branch `scheduler-and-live`, one PR "Scheduler and live programming", one clone; the two tracks' commits stay grouped; live never rebases onto scheduler changes, it merges them and walks the seam list |
| replay (2026-09-21) | all three recommendations of `replay-effects-2026-09-21.md`: dimmed echo of the old run's output on resume; refuse to resume past an unreproducible effect (open stream, live subprocess, undeclared native call) with a message naming the step; warn on a file read whose file changed since the log |
| awaits, gate, tracing (2026-09-21) | the two `await`s stay (`await x` on a value, `Exec.await h` on a handle); the perf gate budget is raised in the merge commit with `gate --update`, reason in `docs/perf/history.md`; tracing on by default behind the retention cap once the cap exists |
| unlooked reads (2026-09-21) | a read nobody looks at is waited for at the end of the run, and its failure fails the run there, naming the read (the JS unhandled-rejection rule); forcing at every frame's return was the barrier nobody wanted |
---

---

## Decisions made while building

Departures from the plan, each with its reason; as binding as the table above. Details
are in `docs/processes.md` and `docs/live.md` in the tree.

- Cores: no copy-on-spawn caches; they were already concurrent and content-keyed, and a
  per-process copy would break a lambda created in one process and applied in another.
  The tracer is the one per-process thing; denial lists are shared and locked so a
  child's denial reaches the parent.
- Reads: a read in flight is the builtin's own task wrapped as a promise in the
  register, forced by the first instruction that inspects it; not a spawned process.
  `Http` is not a read effect (`http` stays one word in the permission language: a
  policy grants a URL, not a method); GET and HEAD are `httpClientRead`, which
  `Effects.readsOnly` names a read per builtin, so `HttpClient.get`, `head` and `request
  "GET"` are reads and every other method keeps its order. `Clock` and `Random` complete
  synchronously and stay in order. A denied read raises at the call site; a failed read
  raises at the force point.
- Executions: the log is the state; no frame snapshots. A replayed `printLine` prints
  nothing (pending Stachu's call on the replay analysis). The recorded run's process ids
  are matched to the resumed run's in order of first appearance, which is also the order
  a parent spawns its children: a resumed run performs its spawns again, and each new
  child replays the recorded child's rows.
- Re-entry removal: List and Stream families done; the HTTP server done on the live
  side. The interpreter honours a request after a builtin's first wait (`landBuiltin`),
  rather than threading a continuation through the stream implementation.
- Policy: an F# loop asks a Dark chooser which runnable process steps next, instead of
  the plan's Dark-owned loop over step builtins, which moved the hot loop into Dark for
  no gain.
- Ply out: every library that can park now hands the loop a host operation; the
  store-facing builtins keep `Ply` because SQLite has no asynchronous I/O and they never
  park; the builtin signature stays `Ply<Dval>`.
- The perf gate: over budget by honest startup work (a bigger package set, the registry
  file, the worker pool), nothing per step, spawn or event; raised with `gate --update`,
  said in the PR and in `docs/perf/history.md`.
- The two `await`s: `Stdlib.await x` forces a value, `Exec.await h` waits on a handle;
  the same word, different modules, because one function cannot be typed as both.
- One branch: `scheduler-and-live`, made by merging `live-programming` into the
  scheduler's integration branch, no conflicts; the live clone's container stopped,
  nothing deleted.
- `Http` as a read: no second effect name. `httpClientRead` (GET and HEAD) is named a
  read by `Effects.readsOnly`, a per-builtin table beside `isRead`, since `http` in a
  policy grants a URL, not a method, and a new effect name is one every policy and older
  binary would have to know. `HttpClient.request "GET"` dispatches to it in Dark, so the
  user surface did not change.
- The trims: `exec.maxInflight` stayed a `mutable` in `Interpreter.Promises` (not a
  config key) because one test lowers it to 2 to see the bound hold.
- Replay refinements (2026-09-21, coordinator): the three rules live in
  `Interpreter.ReplayPolicy`, run before a logged result is handed back. The "undeclared
  native call" refusal is by builtin name, not by the `Native` effect: the runtime's own
  libc-backed builtins (every posix call) carry `Native`, so the effect would have
  refused every file read; a user-level FFI builtin joins the name set when one exists.
- Tracing on by default (2026-09-21, coordinator): the default records the impure calls
  only (Stachu's call). The three levels were later cut to two switches, recording on/off
  and values on/off, which is the same thing said in fewer words; the cap is two config keys, `trace.keep` (200 traces) and `trace.maxMb` (256 MB
  of args and results), enforced at store time, a suspended execution's trace exempt; a
  pass costs one count per store and scans byte weights only past fifty traces, at most
  every ten seconds. Secrets in the log stay an open question; the docs say what is in
  the clear.
- Suspended runs across machines (2026-09-21, coordinator, REVERSED 2026-09-24): a text
  bundle of the rows was built and then removed again (Stachu's call). It carried no
  code, no blobs and no argv, so it moved the runs whose log happened to be
  self-contained and quietly went live early for the rest. Moving a run belongs with
  synchronising runs properly, and should arrive with that design.
- Per-process caps (2026-09-21, coordinator): `exec.maxTurns` and `exec.maxBytes` as
  expert config keys, unset = no cap, checked before each slice; bytes are
  `GC.GetAllocatedBytesForCurrentThread` around the slice on the scheduler's own thread,
  so the delta is that process's. Not defaults: a default cap would break a legitimate
  long run; the keys are for a host that wants a fence. `ps show` prints the allocation
  either way.
- The live process view (2026-09-21, coordinator). The registry is a directory of files
  under `<rundir>/run/ps/`, one per OS process, written by the CLI at startup with pid,
  title, command, branch and start time, removed on `ProcessExit`, stale ones (pid gone)
  dropped by readers. Chosen over a socket or the store: nothing to keep alive, nothing
  to migrate, readable by any shell, and the daemons' pidfiles already live beside it.
  Cross-process reach is a signal (INT = the Ctrl-C suspend path, KILL); TERM was the
  first cut and did not reach the suspend handler; a process's own Dark tree is not
  published, so `ps` shows the machine's OS rows plus this instance's tree. A view can
  now declare `every: Some ms` and gets a `Tick` event; `ps --watch` is such a view
  (cursor, `c`, `k`, Escape). The workbench pane lists the machine rows under its own
  table. No browser version: nothing serves a page for it yet, and a `serve` of the view
  would need a request loop of its own; a follow-up if wanted.
- The perf table (2026-09-21, coordinator), raw runs. Setup: a throwaway clone
  `perf-main` at the branch's base (the merge-base with `origin/main`, "Merge pull
  request #5774"), published build (`scripts/dev/build --optimize` after `export-seed`),
  its own container; the branch's published build in the scheduler container; workloads
  under `rundir/perf-workloads/` in both (`httpgets.dark`, `httpone.dark`,
  `parmap.dark`, `arith.dark`, `listwork.dark`, `slow-server.dark`). A local slow server
  was the plan but a `run` script is a guest and may not reach a private address
  (`canUsePrivateNetworkHttp`), so the HTTP row uses `https://httpbin.org/delay/1` with
  `permissions allow http GET` on both stores. HTTP GETs x20, ms: main 20601, 19723,
  20761; branch 3090, 2202, 1378. `parmap.dark` (branch): workers=1 map/parallelMap
  2003/1823, 1965/1811, 2129/1929; workers=8 2011/309, 1988/282, 1949/284; workers=48
  1995/296, 1935/255, 1949/266. `arith.dark` interleaved main/branch x8: 28/31 28/30
  28/30 28/30 28/30 27/30 27/30 28/32. `listwork.dark` x8: 948/812 954/879 946/868
  959/829 937/875 930/868 950/861 940/870. `dark eval 1L` x20: main min/median/max
  593/605/639, branch 674/689/744. `scripts/perf/http --release -n 2000 -c 16`: main
  4414, 4478, 4815 req/s, p50 3.12/3.12/2.96 ms, 49.79/49.78/49.80 KB; branch 4383,
  4395, 3841 req/s, p50 2.40/2.51/4.15 ms, 84.55/85.45/87.07 KB. Two findings: startup
  +84 ms (30 ms is the scheduler path, the rest the bigger package set and live's
  startup; the workers were already lazy) and +35 KB per HTTP request, cut to +5.6 KB
  (55.4 KB vs 49.8; `scripts/perf/http --release` after the child-count change:
  4526/3951/4078 req/s, p50 2.25/3.91/3.79 ms, 55.39/55.39/55.41 KB) by not scanning
  every scheduler for a finished leaf's children.

---

---

## Seam list: every place live touches the scheduler

The two tracks are one branch now; this is the map of where they meet, for whoever
changes one side and has to walk the other. `Stdlib.Host.await` and its
`EventSpec`/`Event` types (`stdlib/hostAwait.dark`; the host loop in
`cli/apps/host.dark`, the workbench loop in `cli/loop.dark`, `Live.poll` describing a
change). The store version source `HostEvents.sources.storeVersion`, installed from
`Cli/Cli.fs` over `LibDB.Sqlite.DataVersion`. `Scheduler.SpawnApply` and `AwaitWithin`
in `HttpServer.fs`. The test harness's `loopDriver`/`stepOn`/`pushKey`/`pushTick` over
`Scheduler.PushEvent`. `cli/ps.dark` and the workbench's Processes pane over
`Exec.list`/`inspect`/`cancel`. Live values: `TraceExpr` and `storeExprResult` in
`Interpreter.fs`, `Tracing.forProcess`, `LiveValues.fs`. `Cli/Cli.fs`: `apps daemon-main
<slug>` beside main-through-the-scheduler and `DARK_SCHEDULER=off`. `docs/live.md` names
scheduler surfaces in "The host loop", "Live values" and "The agent channel".
