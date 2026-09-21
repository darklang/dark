# Processes and the scheduler

Status: the baseline, cores, concurrent reads, executions, and the list
builtins with no host re-entry. A running computation is a value the runtime
can step, park, resume and inspect; one thread runs many of them, and a group
of worker threads (one per core) runs many more. Reads run concurrently on
their own and writes keep their order; `Exec.spawn`/`await` run chosen work
in the background. A traced run is an execution: kept with the log of what it
did to the world, suspended by Ctrl-C, resumed or forked by replaying that
log. A lambda that `List.map` (and the other list builtins) applies is a
frame on the process's own stack. A builtin that needs the host (a file, the
environment, a process, the network) names the operation and the loop
performs it. The follow-ups at the end are where the rest goes.

The one-paragraph version: a process is a `VMState` plus the `ExecutionState`
it runs under plus a status. A scheduler steps a process until it finishes,
has to wait for something, or spends its instruction budget. A waiting process
is parked on the task it waits for; when that completes, an event lands on the
scheduler's queue and the scheduler thread resumes the process. A preempted
process goes to the back of the line. Keys, timers and store changes arrive on
the same queue, so `readKey` parks instead of holding the thread. A scheduler
is one thread; a process spawned on a worker scheduler runs on another core
for its whole life, sharing nothing with its neighbours but the state's
concurrent caches.

---

## What a process is

`LibExecution/Scheduler.fs`:

- `Process`: `vm`, `exeState`, `entry` (the function it was spawned on, or
  `EntryExpr`), `parent`, `started`, `status`, `slices` (how many times the
  budget was refilled), the completion F# callers await, and the park state.
- `Status`: `Runnable | Parked of Parked | Done of Dval | Failed of rte * stack`.
- `Parked`: what a parked process waits for, for `ps`. `OnBuiltin name`
  (a `sleep`, an HTTP call, the script an `eval` runs), `OnPackageFn hash`,
  `OnLambda`, `OnRareOpcode` (the interpreter waiting on the store),
  `OnEvent specs` (a `Host.await`).

`VMState` was already self-contained (its own frames, frame pool, caches), so
wrapping it cost nothing. The one field added is `budget`.

## The step

`Interpreter.stepScheduled exeState vm : StepOutcome` is the scheduler's whole
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
resume)` at the bail site. `stepScheduled` is that loop; an unscheduled run
(`execute`: tests, the LSP, a host running a function itself) is
`driveToEnd`, fifteen lines that await each `wait` in place and step again.
Its budget is negative, so it never sees `StepBudget`. The task-based second
loop (`executeInnerTask`, with `handleFrameStep` re-deciding every wait) is
gone: a wait is decided once, where it happens, and the scheduler and the
plain run differ only in who waits.

## The budget

`runSyncInstructions` counts `vm.budget` down by one per instruction and stops
at zero; `runFrame` reports that as `FrameBudget`. The default quantum is
10,000 instructions (`Scheduler.defaultQuantum`), refilled before every slice.
A negative budget means unlimited, which is what every VM nobody schedules runs
with. (A callable a builtin applies runs as a frame of the same VM now, so it
is preempted like anything else; the borrowed-VM case is gone.)

Measured with `scripts/perf/bench ab` against the pre-change binary: within
noise on `interp-arith`, `interp-list` and `eval-listheavy` (median paired
difference +0.0%, -0.3%, -0.5%; IQRs 14 to 24 ms). The instrument cannot
resolve a 1% change either way, so the per-instruction check stayed and the
fallback (check on jumps and calls only) was not needed.

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
- `StoreChanged change`: a 200 ms poll of `PRAGMA data_version` on a
  connection of its own (the pragma answers per connection), posted to every
  queue watching. `change` is `Host.Change.Unknown` until the live track's
  `scmOpsSince` can say what changed. Latched per process: a process that
  subscribes after a change it has not been told about is woken at once, so
  a change during a render is not lost.
- `Completed pid`: internal; the parked task finished.
- `ExecDone (pid, dv)`: a process finished, for Dark subscribers. Posted to
  every queue in the group, since the subscriber may be on another scheduler.
- `Wake`: nothing to route; the loop, blocked with nothing runnable, looks
  again (a spawn from another thread, a `Stop`).

The sources `LibExecution` cannot provide itself (the console, the store) are
installed by the host: `Stdin.fs` installs the key source, `Cli.fs` the store
version.

## Cores: workers

A `Scheduler` is one loop on one thread. `Scheduler.Workers` is a group: the
root plus N more schedulers, each looping on a background thread of its own
(`dark-worker-<i>`), started the first time anything asks for them. N is
`exec.workers` in the store's config (`dark config set exec.workers 4`), or
`DARK_EXEC_WORKERS`, or one per core; `Cli.fs` reads it into
`Scheduler.defaultWorkers` before the root starts.

- `root.SpawnOn(...)` spawns on the least loaded worker (fewest runnable or
  parked processes); the process runs there for its whole life. `Spawn`
  keeps it on the calling scheduler.
- `Await` is the process's completion task and works from anywhere. `ps` and
  `kill` from any scheduler in the group see and reach every process in it.
- Nothing in the CLI spawns on workers yet: every process today (the CLI's
  root, each `eval` expression) is on the root. The implicit-reads step and
  the Http server's handlers are what will use them. So a CLI run that never
  spawns on a worker never starts the threads.
- Measured on the shared desktop (a Threadripper 3960X), warm: four
  CPU-bound processes on four workers finish in 0.59 to 0.65 of the
  one-thread wall time published, 0.34 in Debug. Not the 1/4 an idle
  machine would give a compute loop, and the scheduler is not why: four
  plain `Interpreter.execute` calls on four threads, with or without a
  shared state, scale the same, and so does a plain F# loop that only
  allocates (546 ms alone, 871 ms four at once), while a loop that only
  computes scales nearly perfectly. The interpreter allocates per value,
  so the allocator's scaling is its ceiling; server GC and a larger gen0
  budget did not move it. The allocation work in `docs/perf/roadmap.md` is
  therefore also the multi-core work. The test bounds the ratio at 0.8.
- A spawn onto a worker costs about 9 us in Debug (10,000 spawns of a
  trivial program in 90 ms, including the placement scan and the `Wake`).

## What a process shares and what it owns

The audit of `ExecutionState`, field by field, for two processes on two
threads under one state. The short answer: the interpreter already ran on
many threads with one state (the Http server's handlers, the parallel test
suite), and the caches were made concurrent for that (`fix-types-cache-race`,
August 2026), so a process copies almost nothing.

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
let pages = List.map HttpClient.get urls   // every GET is in flight, at once
print "fetching"                           // a write: runs now
let first = List.head pages                // looks at the list: waits for all
File.write out first.body                  // in order
```

`Exec.demand x` forces a read now rather than at its first use; it is the
identity function, since calling anything with the value is what forces it.
`Exec.demandAll` is the same for a list.

How it works (`Interpreter.Promises`, `RuntimeTypes.Promise`):

- At the builtin call site, when the builtin's `Ply` is not finished and the
  call is deferrable, the register gets a `DPromise` (the task, the builtin's
  name and the frame's execution point) instead of the process parking. A
  call is deferrable when `Effects.allReads fn.callEffects`, or when the body
  set `vm.readHint` for this call. `Http` is not a read effect, because one
  builtin carries every method; `httpClientRequest` sets the hint for GET and
  HEAD, per call. `Clock` and `Random` are not read effects either: reading
  them never waits, and `sleep`, the one clock call that does, is a wait the
  program means to take (it was deferred in a first cut, and `let _ = sleep`
  then slept nobody). A read that finishes synchronously (most file and db
  reads in this runtime) is never a promise; only a real wait is.
- A promise is only ever at the top level of a register, a frame's result, or
  a builtin's returned value. Every instruction that inspects, stores or
  passes a value forces it first: `Apply` forces the callee and every
  argument (so no builtin body ever sees one, and `demand` is an identity
  function), record, enum, list, tuple, dict and string construction force
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
  anything is in flight: the ambient effect check runs before the body.
- `List.map` is promise-aware (`Execution.executeApplicable1Deferred`): a
  lambda that returns a read in flight hands it back rather than being forced
  at the end of its run, and the map's result is one promise for the whole
  list, landing when every element has. Everything else that applies a lambda
  gets the lambda's result forced. So `List.map get urls` is where the reads
  fan out; `List.filter get urls` would run them one by one.
- The bound: at most `Promises.maxInflight` reads in flight per OS process
  (`dark config set exec.maxInflight N`, `DARK_EXEC_MAX_INFLIGHT`, default
  256). Past it a read is awaited in program order, so a map over a hundred
  thousand urls does not open a hundred thousand sockets.
- Tracing: the builtin's result is recorded when it lands (the recording is
  inside the builtin's own `Ply`); the trace's `seq` is completion order. A
  builtin that combined reads (`List.map`) records its value when the
  combination lands. Under tracing an awaiting builtin's arguments are copied
  before the wait, since the frame's argument buffer is reused once the frame
  runs on.
- Cost when nothing is in flight: one type test per operand on the
  instructions above. The gate is unchanged. A first cut restructured
  `finishBuiltin` around a `match` on the result and cost 200 bytes per
  builtin call (the `uply` arm's closure was built on every call); the check
  moved into `tryUnifySync` instead. Keep it there.

Measured: three reads under `List.map` are all in flight before anything
waits, and the statement after the map runs while they are; two reads in
program order with a write between them: the write runs before either lands;
a failed read raises at `demand` with "after the call" already run; the bound
holds (`Scheduler.Tests.fs`, the reads group).

## `Exec.spawn`, `await`, `select`

`Exec.spawn f` starts `f ()` as a process of its own on a worker (the least
loaded), under the access the caller had at the spawn, like a closure, and
hands back a `Handle<'a>`; `Exec.await h` is the value it finished with, or
its error raised again with the child's frames kept below the caller's;
`Exec.select hs` is the first to finish with its value. `spawn` carries the
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

A builtin that takes a callable used to apply it by running a nested VM on the
host stack (`Execution.executeApplicable`): the lambda's frames were invisible
to `ps`, could not be preempted by the budget, and a read in the lambda held
the .NET stack. The list builtins now ask instead (`Interpreter.requestApply`):

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
  flight and the list comes back as one promise, as before.
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

Migrated: `List.map`, `indexedMap`, `map2shortest`, `fold`, `filter`,
`filterMap`, `findFirst`, `any`, `sortBy` (`Builtins.Pure/Libs/List.fs`), each
with one continuation over two mutable cells rather than a closure per
element. `Dict`, `Option`, `Result` and `String` have no re-entry on this
branch (they are Dark, or take no callable).

Streams (`Stream.unfold`, `map`, `filter`; `Builtins.Pure/Libs/Stream.fs`):
a transform node holds its callable, not a closure over it (`StreamImpl.
Unfold/Mapped/Filtered`), and a pull is a step machine (`Stream.pull`):
`Pulled` an element, `Apply` this callable to this element and continue, or
`Wait` on native IO and continue. The pulling builtin (`next`, `toList`,
`toBlob`) drives it: an `Apply` is a `requestApply`, so the transform runs
as a frame of the pulling process, its answer forced (`withValue`) and
handed back to the pull; a `Wait` is waited for, and a request after it is
the landing case above. The access re-intersection that used to happen on
every pull happens once, when the transform is built: the builder's active
access is folded into the callable (`narrowedBy`), and the frame push
narrows the puller's access by it, as `Apply` narrows any frame's. So a
narrow producer's transform stays narrow under a wide consumer, and the
deferred-execution matrix in `PermissionsGate` still holds. F# code that
owns a native stream (the HTTP client's body, tests) pulls with
`Stream.readNext`, which drives `Wait` and raises on `Apply`: a stream that
runs Dark code is pulled from a Dark process.

Not migrated, and why:

- `HttpServer.fs` (the per-request handler, `onListening`): the handler runs
  on a pool thread through `executeApplicable`. The plan's leaf makes it a
  spawned process on a worker (`Scheduler.SpawnApply` is there for it); left
  for the live track, whose file it is, since it changes `serve`'s latency
  shape and is measured by `scripts/perf/http`.

Measured, the list family: gate 9.5 MB against 9.4 (exact totals 9,463,000
against 9,428,440 bytes: +0.4%, inside the 0.8% noise band; the first cut,
with a record per request and a closure per element, was 11.1 MB, +19%);
`bench ab` before against after, interp-list -1.7% (13 of 15 pairs faster),
eval-listheavy -0.1%, eval-map1000 -0.4%, interp-arith +1.5% (2 of 15;
arith applies no lambda, so that is the bigger step structs or noise).
Tests (`Scheduler.Tests.fs`): a process parked inside `List.map f` shows the
lambda's frame in `ps` and resumes; a tight loop inside a mapped lambda is
preempted and another process runs between the slices; an error inside a
mapped lambda names the lambda's frame; all 6,734 testfile cases pass over
the migrated builtins.

Measured, the stream family: gate 9.5 MB, unchanged (the reference workload
has no stream); `bench ab` before against after, eval-stream (3,000
elements through a map and a filter) -1.8%, 14 of 15 pairs faster;
interp-arith +0.5% (noise). Tests: a process parked inside a stream
transform shows the lambda's frame in `ps` and resumes; a transform over a
stream whose source waits on the host before every element (a test stream
built like a network one) runs as a frame, scheduled and unscheduled; the
stream testfiles and the SSE parser (an `unfold` whose step pulls bytes)
pass unchanged.

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

Trace detail has three levels (`DARK_CONFIG_TRACE_DETAIL`): `off`; `effects`,
the classic rule (only builtin calls with non-empty `callEffects`, with their
ordinals; no frames, no pure calls; the interpreter keeps its fast paths); and
`on`, every call, frame and lambda, the tree `traces view` renders, which
carries the effect log too. The default stays `off` until trace retention
exists; `effects` is what makes a run resumable.

## Executions

A traced `eval` or `run` is an execution (`LibDB.Executions`, the
`executions` table): its input (the expression or the script's source, the
same the trace row stores), its trace, its status (`running`, `done`,
`failed`, `suspended`), and, for a fork, the execution and the position it
branched from. `dark exec` lists them; `exec show`, `exec resume`, `exec fork
[--at <position>]`. `Stdlib.Exec.Execution` is the Dark side (`list`, `get`,
`fork`, `armResume`).

- Ctrl-C during a traced run: the CLI's handler stores the log as it stands,
  marks the execution suspended, prints the resume command and leaves
  (`Cli.fs`, `installSuspendOnInterrupt`; `Executions.Foreground`). A TUI
  reading keys takes Ctrl-C as input and never gets here. A run the suspend
  took out of the foreground stores nothing more if it goes on (a test's does;
  the CLI's has exited).
- `resume`: `armResume` then the same input through the ordinary `eval` or
  `run` path; the script runner takes the armed resume in place of a fresh
  tracer (`Tracing.createReplayTracer`). Every effectful call whose
  `(process, ordinal)` the log has is answered from it, and not performed: a
  replayed `printLine` prints nothing, since the world already saw it. The
  first ordinal a process asks for that the log lacks ends that process's
  replay for good, so nothing later in the log can be handed to it after a
  live call; from there the run is live, still recording, and the stored
  trace ends up as the replayed prefix plus what ran after. The recorded
  process ids are the recorded run's; a resumed run's processes are matched
  to them in the order they first appear in the log, which is the order a
  script's expressions start in. Processes started with `Exec.spawn` may not
  match up; a resume with those is best effort. A run nobody scheduled (a
  plain `execute`, as in the test harness) records and replays under one
  process id.
- `fork`: a new execution with the same input, a new trace holding the
  parent's rows with `seq` below the position (the whole log with no
  `--at`), suspended; resume it and it diverges where the log ends. Cutting
  by `seq` can leave a process's later ordinals without earlier ones, which
  the rule above turns into "live from the first hole".
- Replay after a package edit: the input is re-parsed, so names resolve to
  the new code, and the effects come from the log: the new pure code runs
  against the old I/O. Live's H9 (live values) can start from this.
- The determinism audit (every pure builtin, run twice on the same inputs,
  must agree): a scan of every builtin declared with no effects for the
  nondeterministic APIs (guids, clocks, random, environment, hash codes,
  unordered enumeration) finds three, all host facts read live and not
  recorded, by design: `cliTerminalColorEnabled` (the terminal's colour
  support), `interpreterStatsEnableDetailedTiming` and `interpreterStatsGet`
  (dev instrumentation). A replay in another terminal renders for that
  terminal. Dark's dict is an ordered map, so enumeration is deterministic.
  `uuidGenerate` declares `Random` and is in the log.

Tested (`CliExec.Tests.fs`): a run is kept and `resume` gives the same two
uuids; a fork at position 1 keeps the first uuid and makes the second afresh,
and `show` names its parent; a run suspended mid-way (its log has the first
uuid only) resumes with that uuid answered from the log and the rest live,
and the interrupted run's own ending leaves the suspend alone; a package edit
between record and resume runs the new code (`v2:`) against the recorded
uuid.

## Host operations are requests: a builtin names, the loop performs

A builtin that touches the OS used to call `PermissionCheck.performHost` from
inside its `uply` body: the check, the wait and the result were all inside a
builder the loop could only park on as an opaque task. It names the operation
instead (`Interpreter.requestHost vm op next`, in `Builtins.Cli`: `File`,
`Directory`, `Environment`, `Execution`, `Posix`):

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
- Denials and rejections raise at the call as before: the check runs on the
  loop's thread, before anything is performed, under the same access the body
  ran with.

The host boundary itself (`Host.perform`: resolve, check, execute, audit) did
not move. What moved is who calls it: the loop, from one line, for every
OS-facing builtin, which is the shape the Rust port wants (an operation is a
value the host answers) and what lets `ps` name the wait.

## `Host.await`, the contract

```
module Darklang.Stdlib.Host
type EventSpec = Key | StoreChanged | Timer of ms: Int64 | ExecDone of id: Uuid
type Change = Unknown
type Event = Key of KeyRead | StoreChanged of Change | Timer | ExecDone of id: Uuid
let await (specs: List<EventSpec>) : Event
```

Under the scheduler the calling process parks on the first spec to fire.
Outside one (a plain `execute`) it blocks the thread polling, which is what the
live track's shim does today and what the rebase deletes. `Builtin.hostAwait`
declares `{Stdin; PackageRead}` statically, the union of what any spec could
need.

`readKey ()` is unchanged for users; under the scheduler its body is "park on
`[Key]`, return the key".

## Entry points

- `Cli.fs` `main`: the entry function is the root process of a fresh scheduler
  that runs on the main thread until it finishes. `DARK_SCHEDULER=off` is the
  escape hatch back to a plain run.
- `cliParseAndExecuteScript`: each expression is a child process of the CLI's,
  awaited in order. So a script budget-yields, a `readKey` in it parks, and
  `ps` lists it. Daemons are launched as `eval` and ride the same path.
- `execute` everywhere else, unchanged.

## `dark ps`

`Stdlib.Exec.list/inspect/kill` over `Builtin.execList/execInspect/execKill`
(`Builtins.Language/Libs/Exec.fs`), rendered by `cli/ps.dark`. Rows are copies;
nothing hands Dark a reference into a running VM. A process on a worker is
snapshotted from another thread: its call stack is read best-effort (a frame
popped under the read comes back as no frames, never a fault). One group per
OS process, so `dark ps` from a shell is the CLI alone; from inside an `eval`
it is the CLI parked on `cliEvaluateExpression` plus the expression's process,
with `ps show` giving both call stacks.

`ps kill` sets a flag the process sees at its next turn; a parked process is
given that turn at once and whatever it waited for is abandoned. A running
process finishes its slice first, so a short program that kills itself
completes.

## The scheduling policy, in Dark

Which runnable process a scheduler steps next is round robin: the one that
has waited longest. That is F#, and the default. A store can name a Dark
function instead:

    dark config set exec.policy Darklang.Stdlib.Exec.Policy.youngestFirst

(`DARK_EXEC_POLICY` overrides for one run.) The function takes the runnable
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

## Not here yet

Follow-ups in the scheduler plan, in order, and the edges of what is here:

- The HTTP server's per-request handler runs as a process (live's change);
  `LiveValues.fs` still applies a callable through `executeApplicable`, on a
  VM of its own, since it runs a function for inspection rather than as part
  of a program.
- A policy chooses which runnable process to step, not where a spawn lands:
  `Exec.spawn` still goes to the least loaded worker, in F#.
- A resume matches recorded processes to new ones by start order; a run that
  spawned may not line up. `resume` is the CLI's, since it runs the input
  through the CLI's own paths; `Exec.fork` from Dark exists.
- `ps show` says how many reads a process has in flight, not which.
- A builtin's wait is still a `Ply` the loop parks on as a task, and a host
  operation's answer comes back through that task rather than as an event on
  the queue. The loop itself is plain code (`executeSync`, `awaitOf`,
  `driveToEnd`); the `uply`s left in `Interpreter.fs` are the slow paths (a
  type check that needs the store, a builtin's result landing). The HTTP
  client and server still perform their operations from inside the body;
  store-facing builtins (`DB`, the package manager, traces) await SQLite,
  which is not a host operation and has no request form yet.
- `Event.ExecDone` carries only the id; a Dark enum cannot hold an untyped
  value. `Exec.await` is how a value comes back.
- `ps show` shows the call stack, not registers.
- The reader thread has not been checked on Windows.
