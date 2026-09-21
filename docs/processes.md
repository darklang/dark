# Processes and the scheduler

Status: the baseline, cores, and concurrent reads. A running computation is a
value the runtime can step, park, resume and inspect; one thread runs many of
them, and a group of worker threads (one per core) runs many more. Reads run
concurrently on their own and writes keep their order; `Exec.spawn`/`await`
run chosen work in the background. The follow-ups at the end are where the
rest goes.

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
  register and is run on the scheduler thread when the process's turn comes.
  For the rare opcodes and the deferred return-type check, `wait` is the
  existing `handleFrameStep` task, which advances the VM itself as it completes,
  and `resume` does nothing.

It reuses `executeSync` and `handleFrameStep` as they were; `execute` (tests,
the LSP, the HTTP server's per-request handlers) is unchanged and never sees a
budget bail.

## The budget

`runSyncInstructions` counts `vm.budget` down by one per instruction and stops
at zero; `runFrame` reports that as `FrameBudget`. The default quantum is
10,000 instructions (`Scheduler.defaultQuantum`), refilled before every slice.
A negative budget means unlimited, which is what every VM nobody schedules runs
with, including the VM a builtin borrows to apply a lambda: a process parked
inside `List.map f` is parked as one Ply, not preempted inside `f`.

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
(a file, env, db, package or trace read, the clock, random; an HTTP GET or
HEAD) that has to wait does not stop your program. You get its result back at
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
  HEAD, per call. A read that finishes synchronously (the clock, most file and
  db reads in this runtime) is never a promise; only a real wait is.
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

## Traces

`trace_fn_calls` rows carry `process_id` and `seq` (`migrations/schema/
08-traces.sql`; existing stores get the columns from `LibDB/Releases.fs`,
with `''` and `0` for old rows). `seq` is assigned as calls complete, under
the tracer's lock, across every process writing the trace: one process's rows
in `seq` order are its log, all of them are the interleaving. `Tracing.FnCall`
in Dark carries both (`processId : Option<Uuid>`, `seq`). A run nobody
scheduled writes `''`. The executions step reads these back for replay.

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

## Not here yet

Each of these is a follow-up in the scheduler plan, in this order:

- Record/replay, resume after Ctrl-C, fork. The `traceId` link is not on the
  process yet; the `(process_id, seq)` it will replay from is.
- `ps show` says how many reads a process has in flight, not which.
- Removing host re-entry. `List.map f` still runs `f` in a nested VM on the
  .NET stack; a process parked inside it shows the frame that called the
  builtin, not `f`'s.
- Ply out of the interpreter. Awaits are still Plys, parked on as tasks.
- `Event.ExecDone` carries only the id; a Dark enum cannot hold an untyped
  value. `Exec.await` is how a value comes back.
- `ps show` shows the call stack, not registers.
- The reader thread has not been checked on Windows.
