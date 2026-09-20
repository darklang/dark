# Processes and the scheduler

Status: the baseline. A running computation is a value the runtime can step,
park, resume and inspect, and one thread runs many of them. Nothing
user-visible changed except `dark ps`; the follow-ups at the end are where
the rest goes.

The one-paragraph version: a process is a `VMState` plus the `ExecutionState`
it runs under plus a status. The scheduler steps a process until it finishes,
has to wait for something, or spends its instruction budget. A waiting process
is parked on the task it waits for; when that completes, an event lands on the
scheduler's queue and the scheduler thread resumes the process. A preempted
process goes to the back of the line. Keys, timers and store changes arrive on
the same queue, so `readKey` parks instead of holding the thread.

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

Only the scheduler thread steps. Everything else (the reader thread, timer
callbacks, the store poll, Ply continuations) only posts to the queue. `Step`
checks the thread id and raises if it is ever wrong.

The one exception is documented at `StepOutcome`: a rare opcode's deferred
completion writes the VM it belongs to, on whatever thread completes it. The
process is parked meanwhile and nothing looks at its VM until the completion
has posted, so it is exclusive, not shared.

## The event queue and its sources

`LibExecution/HostEvents.fs`. One `Queue` per scheduler:

- `Key of KeyRead`: from the stdin reader thread. It starts on the first
  `Key` subscription and reads one key per request, so a run that never waits
  on a key never touches the console, and nothing eats keys meant for a
  `readLine` after a TUI has quit. Redirected stdin never starts it:
  `readKey` answers Escape at once, as it always did.
- `Timer id`: a one-shot `System.Threading.Timer` armed per `Timer ms` spec,
  disposed when something else satisfies the subscription; a late fire posts
  an id nobody wants and is dropped.
- `StoreChanged change`: a 200 ms poll of `PRAGMA data_version` on a
  connection of its own (the pragma answers per connection). `change` is
  `Host.Change.Unknown` until the live track's `scmOpsSince` can say what
  changed. Latched per process: a process that subscribes after a change it
  has not been told about is woken at once, so a change during a render is
  not lost.
- `Completed pid`: internal; the parked task finished.
- `ExecDone (pid, dv)`: a process finished, for Dark subscribers.

The sources `LibExecution` cannot provide itself (the console, the store) are
installed by the host: `Stdin.fs` installs the key source, `Cli.fs` the store
version.

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
(`Builtins.Language/Libs/Exec.fs`), rendered by `cli/ps.dark`. Rows are copies
taken on the scheduler thread; nothing hands Dark a reference into a running
VM. One scheduler per OS process, so `dark ps` from a shell is the CLI alone;
from inside an `eval` it is the CLI parked on `cliEvaluateExpression` plus the
expression's process, with `ps show` giving both call stacks.

`ps kill` sets a flag the process sees at its next turn; a parked process is
given that turn at once and whatever it waited for is abandoned. A running
process finishes its slice first, so a short program that kills itself
completes.

## Not here yet

Each of these is a follow-up in the scheduler plan, in this order:

- Implicit concurrent reads, `demand`, `Exec.spawn` for users. Today nothing in
  Dark can start a second process; two evals interleaving is shown by
  `Scheduler.Tests.fs`, not by anything you can type.
- Cores: per-process `ExecutionState` copies. One scheduler thread today.
- Record/replay, resume after Ctrl-C, fork. The `traceId` link is not on the
  process yet.
- Removing host re-entry. `List.map f` still runs `f` in a nested VM on the
  .NET stack; a process parked inside it shows the frame that called the
  builtin, not `f`'s.
- Ply out of the interpreter. Awaits are still Plys, parked on as tasks.
- `Event.ExecDone` carries only the id; a Dark enum cannot hold an untyped
  value, and nothing spawns from Dark yet.
- `ps show` shows the call stack, not registers.
- The reader thread has not been checked on Windows.
