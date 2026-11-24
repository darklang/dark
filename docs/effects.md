# Effects and permissions

Effects describe what code might do. Permissions decide what it may do when it
runs. Both matter whether we write the code ourselves, use a library, or ask an
AI agent to write it: we can see which files code might read or change, where it
might send requests and which programs it might start, and then decide what to
allow. When Dark cannot work something out, it says so.

Access comes from three places:

- instance rules, the defaults for your Dark installation;
- a saved approval for the current version of a function or script;
- grants for one run.

You control all three through the CLI. Code running under a restriction cannot
approve its own requests.

## Effects: what code might do

An effect describes something code might do, such as reading a file or making an
HTTP request. If your function calls another function that reads a file, your
function has the FileRead effect too.

- Http: send requests, such as fetching GitHub issues.
- HttpServer: run a web server, such as a local site on localhost:9099.
- FileRead: read files or look inside folders.
- FileWrite: create, append to, overwrite or delete files.
- EnvRead: read environment variables.
- EnvWrite: set or change environment variables.
- Process: start external programs and communicate with them.
- TraceRead: read recordings of earlier runs.
- TraceWrite: write, change or delete those recordings.
- Concurrency: start Dark background work with `Stdlib.Exec.spawn`. Shown, but not a
  permission: that work keeps the restrictions of the code that started it.
- Native: use access Dark cannot limit to one resource, such as stopping a process by its ID.
- Clock: read the time or use timers.
- Random: generate random values.
- Stdin: read the input supplied to the run.
- Stdout: write output.
- PackageRead: read code and information from the local package store.
- PackageWrite: change the local package store.
- Runtime: refresh Dark's view of stored code and watch for edits.

Clock and Random are allowed by default but still tracked: the same inputs can
give different results. A deadline check can pass now and fail later, and a
generated password should differ each time. Dark accounts for this before
reusing a saved result or an earlier test pass.

An effect says what code may do, not what it did on a given run. FileRead means
the code may read a file; it still needs permission before it does.

### How Dark finds effects

`dark effects Me.reports.report` shows a function's effects, a call path for each,
and whether the analysis is complete. It does not run the code.

Every builtin declares its `callEffects`. The analysis follows the immutable call
graph from there: possible branches, callbacks it can identify, the functions
behind operators and trait methods, and returned code. Recursive and mutually
recursive functions are handled. Call discovery records argument sources and
possible return values alongside each call, and requirements and explanations
follow these summaries without walking the source again. When a call cannot be
resolved, Dark shows the effects it knows about and marks the result incomplete.
Incomplete analysis never grants the unknown effects.

The list is conservative. If a function returns another function that reads a
file, the result includes FileRead even when the returned function is never
called; the same goes for streams whose values are never read. The list is review
information, not an authorization check.

For `syscallOpen`, the analysis carries small sets of possible integer arguments
through bindings, wrappers and return values. Known read-only flags contribute
only FileRead; create, truncate and append flags also need FileWrite, and unknown
flags keep both. Branches are combined without running code, and value sets and
call contexts widen to unknown when they grow too large.

### Raise

`Raise` marks a builtin that can stop with a runtime error on arguments of its
declared types: dividing by zero, an argument out of range, `unwrap` of `None`.
It is information, not a permission. No rule grants or denies it, it reaches no
gate, it is not logged, and a call whose only effect is `Raise` stays pure. It
flows through the call graph like any effect, so `view` says when a function can
raise and `effects` shows a call path to the raise; permission requirements,
approvals and contract comparisons leave it out. Some stdlib functions test for
every case where what they call raises, such as `Int64.checkedDivide`; they are
listed in `LibDB.Raises.cannotRaise`, and the path does not go through them.
`Builtin.Tests.raisingBuiltinsDeclareRaise` holds every builtin with a raise site
in its source to it.

`Concurrency` marks `Exec.spawn`: the call starts a process that runs beside this
one. It is information, not a permission, like `Raise`. The child runs under the
spawner's access, so a grant would give it nothing. `view` and `effects` show it;
policies, prompts and approvals leave it out. A saved rule that names it is
ignored.

## Permissions: what code may do

A new installation allows these without asking:

- reading terminal input and printing output;
- reading the time, using timers and generating random values;
- reading the local package store;
- reading and writing traces.

You can remove or deny any of these. Files, HTTP, servers, environment variables,
external programs, package writes, Runtime and Native all need a grant.

Instance rules work like this:

- Allow gives access without asking, unless a deny rule or run restriction blocks it.
- Deny blocks access, even when another rule or an approval allows it.
- With no matching rule, Dark checks for an approval for this code or a grant for
  this run. If there is none, it asks you; if it cannot ask, the run stops with an
  error naming the missing access.

Package values evaluate without effects: a saved value never performs an
operation when it is loaded.

### What happens when you run code

You can write, edit and typecheck code without any permission. At an interactive
terminal, `run` can ask about missing access. `eval`, server handlers and runs
without a terminal report a denial with a suggested rule instead of asking.

Before the body starts, `run` works out the access it can from the code and asks
about all of it in one prompt. This preflight follows calls that always happen,
with arguments written in the code, and conditions it can resolve from them. It
stops at unknown branches and does not run callbacks or deferred bodies. Requests
that depend on computed paths, unknown conditions, callbacks or later work are
checked just before the operation, with a prompt of their own, so one run can
ask more than once.

Suppose `Me.Reports.report` wants to read `/reports/january.csv` and has no access
yet. The prompt offers:

- **Allow this run**: read the file this time.
- **Remember for this code version**: also allow future runs of this function, at
  this version and with these dependency versions, to read it. Instance rules are
  unchanged.
- **Cancel**: allow nothing and save nothing.

Approving during a run continues the operation from where it paused; earlier
work is not repeated. Interactive runs wait on reads in order, so later effects do
not run past a pending question; runs without a terminal keep concurrent reads.
Cancelling before the body starts means it never starts. Cancelling later stops
the run; changes already made, such as a written file, are not undone. Explicit
denies and run restrictions cannot be overridden from a prompt. A denied operation
in a server handler fails that request; the server keeps running.

Prompts capture the complete policy before you review it. If it changes before
saving, the approval is refused and you review again.

`run --allow 'file read /reports' report.dark` grants access for that run only.
Grants are not saved in traces, so `traces resume` and `traces fork` check
permissions again. `--sandbox` adds a deny-all run restriction; it is not an OS
sandbox. There is no warning-only mode.

### Approved code calling other code

The first function that is not part of Dark's own bundled packages sets the run's
access. An approval covers that function and the code it depends on: its helpers
use the access while working for it, and get no approvals of their own. Running a
helper on its own is a separate decision; it does not inherit the approval of a
function that once called it. A helper's own approval cannot give an unapproved
caller more access. Bundled helpers and a script's own functions use the run's
access, and bundled code has no exemption from Native checks.

Callbacks, streams and background work keep the restrictions they had when they
were created, and are also limited by the code that later calls or reads them. A
trait implementation the caller picks, by a type argument or the value it passes,
runs with the caller's access, like a callback. Nested evaluation and trace replay
keep the caller's restrictions. Whole-store backups need the CLI itself.

A saved value that refers to a function keeps the name it resolved to, not any
permission to use it. Loading it later cannot restore access from an earlier run.

### Rules

**Files.** A rule covers one file, or a folder and everything under it.

- `/reports/january.csv` covers only that file.
- `/reports` includes its subfolders, but not `/reports-private`.
- Read access allows no changes. Write access allows creating, appending,
  overwriting and deleting; there is no separate delete permission.
- Dark resolves every linked folder in a path before checking it, so a link inside
  `/reports` cannot reach `/private`, and a rule for `/tmp/x` on macOS also covers
  `/private/tmp/x`. A link that cannot be resolved is refused. On Linux and macOS,
  a link swapped in between the check and the operation is refused too; Windows
  relies on the check alone.
- `~/.darklang/policy/` is refused under any spelling, even with every file allowed.
- SQLite databases are files too. `Darklang.Sqlite.query` needs read access to the
  database file, and `Darklang.Sqlite.execute` write access. SQLite's own side files
  (`-wal`, `-shm`, `-journal`) need no rule of their own. The SQL can't reach any other
  file: `ATTACH` and `VACUUM INTO` are refused, and so is Dark's own store.

**Environment variables.** Rules name the variables code can read or change.
Reading `APP_MODE` does not allow changing it or reading others. Denying any
variable also blocks listing all of them, so code cannot get around the deny.

**HTTP.** Rules choose the method, protocol, host, port and path.

- `example.com` excludes subdomains; `*.example.com` includes the parent domain
  and all its subdomains.
- `GET` does not allow `POST`, `https` does not allow `http`, and one port does
  not allow another.
- `/orders` allows only that path. `/orders/*` also allows paths below it, but not
  `/orders-old`. No path allows every path on the host. Prompts and suggested
  rules use exact paths.
- A rule without a query allows any query.
- Each redirect needs permission for its own URL.
- Code cannot set the `Host` header; the checked URL decides where the request goes.
- Rules and requests use the same URL normalization, including Unicode hosts,
  escaped paths and queries.
- Loopback and private addresses also need the `http.loopback` or `http.private`
  setting (see below).

**Servers.** Rules choose the address and port a server can listen on.

- `http-server 127.0.0.1 9099` allows local connections on port 9099.
- `http-server * 9099` allows any address on that port.
- `localhost` means `127.0.0.1`; other names are not looked up.
- A denied address also blocks binding all interfaces on that port.
- `serve` listens on `127.0.0.1`; `--bind 0.0.0.0`, with a rule for it, listens on
  all IPv4 interfaces.

IPv6 binds are not supported, and Windows refuses address-specific binds because
its server backend cannot enforce them.

**External programs.** Rules name an executable by its full path and can limit its
arguments.

- `process /usr/bin/python3` allows any arguments.
- `process /usr/bin/python3 report.py` allows only that argument list.
- `process-exact /usr/bin/python3` allows no arguments.

The executable is resolved before the check, and arguments keep their order. These
rules control starting the program, not what it does afterwards.

**Traces.** `trace-read` allows reading every recording in the local store;
`trace-write` allows writing, changing or deleting them. There is no rule for one
recording.

### Saved approvals

An approval records the function's full name and its current version. Two names
with identical code have separate approvals. Dependencies are part of the
version, so editing the function or anything it depends on means new runs no
longer have the old approval; instance rules and run grants still apply. Running
a name always uses its current version on the selected branch; an approval never
pins code. Runs already going keep the code and policy they started with.
Scripts are listed as `script:/absolute/path`, and editing a script ends its
approval in the same way.

Explicit rules may be narrower than the analyzed effects. A read-only approval is
accepted even when a general-purpose helper could also write; if the run actually
asks for the missing access, it stops there.

- **Remember**, or `permissions approve --for <fn> <rules>`, adds to the saved
  access for the same version, keeping explicit denies. Rules are required:
  without them `approve` says "No permission rules specified" and saves nothing,
  even with `--yes`, and for an effect-free function says that no approval is
  needed. Repeating an approval with the same version, rules and analysis says
  "Already approved; no changes" and writes nothing.
- `permissions remove --for <name> <rules>` removes exact rules from a saved
  approval and leaves its other rules and version alone. If any rule is missing,
  nothing is removed. A rule in both the allow and deny lists goes from both.
- `permissions remove --for <name> --all` forgets the name's whole approval.
  `--all` cannot be combined with rules; a plain `all` removes only a saved `all`
  rule. `permissions unapprove <name>` is the same as `--all`.
- `permissions update <fn>` reviews access for the current version. It shows the
  previous rules, the changes and the resulting access before one confirmation.
  Explicit rules carry over and new rules add to them; inferred effects are
  recomputed for changed code. Declining saves nothing, `--yes` skips the
  confirmation, and an unchanged approval returns without prompting or writing.
- `permissions revoke-package <hash>` removes every named approval for that version.

Removing an approval affects new runs. Interactive runs also check current
instance deny rules, so a new deny can block their next operation. Removing an
approval does not take away access other rules give; use a deny rule for that.

If the analysis is incomplete, give explicit rules to bound access. Dark does not
guess what the unknown effects need.

### Managing permissions

- `permissions list` shows instance allow and deny rules.
- `permissions approved` lists saved approvals by name and version.
- `permissions show <fn>` shows a function's effects and saved approvals.
- `permissions requirements <fn>` shows the access a function may need.
- `permissions allow`, `deny` and `remove` edit instance rules; each takes several
  rules separated by `;`, and saves none if any is not a rule.
- `permissions set` replaces all instance rules; `permissions clear` removes them
  and leaves saved approvals.

```text
dark permissions allow 'file read /reports'
dark permissions deny 'file read /private'
dark permissions approve --for Me.reports.report 'file read /reports'
dark run --allow 'file read /reports' report.dark
dark permissions remove --for Me.reports.report --all
```

### Profiles

Profiles are ready-made instance rules:

- `default` restores the installation defaults.
- `read-only <ROOT>` allows reading that directory and HTTPS GET requests to any
  host on port 443, plus the basics such as terminal input and output.
- `local-dev <ROOT>` also allows writing in that directory, changing packages,
  following code edits and serving on `127.0.0.1` only.

No profile grants processes, Native or environment access. `permissions profile
local-dev /home/me/project` previews a profile; add `--yes` to apply it. Applying
one replaces the whole instance policy, including deny rules.

### HTTP to loopback and private addresses

Guest code reaches neither loopback nor private addresses by default.
`http.loopback` allows loopback, and `http.private` allows RFC 1918, RFC 6598
shared space and IPv6 unique-local addresses. Both are off until set by name and
widen nothing else. A setting is needed but not enough: the run still needs a
grant for the URL, so a missing grant is a raise where a missing setting is
`Error(BadUrl(InvalidHost))`.

Only the person running Dark sets them, with `dark config set`, or
`DARK_CONFIG_HTTP_LOOPBACK` / `DARK_CONFIG_HTTP_PRIVATE` for one environment.
`configSet` refuses `http.loopback`, `http.private` and `trace.record`
(`LibDB.Config.personOnly`) unless the CLI itself is the caller
(`canManagePolicies`), the same line policy edits use. They are settings rather
than rules so that `permissions allow all` does not open them too.

**A configured proxy is trusted.** HTTP clients use the configured proxy
(`HTTP_PROXY`, `HTTPS_PROXY`, `ALL_PROXY`, `NO_PROXY`). The guard checks the
destination, not the proxy:

- Dark connects to the proxy's own host and port even on a loopback or private
  address, without either setting.
- A URL whose host is a blocked address literal, `localhost` or a metadata name,
  and a metadata request header, are refused before a proxy is chosen.
- A destination given by name is resolved by the proxy, so Dark never sees the
  address it lands on and cannot refuse it.
- A request the proxy does not take (`NO_PROXY`) connects directly and is checked
  as usual. Every URL still needs a grant.

For `http://` the proxy sees everything: the URL, every header (`Authorization`
and cookies included) and both bodies. For `https://` it sees the host and port,
when, and how much goes each way, but not the path, headers or bodies, unless it
is an intercepting proxy whose certificate this machine trusts. Configure only a
proxy you would trust with that.

## Where permissions are saved

Instance rules and named approvals live in `~/.darklang/policy/policies.bin`,
written atomically: a change is saved whole or not at all. Each approval holds
the version, its rules, whether you gave them or Dark inferred them, and a
fingerprint of the analysis and builtin effect declarations. A changed
fingerprint needs a new review before the approval is used. The file holds
current decisions, not a history.

Only the current format is read. A missing, damaged or unsupported file grants
nothing and is not replaced; move it aside and set up a fresh policy. Only a new
installation gets the default rules.

## The checked host boundary

Every host action goes through one door, `LibExecution.Host`. A builtin never
touches the OS itself: it builds an `Operation`, the full description of one host
action, and hands it to `Host.perform` or the execution-scoped
`Host.performInSession`. That function derives the exact permission `Request`
from the same value, checks it, and only then performs it, so what is checked and
what is done cannot differ, and a builtin cannot skip the check. Each operation:

1. builds and validates the `Request` from the `Operation`;
2. checks it against the run's access;
3. performs the host operation;
4. records a structured audit event;
5. returns a structured result.

The host modules live in `LibExecution/Host/` and compile before
`RuntimeTypes.fs`; they depend on the effect and policy vocabulary, not on VM
state. Filesystem, directory, environment, process and HTTP client builtins go
through them. The process table (`LibExecution.HostProcess`) and the HTTP
machinery with its streaming-response table (`LibExecution.HostHttp`) are owned
by the host. An HTTP operation names a profile; there is one, `Guest`, the
host's configured client with its timeouts and address guards
(`HostHttp.setGuestConfig`).

Kernel primitives use the same door, through `HostPosix` and `HostKernel`. Path
operations check exact read or write requests. Descriptors opened under those
checks come back as unguessable tokens owned by the execution; every use checks
ownership, open mode and the run's current access, and a lease stops a close or
reuse from redirecting a call in flight. Raw OS descriptor numbers and arbitrary
PIDs need `Native`, and an owned token cannot be turned into an OS descriptor.
Tokens from another execution, and unknown or closed tokens, name nothing.
Starting a child checks the resolved executable and ordered arguments, and later
child and pipe operations check that grant. A blocking wait on a child releases
its lease first, so cleanup can still stop the child.

Returned working-directory and executable paths are authorized as reads of the
actual answer. Reading account files is an ordinary checked file read, and the
home-directory fallback also authorizes the candidate path. Host facts that name
no resource, such as the process ID, need no grant; arbitrary sysctl keys and
another process's affinity need `Native`. An HTTP server bind returns a listener
handle only after the exact address and port are allowed and the bind succeeds.
Temporary-file operations authorize the containing directory, because the new
name does not exist yet at check time. Entry operations such as `readlink` may
inspect a final link without following it.

Dark code has no raw SQL. The package store is read and changed only through the
typed package and SCM builtins, which declare PackageRead and PackageWrite.
Reading persistent bytes from the host-owned blob store is loading a value, not a
call, so it has no effect.

`HostBoundary.Tests` scans the compiled IL and fails when OS APIs that open a
resource (filesystem, process, HTTP client, HTTP listener, environment
variables) are referenced outside the host modules. Two exceptions are allowed:
terminal capability checks, which must work under deny-all to draw the
permission prompt itself, and the `PackageRefs` startup read. A stale exception
also fails the test.

## Where the protection ends

- This is runtime authorization, not OS isolation. External programs and Native
  operations have the OS access of the user running Dark, and Dark does not
  contain a compromised interpreter or limit resource use.
- `--sandbox` blocks permission-checked operations, but does not isolate
  external programs or Native operations.
- An agent with an unrestricted shell can change Dark's settings just as you can.
- Reading permission settings needs Native access.
- Code allowed to read a private file and to contact a website can send the
  file's contents there.
