# Effects and permissions

Effects describe what code might do. Approvals allow access. Explicit restrictions
always win. Author-declared ceilings and warning-only enforcement have been removed.

## What is checked

A protected operation needs an instance default grant, a temporary run grant, or a
remembered approval for the named entry point. An explicit instance deny or a run
restriction blocks it regardless of grants. Every actual path, URL, environment
variable, executable and argument list is checked at the host boundary.

The first non-bundled function establishes the invocation's approval. Its helpers
inherit that access; they neither need separate approval nor acquire their own
remembered grants. Calling an approved function from an unapproved library cannot
increase the caller's access. Bundled helpers and the script's own functions use
the invocation's access. Closures, deferred streams and background work retain
captured restrictions and are also bounded by the caller that executes them.
Bundled code has no exemption from Native checks. Nested evaluation and trace replay
retain the guest caller's restrictions. Whole-store backups require the trusted CLI.

Package values still evaluate without effects: a saved value must not perform an
operation when it is materialized. This language rule is not an author ceiling.

## Approvals

`permissions approve Me.report 'file read /reports'` remembers those rules for
that full name and immutable version. Two names with identical code have separate
approvals. Dependencies are part of the immutable code identity, not separate
mutable policy entries. Running a name always uses its current version on the selected branch; approval
never pins code. Edits to the function or its dependencies invalidate its remembered
access for new runs. Instance defaults and temporary run grants still apply.
`permissions update` reviews access for the current version; existing runs retain
the code and policy captured when they started. `permissions unapprove Me.report` removes that
name's approval. Revoking a hash explicitly removes all named approvals for it.
Approval and update prompts capture the complete policy before review. If it changes
before saving, the approval is refused and must be reviewed again. Updating unchanged
code can replace its rules or refresh its analysis fingerprint.

Explicit rules may be narrower than the analyzed effects. That is useful: a
read-only approval should be accepted even when a general-purpose helper can also
write. If execution actually requests the missing access, it stops there.
Incomplete analysis never automatically grants unknown effects.

`run --allow 'file read /reports' report.dark` grants access for that run only.
The trusted CLI accepts these grants; guest code cannot approve itself. Grants are
not saved in traces, so resume and fork check fresh permissions. `--sandbox` adds
a deny-all run restriction; it is not an OS sandbox. There is no warning-only mode.

At an interactive terminal, `run` reviews exact, statically known missing access
in one prompt before executing the body. Choose **Allow this run**, **Remember for
this code version**, or **Cancel**. Remember saves access for this entry and its
reviewed dependency versions, without changing instance defaults. Scripts are
listed as `script:/absolute/path`; use `permissions show` or `unapprove` with that
name to inspect or remove the approval. Editing the script invalidates its review.

Access discovered later prompts immediately before the protected operation.
Approval continues that operation without replaying previous work. Interactive
runs await reads in order so later effects do not run past a pending question.
Unattended runs retain concurrent reads. Cancellation
stops the run; completed writes are not undone. Noninteractive runs fail with a
suggested rule instead of waiting for input. Explicit denies and mandatory run or
value restrictions cannot be overridden by a prompt.

`process-exact /absolute/executable` permits a process with no arguments;
`process /absolute/executable` permits any arguments. Both forms can include an
exact argument list. HTTP rules with no query permit any query on the allowed path. A written HTTP
path is exact: `/orders` does not allow `/orders/123`. Use `/orders/*` to allow
that path and its children, or omit the path to allow every path on the host.
Approval prompts and suggested rules use exact HTTP paths. Rules and requests use
the same URL normalization, including Unicode hosts, escaped paths and queries.
Custom `Host` headers are rejected; the checked URL determines the HTTP authority.

Denying read access to any environment variable also blocks listing all variables.
Individual reads of other allowed variables still work.

Server rules name both an address and a port: `http-server 127.0.0.1 9090`.
`localhost` means `127.0.0.1`; addresses are literal IPv4, never DNS names.
`http-server * 9090` permits any bind address on that port. A denied address
also prevents binding all interfaces on the same port. `serve` defaults to
`127.0.0.1`; `--bind 0.0.0.0` explicitly requests all IPv4 interfaces.
The local-dev profile permits only loopback serving. Address-specific binds
fail closed on Windows because its HttpListener backend cannot enforce an
interface-specific socket bind. IPv6 server binds are not supported yet.

## Analysis

Every builtin declares `callEffects`. Requirements analysis follows the immutable
call graph, callbacks it can identify, operator and trait implementations, and
returned code. It reports the effects found and whether analysis is complete.
Requirements are conservative review information, not an authorization check.
Use `effects Me.reports.report` to see the effects and a call path for each.
Incomplete analysis is labeled explicitly. This conservative effect list includes
returned code. A separate preflight walks unconditional calls with known literal
arguments; it stops at unknown branches and does not execute callbacks or deferred
bodies. It retains the named entry's approval context, just as execution does.
Anything preflight cannot establish is checked at the protected operation.

`Raise` marks a builtin that can stop with a runtime error on arguments of its
declared types: dividing by zero, an argument out of range, `unwrap` of `None`.
It is information, not a permission. No rule grants or denies it, it reaches no
gate, it is not logged, and a call whose only effect is `Raise` stays pure. It
flows through the call graph like any effect, so `view` says when a function can
raise; permission requirements, approvals and contract comparisons leave it out.
`Builtin.Tests.raisingBuiltinsDeclareRaise` holds every builtin with a raise site
in its source to it.

## Storage

The protected, atomic policy file is `~/.darklang/policy/policies.bin`. Named
approvals use one record per name, containing the version, rules, their explicit or
inferred origin, and analysis/builtin fingerprint; a changed fingerprint
requires re-review before those grants can be used. Missing or damaged policy
files grant nothing and are not silently replaced.

Only the current policy format is supported. Startup does not back up, convert,
or reset existing policy files. An unsupported file grants nothing; move it aside
and configure a fresh policy to replace it.

Stored programs use only the current binary format. Ceiling metadata and readers
for older formats have been removed. Runtime function references retain the
resolved name without saving permission grants.

## Checked host boundary

The host modules live in `LibExecution/Host/` and compile before
`RuntimeTypes.fs`; they depend on the effect and policy vocabulary, not VM state. This keeps the dependency direction suitable for a later assembly
split. Today the separation is enforced by F# compile order and the compiled IL scan below. A separate assembly could also hide subsystem implementations
behind `internal`; it would need to take the shared effect/policy types with it so the interpreter can depend on the host without a cycle.

The boundary is one door: `LibExecution.Host`. A converted builtin never
touches the OS; it constructs an `Operation` — the full description of one
host action — and hands it to `Host.perform` or the execution-scoped
`Host.performInSession`, which derives the exact
permission `Request` from that same value, checks it, and only then executes.
The checked thing and the performed thing therefore cannot diverge, and
forgetting the check is unwritable at builtin call sites. The filesystem,
directory, environment, process, and HTTP client builtins route through it;
the host-owned process table (`LibExecution.HostProcess`) and the HTTP
machinery with its streaming-response table (`LibExecution.HostHttp`) live
behind it. An HTTP operation names a profile, of which there is one: `Guest`
(the host's configured guest client, `HostHttp.setGuestConfig`: timeouts and
SSRF guards), so configuration and client objects stay in host-owned state.
The DU stays a DU so that a second configuration does not have to widen every
call site again; there WAS a second, reachable only from an all-bundled call
chain and only to reach a peer on loopback or a private range, and it left with
the code that used it.

Guest code reaches neither by default. `http.loopback` widens loopback and
`http.private` widens RFC 1918, RFC 6598 shared space and IPv6 unique-local;
both are off until set by name, and widen nothing else. A key is necessary and
not sufficient: the invocation still needs a grant for the URL, so a missing
grant is a raise where a missing key is `Error(BadUrl(InvalidHost))`.

Only the person running Dark sets them: `dark config set`, or
`DARK_CONFIG_HTTP_LOOPBACK` / `DARK_CONFIG_HTTP_PRIVATE` for one environment.
Code Dark runs cannot, whatever it was granted. `configSet` refuses
`http.loopback`, `http.private` and `trace.record` (`LibDB.Config.personOnly`)
unless the CLI itself is the caller (`canManagePolicies`), the same line policy
edits use, and says what to run instead. They are not a grant, because
`permissions allow all` would then open the guard too.

**A configured proxy is trusted.** HTTP clients use .NET's configured proxy
(`HTTP_PROXY`, `HTTPS_PROXY`, `ALL_PROXY`, `NO_PROXY`). The guard checks the
destination, not the proxy:

- Dark connects to the proxy's own host and port even on loopback or a private
  address, without `http.loopback` or `http.private`.
- A URL whose host is a blocked address literal, `localhost` or a metadata
  name, and a metadata request header, are refused before a proxy is chosen.
- A destination given by name is resolved by the proxy, so Dark never sees the
  address it lands on and cannot refuse it.
- A request the proxy does not take (`NO_PROXY`) connects directly and is
  checked as usual. Every URL still needs a permission grant.

What the proxy sees: for `http://`, everything, the URL, every header
(`Authorization` and cookies included) and both bodies. For `https://`, the
host and port it is asked to reach, when, and how much goes each way, but not
the path, headers or bodies, unless it is an intercepting proxy whose
certificate this machine trusts, which sees everything. Configure only a proxy
you would trust with that.

Kernel primitives use the same boundary, with `HostPosix` and `HostKernel`
owned by the host. Path operations check exact read/write requests; descriptors
opened under those checks are returned as unguessable execution-local tokens.
Every use checks ownership, open mode and current invocation access while a descriptor lease prevents close/reuse
from redirecting a call in flight. Raw OS descriptors
require `Native`; an owned token cannot be narrowed into an OS descriptor number.
Child launch checks the resolved executable and ordered arguments, and later
child/pipe operations check that retained process grant. A blocking child wait
releases its ownership lease before waiting so cleanup can terminate the child.

Returned working-directory and executable paths are authorized as reads of the
actual answer. Account-file parsing performs an ordinary checked file read;
the home-directory fallback also authorizes the candidate home path. Known host
facts are available without `Native`, but arbitrary sysctl keys and another
process's affinity require it. Open flags are Dark constants, and Dark computes
CPU counts from the permitted kernel inputs. HTTP server binds also cross the
boundary; the host returns an opaque listener handle only after the exact address and port
request is allowed and the bind succeeds.

Dark code has no raw SQL. The package store is read and changed only through
the typed package and SCM builtins, which declare `package-read` and
`package-write`.

The migration is enforced mechanically: `HostBoundary.Tests` scans compiled
IL and fails when resource-opening OS APIs (filesystem, process, HTTP client,
HTTP listener binding, environment-variable access) are referenced outside
the host modules. The allowlist is
down to two permanent, documented exceptions — terminal capability sniffing
(which must work under deny-all to render the permission prompt itself) and
the `PackageRefs` startup read — and a stale entry also fails, so a finished
family cannot quietly keep its exemption. Tests that merely search builtin
source for a check call are not a security boundary.

The in-process implementation normalizes paths once at the boundary,
resolving every symlinked ancestor to its target so that a request and a rule
root compare as filesystem identities (`/tmp/x` on macOS is checked and
opened as `/private/tmp/x`, and a rule for either spelling covers both); an
ancestor link that cannot be resolved is rejected. It hard-rejects the
host-owned `~/.darklang/policy/` directory under any spelling, even under an
allow-all file policy. Entry operations such as `readlink` may inspect the
final symlink without following it. Temporary-file operations authorize the
containing directory because the generated sibling name does not exist at
check time. On POSIX the host adapter performs path operations relative
to a directory descriptor it reached by walking the components with
`O_NOFOLLOW`, so a link swapped in between the check and the operation is
met and refused at operation time rather than followed. Retained managed host
operations on other targets use lexical checks; they do not provide a fallback
for every new kernel primitive. Unsupported operations remain unavailable.

Each operation:

1. constructs and validates an exact `Request` derived from the `Operation`;
2. checks `Access`;
3. performs the host operation after the check;
4. records a structured audit event;
5. returns a structured result.

HTTP redirects are disabled, executables are resolved before checking, and
arguments preserve order. Raw OS descriptor numbers and arbitrary PIDs require
the deliberately broad `Native` boundary. Execution-owned descriptor and child
tokens instead carry scoped grants, which are checked again on use. Tokens from
another execution and unknown or closed tokens cannot name owned resources.


## Limits

This is runtime authorization, not OS isolation. External processes and Native
operations have ambient OS authority. The system does not contain a compromised
interpreter or limit resource exhaustion. See permissions-todos.md.
