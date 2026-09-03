# Effects and permissions

Status: runtime foundation and host-owned policy persistence implemented;
named consumer approvals and version pins are available in the CLI.

Dark uses related but separate descriptions and enforcement:

- **Call effects** describe what happens while a builtin is invoked.
- **Permission requirements** conservatively collect every host effect reachable
  from a package function, including code in closures that may be returned and
  invoked later. They drive approval tooling; they are not function effect rows.
- **Permissions** decide whether a running program may perform one exact host
  operation. They are enforced at runtime.

Keeping these separate is the central design rule. A function can require HTTP
without being authorized to contact every host, and a machine can permit HTTP
without making every function effectful.

## Vocabulary

- `Effect`: one statically visible kind of behavior, such as `Http` or
  `FileWrite`. `Effects.all` is the inventory; the Dark rule grammar's
  `effectNames` is pinned to it by `Builtin.Tests.darkEffectNamesMatchRuntime`.
- `callEffects`: a builtin's declared invocation-effect set, a required
  `Set<Effect>` on every builtin record, so a builtin cannot be defined without
  one. `Effects.isScoped` splits it: ambient effects (`Stdout`, `Clock`,
  `PackageRead`, ...) are checked once at the interpreter gate before the body
  runs; scoped effects name a resource and are checked by the body or, for the
  OS-facing ones, by the host boundary from the `Operation`.
- `PermissionRequirements`: the complete, transitive set used for review.
- `Request`: one exact runtime operation, such as `GET
  https://api.stripe.com:443/v1/payment_intents?expand=charges`. "Exact"
  includes the query string: a rule for `/download?id=public` does not cover
  `/download?id=secret`. A rule written without a query matches any query on
  the path, so the intuitive path-only grant still works; likewise a rule
  without a path matches any path, `:*` matches any port, and `https://*`
  means any https host and port.
- `Rule`: a pattern that may cover requests.
- `Policy`: allow and deny rules.
- `Access`: the immutable policies currently constraining an execution.

`Access` is an internal runtime value and cannot be created by Dark code.
Trusted F# hosts configure it through three functions:

```fsharp
state
|> Execution.setInstancePolicy instancePolicy
|> Execution.restrictRun runPolicy
|> Execution.setPackagePolicies lookup   // Hash -> Option<Policy>
```

`setPackagePolicies` denies packages the lookup does not know. The CLI's
`PolicyStore.guestState` builds all three layers for a `run` or `eval`: explicit
approvals and the script's own fns from a small map, the bundled Darklang set
from a membership check (it has thousands of members, so it is never
materialized as a policy map). Low-level `createState` defaults to
**deny-all** (fail-safe): an embedder who installs no policy gets a confined
execution, not an unrestricted one. Trusted internal callers (the CLI outer
state, seeding, tests) opt into a permissive policy explicitly with
`setInstancePolicy`; the CLI installs the persisted deny-by-default instance
policy before guest execution.

The CLI host loads the operator instance policy, account-scoped immutable
package approvals, and account-scoped logical-function version pins from
`~/.darklang/policy/policies.bin`. Instance grants are machine-wide; consumer
decisions belong to the logged-in account. The file is atomic, versioned, and
separate from package data. Missing, malformed, or unreadable policy state
denies all host effects, and mutation refuses to overwrite a file it cannot
read. Package approval is always enforced. The script's own compiled functions
and the bundled Darklang-owned package set are part of the trusted application
and therefore exempt from the consumer-approval layer; the instance and run
policies still bound them. Approvals govern third-party packages. The normal
approval workflow uses logical names and shows short version identifiers; the
diagnostic commands (`requirements`, `show`, `revoke-package`) also accept a
full immutable hash.

A few builtins are reserved for first-party code. `httpGetUnsafeBytes` — the
SSRF-guards-off fetch the sync code uses to reach a peer on loopback/LAN —
requires two things at once: the host capability `canUsePrivateNetworkHttp`,
which only the outer `dark sync` state has (every `guestState` clears it), and
a call chain in which every frame is a bundled Darklang function
(`ExecutionState.isBundledPackageFn`, checked by `requireBundledCaller`). A
third-party package cannot reach it even with a broad `http` grant, and
neither can a `run` script that wraps it in a bundled helper.

The CLI exposes the typed policy directly: `permissions allow http GET
https://api.example.com/v1` adds a narrow rule, `permissions requirements
<fn|hash>` shows permission requirements, and `permissions approve <fn>`
records a consumer approval keyed to the immutable function hash. Broad or
computed access always requires an explicit operator rule.

The Workbench function detail pane and `permissions show <fn|hash>` present a
layered access report: analyzed requirements and completeness, author ceiling,
immutable version, account approval and pins, package and instance rules, and
the effective runtime intersection. It deliberately says `requires` rather
than `has access`; exact paths, URLs, ports, names, and arguments are decided
from all active layers when the operation occurs.

## Controls

There are four policy layers, and effective access is their intersection:

| layer | owner | meaning |
|---|---|---|
| instance | machine/operator | the hard maximum this installation permits |
| run | invoker | the subset granted to this invocation |
| package | consumer | the subset approved for an immutable release |
| function | author | an optional ceiling declared in the source; it only restricts |

A package's published requirements are a request, not an approval. Only the
instance operator or invoker can grant access. The function ceiling is the one
layer the author controls, and it can only take away.

### Function ceilings

A function declares its ceiling as an effect row on the return colon:

```text
let fetch (url: String) :{Http, Clock} String = ...
let add (a: Int64) (b: Int64) :{} Int64 = a + b      // effect-free
let helper (x: Int64) : Int64 = ...                  // no ceiling declared
```

The names are the effect case names (`Http`, `FileRead`, `Clock`, …); an
unknown name is a parse error, never a wildcard. The row is stored on the
package function (`permissionCeiling`), is part of its content hash, and is
compared as part of the permission contract when a pin moves. At runtime a
call into the function appends `Policy.allowEffects ceiling` as a
`Layer.Function` restriction, so a body that reaches past its own row is
denied by "function policy" even under an allow-all instance. `permissions
show <fn>` prints the ceiling next to the body's analyzed requirements and
warns when a complete requirement set exceeds it.

## Runtime rule

```text
instance policy  AND  run policy  AND  each entered package's approval
  AND  each entered function's ceiling
```

Implementation does not materialize a complicated intersection. `Access`
holds an immutable list of policies and a request is allowed only when every
policy allows it. Entering a package appends another restriction; nothing
removes one. This gives the runtime one monotonic invariant:

```text
child access is never wider than parent access
```

Frames receive their `Access` when created. Named function values and closures
capture it when evaluated, before they can be placed in containers, returned,
or handed to a builtin; later invocation intersects it with the invoking
frame, so neither side can widen the other. Callbacks reached through those
values inherit the guarantee, because they run through the same
`Exe.executeApplicable`: a **stream** transform (`streamMap`/`streamFilter`)
and an **HTTP server handler** both invoke a captured applicable. **Tasks**
are not a runtime value, so there is no deferred task to lose access on.
Materializing a package `val` does the same walk recursively: every uncaptured
named function nested in its records, collections, closures, or partial
arguments captures the loading frame before the value can escape.

Two paths are deliberately *capability*-based rather than access-propagating:
an HTTP **stream's IO leaf** checks the permission once when the request opens
(`Operation.HttpStreamOpen`) and then serves chunks from a host-issued,
execution-owned handle (`Host.httpStreamRead` makes no second decision); and
raw **resource handles** (file descriptors, process handles) carry the
all-or-nothing `Native` gate rather than a per-handle `Access`.

## Permission requirements

Package approval walks the immutable call graph and derives permission
requirements from builtin call metadata. It also walks lambda bodies, so
returned code is included. Unresolved references — a call through a variable
or argument, e.g. the callback in `List.map` — mark the analysis incomplete,
as does a package-value reference until value bodies join closure analysis,
and so does a builtin the running host does not know. Incomplete analysis is
surfaced to *display* as "incomplete", not as effect-free and not by crashing
the command: the requirements query returns the partial effects with a
completeness flag.

Approval fails closed on incompleteness by default. A human can pass an
explicit acknowledgement (the CLI's `approve`/`update` show the incomplete
warning and take the confirmation as that acknowledgement), and each
incomplete member is then approved as allow-all, bounded by the instance/run
policies — never pinned to its inexact lower bound. Pinning the lower bound
would deny the effects the walk missed, and for a member with an empty lower
bound (a bundled dependency like `List.map`) that allowlist is deny-all, which
would poison the dependency account-globally.

An empty complete requirement set is called `effect-free`, not `pure`: it does
not establish termination or determinism. Resource details do not belong in
function types: a dynamic path or URL remains dynamic and requires an explicit
runtime rule. Effect rows on function *types* (so that a callback's effects
flow through `List.map`) are later type-system work; see the todo file. The
declaration-level row above is the runtime half of that design, available now.

## Requests and policies

A `Request` is exact and already normalized by the checked host boundary. A
`Rule` may cover a set of requests. Invalid input never becomes a wildcard.

The package and trace stores are host-owned wholes with no per-resource
scoping: `package-read`, `package-write`, `trace-read` and `trace-write` are
ambient effects, granted or denied as a whole like `stdout`. Datastores keep a
per-table rule.

Policies are allowlists with optional explicit denies:

1. A matching deny always wins inside a policy.
2. Otherwise a matching allow is required.
3. No matching rule means deny.
4. Every active policy must allow the request.

Policies are serialized in a versioned binary format. The Dark CLI receives a
typed mirror of the same `Policy`; its shell-quoted text grammar is
presentation only, and every rule renders to text that parses back to the same
rule (a bare effect
name such as `http` is the whole effect; the scoped rules' broad spellings are
`http * *`, `http-server *`, `process *`). Incomplete analysis, invalid ports,
malformed URLs, and unknown rule kinds fail closed with a diagnostic.

An approval can carry the consumer's own resource rules: `permissions
approve MyApp.fetch http GET 'https://api.example.com'` stores that policy as
the root's package layer instead of its derived effect set. Entering the root
appends the policy as a restriction every callee inherits, so the function
and everything it calls is confined to those resources; its dependencies keep
their own derived policies, so a shared bundled dependency is never narrowed
account-wide by one root's rules. The rules must be able to cover the root's
analyzed requirements, or the approval is refused naming the effects they
miss. Rules given on `approve` carry over an `update` unless new ones are
given; a derived approval is recomputed from the new version instead.

Package approvals are stored outside package-controlled data and keyed by
account plus immutable release identity. A newer version is a new hash and
inherits nothing. Moving a pin is one trusted operation
(`PackagePermissions.approveAndPinFunctionVersion`): it compares the callable
type and the complete transitive requirements against the current pin before
storing anything, then writes the closure approval and the pin together with a
compare-and-swap, so a version the reviewer rejects is never left approved. A
changed contract always requires review; the CLI's second confirm prompt is
that review, passed through as an explicit acknowledgment. `unpin` releases
the name only; `revoke-package` drops the approval and the dependencies no
other approved root still needs.

## Checked host boundary

The boundary is one door: `LibExecution.Host`. A converted builtin never
touches the OS; it constructs an `Operation` — the full description of one
host action — and hands it to `Host.perform`, which derives the exact
permission `Request` from that same value, checks it, and only then executes.
The checked thing and the performed thing therefore cannot diverge, and
forgetting the check is unwritable at builtin call sites. The filesystem,
directory, environment, process, and HTTP client builtins route through it;
the host-owned process table (`LibExecution.HostProcess`) and the HTTP
machinery with its streaming-response table (`LibExecution.HostHttp`) live
behind it. An HTTP operation names one of two profiles: `Guest` (the host's
configured guest client, `HostHttp.setGuestConfig`: timeouts and SSRF guards)
or `Sync` (the loopback/LAN-capable transport, reachable only with
`canUsePrivateNetworkHttp` from an all-bundled call chain), so configuration
and client objects stay in host-owned state.

The libc-backed posix twins route through it too, with the libc bridge
(`LibExecution.HostLibc`) owned by the boundary: scoped posix operations
(paths, env names, spawns) go through `Host.perform`; an answer that is a
path (the working directory, the home directory, a file's owner, the running
binary) is checked as a read of that path. Raw fds, pids and spawns are
`Native`. Plain host facts (uname, pid, uid, cpu count, terminal size) are
effect-free and call `HostLibc` directly. Native-gated operations that do go
through the boundary (fd reads, process I/O) are checked once, at the
interpreter's gate; the boundary executes and audits them without a second
decision. HTTP server binds
also cross the boundary; the host returns an opaque listener handle only after
the exact port request is allowed and the bind succeeds.

Arbitrary SQLite SQL is deliberately `Native`, not scoped `FileRead` or
`FileWrite`. SQLite statements can attach, vacuum into, or otherwise open paths
that are not the primary connection path, and the managed API does not expose a
complete SQLite authorizer. Treating only that first path as confinement would
be unsound.

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
check time. On POSIX the libc bridge performs every path operation relative
to a directory descriptor it reached by walking the components with
`O_NOFOLLOW`, so a link swapped in between the check and the operation is
met and refused at operation time rather than followed; on Windows the .NET
calls run behind the lexical check alone.

Each operation:

1. constructs and validates an exact `Request` derived from the `Operation`;
2. checks `Access`;
3. performs the host operation after the check;
4. records a structured audit event;
5. returns a structured result.

The browser (WASM) host has no socket layer, so the DNS-level private-address
guard the CLI's HTTP client applies cannot run there; the browser's own
same-origin rules bound that host instead, and the REPL runs allow-all by
design.

HTTP redirects are disabled, executables are resolved before checking, and
arguments preserve order. Raw integer file descriptors, PIDs, and process
handles are explicitly `Native`: restricted code cannot use them without
granting the deliberately broad native boundary. New APIs should use opaque,
execution-owned handles so they can carry narrower access.

## Policy administration

Guest code cannot change instance, run, or package approval policy. The
policy builtins are host-only (`ExecutionState.canManagePolicies`), granted
only to the trusted `dark permissions` command.

**This is runtime authorization, not OS isolation.** Every effect a Darklang
program performs *through the interpreter* is checked against the instance
policy: scoped resources cross `Host.perform`, while ambient and `Native`
effects cross the interpreter gate. The IL scan proves OS APIs are confined to
the host modules and documented bootstrap exceptions, so interpreted code is
bounded by the instance policy.
It deliberately does **not** contain, and must not be relied on to contain:

- **A compromised interpreter or runtime.** A memory-safety exploit that
  escapes the interpreter can call the OS directly, outside the checked
  boundary.
- **Spawned subprocesses.** Granting `Process`/`Native`, or invoking a local
  agent (an LLM/Codex-style tool), hands control to native code the permission
  model cannot reach; it runs with the process's ambient OS authority,
  including the ability to write policy files.
- **Denial of service.** Permissions authorize effects; they do not limit CPU,
  memory, time, output, concurrent tasks, open handles, or request sizes.

Containing those requires an OS-level sandbox, which is separate deferred
work. Until it lands, the trust boundary is
exactly this: the instance policy bounds all interpreted code, and escaping it
needs a runtime exploit or a spawned process, neither of which is confined
here.

**The bundled owner is reserved.** Packages owned by `Darklang` are the
bundled, trusted set, exempt from consumer approval. A malicious package could
otherwise publish itself under `owner = "Darklang"` and look like built-in
code, so before package operations are inserted the code looks at the owner on
every operation, rejects `Darklang`-owned operations that come from guest or
peer input, and allows only trusted internal seeding to create those entries.
The check applies across normal writes, propagated updates, reverts, atomic
undo, and sync batches. It protects the package APIs; the bundled set is
still computed from the mutable `owner` column, so anything that can write the
package database directly could forge trust. Moving that anchor into the
protected policy store is part of the deferred sandbox work.

## Diagnostics

At the host boundary a denial is **structured** — `Outcome.Denied` carries the
policy layer that denied it, the reason, the exact resource, and a narrowly
scoped suggested rule — and that structure drives the audit log and the
actionable `permissions allow <rule>` hint. The value surfaced to *guest* code
is still a rendered `RuntimeError` string; exposing the structured denial as a
Dark value is tracked in the todo file.

A denial is actionable: it names the exact `permissions allow <rule>` that
would fix an instance-layer denial, derived from the operation and the check
that actually failed (a read-write op names read or write, whichever was
denied), and the CLI grammar accepts every shape the hint emits. Combined with
the seeded default instance policy — a fresh install allows a guest run to
load packages, compute, use the local store, and print, but denies the
filesystem, network, processes, and the environment until granted — this
gives a deny-by-default posture that is usable out of the box. At an
interactive terminal, `run` turns an instance-layer denial into a question:
allow the exact rule once (this run only, widening the instance layer in
memory), allow it always (saved through the same path as `permissions
allow`), or deny, and retries on allow. Non-interactive execution, `--sandbox`
and `--warn-permissions` never prompt and never widen policy automatically.

Every host operation is recorded to `rundir/logs/host-audit.jsonl` — one
structured line with the operation summary and its policy decision (allowed,
denied, failed, or rejected) — written in-process at the single
`Host.perform` choke point, so each operation is logged exactly once.
`DARK_AUDIT=off` disables it. Stream-chunk continuations are not logged (they
make no policy decision and would flood the log). The `--warn-permissions`
audit mode additionally records-and-proceeds past run- and package-layer
denials; it never proceeds past an instance denial or an author's function
ceiling, is only available for trusted code, and never saves policy
automatically.

## Invariants and their tests

These tests turn the design guarantees below into regression checks.

### Analysis and runtime access

- Requirements include transitive calls and deferred lambda bodies. Missing or
  opaque code makes analysis incomplete. Tests: `PackagePermissions.Tests`
  (`deferredCodeRequirementsAreIncluded`, `missingCodeIsIncomplete`,
  `packageValuesMakeAnalysisIncomplete`).
- Policies deny by default, deny rules win, and restricting `Access` can never
  widen it. Tests: `Permissions.Tests` (`policiesDenyByDefault`,
  `denyOverridesAllow`, `accessOnlyNarrows`, `instanceDenialWinsOverInnerDenial`)
  and the `PermissionsGate` denial tests.
- Closures, named functions, partial applications, and nested package values
  retain captured access without widening the caller. Tests:
  `Permissions.Tests.capturedAccessCannotWidenCaller`,
  `materializedValuesCaptureCallablesRecursively`, and the `PermissionsGate`
  capture tests.

### Requests and host safety

- HTTP, file, and process rules match the complete normalized request. Invalid
  input never becomes `All`. Tests: `Permissions.Tests`
  (`httpRulesKeepDimensionsCoupled`, `httpQueryIsPartOfTheOperation`,
  `fileRulesRespectBoundaries`, `processRulesPreserveArgumentOrder`,
  `invalidRequestsFailClosed`).
- Rule text round-trips safely, including quoting, metacharacters, IPv6, and
  denial suggestions. Tests: `permissions-grammar.dark` and
  `Permissions.Tests.suggestRuleIsActionable`.
- Host operations use the supplied access and cannot bypass the checked
  boundary. Tests: `Host.Tests` host-operation cases,
  `HostBoundary.Tests.onlyHostModulesTouchTheOS`, and
  `Builtin.Tests.scopedEffectBuiltinsCheckTargets`.
- Guest code cannot access policy files or change policy and approvals. Tests:
  `PermissionsGate.guestFileApiCannotReachPolicyStore`,
  `guestCannotChangePolicies`, `guestCannotApprovePackages`, and
  `Host.Tests.localPolicyPathsRejectTraversal`.

### Approvals and persistence

- Approval and pin updates are atomic; stale reviews are rejected without
  partial state. Test: `PolicyStore.Tests.approvalAndPinAreOneTransaction`.
- Revocation and re-approval preserve shared dependencies and remove obsolete
  ones. Tests: `PolicyStore.Tests.revokingOneRootKeepsSharedDependencies` and
  `reapprovingWithSmallerClosureDropsObsoleteDeps`.
- Reclassification detects stale approvals independently for each root. Test:
  `PolicyStore.Tests.reapprovingOneRootLeavesOthersStale`.
- Policy data round-trips and rejects old, trailing, or malformed bytes. Tests:
  `PolicyStore.Tests.policyStoreRoundTripsVersionedPolicies`,
  `policyStoreRejectsOlderFormat`, and `policyStoreRejectsTrailingData`.
- The Dark effect-name table matches the runtime inventory. Test:
  `Builtin.Tests.darkEffectNamesMatchRuntime`.

Coverage still to add includes changed-contract review prompts, stream and
deserialized-applicable widening, ancestor-symlink rejection, and redirect
handling at the host boundary.
