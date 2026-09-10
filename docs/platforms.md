# Platforms

Status: the model, composition, per-session activation and open capability names are implemented.
Shipping a platform from outside this repository is NOT, and the last section says exactly what
stands in the way.

A **platform** is a named, versioned bundle of builtins plus the effects those builtins may perform.
An executable is a choice of platforms; a session is a choice within that.

## Vocabulary

- `Platform` (`LibExecution/Platform.fs`): name, version, description, builtins, `requires`,
  `dynamicEffects`, `requiresStore`. A VALUE, not an assembly: four assemblies ship several
  platforms each, and nothing stops one platform spanning two assemblies.
- `PlatformSet`: a chosen set plus the single `Builtins` an `ExecutionState` runs against.
  `PlatformSet.make` refuses a builtin name claimed by two platforms, and refuses a set missing
  something a member `requires`. Both are internal errors rather than runtime errors, because they
  are decided by how the executable was built rather than by anything a guest can reach.
- **Fingerprint**: 16 hex characters of SHA-256 over a canonical manifest. Two of them:
  - the SET's, which answers "is the primitive floor the same as when this was compiled, approved
    or cached";
  - each PLATFORM's own, which answers "is this piece the same". A consumer of somebody else's
    platform did not choose the rest of your binary and should not be told their dependency moved
    when it did not.

  Both cover names, versions, type parameters, parameter types, return types and effects. Both
  deliberately exclude descriptions, parameter NAMES and `sqlSpec`: a doc fix must not invalidate
  every cached package and every approval on the machine, because that is a re-review nobody reads.
- **Active**: which platforms a session's GUEST code gets. See below; this is not a permission.
- `dynamicEffects`: effects a builtin may request from inside its own body, which its static
  `callEffects` cannot express. Almost always empty. `Sqlite` is why it exists.

## Activation is not permission

These are two mechanisms and the distinction is load-bearing:

- A **permission** answers "may this code touch that". `dark permissions`, host-owned policy,
  enforced per request at the point of the effect.
- **Activation** answers "is that even here". `dark platforms activate|deactivate|reset`, host-owned
  `activated` file, enforced at builtin lookup.

Turning a platform on does not grant any effect. It makes ASKING possible; the policy still decides.
Every message in the CLI is worded to keep that straight ("lets your code ask for", never "allows").

There is deliberately no `permissions deny Sqlite`. A `Rule` is checked against a `Request`, and a
request does not know which platform produced it: `Sqlite.query` raises a `native` request and so
does `Posix.uname`, so such a rule could only expand to `deny native` and would also deny `Posix`,
`Process`, `Seed` and `Policy`. A control that reads as scoping to a platform and in fact scopes to
an effect is worse than no control. Use deactivation.

### Where activation lives, and why

On the GUEST, not the process (`ExecutionState.guestFloor`, applied in `PolicyStore.guestState`).
The host is a Dark program too: it reaches for `Terminal` to print your answer and `Instance` for
the store path before your code runs at all. Restricting the process starves the thing that would
tell you why.

The always-on floor is `Core` plus `Store` (`Platforms.Sets.alwaysOn`, mirrored in
`Cli.Platforms.alwaysOn` and pinned together by a test). `Core` because nothing computes without it.
`Store` because Dark's error printer is Dark, and turning a hash back into a name needs
`package-read`; with `Core` alone the first run printed `<pretty-print failed>` inside the message
explaining the failure. That is affordable only because `Store` reaches exactly `package-read`, and
it stops being affordable the day `Store` grows a second effect.

`guestFloor` is a FUNCTION rather than a value, so activating something inside a long-running process
(a REPL, or the prompt's own retry) builds the next guest against the new floor. It is memoized on
the activation, and the usual answer, "this instance never narrowed", computes nothing.

### The stored file

`~/.darklang/policy/activated`, plain text, one name per line after a version header. Blank lines
and `#` comments are ignored, so "why Sqlite is off" can live next to the decision.

- **Missing** means never chosen, which means every platform is on. This is the default.
- **Present** is a choice, including an empty one.
- **A header we do not recognize** reads as the empty set, not as everything. Failing open would
  mean one corrupted byte re-enables every platform an instance had switched off.

It is a separate file from `policies.bin` for the conceptual reason above and one practical one:
the policy format's migration path is "back up and reset to deny-all", so adding a field there
would silently reset every existing approval on every machine that upgraded.

`DARK_PLATFORMS` overrides the stored answer for one run: unset means "use the stored answer", empty
means the floor, and a comma list means exactly those. It is for tests and development, not the
interface.

## Capabilities a platform brings

`Effects.Effect` is the well-known vocabulary plus `Custom of string`, namespaced `owner/name`.

Build one only through `Effects.custom`, which validates the shape. An unvalidated `Custom "http"`
would be a second, unrelated value that renders as `http`, matches no `Rule.Effect Http`, and reads
in every message as though it did.

A custom effect is ambient and whole-or-nothing, for the same reason `Native` is: the runtime cannot
build a request naming a resource it knows nothing about, so it will not pretend to confine one.
Unlike `Native`, though, the name IS the grantable unit, so `permissions allow acme/serial` works
and a denial can tell you exactly what to type.

Dark code can narrow itself to one:

    let readTag (port: String) :{"acme/serial"} String = ...

Quoted because a slash is not an identifier. Stored bare, so everything downstream resolves a name
one way; `Permissions.effectSourceSpelling` is what puts the quotes back when a ceiling is rendered
as source.

Effects cross every wire by name through `Effects.fromName`, which resolves well-known then custom,
so no serializer needs a per-effect tag table.

## Writing a platform

Inside this repository, a platform is a record and one line in the catalog:

```fsharp
// In your Builtins.YourThing assembly
let platform : Platform =
  { name = "YourThing"
    version = 0
    description = "One line, for `dark platforms`."
    builtins = Builtin.make [] fns
    requires = [ "Core" ]
    dynamicEffects = Set.empty
    requiresStore = false }
```

```fsharp
// In Platforms/Sets.fs, catalogFor
    Builtins.YourThing.Builtin.platform
```

Rules worth knowing before you add one:

- **A platform has to earn its assembly.** Build cost is about 1.4 seconds per project in the
  closure, paid whether or not anything changed. Split when the pieces are wanted APART, not when
  they are merely different.
- **`requires` is about BUILTINS needing other builtins**, not about package modules. Every platform
  but `Core` requires exactly `Core` today, and `requiresIsJustCore` pins that so a new entry has to
  be justified. Read it loosely and every platform requires every other one.
- **Declare `Native` when you cannot scope honestly.** A builtin given one database path whose SQL
  can say `ATTACH '/home/you/.ssh/id_rsa'` does not have a file-read effect, it has the machine.
- **Declare a `Custom` capability when you have a genuinely new one**, rather than mislabelling
  yourself as an existing effect or reaching for `Native`, which announces the opposite of narrow.

`Tests/Platform.Tests.fs` composes `AcmeSerial@0`, a platform this repository does not contain,
declaring `acme/serial`, and asserts the seams hold: it composes, its capability is in the
fingerprint, it cannot shadow a well-known effect, it survives the wire, a policy granting every
well-known effect still does not reach it, and its denial names its own rule.

## What a third party cannot do yet

The composition API takes a stranger. The build does not. Being in the binary means being in
`fsdark.sln` and in `Platforms/Sets.fs`, which is a fork rather than a distribution.

NativeAOT is a closed world: there is no `Assembly.LoadFrom`, so "a tiny exe that loads platform
DLLs at runtime" is not available. Three real options, none of them written:

1. **Compose at publish.** One AOT executable per platform set. Today's build, times N. This is what
   we do for ourselves, and the honest instruction for somebody else is "fork, add two lines,
   build". Cheap to document, and it is not distribution.
2. **A JIT host with loadable assemblies.** Real runtime install. Gives back the startup that the
   AOT work bought.
3. **Out of process.** The executable stays AOT; a platform is its own AOT executable speaking
   `Operation`/`Response` over a pipe. `Host.perform` already IS that interface, which makes this
   mechanical rather than a rewrite. The cost is an IPC hop per host effect: fine for fetching a
   URL, not fine for terminal painting, which is why `TerminalText` went native. Count operations
   per frame, not what they operate on.

I would take 1 for what we ship and 3 for third parties. Until one of them exists, "a library brings
its own builtins" describes a design that works and a distribution story that does not.

The other open door is smaller: `Platforms/Sets.fs` is an F# list rather than data. Making it
declarative does not enable loading a platform later (NativeAOT still requires the code in the
binary); it makes composing a DIFFERENT binary an edit to data rather than to code.
