# Permissions: deferred work

The items docs/effects.md and AGENTS.md point at when they say "the todo
file" or "future work". Everything here is stated (not decided) in
docs/effects.md; this file just collects it in one place so the references
resolve.

## Trust boundary

- OS-level sandbox. The permission system is runtime authorization, not OS
  isolation: it does not contain a compromised interpreter, spawned
  subprocesses (Process/Native grants hand over ambient OS authority,
  including writing policy files), or resource exhaustion. Containing those
  is the sandbox's job.
- Move the bundled-owner anchor into the protected policy store. The bundled
  set is computed from the mutable `owner` column, so anything that writes
  the package DB directly can forge trust. The insert-time guard protects
  the package APIs only.

## Language and analysis

- Effect rows on function types, so a callback's effects flow through
  higher-order functions like List.map. The declaration-level ceiling row is
  the runtime half of that design.
- Package-value bodies join closure analysis. Today a package-value
  reference marks requirements analysis incomplete.
- Requirements analysis could specialize a scoped effect when the resource
  argument is a literal at the call site (a hardcoded path or URL), giving
  approve-time review an exact rule instead of the bare effect.

## Diagnostics

- Expose the structured denial (layer, reason, resource, suggested rule) as
  a Dark value. Guest code currently sees only the rendered RuntimeError
  string; the structure exists at the host boundary and drives the audit log
  and the CLI hint.

## Host boundary

- New APIs should use opaque, execution-owned handles rather than raw fds,
  PIDs, or process handles, so they can carry narrower access than the
  all-or-nothing Native gate.
- Windows runs the .NET calls behind the lexical path check alone; the
  O_NOFOLLOW directory-walk defense against check-to-use symlink swaps is
  POSIX-only.

## Test coverage still to add

From docs/effects.md "Invariants and their tests":

- changed-contract review prompts
- stream and deserialized-applicable widening
- ancestor-symlink rejection
- redirect handling at the host boundary
