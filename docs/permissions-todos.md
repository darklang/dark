# Permissions: deferred work

Future work beyond the runtime authorization model in docs/effects.md.

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
- Per-blob confidentiality in a shared blob store. Dereferencing a persistent
  blob is ambient (docs/effects.md): content addressing stops a caller
  fabricating a hash, but gives no confidentiality once a hash leaks, and no
  per-blob authorization. Harmless while each store is single-tenant; under a
  shared `package_blobs` it needs provenance-keyed authorization, which a call
  effect cannot express (it gates the operation, not which content).

## Language and analysis

- More precise callback and deferred-effect analysis.
- Follow code held in package values during requirements analysis. Stored
  inert data is already recognized; callable or unevaluated values remain incomplete.
- Extend conservative preflight beyond known literal arguments and unconditional
  calls. Unknown branches and computed resources remain runtime checks.

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

- changed-contract review prompts
- ancestor-symlink rejection
- redirect handling at the host boundary
