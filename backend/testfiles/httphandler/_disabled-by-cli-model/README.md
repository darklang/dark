# Fixtures intrinsically incompatible with the CLI's single-canvas model

These fixtures came over from BwdServer-era tests. The CLI's `Http.serve`
builtin is single-canvas-per-process by design, so multi-canvas dispatch
and load-balancer-fronted host concepts no longer apply.

- `url-custom-domain.test` — used `[domain ...]` to target a non-localhost
  custom domain. CLI is single-canvas; multi-tenancy means running multiple
  CLI processes behind a reverse proxy.
- `x-forwarded-proto-ignored.test` — exercised the canvas-domain branch of
  URL canonicalization (combined with `[domain ...]`). CLI's
  `canonicalizeFromForwardedProto` flag handles the simpler proxy case
  uniformly.
- `_simple-request-headers.test` — already underscore-prefixed (skipped) on
  main; the CLEANUP comment in the file reads "Bring this test back once we
  can parse `\"(\\\"\"`".

If single-canvas semantics ever change, re-evaluate these.
