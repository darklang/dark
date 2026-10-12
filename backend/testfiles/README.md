# Backend test files

Package tests live in `packages/darklang/tests` and run with `dark test`.
The old execution testfiles and their runner have been removed.

This directory keeps the raw HTTP tests and shared test data:

- `httpclient`: HTTP client cases, run by `backend/tests/Tests/HttpClient.Tests.fs`.
- `http-server`: byte-exact HTTP server cases, run by `backend/tests/Tests/HttpServer.Tests.fs`.
- `data`: shared test assets.

The HTTP harnesses are separate from the removed execution-test runner. Both run
as part of `./scripts/run-backend-tests`. See `docs/unittests.md` for filtering
and build instructions, and each HTTP directory's README for its file format.
