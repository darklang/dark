# Clean Up Docs Directory

**Status**: [x] Complete

## Current Docs

```
docs/
  errors.md
  dnsmasq.md
  dblock-serialization.md
  dev-setup/
    vscode-setup.md
    README.md
  production/
    db-creds-rotation.md
    honeycomb.md
    accounts.md
    auditlogs.md
    emergency-login.md
    tls.md
    deployment.md
    README.md
    what-to-do-if-something-goes-wrong.md
  logging-and-telemetry.md
  benchmarking.md
  release.md
  queues.md
  serialization.md
  unittests.md
  writing-docstrings.md
```

## Docs to DELETE (production-related)

The entire `docs/production/` directory:
- `db-creds-rotation.md` - Cloud DB credential management
- `honeycomb.md` - Honeycomb setup (deleted)
- `accounts.md` - Production accounts
- `auditlogs.md` - Production audit logs
- `emergency-login.md` - Production emergency access
- `tls.md` - TLS setup for production
- `deployment.md` - Deployment process
- `what-to-do-if-something-goes-wrong.md` - Production incidents

## Docs to REVIEW

- `logging-and-telemetry.md` - References Honeycomb/OTEL?
- `queues.md` - About QueueWorker (deleted)?
- `dnsmasq.md` - Still relevant?

## Docs to KEEP

- `errors.md` - Error handling docs
- `dblock-serialization.md` - Serialization format
- `dev-setup/` - Developer setup guides
- `benchmarking.md` - Performance testing
- `release.md` - Release process
- `serialization.md` - Data serialization
- `unittests.md` - Testing docs
- `writing-docstrings.md` - Documentation standards

## Steps

1. [ ] Delete entire `docs/production/` directory
2. [ ] Review `logging-and-telemetry.md` - update or delete
3. [ ] Review `queues.md` - delete if about QueueWorker
4. [ ] Review `dnsmasq.md` - delete if not relevant
5. [ ] Check remaining docs for stale references
6. [ ] Run `./scripts/run-backend-tests`
7. [ ] Commit: `trim: clean up production docs`

## Commit Message Template

```
trim: clean up production docs

- Delete entire docs/production/ directory
- Delete logging-and-telemetry.md (referenced deleted components)
- Delete queues.md (about deleted QueueWorker)
- [other deleted docs]

Production documentation no longer relevant for local-first development.
```

## Notes

- This is documentation cleanup - low risk
- May want to archive rather than delete if there's useful info
- Check if any docs are linked from code comments
