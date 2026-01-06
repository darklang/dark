# Kill Production Containers Directory

**Status**: [x] Complete

## What's Being Removed?

The `containers/` directory with Dockerfiles for production services:
- `bwdserver/` - Main Dark app server container
- `cronchecker/` - Cron job scheduler container
- `prodexec/` - Production exec with SSH tunnel
- `queueworker/` - Background queue processing container
- `base-service-Dockerfile` - Base Ubuntu image
- `fsharp-service-Dockerfile` - F# runtime base

Since we removed ProdExec, QueueWorker, and CronChecker in Phase 1, and we're not deploying containers, this entire directory can go.

## Directory to Delete

```
containers/
  base-service-Dockerfile
  bwdserver/
    Dockerfile
  cronchecker/
    Dockerfile
  fsharp-service-Dockerfile
  prodexec/
    Dockerfile
    README.md
  queueworker/
    Dockerfile
```

## Related Scripts to Remove

```
scripts/deployment/new-build-containers.sh
scripts/deployment/new-push-containers.sh (if exists)
```

## Search Commands

```bash
grep -r "containers/" --include="*.sh" --include="*.yml" scripts/ .circleci/
grep -r "base-service-Dockerfile\|fsharp-service-Dockerfile" .
```

## Steps

1. [ ] Delete entire `containers/` directory
2. [ ] Delete `scripts/deployment/new-build-containers.sh`
3. [ ] Search for other container-related scripts and remove
4. [ ] Clean up any CircleCI references (should already be cleaned from task 02)
5. [ ] Run `./scripts/run-backend-tests`
6. [ ] Commit: `trim: delete containers/ directory`

## Commit Message Template

```
trim: delete containers/ directory

- Remove entire containers/ directory
- Remove container build scripts
- Includes BwdServer, CronChecker, ProdExec, QueueWorker Dockerfiles

Production container definitions no longer needed for local-first
development.
```

## Notes

- This should be safe after Phase 1 deletions and CircleCI cleanup
- The main `Dockerfile` (dev container) is NOT in this directory - keep that
