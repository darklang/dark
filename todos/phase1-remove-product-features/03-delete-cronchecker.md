# Delete CronChecker

**Status**: [ ] Not started

## What is CronChecker?

CronChecker runs in a loop checking and scheduling work for all crons. It integrates with Kubernetes for health checks and graceful shutdown.

We no longer need this for local-first single-exe experience.

## Files to Delete

```
backend/src/CronChecker/
  CronChecker.fs
  CronChecker.fsproj
  README.md
  paket.references
```

## Other References to Remove

1. **Solution file**: Remove from `backend/fsdark.sln`
2. **Scripts**: Check `scripts/run-backend-server` (it starts CronChecker)
3. **Containers**: Delete `containers/cronchecker/` directory
4. **CircleCI**: Check for CronChecker jobs
5. **LibCloud**: May have cron-related code (`LibCloud.Cron`)

## Search Commands

```bash
grep -r "CronChecker" --include="*.fs" --include="*.fsproj" --include="*.sln" --include="*.yml" --include="*.sh" backend/ scripts/ .circleci/
grep -r "Cron" --include="*.fs" backend/src/LibCloud/
```

## Steps

1. [ ] Delete `backend/src/CronChecker/` directory
2. [ ] Remove from `backend/fsdark.sln`
3. [ ] Update `scripts/run-backend-server` to not start CronChecker
4. [ ] Delete `containers/cronchecker/` directory
5. [ ] Remove CronChecker from `.circleci/config.yml`
6. [ ] Check LibCloud for cron dependencies (investigate before removing)
7. [ ] Run `./scripts/run-backend-tests`
8. [ ] Wait for build
9. [ ] Commit: `trim: delete CronChecker`

## Commit Message Template

```
trim: delete CronChecker

- Remove backend/src/CronChecker/ directory
- Remove from fsdark.sln
- Delete containers/cronchecker/
- Update run-backend-server to not start CronChecker
- Remove from CircleCI config

CronChecker scheduled cron jobs in cloud deployment. No longer needed
for local-first architecture.
```

## Potential Complications

- LibCloud.Cron module may be referenced elsewhere - investigate dependencies
- Like QueueWorker, need to carefully edit `run-backend-server` rather than delete it
