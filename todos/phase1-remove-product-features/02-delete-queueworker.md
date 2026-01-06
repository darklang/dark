# Delete QueueWorker

**Status**: [ ] Not started

## What is QueueWorker?

QueueWorker processes user "Workers"/Queues - users emit events which get pulled from a queue and executed. It uses Google Cloud PubSub for queue management.

We no longer need this as:
- We're not deploying to cloud
- The queue/worker pattern isn't needed for local-first single-exe experience

## Files to Delete

```
backend/src/QueueWorker/
  QueueWorker.fs
  QueueWorker.fsproj
  README.md
  paket.references
```

## Other References to Remove

1. **Solution file**: Remove from `backend/fsdark.sln`
2. **Scripts**: Check `scripts/run-backend-server` (it starts QueueWorker)
3. **Scripts**: Delete `scripts/run-pubsub-emulator`
4. **CircleCI**: Check for QueueWorker jobs
5. **Containers**: Delete `containers/queueworker/` directory
6. **LibCloud**: May have queue-related code to stub out or remove

## Search Commands

```bash
grep -r "QueueWorker" --include="*.fs" --include="*.fsproj" --include="*.sln" --include="*.yml" --include="*.sh" backend/ scripts/ .circleci/
grep -r "queue" --include="*.fs" backend/src/LibCloud/
```

## Steps

1. [ ] Delete `backend/src/QueueWorker/` directory
2. [ ] Remove from `backend/fsdark.sln`
3. [ ] Update `scripts/run-backend-server` to not start QueueWorker
4. [ ] Delete `containers/queueworker/` directory
5. [ ] Delete `scripts/run-pubsub-emulator`
6. [ ] Remove QueueWorker from `.circleci/config.yml`
7. [ ] Check LibCloud for queue dependencies (may need to stub/remove)
8. [ ] Run `./scripts/run-backend-tests`
9. [ ] Wait for build
10. [ ] Commit: `trim: delete QueueWorker`

## Commit Message Template

```
trim: delete QueueWorker

- Remove backend/src/QueueWorker/ directory
- Remove from fsdark.sln
- Delete containers/queueworker/
- Update run-backend-server to not start QueueWorker
- Delete run-pubsub-emulator script
- Remove from CircleCI config

QueueWorker processed async events via PubSub. No longer needed
for local-first architecture.
```

## Potential Complications

- `scripts/run-backend-server` orchestrates multiple services - need to carefully edit, not delete
- LibCloud may have queue-related code that other parts depend on - investigate before removing
- The BuiltinDarkInternal Workers module references queues - will be handled in task 04
