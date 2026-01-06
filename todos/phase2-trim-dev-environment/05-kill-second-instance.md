# Kill Second Instance Scripts

**Status**: [ ] Not started

## What's Being Removed?

Scripts for running a second Darklang instance for testing synchronization:
- Creates instance2 with different port and database
- Used for testing sync between multiple Dark instances

Not needed for local-first single-instance architecture.

## Files to Delete

```
scripts/run-second-instance
scripts/stop-second-instance
```

## Related Configuration

The second instance uses:
- Port 11003 (vs 11001 for main)
- Database `data-instance2.db`
- Kubernetes port 11004

These port references might exist elsewhere.

## Search Commands

```bash
grep -r "second-instance\|instance2\|11003\|11004" --include="*.sh" --include="*.json" scripts/ .devcontainer/
```

## Steps

1. [ ] Delete `scripts/run-second-instance`
2. [ ] Delete `scripts/stop-second-instance`
3. [ ] Search for any references to instance2 or ports 11003/11004
4. [ ] Clean up any found references
5. [ ] Run `./scripts/run-backend-tests`
6. [ ] Commit: `trim: remove second instance scripts`

## Commit Message Template

```
trim: remove second instance scripts

- Delete scripts/run-second-instance
- Delete scripts/stop-second-instance

Second instance was used for sync testing between Dark instances.
Not needed for single-instance local-first architecture.
```

## Notes

- Quick deletion task
- May reveal port configurations that can be simplified later
