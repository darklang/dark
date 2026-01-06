# Delete DataTests

**Status**: [x] Complete

## What's Being Removed?

DataTests are data integrity tests that run against the database. They were used to verify cloud deployment data consistency.

## Files to Delete

```
backend/src/DataTests/   (if exists)
scripts/run-backend-datatests
```

## Search Commands

```bash
find backend/src -name "*DataTest*" -o -name "*Datatest*"
grep -r "DataTest\|datatests" --include="*.fs" --include="*.fsproj" --include="*.sln" backend/
```

## Steps

1. [ ] Find DataTests project location
2. [ ] Delete DataTests project directory
3. [ ] Remove from `backend/fsdark.sln`
4. [ ] Delete `scripts/run-backend-datatests`
5. [ ] Search for any remaining references
6. [ ] Run `./scripts/run-backend-tests`
7. [ ] Commit: `trim: delete DataTests`

## Commit Message Template

```
trim: delete DataTests

- Remove DataTests project
- Remove from fsdark.sln
- Delete run-backend-datatests script

DataTests verified cloud database integrity. Not needed for
local SQLite-based development.
```

## Notes

- Regular backend tests (`run-backend-tests`) should still work
- This is a quick cleanup task
