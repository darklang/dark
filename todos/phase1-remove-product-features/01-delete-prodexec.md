# Delete ProdExec

**Status**: [ ] Not started

## What is ProdExec?

ProdExec is a CLI utility for running one-off production tasks like:
- Triggering Rollbar test messages
- Data format conversions (SerializableTypes to RuntimeTypes)
- Other production maintenance tasks

We no longer need this as we're not deploying to production cloud infrastructure.

## Files to Delete

```
backend/src/ProdExec/
  ProdExec.fs
  ProdExec.fsproj
  README.md
  paket.references
```

## Other References to Remove

After deleting the directory, search for and remove references:

1. **Solution file**: Remove ProdExec from `backend/fsdark.sln`
2. **Scripts**: Check for references in `scripts/` (especially `scripts/run-prod-exec`)
3. **CircleCI**: Check `.circleci/config.yml` for ProdExec build/deploy jobs
4. **Containers**: Delete `containers/prodexec/` directory
5. **Terraform**: Check `tf/` for ProdExec service definitions

## Search Commands

```bash
# Find all references
grep -r "ProdExec" --include="*.fs" --include="*.fsproj" --include="*.sln" --include="*.yml" --include="*.sh" backend/ scripts/ .circleci/ tf/
```

## Steps

1. [ ] Delete `backend/src/ProdExec/` directory
2. [ ] Remove from `backend/fsdark.sln`
3. [ ] Delete `scripts/run-prod-exec` if it exists
4. [ ] Delete `containers/prodexec/` directory
5. [ ] Remove ProdExec references from `.circleci/config.yml`
6. [ ] Remove ProdExec references from `tf/` (cloudrun.tf, etc.)
7. [ ] Search for any remaining references and remove them
8. [ ] Run `./scripts/run-backend-tests`
9. [ ] Wait for build to complete (check build-server.log)
10. [ ] Commit: `trim: delete ProdExec`

## Commit Message Template

```
trim: delete ProdExec

- Remove backend/src/ProdExec/ directory
- Remove from fsdark.sln
- Delete containers/prodexec/
- Remove from CircleCI config
- Remove from Terraform config
- Delete run-prod-exec script

ProdExec was used for one-off production tasks. No longer needed
as we're not deploying to cloud infrastructure.
```

## Notes

- ProdExec uses chisel for SSH tunneling - we'll remove chisel separately in Phase 2
- Some terraform files reference ProdExec - we're deleting all of tf/ anyway, but clean refs for now
