# Clean Up Scripts Directory

**Status**: [ ] Not started

## Scripts to Audit

After all previous trimming, many scripts may now be orphaned or broken.

## Categories of Scripts

### Deployment Scripts (scripts/deployment/)
Most should be deleted:
- [ ] `gke-deploy` - DELETE
- [ ] `new-deploy.sh` - DELETE
- [ ] `manual-deploy` - DELETE
- [ ] `deploy-lock-*` - DELETE all
- [ ] `_notify-deployment-*` - DELETE
- [ ] `publish-github-release` - KEEP (for CLI releases)
- [ ] `publish-vs-code-extension` - KEEP
- [ ] `replace-prod-packages` - REVIEW
- [ ] `buildcontainers.dark` - DELETE

### Production Scripts (scripts/production/)
Should all be deleted:
- [ ] `gcp-get-logs` - DELETE
- [ ] `connect-to-prod-exec` - DELETE
- [ ] Any others - DELETE

### Build Scripts (scripts/build/)
Most should be kept:
- [ ] `compile` - KEEP (core build)
- [ ] `_build-server` - KEEP
- [ ] `build-parser` - KEEP
- [ ] `build-release-cli-exes.sh` - KEEP
- [ ] `reload-packages` - KEEP
- [ ] `clear-all-local-dbs` - REVIEW/UPDATE
- [ ] `clear-builder-volumes` - REVIEW
- [ ] Others - REVIEW

### Run Scripts
- [ ] `run-backend-server` - KEEP (but may need updates)
- [ ] `run-backend-tests` - KEEP
- [ ] `run-local-exec` - KEEP
- [ ] `run-cli` - KEEP
- [ ] Already deleted: run-prod-exec, run-pubsub-emulator, run-second-instance, etc.

## Steps

1. [ ] List all scripts in `scripts/` directory
2. [ ] For each script, determine if still needed
3. [ ] Delete orphaned scripts
4. [ ] Update scripts that reference deleted components
5. [ ] Test remaining scripts work
6. [ ] Run `./scripts/run-backend-tests`
7. [ ] Commit: `trim: clean up orphaned scripts`

## Search Commands

```bash
# Find scripts referencing deleted things
grep -r "ProdExec\|QueueWorker\|CronChecker\|pubsub\|terraform" --include="*.sh" scripts/
grep -r "gcloud\|docker\|containers/" --include="*.sh" scripts/
```

## Commit Message Template

```
trim: clean up orphaned scripts

Deleted scripts:
- scripts/deployment/* (except publish-*)
- scripts/production/*
- [other deleted scripts]

Updated scripts:
- [list any updated scripts]

These scripts referenced deleted components or deployment infrastructure.
```

## Notes

- This is a significant cleanup task - take time to audit carefully
- Some scripts may be used by CI - check CircleCI config
- Test that build and run workflows still function
