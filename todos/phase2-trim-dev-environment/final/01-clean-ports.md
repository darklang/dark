# Clean Up Forwarded Ports

**Status**: [x] Complete

## Port Investigation Results

| Port | Used For | Action |
|------|----------|--------|
| 3275 | Old Executor serve port | REMOVED |
| 3276 | Old Executor health check port | REMOVED |
| 9000 | Old ApiServer port | REMOVED |
| 9001 | Old ApiServer related | REMOVED |
| 9002 | Old ApiServer related | REMOVED |
| 10011 | Test BwdServer backend port | KEEP |
| 10012 | Test BwdServer Kubernetes port | KEEP |
| 10030 | Test HTTP client server port | KEEP |
| 11001 | BwdServer main port | KEEP |
| 11002 | BwdServer Kubernetes port | KEEP |
| 12002 | Old CronChecker port | REMOVED |
| 13002 | Old QueueWorker port | REMOVED |

## Search Commands

```bash
# Find port references
grep -r "3275\|3276\|9000\|9001\|9002" --include="*.fs" --include="*.sh" --include="*.json" backend/ scripts/ config/
grep -r "10011\|10012\|10030\|11001\|11002\|12002\|13002" --include="*.fs" --include="*.sh" --include="*.json" backend/ scripts/ config/
grep -r "PORT" --include="*.fs" backend/src/
```

## Steps

1. [ ] Research what each port is used for
2. [ ] Document findings in this file
3. [ ] Remove unused ports from devcontainer.json
4. [ ] Update any scripts that reference removed ports
5. [ ] Run `./scripts/run-backend-tests`
6. [ ] Commit: `trim: clean up unused port forwards`

## Commit Message Template

```
trim: clean up unused port forwards

- Remove ports [list removed ports]
- Keep ports [list kept ports] for [reasons]

Document port usage for future reference.
```

## Notes

- Be conservative - better to keep an unused port than break something
- Document findings for future reference
- May need to check `config/dev` environment file too
