# Clean Up Forwarded Ports

**Status**: [ ] Not started

## Current Port Forwarding

From `.devcontainer/devcontainer.json`:
```json
"forwardPorts": [
  3275, 3276, 9000, 9001, 9002, 10011, 10012, 10030, 11001, 11002, 12002, 13002
]
```

## Investigate Each Port

| Port | Used For | Keep? |
|------|----------|-------|
| 3275 | ? | INVESTIGATE |
| 3276 | ? | INVESTIGATE |
| 9000 | ? | INVESTIGATE |
| 9001 | ? | INVESTIGATE |
| 9002 | ? | INVESTIGATE |
| 10011 | ? | INVESTIGATE |
| 10012 | ? | INVESTIGATE |
| 10030 | ? | INVESTIGATE |
| 11001 | BwdServer main | KEEP |
| 11002 | BwdServer k8s | REVIEW |
| 12002 | ? | INVESTIGATE |
| 13002 | ? | INVESTIGATE |

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
