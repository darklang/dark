# Trim BuiltinDarkInternal

**Status**: [ ] Not started

## What is BuiltinDarkInternal?

BuiltinDarkInternal provides builtin functions for Dark-internal operations, accessible only within the internal canvas. We're trimming parts that relate to cloud infrastructure we're removing.

## Modules to Remove/Trim

Located in `backend/src/BuiltinDarkInternal/Libs/`:

| Module | Status | Notes |
|--------|--------|-------|
| `F404s.fs` | **DELETE** | 404 tracking for cloud deployment |
| `Infra.fs` | **DELETE** | Logging/table size inspection for cloud |
| `Workers.fs` | **DELETE** | Queue management, scheduling rules |
| `Secrets.fs` | **REVIEW** | May still be useful - check usage |
| `Canvases.fs` | KEEP | Core functionality |
| `DBs.fs` | KEEP | Core functionality |
| `Domains.fs` | KEEP | Core functionality |
| `Users.fs` | KEEP | Core functionality |

## Files to Modify

```
backend/src/BuiltinDarkInternal/
  Builtin.fs         <- Remove references to deleted modules
  Libs/
    F404s.fs         <- DELETE
    Infra.fs         <- DELETE
    Workers.fs       <- DELETE
    Secrets.fs       <- REVIEW (might keep)
```

## Search Commands

```bash
# Check what references these modules
grep -r "F404s\|darkInternal404" --include="*.fs" backend/
grep -r "Infra\|darkInternalInfra" --include="*.fs" backend/
grep -r "Workers\|darkInternalCanvasQueue" --include="*.fs" backend/
grep -r "Secrets\|darkInternalSecret" --include="*.fs" backend/
```

## Steps

1. [ ] Check usage of Secrets.fs - decide keep or delete
2. [ ] Delete `backend/src/BuiltinDarkInternal/Libs/F404s.fs`
3. [ ] Delete `backend/src/BuiltinDarkInternal/Libs/Infra.fs`
4. [ ] Delete `backend/src/BuiltinDarkInternal/Libs/Workers.fs`
5. [ ] Delete Secrets.fs if decided
6. [ ] Update `backend/src/BuiltinDarkInternal/Builtin.fs` to remove imports/references
7. [ ] Search for any broken references elsewhere
8. [ ] Run `./scripts/run-backend-tests`
9. [ ] Wait for build
10. [ ] Commit: `trim: remove unused BuiltinDarkInternal modules`

## Commit Message Template

```
trim: remove unused BuiltinDarkInternal modules

- Delete Libs/F404s.fs (404 tracking)
- Delete Libs/Infra.fs (cloud infrastructure functions)
- Delete Libs/Workers.fs (queue management)
- [Delete Libs/Secrets.fs if applicable]
- Update Builtin.fs to remove references

These modules provided cloud deployment functionality we no longer need.
```

## Notes

- The main `Builtin.fs` aggregates all the libs - make sure to update it
- Some functions might be referenced in `.dark` packages - search `packages/` too
- If Secrets.fs is needed for local secret management, keep it
