# Kill PostgreSQL and YugaByte

**Status**: [x] Complete

## What's Being Removed?

Database volume mounts for PostgreSQL and YugaByte. We use SQLite for local development.

## Devcontainer Changes

In `.devcontainer/devcontainer.json`, remove these mounts:
```json
// Postgres
"type=volume,src=pgconf,dst=/etc/postgresql",
"type=volume,src=pglogs,dst=/var/log/postgresql",
"type=volume,src=pgdata,dst=/var/lib/postgresql",

// Yugabyte
"type=volume,src=yugabyte,dst=/home/dark/yugabyte-data",
```

## Scripts to Check

```bash
grep -r "postgres\|yugabyte\|pgdata\|pgconf\|pglogs" --include="*.sh" scripts/
```

Known script:
- `scripts/build/clear-all-local-dbs` - Clears Docker volumes including pg*

## Steps

1. [ ] Edit `.devcontainer/devcontainer.json`:
   - Remove PostgreSQL volume mounts (pgconf, pglogs, pgdata)
   - Remove YugaByte volume mount
2. [ ] Check `scripts/build/clear-all-local-dbs` - update or delete
3. [ ] Search for other postgres/yugabyte references
4. [ ] Run `./scripts/run-backend-tests`
5. [ ] Commit: `trim: remove PostgreSQL and YugaByte volumes`

## Commit Message Template

```
trim: remove PostgreSQL and YugaByte volumes

- Remove PostgreSQL volume mounts from devcontainer.json
- Remove YugaByte volume mount
- Update clear-all-local-dbs script

We use SQLite for local development. No PostgreSQL/YugaByte needed.
```

## Notes

- This removes volumes only - no actual database server was installed in Dockerfile
- The `clear-all-local-dbs` script may need updating to only handle SQLite
- SQLite-related mounts should be preserved
