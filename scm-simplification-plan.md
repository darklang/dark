# SCM Simplification Plan

## Goal
Remove all complexity around accounts, branches, instances, sync, and approvals.
The words "branch", "account", "instance", "sync", "approval" should not exist in the codebase after this.

Replace with simple **Committed** vs **WIP** model for local development.

---

## Current State (to be removed)

### Tables to DROP
- `accounts_v0` - User accounts
- `branches` - Per-account work areas
- `instances` - Remote sync targets
- `syncs` - Sync history
- `namespace_access` - Approval permissions
- `approval_requests` - PR workflow
- `approval_request_items` - PR items

### Columns to remove from remaining tables
- `package_ops.instance_id` - No more remote tracking
- `package_ops.branch_id` → rename to `is_wip` (boolean)
- `locations.branch_id` → rename to `is_wip` (boolean)
- `locations.approval_status`, `reviewed_by`, `reviewed_at` - No approvals

---

## Target State

### New Model: Committed vs WIP

| State | Storage | Behavior |
|-------|---------|----------|
| **Committed** | `is_wip = false` | Permanent, canonical package state |
| **WIP** | `is_wip = true` | Work in progress, persists across sessions, requires explicit commit |

- All new ops are WIP by default
- Explicit `commit` command moves WIP → Committed
- `discard` removes WIP ops
- WIP ops shadow committed state (same name resolution, WIP takes precedence)

---

## Files to DELETE

### CLI (Dark)
- `packages/darklang/cli/account.dark`
- `packages/darklang/cli/instances.dark`
- `packages/darklang/cli/syncService.dark`
- `packages/darklang/cli/packages/branch.dark`

### SCM (Dark)
- `packages/darklang/scm/sync.dark`
- `packages/darklang/scm/instances.dark`
- `packages/darklang/scm/approvals.dark`
- `packages/darklang/scm/branch.dark`

### Backend (F#)
- `backend/src/BuiltinPM/Libs/Accounts.fs`
- `backend/src/BuiltinPM/Libs/Instances.fs`
- `backend/src/BuiltinPM/Libs/Sync.fs`
- `backend/src/BuiltinPM/Libs/Branches.fs`
- `backend/src/LibPackageManager/Instances.fs`
- `backend/src/LibPackageManager/Branches.fs`

---

## Files to MODIFY

### CLI (Dark)

**`packages/darklang/cli/core.dark`** - Simplify AppState:
```dark
type AppState =
  { isExiting: Bool
    prompt: Prompt.State
    needsFullRedraw: Bool
    packageData: Packages.State
    currentPage: Page
    // REMOVED: accountName, accountID, currentBranchId
  }
```
- Remove all account/branch initialization
- Remove sync service startup

**`packages/darklang/cli/help.dark`** - Remove:
- "Source Control" section (branch, instance commands)
- Account command from Utilities
- Update command descriptions

**`packages/darklang/scm/packageOps.dark`** - Simplify:
- Remove branch_id/instance_id parameters
- Add `isWip: Bool` parameter instead
- Simplify `add` function signature

### Backend (F#)

**`backend/src/BuiltinPM/Libs/PackageOps.fs`** - Simplify:
- Remove branch/instance parameters from builtins
- Add `isWip` boolean parameter

**`backend/src/LibPackageManager/Inserts.fs`** - Simplify:
- Remove branch/instance logic
- `insertOp(op, isWip: bool)`

**`backend/src/LibPackageManager/Queries.fs`** - Simplify:
- Remove branch-aware queries
- Simple WIP vs committed filtering

**`backend/src/BuiltinPM/BuiltinPM.fs`** - Remove registrations for deleted modules

---

## New Commands

Replace branch/instance/account commands with:

| Command | Description |
|---------|-------------|
| `status` | Show WIP changes summary |
| `commit` | Move all WIP ops to committed |
| `discard` | Delete all WIP ops |
| `diff` | Show what's in WIP vs committed |

---

## Database Migration

```sql
-- Migration: simplify_to_wip_model

-- Drop all the complex SCM tables
DROP TABLE IF EXISTS accounts_v0;
DROP TABLE IF EXISTS branches;
DROP TABLE IF EXISTS instances;
DROP TABLE IF EXISTS syncs;
DROP TABLE IF EXISTS namespace_access;
DROP TABLE IF EXISTS approval_requests;
DROP TABLE IF EXISTS approval_request_items;

-- Migrate package_ops: branch_id → is_wip
ALTER TABLE package_ops ADD COLUMN is_wip INTEGER NOT NULL DEFAULT 0;
UPDATE package_ops SET is_wip = 1 WHERE branch_id IS NOT NULL;
-- Then drop old columns (SQLite requires table rebuild)

-- Migrate locations: branch_id → is_wip, remove approval columns
ALTER TABLE locations ADD COLUMN is_wip INTEGER NOT NULL DEFAULT 0;
UPDATE locations SET is_wip = 1 WHERE branch_id IS NOT NULL;
-- Drop: branch_id, approval_status, created_by, reviewed_by, reviewed_at

-- Rebuild tables without old columns (SQLite way)
-- ... (detailed rebuild SQL)
```

---

## Implementation Order

### Phase 1: Database & Core Backend
1. Create migration to transform schema
2. Update `Inserts.fs` and `Queries.fs` for new model
3. Update `PackageOps.fs` builtins

### Phase 2: Remove Dead Code
1. Delete F# files listed above
2. Delete Dark files listed above
3. Update `BuiltinPM.fs` registrations

### Phase 3: CLI Updates
1. Simplify `core.dark` AppState
2. Add new commands (status, commit, discard)
3. Update `help.dark`
4. Remove references in navigation/UI

### Phase 4: Cleanup
1. Search for any remaining "branch", "account", "instance", "sync", "approval" strings
2. Update tests
3. Update any documentation

---

## Verification Checklist

After implementation, these searches should return NO results:
```bash
grep -r "branch" packages/darklang/cli/
grep -r "branch" packages/darklang/scm/
grep -r "account" packages/darklang/cli/
grep -r "instance" packages/darklang/cli/
grep -r "sync" packages/darklang/cli/
grep -r "approval" packages/darklang/
grep -ri "branch" backend/src/BuiltinPM/
grep -ri "branch" backend/src/LibPackageManager/
grep -ri "instance" backend/src/BuiltinPM/
grep -ri "account" backend/src/BuiltinPM/
```

(Excluding false positives like "TreeSitter" containing "tree")
