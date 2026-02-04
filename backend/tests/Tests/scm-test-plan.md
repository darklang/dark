# SCM Test Plan

## Overview

Add `backend/tests/Tests/SCM.Tests.fs` covering branches, WIP ops, commits, discard, rebase, and merge. Register as `Tests.SCM.tests` in `Tests.fs` (replacing the TODO comment at line 47-54).

All tests use the existing Expecto `testTask` pattern. No mocking needed — tests operate on the real SQLite test DB (`test-data.db`), which is wiped and migrated fresh before each test run by `run-backend-tests`.

## Isolation strategy

The test DB is shared across all tests in a single run. Since all SCM operations are branch-scoped, the primary isolation mechanism is: **each test creates its own branch with a unique GUID**.

However, some operations touch global state:

- **`Branches.list()`** returns all active branches — so assertions must use `List.exists` / `List.filter`, never assert exact list equality.
- **`Branches.getByName`** is global — use unique names per test (e.g., embed a GUID suffix).

**Never operate directly on main.** Tests that need a parent-child relationship (rebase, merge, conflict detection) should create a **two-level branch hierarchy**: a unique "fake parent" branch off main, and then the test branch off that fake parent. This way commits to the "parent" are fully isolated from other tests.

```
main
 └── fake-parent-{guid}        ← test commits "parent changes" here
      └── test-branch-{guid}   ← test commits "branch changes" here
```

Location paths should also be unique per test (e.g., `TestOwner.Mod_{guid}.valueName`) to prevent cross-test interference in location queries.

---

## Test helpers to add (top of file)

```fsharp
/// Create a minimal PackageValue for testing
let testPackageValue () : PT.PackageValue.PackageValue =
  { id = System.Guid.NewGuid()
    description = "test value"
    deprecated = PT.NotDeprecated
    body = PT.EInt64(gid(), 42L) }

/// Create an AddValue + SetValueName op pair with unique location path
let testAddValueOps (owner: string) (modules: string list) (name: string) =
  let pv = testPackageValue()
  let loc = { PT.PackageLocation.owner = owner; modules = modules; name = name }
  let addOp = PT.PackageOp.AddValue pv
  let nameOp = PT.PackageOp.SetValueName(PT.FQValueName.Package pv.id, loc)
  (addOp, nameOp)

/// Create a unique location path scoped to a test
let uniqueModules () = [ $"TestMod_{System.Guid.NewGuid().ToString("N").[..7]}" ]

/// Create a branch off a given parent for test isolation.
/// For simple tests, parent = mainBranchId.
/// For rebase/merge tests, create a fake parent first.
let createTestBranch (name: string) (parentId: PT.BranchId) =
  Branches.create name parentId

/// Set up a two-level branch hierarchy for rebase/merge tests.
/// Returns (parentBranch, childBranch).
let createIsolatedBranchPair (testName: string) =
  task {
    let! parent = Branches.create $"parent-{testName}-{Guid.NewGuid()}" PT.mainBranchId
    let! child = Branches.create $"child-{testName}-{Guid.NewGuid()}" parent.id
    return (parent, child)
  }
```

---

## Test groups

### 1. Branch CRUD (`testList "branches"`)

Each test creates its own uniquely-named branch. Assertions on `list()` use `List.exists`.

| Test | What it does |
|---|---|
| `create branch` | `Branches.create` with unique name off main → verify returned branch has correct name, parentBranchId = Some mainBranchId, mergedAt = None |
| `list includes new branch` | Create branch → `Branches.list()` → assert `List.exists (fun b -> b.id = branch.id)` |
| `get branch by id` | Create branch → `Branches.get id` → assert Some with correct fields |
| `get branch by name` | Create branch with unique name → `Branches.getByName name` → assert Some |
| `get nonexistent branch` | `Branches.get (Guid.NewGuid())` → assert None |
| `rename branch` | Create → `Branches.rename id "new-name-{guid}"` → `Branches.get id` → verify name changed |
| `rename to duplicate name fails` | Create two branches with unique names → rename second to first's name → assert Error |
| `delete branch` | Create → `Branches.delete id` → `Branches.get id` → assert None |
| `delete main branch fails` | `Branches.delete mainBranchId` → assert Error |
| `delete branch with children fails` | Create parent off main → create child off parent → delete parent → assert Error |
| `getBranchChain` | Create branch off main → `Branches.getBranchChain id` → assert `[id; mainBranchId]` |

### 2. WIP Operations (`testList "wip"`)

Each test creates its own branch. Location paths use `uniqueModules()`.

| Test | What it does |
|---|---|
| `insert WIP ops` | Create branch → `insertAndApplyOpsAsWip branch [addOp; nameOp]` → assert returns 2 |
| `getWipOps returns inserted ops` | Insert WIP → `Queries.getWipOps branch` → assert length = 2 |
| `getWipSummary counts correctly` | Insert 1 AddValue + 1 SetValueName → `Queries.getWipSummary` → verify counts |
| `WIP ops are branch-scoped` | Insert WIP on branch A → `Queries.getWipOps branchB` → assert empty |
| `duplicate ops are skipped` | Insert same ops twice → assert second call returns 0 (content-addressed dedup) |

### 3. Commit (`testList "commit"`)

Each test creates its own branch. All commits happen on test-owned branches only.

| Test | What it does |
|---|---|
| `commit WIP ops` | Insert WIP → `commitWipOps branch "test commit"` → assert Ok with commit id |
| `commit with no WIP fails` | `commitWipOps branch "empty"` on clean branch → assert Error |
| `committed ops no longer in WIP` | Insert WIP → commit → `Queries.getWipOps` → assert empty |
| `getCommits returns commit` | Insert WIP → commit → `Queries.getCommits branch 10` → assert 1 commit with correct message |
| `getCommitOps returns ops` | Insert WIP → commit → get commit id → `Queries.getCommitOps commitId` → assert ops present |
| `multiple commits ordered correctly` | Commit twice → `getCommits` → assert most recent first |

### 4. Discard (`testList "discard"`)

Each test creates its own branch.

| Test | What it does |
|---|---|
| `discard WIP ops` | Insert WIP → `discardWipOps branch` → assert Ok with count |
| `discard with no WIP fails` | `discardWipOps` on clean branch → assert Error |
| `discarded ops are gone` | Insert WIP → discard → `getWipOps` → assert empty |
| `discard doesn't affect committed ops` | Insert WIP → commit → insert more WIP → discard → `getCommits` still has 1, `getWipOps` empty |

### 5. Rebase (`testList "rebase"`)

**Uses two-level branch hierarchy** (`createIsolatedBranchPair`) so parent commits never touch main.

| Test | What it does |
|---|---|
| `rebase with no parent changes` | Create pair → commit on child only → `Rebase.rebase child` → Ok "Already up to date" |
| `rebase after parent commit` | Create pair → commit on parent → `Rebase.rebase child` → Ok, base_commit_id updated |
| `rebase detects conflicts` | Create pair → commit value at `Mod.x` on both parent and child → `Rebase.getConflicts child` → non-empty |
| `rebase fails with conflicts` | Same setup → `Rebase.rebase child` → Error with conflict list |
| `rebase no-ops on main` | `Rebase.rebase mainBranchId` → Ok "Main branch, nothing to rebase" |
| `rebase with non-overlapping changes succeeds` | Parent commits `Mod.x`, child commits `Mod.y` (unique modules) → rebase → Ok |

### 6. Merge (`testList "merge"`)

**Uses two-level branch hierarchy** so merge target is an isolated parent, not main.

| Test | What it does |
|---|---|
| `canMerge - happy path` | Create pair → commit on child → rebase → `Merge.canMerge child` → Ok |
| `canMerge - main branch fails` | `Merge.canMerge mainBranchId` → Error IsMainBranch |
| `canMerge - not rebased fails` | Create pair → commit on parent → `canMerge child` (without rebasing) → Error NotRebased |
| `canMerge - has WIP fails` | Create pair → insert WIP on child (don't commit) → `canMerge child` → Error HasWip |
| `canMerge - has children fails` | Create pair → create grandchild off child → `canMerge child` → Error HasChildren |
| `canMerge - nothing to merge` | Create pair (no commits on child) → `canMerge child` → Error NothingToMerge |
| `merge moves ops to parent` | Create pair → commit on child → rebase → merge → `getCommits parent` includes child's commit |
| `merge marks branch as merged` | Merge → `Branches.get child` → mergedAt is Some |
| `merge deprecates conflicting parent locations` | Parent has `Mod.x` → child redefines `Mod.x` → commit both, rebase (no conflict if parent committed before branch created) → merge → parent's old location deprecated, child's now active on parent |
| `merged branch not in active list` | Merge → `Branches.list()` → `not (List.exists (fun b -> b.id = child.id))` |

---

## File changes

### New file: `backend/tests/Tests/SCM.Tests.fs`

```fsharp
module Tests.SCM

open System.Threading.Tasks
open FSharp.Control.Tasks
open Expecto
open Prelude
open TestUtils.TestUtils
open LibDB.Db

module PT = LibExecution.ProgramTypes
module Branches = LibPackageManager.Branches
module Inserts = LibPackageManager.Inserts
module Queries = LibPackageManager.Queries
module Rebase = LibPackageManager.Rebase
module Merge = LibPackageManager.Merge

// ... helpers and tests as described above ...

let tests =
  testList
    "scm"
    [ testList "branches" [ ... ]
      testList "wip" [ ... ]
      testList "commit" [ ... ]
      testList "discard" [ ... ]
      testList "rebase" [ ... ]
      testList "merge" [ ... ] ]
```

### Modified: `backend/tests/Tests/Tests.fs`

Replace the TODO comment block (lines 47-54) with:

```fsharp
Tests.SCM.tests
```

### Modified: `backend/tests/Tests/Tests.fsproj`

Add `<Compile Include="SCM.Tests.fs" />` to the ItemGroup (order matters in F# — add it after Canvas.Tests.fs or similar).

---

## Notes

- **No test cleanup needed**: The test DB is wiped before each `run-backend-tests` invocation.
- **Never touch main directly**: All parent-child scenarios use a two-level hierarchy with unique fake-parent branches. This prevents any cross-test interference through shared main-branch state.
- **Unique names everywhere**: Branch names include GUIDs. Location paths use `uniqueModules()`. This prevents collisions in `getByName`, location queries, and conflict detection.
- **Assertions on global queries**: `Branches.list()` assertions use `List.exists` / `List.filter`, never exact list equality.
- **Test ordering**: Tests should not depend on ordering. Each test sets up its own state from scratch.
- **Run with**: `./scripts/run-backend-tests --filter-test-list scm` to run just the SCM tests.
