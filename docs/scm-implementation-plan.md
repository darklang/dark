# SCM Implementation Plan

## Overview

Replace `is_wip` boolean with proper `commit_id` reference. Commits are an ordered list (not a tree).

## Phase 1: Database Schema Changes

### New Migration: `20260128_000000_scm_commits.sql`

```sql
-- Create commits table
CREATE TABLE IF NOT EXISTS commits (
  id TEXT PRIMARY KEY,
  message TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX idx_commits_created ON commits(created_at);

-- Add commit_id to package_ops (nullable - NULL means WIP)
ALTER TABLE package_ops ADD COLUMN commit_id TEXT REFERENCES commits(id);

-- Migrate existing data:
-- - is_wip = 0 ops get a "legacy" commit
-- - is_wip = 1 ops stay NULL (WIP)
INSERT INTO commits (id, message, created_at)
SELECT
  '00000000-0000-0000-0000-000000000001',
  'Legacy committed ops (pre-SCM)',
  MIN(created_at)
FROM package_ops
WHERE is_wip = 0
HAVING COUNT(*) > 0;

UPDATE package_ops
SET commit_id = '00000000-0000-0000-0000-000000000001'
WHERE is_wip = 0;

-- Update locations table similarly
ALTER TABLE locations ADD COLUMN commit_id TEXT REFERENCES commits(id);

UPDATE locations
SET commit_id = '00000000-0000-0000-0000-000000000001'
WHERE is_wip = 0;

-- Create indexes for efficient queries
CREATE INDEX idx_package_ops_commit ON package_ops(commit_id);
CREATE INDEX idx_package_ops_wip ON package_ops(commit_id) WHERE commit_id IS NULL;
CREATE INDEX idx_locations_commit ON locations(commit_id);

-- Can drop is_wip columns after confirming migration works
-- ALTER TABLE package_ops DROP COLUMN is_wip;
-- ALTER TABLE locations DROP COLUMN is_wip;
```

## Phase 2: Backend Builtins

### File: `backend/src/BuiltinPM/Libs/SCM.fs`

```fsharp
module Builtin.Libs.SCM

// Get all WIP ops (commit_id IS NULL)
let scmGetWipOps () : List<PackageOp>

// Get all commits, ordered by date descending
let scmGetCommits (limit: Int64) : List<Commit>
  // Commit = { id: Uuid, message: String, createdAt: DateTime, opCount: Int64 }

// Get ops for a specific commit
let scmGetCommitOps (commitId: Uuid) : List<PackageOp>

// Create a commit from all WIP ops
// Returns the new commit ID on success, or error message
let scmCommit (message: String) : Result<Uuid, String>
  // 1. Run validation checks
  // 2. Generate new commit UUID
  // 3. INSERT INTO commits
  // 4. UPDATE package_ops SET commit_id = ? WHERE commit_id IS NULL
  // 5. UPDATE locations SET commit_id = ? WHERE commit_id IS NULL
  // 6. Return commit ID

// Discard all WIP ops (delete them and their effects)
let scmDiscard () : Result<Int64, String>
  // 1. Get all WIP ops
  // 2. Reverse-apply their effects (delete from projection tables)
  // 3. DELETE FROM package_ops WHERE commit_id IS NULL
  // 4. DELETE FROM locations WHERE commit_id IS NULL
  // 5. Return count of discarded ops

// Get WIP summary (counts by type)
let scmGetWipSummary () : WipSummary
  // WipSummary = {
  //   addedTypes: Int64,
  //   addedValues: Int64,
  //   addedFns: Int64,
  //   renamedItems: Int64
  // }
```

### Validation Checks (in scmCommit)

#### Blocking Constraints (commit fails)

1. **Unresolved References**: All WIP items must only reference:
   - Other WIP items (will be committed together)
   - Already-committed items
   - Stdlib/builtin items

2. **Duplicate Names in WIP**: No two WIP items can claim the same location

3. **Parse Errors**: Code must parse without errors

#### Warning Constraints (commit proceeds with warning)

1. **Shadowing Committed**: WIP location shadows an existing committed location
   - Warning: "greet shadows existing committed function"
   - Allowed because this is how you update things

2. **Unused Items**: WIP item not referenced by anything
   - Warning: "helperFn is not used by any other item"
   - Allowed because it might be used later

3. **Deprecation Without Replacement**: Deprecating something without providing alternative
   - Warning: "oldFn is deprecated but has no replacement specified"

## Phase 3: Darklang Package API

### File: `packages/darklang/scm/packageOps.dark`

```darklang
module Darklang.SCM

module Commit =
  type Commit =
    { id: Uuid
      message: String
      createdAt: DateTime
      opCount: Int64 }

module PackageOps =
  /// Add ops as WIP (commit_id = NULL)
  let addWip (ops: List<LanguageTools.ProgramTypes.PackageOp>)
    : Stdlib.Result.Result<Int64, String> =
    Builtin.scmAddOps ops  // Modified to always add as WIP

  /// Get all WIP ops
  let getWip () : List<LanguageTools.ProgramTypes.PackageOp> =
    Builtin.scmGetWipOps ()

  /// Get WIP summary
  let getWipSummary () : WipSummary =
    Builtin.scmGetWipSummary ()

  /// Get recent commits
  let getCommits (limit: Int64) : List<Commit.Commit> =
    Builtin.scmGetCommits limit

  /// Get ops for a specific commit
  let getCommitOps (commitId: Uuid) : List<LanguageTools.ProgramTypes.PackageOp> =
    Builtin.scmGetCommitOps commitId

  /// Commit all WIP with a message
  let commit (message: String) : Stdlib.Result.Result<Uuid, String> =
    Builtin.scmCommit message

  /// Discard all WIP
  let discard () : Stdlib.Result.Result<Int64, String> =
    Builtin.scmDiscard ()
```

## Phase 4: CLI Commands

### File: `packages/darklang/cli/scm/status.dark`

```darklang
module Darklang.Cli.SCM.Status

let execute (state: AppState) (args: List<String>) : AppState =
  let summary = SCM.PackageOps.getWipSummary ()

  if summary.total == 0L then
    Stdlib.printLine "No uncommitted changes."
  else
    Stdlib.printLine "Uncommitted changes:"
    Stdlib.printLine $"  {summary.addedTypes} type(s)"
    Stdlib.printLine $"  {summary.addedValues} value(s)"
    Stdlib.printLine $"  {summary.addedFns} function(s)"
    Stdlib.printLine $"  {summary.renamedItems} rename(s)"
    Stdlib.printLine ""
    Stdlib.printLine "Use 'scm commit \"message\"' to commit."
    Stdlib.printLine "Use 'scm discard' to discard changes."

  state
```

### File: `packages/darklang/cli/scm/log.dark`

```darklang
module Darklang.Cli.SCM.Log

let execute (state: AppState) (args: List<String>) : AppState =
  let limit =
    match args with
    | [limitStr] ->
      match Stdlib.Int64.parse limitStr with
      | Ok n -> n
      | Error _ -> 20L
    | _ -> 20L

  let commits = SCM.PackageOps.getCommits limit

  if Stdlib.List.isEmpty commits then
    Stdlib.printLine "No commits yet."
  else
    commits |> Stdlib.List.iter (fun c ->
      let dateStr = Stdlib.DateTime.toString c.createdAt
      let shortId = Stdlib.String.take (Stdlib.Uuid.toString c.id) 8L
      Stdlib.printLine $"{shortId}  {dateStr}  ({c.opCount} ops)"
      Stdlib.printLine $"    {c.message}"
      Stdlib.printLine ""
    )

  state
```

### File: `packages/darklang/cli/scm/commit.dark`

```darklang
module Darklang.Cli.SCM.Commit

let execute (state: AppState) (args: List<String>) : AppState =
  let autoConfirm = Stdlib.List.member_v0 args "--yes" || Stdlib.List.member_v0 args "-y"

  // Get message from args (everything that's not a flag)
  let messageParts =
    args |> Stdlib.List.filter (fun a ->
      Stdlib.Bool.not (Stdlib.String.startsWith a "--") &&
      Stdlib.Bool.not (Stdlib.String.startsWith a "-"))

  let message = Stdlib.String.join messageParts " "

  if Stdlib.String.isEmpty message then
    Stdlib.printLine (View.formatError "Commit message required.")
    Stdlib.printLine "Usage: scm commit \"Your commit message\""
    state
  else
    // Show what will be committed
    let summary = SCM.PackageOps.getWipSummary ()

    if summary.total == 0L then
      Stdlib.printLine "Nothing to commit."
      state
    else
      Stdlib.printLine "Will commit:"
      Stdlib.printLine $"  {summary.addedTypes} type(s)"
      Stdlib.printLine $"  {summary.addedValues} value(s)"
      Stdlib.printLine $"  {summary.addedFns} function(s)"
      Stdlib.printLine ""

      let proceed =
        if autoConfirm then
          true
        else
          Stdlib.printLine "Proceed? (y/n): "
          let response = Builtin.stdinReadLine ()
          response == "y" || response == "Y"

      if proceed then
        match SCM.PackageOps.commit message with
        | Ok commitId ->
          let shortId = Stdlib.String.take (Stdlib.Uuid.toString commitId) 8L
          Stdlib.printLine (View.formatSuccess $"Created commit {shortId}")
          state
        | Error e ->
          Stdlib.printLine (View.formatError $"Commit failed: {e}")
          state
      else
        Stdlib.printLine "Commit cancelled."
        state
```

### File: `packages/darklang/cli/scm/discard.dark`

```darklang
module Darklang.Cli.SCM.Discard

let execute (state: AppState) (args: List<String>) : AppState =
  let autoConfirm = Stdlib.List.member_v0 args "--yes" || Stdlib.List.member_v0 args "-y"

  let summary = SCM.PackageOps.getWipSummary ()

  if summary.total == 0L then
    Stdlib.printLine "Nothing to discard."
    state
  else
    Stdlib.printLine "Will discard:"
    Stdlib.printLine $"  {summary.addedTypes} type(s)"
    Stdlib.printLine $"  {summary.addedValues} value(s)"
    Stdlib.printLine $"  {summary.addedFns} function(s)"
    Stdlib.printLine ""

    let proceed =
      if autoConfirm then
        true
      else
        Stdlib.printLine (Colors.warning "This cannot be undone. Proceed? (y/n): ")
        let response = Builtin.stdinReadLine ()
        response == "y" || response == "Y"

    if proceed then
      match SCM.PackageOps.discard () with
      | Ok count ->
        Stdlib.printLine (View.formatSuccess $"Discarded {count} ops.")
        state
      | Error e ->
        Stdlib.printLine (View.formatError $"Discard failed: {e}")
        state
    else
      Stdlib.printLine "Discard cancelled."
      state
```

### File: `packages/darklang/cli/scm/show.dark`

```darklang
module Darklang.Cli.SCM.Show

let execute (state: AppState) (args: List<String>) : AppState =
  match args with
  | [] ->
    Stdlib.printLine "Usage: scm show <commit-id>"
    state
  | [commitIdStr] ->
    match Stdlib.Uuid.parse commitIdStr with
    | Ok commitId ->
      let ops = SCM.PackageOps.getCommitOps commitId

      if Stdlib.List.isEmpty ops then
        Stdlib.printLine "Commit not found or has no ops."
      else
        Stdlib.printLine $"Commit {commitIdStr}:"
        Stdlib.printLine ""
        ops |> Stdlib.List.iter (fun op ->
          let opStr = Darklang.PrettyPrinter.ProgramTypes.packageOpToString op
          Stdlib.printLine $"  {opStr}"
        )

      state
    | Error _ ->
      Stdlib.printLine (View.formatError "Invalid commit ID format.")
      state
  | _ ->
    Stdlib.printLine "Usage: scm show <commit-id>"
    state
```

### Register SCM Commands

In `packages/darklang/cli/core.dark`, add to `allCommands`:

```darklang
("scm", "Source control commands", [], SCM.execute, SCM.help, SCM.complete)
```

With `SCM.execute` routing to subcommands:
- `scm status` → Status.execute
- `scm log` → Log.execute
- `scm commit` → Commit.execute
- `scm discard` → Discard.execute
- `scm show` → Show.execute

## Phase 5: Modify Existing Code

### Update `scmAddOps`

Change signature from `scmAddOps(isWip, ops)` to `scmAddOps(ops)` - always adds as WIP.

The `isWip` parameter is no longer needed because:
- New ops always start as WIP (commit_id = NULL)
- `scmCommit` moves WIP → committed

### Update Package Loading (LocalExec)

When LocalExec loads packages from disk, it should create an "init" commit:

```fsharp
// In LocalExec.HandleCommand.reloadPackages:
let reloadPackages () : Ply<Result<unit, string>> =
  uply {
    let! ops = LoadPackagesFromDisk.load Builtins.all

    print "Purging ..."
    do! LibPackageManager.Purge.purge ()

    print "Filling ..."
    // Create "init" commit with all packages from disk
    let! commitId = LibPackageManager.Inserts.insertAndApplyOpsWithCommit
                      "Init: packages loaded from disk"
                      ops

    print $"Created init commit {commitId}"
    // ... rest of function
  }
```

This means:
- Fresh DB start: All packages go into a single "init" commit
- Subsequent file edits: New ops added as WIP until explicitly committed

For development workflow:
1. `reload-packages` creates/updates "init" commit with all disk packages
2. Runtime edits (via CLI `fn`, `type`, `val` commands) create WIP ops
3. `scm commit` moves WIP to a new commit

## Implementation Order

1. **Migration** - Add commits table, commit_id columns
2. **Backend builtins** - scmGetWipOps, scmCommit, scmDiscard, scmGetCommits, etc.
3. **Darklang API** - SCM.PackageOps module updates
4. **CLI commands** - status, log, commit, discard, show
5. **Update existing code** - Remove isWip parameter, update callers
6. **Validation** - Add pre-commit checks

## Example Session

```
$ dark
> fn greet (name: String) : String = $"Hello, {name}!"

> scm status
Uncommitted changes:
  0 type(s)
  0 value(s)
  1 function(s)

Use 'scm commit "message"' to commit.

> scm commit "Add greet function"
Will commit:
  1 function(s)

Proceed? (y/n): y
Created commit a1b2c3d4

> scm log
a1b2c3d4  2026-01-28 12:34:56  (2 ops)
    Add greet function

00000000  2026-01-15 00:00:00  (1523 ops)
    Legacy committed ops (pre-SCM)
```
