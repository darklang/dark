-- The sync journal (`LibDB.OpSync`): an append-only record of every change to the op log, the
-- commits and the branch registry, so a removal travels to another store instead of coming back
-- from it.
--
-- Filled by the triggers below, never by the code that makes the change, so a discard, a merge's
-- move, an archive and the draft refresh's delete-and-reinsert are all caught without touching
-- them. Each entry says what one row is NOW: `op` (the op is on this branch, in this commit),
-- `op-gone` (it is no longer on this branch), `commit` (this commit is in this branch's history) or
-- `branch` (this branch has this name and these marks). `key` names the row, and the latest entry
-- for a key wins.
--
-- `id` is random, so an entry keeps its identity as it travels and a store holds each one once.
-- `at` is milliseconds, and never less than one past the newest entry the store holds, its own or
-- one that arrived: an entry made here after an import sorts after everything imported. Ties are
-- broken by `id`, so every store orders the same entries the same way. `seq` is this store's own
-- order.
--
-- Canonical, like the log it describes: not a projection, and a schema change keeps it. A purge
-- and a seed export empty it. A new table, so an existing store gets it from this file at
-- startup; the triggers run in the pass after the release steps.
CREATE TABLE IF NOT EXISTS sync_journal (
  seq INTEGER PRIMARY KEY,
  id TEXT NOT NULL UNIQUE,
  at INTEGER NOT NULL,
  kind TEXT NOT NULL,
  key TEXT NOT NULL,
  branch TEXT NOT NULL,
  op TEXT NOT NULL DEFAULT '',
  commit_hash TEXT NULL,
  name TEXT NULL,
  merged_at TEXT NULL,
  archived_at TEXT NULL
);

-- Holds a row only inside an import's transaction, while it applies entries that arrived with
-- their own ids. The triggers stay quiet then, so an imported change is not journalled again.
CREATE TABLE IF NOT EXISTS sync_replaying (
  held INTEGER PRIMARY KEY
);

CREATE INDEX IF NOT EXISTS idx_sync_journal_at ON sync_journal(at);
CREATE INDEX IF NOT EXISTS idx_sync_journal_key ON sync_journal(key, at);

CREATE TRIGGER IF NOT EXISTS sync_journal_op_added AFTER INSERT ON package_ops
WHEN NOT EXISTS (SELECT 1 FROM sync_replaying)
BEGIN
  INSERT INTO sync_journal (id, at, kind, key, branch, op, commit_hash)
  VALUES (
    lower(hex(randomblob(16))),
    MAX(CAST((julianday('now') - 2440587.5) * 86400000 AS INTEGER),
        COALESCE((SELECT MAX(at) FROM sync_journal), 0) + 1),
    'op', 'op ' || NEW.branch_id || ' ' || NEW.id, NEW.branch_id, NEW.id, NEW.commit_hash);
END;

-- A merge moves a row to the parent: gone from one branch, on the other.
CREATE TRIGGER IF NOT EXISTS sync_journal_op_changed
AFTER UPDATE OF branch_id, commit_hash ON package_ops
WHEN (OLD.branch_id IS NOT NEW.branch_id OR OLD.commit_hash IS NOT NEW.commit_hash)
  AND NOT EXISTS (SELECT 1 FROM sync_replaying)
BEGIN
  INSERT INTO sync_journal (id, at, kind, key, branch, op, commit_hash)
  SELECT
    lower(hex(randomblob(16))),
    MAX(CAST((julianday('now') - 2440587.5) * 86400000 AS INTEGER),
        COALESCE((SELECT MAX(at) FROM sync_journal), 0) + 1),
    'op-gone', 'op ' || OLD.branch_id || ' ' || OLD.id, OLD.branch_id, OLD.id, OLD.commit_hash
  WHERE OLD.branch_id IS NOT NEW.branch_id;
  INSERT INTO sync_journal (id, at, kind, key, branch, op, commit_hash)
  VALUES (
    lower(hex(randomblob(16))),
    MAX(CAST((julianday('now') - 2440587.5) * 86400000 AS INTEGER),
        COALESCE((SELECT MAX(at) FROM sync_journal), 0) + 1),
    'op', 'op ' || NEW.branch_id || ' ' || NEW.id, NEW.branch_id, NEW.id, NEW.commit_hash);
END;

CREATE TRIGGER IF NOT EXISTS sync_journal_op_gone AFTER DELETE ON package_ops
WHEN NOT EXISTS (SELECT 1 FROM sync_replaying)
BEGIN
  INSERT INTO sync_journal (id, at, kind, key, branch, op, commit_hash)
  VALUES (
    lower(hex(randomblob(16))),
    MAX(CAST((julianday('now') - 2440587.5) * 86400000 AS INTEGER),
        COALESCE((SELECT MAX(at) FROM sync_journal), 0) + 1),
    'op-gone', 'op ' || OLD.branch_id || ' ' || OLD.id, OLD.branch_id, OLD.id, OLD.commit_hash);
END;

CREATE TRIGGER IF NOT EXISTS sync_journal_commit_added AFTER INSERT ON commits
WHEN NOT EXISTS (SELECT 1 FROM sync_replaying)
BEGIN
  INSERT INTO sync_journal (id, at, kind, key, branch, commit_hash)
  VALUES (
    lower(hex(randomblob(16))),
    MAX(CAST((julianday('now') - 2440587.5) * 86400000 AS INTEGER),
        COALESCE((SELECT MAX(at) FROM sync_journal), 0) + 1),
    'commit', 'commit ' || NEW.hash, NEW.branch_id, NEW.hash);
END;

-- A merge moves the branch's commits to the parent.
CREATE TRIGGER IF NOT EXISTS sync_journal_commit_moved AFTER UPDATE OF branch_id ON commits
WHEN OLD.branch_id IS NOT NEW.branch_id AND NOT EXISTS (SELECT 1 FROM sync_replaying)
BEGIN
  INSERT INTO sync_journal (id, at, kind, key, branch, commit_hash)
  VALUES (
    lower(hex(randomblob(16))),
    MAX(CAST((julianday('now') - 2440587.5) * 86400000 AS INTEGER),
        COALESCE((SELECT MAX(at) FROM sync_journal), 0) + 1),
    'commit', 'commit ' || NEW.hash, NEW.branch_id, NEW.hash);
END;

CREATE TRIGGER IF NOT EXISTS sync_journal_branch_added AFTER INSERT ON branches
WHEN NOT EXISTS (SELECT 1 FROM sync_replaying)
BEGIN
  INSERT INTO sync_journal (id, at, kind, key, branch, name, merged_at, archived_at)
  VALUES (
    lower(hex(randomblob(16))),
    MAX(CAST((julianday('now') - 2440587.5) * 86400000 AS INTEGER),
        COALESCE((SELECT MAX(at) FROM sync_journal), 0) + 1),
    'branch', 'branch ' || NEW.id, NEW.id, NEW.name, NEW.merged_at, NEW.archived_at);
END;

-- A rename, a merge or an archive.
CREATE TRIGGER IF NOT EXISTS sync_journal_branch_changed
AFTER UPDATE OF name, merged_at, archived_at ON branches
WHEN (OLD.name IS NOT NEW.name OR OLD.merged_at IS NOT NEW.merged_at
      OR OLD.archived_at IS NOT NEW.archived_at)
  AND NOT EXISTS (SELECT 1 FROM sync_replaying)
BEGIN
  INSERT INTO sync_journal (id, at, kind, key, branch, name, merged_at, archived_at)
  VALUES (
    lower(hex(randomblob(16))),
    MAX(CAST((julianday('now') - 2440587.5) * 86400000 AS INTEGER),
        COALESCE((SELECT MAX(at) FROM sync_journal), 0) + 1),
    'branch', 'branch ' || NEW.id, NEW.id, NEW.name, NEW.merged_at, NEW.archived_at);
END;
