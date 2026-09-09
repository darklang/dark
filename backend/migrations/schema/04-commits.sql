-- Commits, and the per-source agreement a sync diffs against.

-- COMMITS: the checkpoint record. Authoring is live-on-write, so a commit is NOT a gate -- an op
-- takes effect the moment it folds. A commit is the one human moment: review what changed, let
-- propagation run over the FINAL versions (so five edits to one fn collapse to one repoint),
-- surface conflicts, mark the dep-closed set. Ops with `commit_hash IS NULL` are the DRAFT.
CREATE TABLE IF NOT EXISTS commits (
  -- Content-derived, over the message, author, stamp, PARENT and the sorted ids of the ops it names.
  -- The same work committed onto the same history gets the same id on two machines; the same work onto
  -- different histories does not, the way a git commit's id depends on its parent.
  hash TEXT PRIMARY KEY,
  message TEXT NOT NULL DEFAULT '',
  author TEXT NOT NULL DEFAULT '',
  origin_ts TEXT NOT NULL,          -- portable authoring stamp, same clock as package_ops.origin_ts
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  parent TEXT NOT NULL DEFAULT ''   -- the commit this one follows; '' for the first in a store
);
CREATE INDEX IF NOT EXISTS idx_commits_created ON commits(created_at);


-- The per-name BASE for a SYNC SOURCE: the hash this instance and that source last AGREED on.
-- Mirrors branch_name_bases, keyed by source id (a relay url, or "file:<path>") -- a sync-import IS
-- a merge, the incoming ops being a delta against the state at the last common sync. So ONE
-- base-agnostic detector serves both, each asking "did BOTH sides move this name since the base?".
-- A MISSING row means no recorded agreement, which is NOT a conflict.
CREATE TABLE IF NOT EXISTS sync_bases (
  source_id TEXT NOT NULL,          -- the peer/relay identity (a url), or "file:<path>" for a file import
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  base_hash TEXT NOT NULL,          -- the agreed item_hash ('' = the name did not exist at the base)
  updated_at TEXT NOT NULL DEFAULT (datetime('now')),
  PRIMARY KEY (source_id, owner, modules, name)
);
CREATE INDEX IF NOT EXISTS idx_sync_bases_source ON sync_bases(source_id);
