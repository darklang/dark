-- Branches: an id, the ops tagged onto it, the per-name fork points, and the relay's copy of a
-- pushed bundle. A branch is an OVERLAY of ops, not a copy of the store.

-- A branch = a stable ID + an optional name alias + a FRONTIER of ops. Refer to a branch BY ID;
-- `name` is a mutable alias. A branch's authored ops are inserted `effective = 0` (present in the
-- shared log, NOT folded into main) and tagged in `op_branches`; its overlay package manager is
-- withExtraOps(core, the branch's ops), and MERGE up flips those ops `effective = 1` and folds.
CREATE TABLE IF NOT EXISTS branches (
  id TEXT PRIMARY KEY,                 -- stable branch id (the handle); name is an alias
  name TEXT NOT NULL DEFAULT '',
  parent_id TEXT NOT NULL DEFAULT '00000000-0000-0000-0000-000000000001',  -- branches off branches: the parent (default main)
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  merged_at TIMESTAMP NULL,            -- set when the branch's work is merged into its parent
  archived_at TIMESTAMP NULL           -- set when the branch is archived (soft delete)
);
-- Deliberately NOT unique, even among live branches: two instances can each start a `fix-auth`,
-- and after a sync both rows live here. `resolveOrCreate` handles the local
-- don't-start-two-under-one-name race.
CREATE INDEX IF NOT EXISTS idx_branches_name ON branches(name) WHERE name != '';
CREATE INDEX IF NOT EXISTS idx_branches_parent ON branches(parent_id);

-- The per-branch frontier: which ops belong to a branch (many-to-many; an op can be shared).
CREATE TABLE IF NOT EXISTS op_branches (
  op_id TEXT NOT NULL,
  branch_id TEXT NOT NULL,
  -- What put this op on the branch: 'op' = authored, 'propagation' = a repoint that followed an edit,
  -- 'resolution' = a conflict override or a pin's rollback. The branch twin of `locations.source`;
  -- without it a branch cannot tell "you typed this" from "this followed something", so `status`,
  -- `undo` and `pin` could only answer about main. Local metadata: it does not travel in a bundle.
  source TEXT NOT NULL DEFAULT 'op',
  PRIMARY KEY (op_id, branch_id)
);
CREATE INDEX IF NOT EXISTS idx_op_branches_branch ON op_branches(branch_id);

-- The per-name BASE for a branch: main's content-hash for a name when the branch FIRST touched it
-- ('' if the name was new). Content hashes are STABLE across a reload (unlike origin_ts, which
-- reload re-stamps). A merge CONFLICT = main's CURRENT hash for the name differs from this base.
CREATE TABLE IF NOT EXISTS branch_name_bases (
  branch_id TEXT NOT NULL,
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  base_hash TEXT NOT NULL,   -- main's item_hash for this name at first touch ('' = the name was new)
  PRIMARY KEY (branch_id, owner, modules, name)
);
CREATE INDEX IF NOT EXISTS idx_branch_name_bases_branch ON branch_name_bases(branch_id);

-- RELAY-side branch storage: a relay is store-and-forward, so it keeps pushed branch BUNDLES (the
-- export JSON) keyed by (owner, branch_id) and hands them back on pull. Only a relay populates it.
CREATE TABLE IF NOT EXISTS relay_branches (
  owner TEXT NOT NULL,
  branch_id TEXT NOT NULL,
  bundle TEXT NOT NULL,
  -- What the stored bundle CONTAINS, so a push can be compared with it without re-parsing: the
  -- newest op stamp in it, and how many ops. The relay keeps one bundle per branch and used to
  -- replace it on every push, so pushing an old copy of a branch served that old copy to everyone
  -- and the newest commits were gone.
  --
  -- A store that already exists gets these from `LibDB.Releases`, not from an incremental .sql:
  -- `relay_branches` is hosted data rather than a projection, so the refold that follows a schema
  -- change leaves it alone, and SQLite has no `ADD COLUMN IF NOT EXISTS`, so a plain ALTER is right
  -- for an old store and fails on every fresh one.
  max_ts TEXT NOT NULL DEFAULT '',
  op_count INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY (owner, branch_id)
);
CREATE INDEX IF NOT EXISTS idx_relay_branches_owner ON relay_branches(owner);
