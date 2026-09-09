-- What a reconciliation could not decide by itself (`conflicts`) and what a person decided in
-- advance (`propagation_policy`). Both are folded from ops; neither is authoritative.

-- CONFLICTS: recorded at RECONCILIATION (merge / rebase / sync-import) when two divergent op sets
-- both rebound the same name to DIFFERENT hashes since a common base. Reconciliation auto-resolves
-- deterministically (origin_ts LWW) AND records the conflict as 'pending', so the loser is not
-- silently gone: a human acks the auto-pick or overrides it. NOT written on the plain local
-- authoring path, since a sequential self-update is not a conflict.
CREATE TABLE IF NOT EXISTS conflicts (
  id TEXT PRIMARY KEY,              -- stable short id
  owner TEXT NOT NULL,              -- the location (name) in contention
  modules TEXT NOT NULL,
  name TEXT NOT NULL,
  item_type TEXT NOT NULL,
  -- WHICH part of a declaration, for a doc divergence: 'item', 'record-field:x', 'enum-case:X',
  -- 'parameter:0'. Empty for a name divergence, which is about the whole binding.
  part TEXT NOT NULL DEFAULT '',
  kind TEXT NOT NULL,               -- 'same-name-different-hash' (room for cap-change / sig-break later)
  candidates TEXT NOT NULL,         -- JSON: [{ hash, origin_ts, author }] -- the competing versions
  auto_resolved_to TEXT NOT NULL,   -- the winning hash reconciliation picked
  reason TEXT NOT NULL,             -- 'later-origin-ts' | 'tie-break-by-hash' | ...
  status TEXT NOT NULL DEFAULT 'pending',  -- pending | acked | overridden
  resolved_by TEXT,                 -- resolution op id, set on override
  origin_ts TEXT,                   -- cross-instance LWW of the conflict record itself
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  -- Which branch this divergence is ON (default main). Without it, settling a conflict from a branch
  -- would close a store-wide record while the fix lives in an unmerged overlay.
  branch_id TEXT NOT NULL DEFAULT '00000000-0000-0000-0000-000000000001'
);
CREATE INDEX IF NOT EXISTS idx_conflicts_status ON conflicts(status);
CREATE INDEX IF NOT EXISTS idx_conflicts_branch ON conflicts(branch_id);
CREATE INDEX IF NOT EXISTS idx_conflicts_name ON conflicts(owner, modules, name);


--------------------
-- Propagation policy
--------------------
-- Which items follow a dependency when it moves, and which stay put. Ownership may inform the
-- DEFAULT, but an explicit row here always wins. Resolution is most-specific-first (item, then
-- module, then each parent module, then the default), the same shape name resolution has;
-- name = '' means a MODULE-level choice covering everything beneath it.
--
-- BRANCH-SCOPED, like everything else a branch can change. A branch inherits main's choices and can
-- override them. The default below is main's well-known uuid, which code spells as
-- `ProgramTypes.BranchId.Main` and never as a literal.
--
-- Derived: folded from `Decision` ops and nothing else writes here, so this is a projection listed
-- in `Seed.projectionTables`. Drop it and the log rebuilds it.
CREATE TABLE IF NOT EXISTS propagation_policy (
  branch_id TEXT NOT NULL DEFAULT '00000000-0000-0000-0000-000000000001',
  owner TEXT NOT NULL,
  modules TEXT NOT NULL,
  name TEXT NOT NULL,               -- '' = a module-level choice
  policy TEXT NOT NULL,             -- 'follow' | 'pin'
  note TEXT,                        -- why, in the author's words; a pin without a reason ages badly
  origin_ts TEXT,                   -- cross-instance LWW, same as everything else
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  PRIMARY KEY (branch_id, owner, modules, name)
);
