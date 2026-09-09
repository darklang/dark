-- The op log: the source of truth every projection below is derived from, plus the two indexes
-- that keep `dark status` cheap and the relay-side record of who pushed what.

--------------------
-- Op log
--------------------

-- The source of truth for all package changes: the content-addressed op log.
CREATE TABLE IF NOT EXISTS package_ops (
  id TEXT NOT NULL,
  op_blob BLOB NOT NULL,
  applied INTEGER NOT NULL DEFAULT 0,
  -- An op is always STORED (synced) but only takes EFFECT -- folds into the live projections, so it
  -- resolves and becomes callable -- when `effective = 1`. Locally-authored and trusted ops are
  -- effective on arrival; untrusted synced ops land `effective = 0`: present in the log, inert.
  effective INTEGER NOT NULL DEFAULT 1,
  -- The commit this op belongs to; NULL = DRAFT (edited and live, not yet committed). Gates nothing
  -- at runtime; it answers "what have I changed", which is what `dark status` reports.
  commit_hash TEXT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  -- Authoring timestamp, PORTABLE across sync. A locally-authored op self-stamps at insert; a SYNCED
  -- op preserves its origin, so every instance agrees and max(origin_ts) picks the same divergence
  -- winner. Distinct from `created_at`, which is local-insert time and differs per instance.
  origin_ts TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
  -- The id is the op's content hash, so INSERT OR IGNORE dedups identical re-adds.
  PRIMARY KEY (id)
);
CREATE INDEX IF NOT EXISTS idx_package_ops_created ON package_ops(created_at);
CREATE INDEX IF NOT EXISTS idx_package_ops_applied
  ON package_ops(applied) WHERE applied = 0;
-- Fast lookup of the "pending review" queue (synced but not yet effective).
CREATE INDEX IF NOT EXISTS idx_package_ops_pending
  ON package_ops(effective) WHERE effective = 0;
-- The draft: `dark status` asks this on every invocation, and it must stay cheap as the log grows.
CREATE INDEX IF NOT EXISTS idx_package_ops_draft
  ON package_ops(commit_hash) WHERE commit_hash IS NULL;

-- Who pushed which op, so a RELAY can serve "your stuff" back by identity. Many-to-many: a
-- content-addressed op can be pushed by several identities. `owner` is a NAME, not an authentication:
-- an unsigned string, trusted by convention, with nothing to stop one identity claiming another's.
-- Only a relay populates this; an instance ignores it.
CREATE TABLE IF NOT EXISTS op_owners (
  op_id TEXT NOT NULL,
  owner TEXT NOT NULL,
  PRIMARY KEY (op_id, owner)
);
CREATE INDEX IF NOT EXISTS idx_op_owners_owner ON op_owners(owner);

-- What each relay is known to HOLD of this store's effective ops, so `push` sends exactly what it does
-- not. A set, not a cursor: an op becomes effective at its AUTHORING rowid (a merge flips a branch op
-- that can sit well below the last rowid pushed), so no position in the log can say "everything above
-- here is new". Ops that arrive FROM a relay are recorded as held by it on import. Local state: never
-- synced, never seeded, purged with the log it describes. Wiping a relay means clearing its rows here.
CREATE TABLE IF NOT EXISTS sync_pushed (
  relay TEXT NOT NULL,
  op_id TEXT NOT NULL,
  PRIMARY KEY (relay, op_id)
);
