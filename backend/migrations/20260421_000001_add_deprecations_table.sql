-- Deprecations table: branch-scoped projection of Deprecate/Undeprecate ops.
--
-- Shape mirrors `locations`: branch-scoped, commit-or-WIP, superseded via
-- unlisted_at so the branch-chain visibility pattern reuses the same JOIN
-- shape the rest of the PM already uses. See thinking/deprecation-redesign.md.

CREATE TABLE IF NOT EXISTS deprecations (
  deprecation_id    TEXT PRIMARY KEY,
  branch_id         TEXT NOT NULL REFERENCES branches(id),
  commit_hash       TEXT REFERENCES commits(hash),   -- NULL = WIP
  item_hash         TEXT NOT NULL,
  item_kind         TEXT NOT NULL,                   -- 'fn' | 'type' | 'value'

  -- 'deprecated' (annotation_blob has kind + message + optional replacement ref)
  -- 'undeprecated' (annotation_blob NULL) — used for ancestor-override on child branches
  state             TEXT NOT NULL,
  annotation_blob   BLOB,

  created_at        TIMESTAMP NOT NULL DEFAULT (datetime('now')),
  unlisted_at       TIMESTAMP                        -- set when a later row supersedes this one
);

CREATE INDEX IF NOT EXISTS idx_deprecations_lookup
  ON deprecations(branch_id, item_hash, item_kind) WHERE unlisted_at IS NULL;

CREATE INDEX IF NOT EXISTS idx_deprecations_wip
  ON deprecations(branch_id) WHERE commit_hash IS NULL;

CREATE INDEX IF NOT EXISTS idx_deprecations_commit_hash
  ON deprecations(commit_hash);
