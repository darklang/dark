-- Content-addressed blob storage.
-- See thinking/blobs-and-streams/00-design.md.
-- Bytes keyed by SHA-256 hash; dedup comes for free via PK uniqueness.
-- Orphan sweeping lives in a later phase (L.3).
CREATE TABLE IF NOT EXISTS package_blobs (
  hash TEXT PRIMARY KEY,
  length INTEGER NOT NULL,
  bytes BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);
