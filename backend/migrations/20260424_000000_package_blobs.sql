-- Content-addressed blob storage.
-- Bytes keyed by SHA-256 hash; dedup comes for free via PK uniqueness.
-- Orphan rows are reclaimed by `LibPackageManager.RuntimeTypes.Blob.sweepOrphans`.
CREATE TABLE IF NOT EXISTS package_blobs (
  hash TEXT PRIMARY KEY,
  length INTEGER NOT NULL,
  bytes BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);
