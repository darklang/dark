-- Which ops the fold has applied: every `package_ops` row whose `seq` is at or below the mark here,
-- except the rows `package_ops_unapplied` lists. Applying ops moves one number rather than writing to
-- every op it applied. `LibDB.AppliedOps` holds the SQL that reads and moves it.
--
-- The exceptions are ops at or below the mark that still have to fold: one this build cannot read,
-- kept so a build that can still will, or one an author's save moved the mark past while it waited.
--
-- A schema change and a seed export set the mark back to 0, so the next start folds the whole log.
CREATE TABLE IF NOT EXISTS package_ops_applied_through (
  id INTEGER PRIMARY KEY CHECK (id = 0),
  seq INTEGER NOT NULL
);
INSERT OR IGNORE INTO package_ops_applied_through (id, seq) VALUES (0, 0);

CREATE TABLE IF NOT EXISTS package_ops_unapplied (
  seq INTEGER PRIMARY KEY
);
