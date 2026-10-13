-- RESTORE: backend/migrations/schema/00-initial-schema.sql. The three files this came out of
--          (02-op-log.sql, 03-branches.sql, 04-commits.sql) no longer exist: the fourteen
--          numbered schema files were collapsed into that one, which IS edited in place now.
-- WAS: the three sync-only tables (sync_pushed, relay_branches, sync_bases) with their indexes,
--      and the Releases step that added relay_branches' freshness columns to existing stores.
-- ANCHOR: in 00-initial-schema.sql, sync_pushed went after op_owners, relay_branches after
--         branch_name_bases, and sync_bases after the commits tables. Those three neighbours are
--         still in that file, so the positions are findable; the Releases step has no anchor,
--         because there are no steps (see the CORRECTION).
-- CORRECTION: an earlier version of this header said two things that are not true.
--       First, it named a step `20261012_000001_drop_sync_tables` as "the matching DROP for stores
--       that already exist". There is no such step. `LibDB/Releases.fs` has NO steps at all, by
--       deliberate reset, because a store older than the schema collapse cannot be opened by this
--       build. So nothing dropped these tables from anybody's store: they are absent from a fresh
--       store because they are absent from the schema. Declaring them again is the whole job.
--       Second, it said `op_owners` STAYED. It did not. `op_owners` appears in no live file, so the
--       `CREATE TABLE op_owners` in here is a fourth table to restore, not context. The sibling
--       fragment later/backend/src/LibDB/Inserts.fs.store-with-owner has the F# that wrote it.
-- WHEN: the sync molt, 2026-10-12


CREATE TABLE IF NOT EXISTS sync_pushed (
  relay TEXT NOT NULL,
  op_id TEXT NOT NULL,
  PRIMARY KEY (relay, op_id)
);

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
    // What a relay's stored bundle CONTAINS, so a push can be compared with it rather than replacing
    // it blind. `relay_branches` is hosted data, not a projection, so nothing else brings these to a
    // store that already exists -- and a relay that has them missing does not degrade quietly, it
    // fails every branch push with "no column named max_ts".
    { name = "20260908_000001_relay_branch_freshness"
      run =
        fun () ->
          addColumnIfMissing "relay_branches" "max_ts" "TEXT NOT NULL DEFAULT ''"
          addColumnIfMissing "relay_branches" "op_count" "INTEGER NOT NULL DEFAULT 0" }



-- --- backend/migrations/schema/02-op-log.sql: op_owners ---
-- Added to this fragment after the first pass. op_owners was kept at first on the belief that it
-- held an op's author locally; it does not. Its only writer was the relay's store path, so once
-- that left the table was never written and the `author` column every reader joined to was
-- permanently NULL. An op's author is on its COMMIT (`commits.author`).

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
