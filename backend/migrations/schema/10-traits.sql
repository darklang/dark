-- The two item kinds traits added: a trait, and an implementation of one for a type.
--
-- Its own file rather than an edit to `06-packages.sql`, because that file is already merged: a
-- schema file stops being editable the moment it lands on main, and a later change arrives as a
-- new file. Filename order is the contract (`LocalExec.Migrations.schemaSql`), and these tables
-- reference nothing, so sorting last is fine.

-- Traits and impls: their own item kinds, folded from AddTrait / AddTraitImpl. `trait_hash` on an
-- impl is the dispatch index: every impl of a trait is one indexed read.
CREATE TABLE IF NOT EXISTS package_traits (
  hash TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  description TEXT NOT NULL DEFAULT ''
);

CREATE TABLE IF NOT EXISTS package_trait_impls (
  hash TEXT PRIMARY KEY,
  trait_hash TEXT NOT NULL,
  pt_def BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  description TEXT NOT NULL DEFAULT '',
  -- The origin_ts of the `AddTraitImpl` op that introduced this impl, so two impls of one trait
  -- for one type can be ordered the same way on every instance: the newer one is the one that
  -- runs (timestamp-LWW, `LibExecution.Lww`), and the older is reported as a rival rather than
  -- erroring at the call. Empty when the impl never came from an op (a script's own impls,
  -- grafted in memory): then there is no winner and the call still errors.
  origin_ts TEXT NOT NULL DEFAULT ''
);
CREATE INDEX IF NOT EXISTS idx_package_trait_impls_trait ON package_trait_impls(trait_hash);
