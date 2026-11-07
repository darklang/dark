-- The package reload's trait-call cache (`LibDB.TraitCallCache`): what the at-rest checker
-- resolved in each body, so the next reload checks only the bodies whose inputs changed.
--
-- `environment_hash` covers everything a body's answer depends on that its own hash does not:
-- the build, the checker, and the tree's types, traits, implementations, names for them and
-- deprecations (`TraitCalls.environmentOf`). `resolutions` is by node position, and the store
-- does not read it.
--
-- A cache, not part of the log: survives a purge (see `hashCoupledSurvivors`), and a seed
-- export empties it. A new table, so an existing store gets it from this file at startup.
CREATE TABLE IF NOT EXISTS trait_call_resolutions (
  environment_hash TEXT NOT NULL,
  item_hash TEXT NOT NULL,
  nodes INTEGER NOT NULL,
  resolutions TEXT NOT NULL,
  PRIMARY KEY (environment_hash, item_hash)
);
