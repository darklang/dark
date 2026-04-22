-- Rename locations.deprecated_at to unlisted_at.
--
-- The previous name was misleading: the column tracks pointer lifecycle
-- (renames, propagation, WIP->committed swaps) rather than author-initiated
-- deprecation. Freeing the word "deprecated" for the new deprecation-as-op
-- annotation layer (see thinking/deprecation-redesign.md).

ALTER TABLE locations RENAME COLUMN deprecated_at TO unlisted_at;

DROP INDEX IF EXISTS idx_locations_branch_lookup;
DROP INDEX IF EXISTS idx_locations_module;

CREATE INDEX IF NOT EXISTS idx_locations_branch_lookup
  ON locations(branch_id, owner, modules, name, item_type)
  WHERE unlisted_at IS NULL;

CREATE INDEX IF NOT EXISTS idx_locations_module
  ON locations(owner, modules) WHERE unlisted_at IS NULL;
