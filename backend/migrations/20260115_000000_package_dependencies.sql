-- Dependency tracking: Track what functions/types/values depend on what other functions/types/values
-- This enables "what depends on this?" and "what does this depend on?" queries.

CREATE TABLE IF NOT EXISTS package_dependencies (
  item_id TEXT NOT NULL,          -- the fn/value/type that has the dependency
  depends_on_id TEXT NOT NULL,    -- the fn/type/value being depended on
  PRIMARY KEY (item_id, depends_on_id)
);

-- Index for efficient "what depends on this?" queries (getDependents)
CREATE INDEX IF NOT EXISTS idx_package_dependencies_depends_on ON package_dependencies(depends_on_id);

-- Index for efficient "what does this depend on?" queries (getDependencies)
CREATE INDEX IF NOT EXISTS idx_package_dependencies_item ON package_dependencies(item_id);
