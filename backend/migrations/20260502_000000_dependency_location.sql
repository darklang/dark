-- Rebuild `package_dependencies` to record the user-typed FQN on each
-- dep edge alongside the content hash. The original
-- `(item_hash, depends_on_hash)` PK silently dropped the second edge
-- when one item referenced two distinct FQNs that shared a hash
-- (e.g. multiple `val ... = 200L`) — INSERT OR IGNORE rejected the
-- duplicate even though the location differed.
--
-- The UNIQUE INDEX wraps the location columns in `COALESCE(col, '')`
-- because SQLite treats raw NULLs as distinct in a UNIQUE index;
-- without coalescing, duplicate NULL-location rows would accumulate
-- on every `updateDependencies` reinsert.
--
-- `package_dependencies` is fully derived from package items, so we
-- drop and recreate; no data preservation needed.

DROP TABLE IF EXISTS package_dependencies;

CREATE TABLE package_dependencies (
  item_hash TEXT NOT NULL,
  depends_on_hash TEXT NOT NULL,
  depends_on_item_type TEXT NOT NULL,
  depends_on_owner TEXT,
  depends_on_modules TEXT,
  depends_on_name TEXT
);

CREATE INDEX IF NOT EXISTS idx_package_dependencies_depends_on
  ON package_dependencies(depends_on_hash);
CREATE INDEX IF NOT EXISTS idx_package_dependencies_item
  ON package_dependencies(item_hash);

-- Partial index for the propagation query: "who depends on this
-- location?" Excludes the NULL backlog so it stays small.
CREATE INDEX IF NOT EXISTS idx_package_dependencies_depends_on_location
  ON package_dependencies(depends_on_item_type, depends_on_owner, depends_on_modules, depends_on_name)
  WHERE depends_on_owner IS NOT NULL;

CREATE UNIQUE INDEX IF NOT EXISTS idx_package_dependencies_unique
  ON package_dependencies(
    item_hash,
    depends_on_hash,
    depends_on_item_type,
    COALESCE(depends_on_owner, ''),
    COALESCE(depends_on_modules, ''),
    COALESCE(depends_on_name, '')
  );
