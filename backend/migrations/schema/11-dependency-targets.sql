-- Edges by the name and version they point at, for "which usages are a version behind"
-- (`SCM.Constraints.detectOutdatedUsages`), which `dark status` runs on every invocation.
--
-- Without it that query visits every edge in the store and looks each one's name up in
-- `locations`: about 32,000 lookups on a fresh store, to return nothing. With it the question
-- is asked per distinct (name, version) instead, from the index alone.
CREATE INDEX IF NOT EXISTS idx_package_dependencies_target
  ON package_dependencies(depends_on_owner, depends_on_modules, depends_on_name, depends_on_hash, item_hash)
  WHERE depends_on_owner IS NOT NULL;
