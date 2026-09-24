-- Which functions a run went through: names only.
--
-- A new table, so a new file (`AGENTS.md`, "Changing the schema"). It exists because the
-- shipped recording level keeps only the impure calls, so nothing in `trace_fn_calls` says that
-- a run passed through `MyApp.Orders.route`. This is what `dark traces calls <fn>` reads: one
-- row per (run, function), no arguments, no results, a few hundred bytes for an ordinary run.

CREATE TABLE IF NOT EXISTS trace_fns (
  trace_id TEXT NOT NULL,
  fn_name TEXT NOT NULL,
  PRIMARY KEY (trace_id, fn_name)
);
CREATE INDEX IF NOT EXISTS idx_trace_fns_fn_name ON trace_fns(fn_name);
