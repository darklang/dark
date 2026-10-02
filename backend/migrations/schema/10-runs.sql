-- A trace IS a run, and this file is everything the run half of that needs that `08-traces.sql`
-- cannot hold. That file has merged, so it is frozen (`AGENTS.md`, "Changing the schema"): the
-- COLUMNS this PR adds to `traces` and `trace_fn_calls` therefore come from steps in
-- `LibDB/Releases.fs`, which is the only thing that can widen a table that already exists. What
-- CAN live here is a new table and any index, so both are here.

--------------------
-- Two things `08-traces.sql` says that are no longer true
--------------------

-- It is frozen, so this is where they get corrected rather than there.
--
-- `traces.root_tlid` is `NOT NULL` and every writer puts 0 in it. In classic it named the
-- handler whose trace this was; here a run is not always a handler invocation, and the question
-- it answered ("which handler served this?") is answered by `entry_hash`, which this PR adds.
-- Nothing reads it, there is no index on it, and a frozen `CREATE TABLE` cannot lose a column,
-- so it stays as a 4-byte-per-row fossil. Do not add a reader.
--
-- `trace_fn_calls`'s comment says every fn call and every lambda gets a row, that `kind`
-- discriminates function / lambda / builtin, and that builtins stay at `duration_ms = 0`. All
-- three are now wrong: only the impure calls are recorded, so `kind` is always 'builtin',
-- `parent_call_id` and `lambda_expr_id` are always NULL, and a builtin carries a real duration
-- (measured at the landing, for a read in flight). The columns stay for the same reason.


--------------------
-- Which functions a run went through: names only
--------------------

-- Recording keeps only the impure calls, so nothing in `trace_fn_calls` says
-- that a run passed through `MyApp.Orders.route`. This is what `dark traces calls <fn>` reads:
-- one row per (run, function), no arguments, no results, a few hundred bytes for a normal run.
--
-- `fn_hash` is the version the run actually went through. A resume replays the recorded log
-- against whatever those names mean NOW, so comparing the two is what lets `traces resume` say
-- which of the run's callees have been edited since.
CREATE TABLE IF NOT EXISTS trace_fns (
  trace_id TEXT NOT NULL,
  fn_name TEXT NOT NULL,
  fn_hash TEXT NOT NULL DEFAULT '',
  PRIMARY KEY (trace_id, fn_name)
);
CREATE INDEX IF NOT EXISTS idx_trace_fns_fn_name ON trace_fns(fn_name);


--------------------
-- Indexes for the run verbs
--------------------

-- Indexes are applied in a pass of their own, after the release steps, so one may name a column
-- a step has just added (`Releases.applySchemaIndexes`). Every listing and retention's scan order
-- by `timestamp DESC`; `status` is how the CLI finds the runs that can be resumed.
CREATE INDEX IF NOT EXISTS idx_traces_timestamp ON traces(timestamp);
CREATE INDEX IF NOT EXISTS idx_traces_status    ON traces(status);

-- Every read of one run's log selects by `trace_id` and orders by `seq`, so the composite
-- serves both halves and the single-column `idx_trace_fn_calls_trace_id` in `08-traces.sql`
-- becomes redundant. That file is frozen, so the old index stays on disk; SQLite will pick
-- this one.
CREATE INDEX IF NOT EXISTS idx_trace_fn_calls_trace_seq
  ON trace_fn_calls(trace_id, seq);
