-- Traces: observations of past runs. Deliberately outlive the code they observed.

--------------------
-- Traces
--------------------

-- One row per handler invocation. Handler input (the parsed dval bound to the handler's parameter:
-- `request` for HTTP, `expression` for eval) lives directly here, as a binary-serialized RT.Dval.
CREATE TABLE IF NOT EXISTS traces (
  id TEXT PRIMARY KEY,
  root_tlid INTEGER NOT NULL,
  handler_desc TEXT NOT NULL,
  timestamp TEXT NOT NULL,                     -- when the run STARTED
  input_name TEXT NOT NULL,
  input_value BLOB NOT NULL,
  account_id TEXT REFERENCES accounts_v0(id),  -- NULL for unattributed (anonymous) runs
  -- A trace IS a run (`LibDB/Traces.fs`, `docs/processes.md`). These five carry the half that
  -- used to live in a second table, `executions`, which is why they are additions to a file that
  -- had already merged: a new COLUMN has nowhere else to go, since a schema file only ever runs
  -- CREATE statements and a fresh store is born from this declaration. Existing stores get them
  -- from steps in `LibDB/Releases.fs`. A new TABLE would go in a new file (`AGENTS.md`).
  status TEXT NOT NULL DEFAULT 'done',         -- running | done | failed | suspended
  parent_id TEXT,                              -- the run this was forked from, if any
  parent_seq INTEGER,                          -- ... and the `seq` it branched at
  pinned INTEGER NOT NULL DEFAULT 0,           -- retention never drops a pinned run
  updated TEXT NOT NULL DEFAULT '',
  entry_hash TEXT                              -- for a served request: the handler that served
                                               -- it, so the run can be previewed against it
);
-- The index on `status` is created by a step in `LibDB/Releases.fs`, not here: on an existing
-- store the schema file runs BEFORE the steps, so an index naming a column the steps are about
-- to add would fail the bootstrap.


-- Every fn call AND every lambda invocation gets one row, linked via parent_call_id (NULL for
-- source-level entries). `kind` discriminates function / lambda / builtin so the renderer can tag
-- without inspecting fn_hash. `args` is a binary-serialized RT.Dval, a `DList` of the call's
-- arguments; `result` is the return Dval.
--
-- function and lambda frames get real `duration_ms`; builtins stay at 0, since the recorder only
-- sees their synchronous storeFnResult and there is no matching entry hook.
CREATE TABLE IF NOT EXISTS trace_fn_calls (
  trace_id TEXT NOT NULL,
  call_id TEXT NOT NULL,
  parent_call_id TEXT,                       -- NULL for source-level
  kind TEXT NOT NULL,                        -- 'function' | 'lambda' | 'builtin'
  fn_hash TEXT,                              -- callee for function/builtin
  lambda_expr_id TEXT,                       -- AST id of the lambda body
  args BLOB NOT NULL,
  result BLOB NOT NULL,
  duration_ms INTEGER NOT NULL DEFAULT 0,
  -- A new COLUMN on a table that has already merged has nowhere else to go: a schema file only ever
  -- runs CREATE statements, so a patch file cannot ALTER, and a fresh store is born from this
  -- declaration. Existing stores get the same columns from a step in `LibDB/Releases.fs`. A new
  -- TABLE is different and goes in a new file (`10-executions.sql`).
  process_id TEXT NOT NULL DEFAULT '',         -- the process that made the call; '' when unscheduled
  seq INTEGER NOT NULL DEFAULT 0,              -- completion order across the whole trace
  ord INTEGER NOT NULL DEFAULT -1,             -- an effectful builtin call's ordinal in its process, taken
                                               -- at the call; -1 otherwise. A replay keys on (process_id, ord)
  PRIMARY KEY (trace_id, call_id)
);
CREATE INDEX IF NOT EXISTS idx_trace_fn_calls_trace_id ON trace_fn_calls(trace_id);
CREATE INDEX IF NOT EXISTS idx_trace_fn_calls_fn_hash  ON trace_fn_calls(fn_hash);
