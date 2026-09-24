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
  timestamp TEXT NOT NULL,
  input_name TEXT NOT NULL,
  input_value BLOB NOT NULL,
  account_id TEXT REFERENCES accounts_v0(id)  -- NULL for unattributed (anonymous) runs
);


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
  process_id TEXT NOT NULL DEFAULT '',         -- the process that made the call; '' when unscheduled
  seq INTEGER NOT NULL DEFAULT 0,              -- completion order across the whole trace
  ord INTEGER NOT NULL DEFAULT -1,             -- an effectful builtin call's ordinal in its process, taken
                                               -- at the call; -1 otherwise. A replay keys on (process_id, ord)
  PRIMARY KEY (trace_id, call_id)
);
CREATE INDEX IF NOT EXISTS idx_trace_fn_calls_trace_id ON trace_fn_calls(trace_id);
CREATE INDEX IF NOT EXISTS idx_trace_fn_calls_fn_hash  ON trace_fn_calls(fn_hash);


--------------------
-- Executions
--------------------

-- A run as a durable thing: what was run (the same input the trace stores, so it can be run again),
-- the trace that is its log of effectful calls, where it stands, and, for a fork, which execution
-- and which ordinal it branched from. `dark exec list/show/resume/fork`; `docs/processes.md`.
-- Status: running | done | failed | suspended.
CREATE TABLE IF NOT EXISTS executions (
  id TEXT PRIMARY KEY,
  handler_desc TEXT NOT NULL,                  -- 'eval' or 'run <file>', as on the trace row
  input_name TEXT NOT NULL,
  input_value BLOB NOT NULL,                   -- binary-serialized RT.Dval; the expression or script source
  trace_id TEXT NOT NULL,
  status TEXT NOT NULL,
  parent_id TEXT,                              -- the execution this one was forked from, if any
  parent_ord INTEGER,                          -- ... and the ordinal it branched at: the log before it is shared
  created TEXT NOT NULL,
  updated TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_executions_status ON executions(status);
