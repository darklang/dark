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
  PRIMARY KEY (trace_id, call_id)
);
CREATE INDEX IF NOT EXISTS idx_trace_fn_calls_trace_id ON trace_fn_calls(trace_id);
CREATE INDEX IF NOT EXISTS idx_trace_fn_calls_fn_hash  ON trace_fn_calls(fn_hash);


-- How many times each loop in a run went round.
--
-- One row per loop, keyed by the lambda's own expression id. Not a tree, and not a row per
-- pass: this answers exactly one question, and it exists for exactly one reason.
--
-- A view recomputes what every line evaluated to by REPLAYING the run with its effects answered
-- from the log, so it can only show the passes it reaches. A run whose log was capped, or which
-- was suspended mid-loop, has passes that happened and that no re-running will show. Without
-- this the view reports "pass 3 of 3" about a loop that went round five times, which is a
-- confident lie in a debugging tool.
--
-- Every pass is counted, including passes that made no impure call. An earlier version stored a
-- row per FRAME and kept only the frames an effectful call sat under, which wrote thousands of
-- rows for one loop AND silently counted zero for a pure one.
--
-- Values are not here, and neither is the tree. Both come from the replay. If something ever
-- needs to read a trace's shape WITHOUT replaying it, this table is where that would go; traces
-- are transient, so widening it later costs nothing.
CREATE TABLE IF NOT EXISTS trace_loops (
  trace_id  TEXT NOT NULL,
  -- The lambda's own expression id. Sibling passes share it, which is what makes them one loop.
  call_site TEXT NOT NULL,
  passes    INTEGER NOT NULL,
  PRIMARY KEY (trace_id, call_site)
);
