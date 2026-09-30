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
  -- Which frame made this call, pointing into trace_frames. What lets three recorded writes be
  -- read as three passes of one loop rather than three writes.
  frame_id TEXT,
  PRIMARY KEY (trace_id, call_id)
);
CREATE INDEX IF NOT EXISTS idx_trace_fn_calls_trace_id ON trace_fn_calls(trace_id);
CREATE INDEX IF NOT EXISTS idx_trace_fn_calls_fn_hash  ON trace_fn_calls(fn_hash);


-- The SHAPE of a run: which frames existed, what each ran, which frame made it, and which pass
-- it was among its siblings at the same call site.
--
-- Values are not here. A view recomputes what every line evaluated to by replaying the run with
-- its effects answered from the log, which is why a trace costs bytes rather than megabytes.
-- What a replay CANNOT recover is the shape it never reaches: a run suspended mid-loop, or one
-- whose replay stops at an effect the log has no answer for, has passes that happened and that
-- no amount of re-running will show. That is what these rows are for.
--
-- One row per frame that is an ANCESTOR of a recorded call, not per frame pushed. A pure helper
-- called in a tight loop pushes frames nobody will ever ask about, and recording every one of
-- them would make the shape cost more than the log it describes.
CREATE TABLE IF NOT EXISTS trace_frames (
  trace_id        TEXT NOT NULL,
  frame_id        TEXT NOT NULL,
  parent_frame_id TEXT,                       -- NULL at the entry
  kind            TEXT NOT NULL,              -- 'source' | 'function' | 'lambda'
  -- The lambda's own expression id, for a lambda frame. Sibling frames sharing a parent and
  -- this are the passes of one loop.
  call_site       TEXT,
  -- The callee's hash, for a function frame.
  fn_hash         TEXT,
  -- Which pass this is among its siblings at the same call site, from 0.
  pass            INTEGER NOT NULL DEFAULT 0,
  -- The order this frame was pushed across the whole run. `pass` counts within one call site
  -- and cannot order frames at different sites.
  ord             INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY (trace_id, frame_id)
);
CREATE INDEX IF NOT EXISTS idx_trace_frames_parent
  ON trace_frames(trace_id, parent_frame_id);
