-- Per-AST-node values, recorded via the interpreter's `traceDval` hook.
-- Powers `view <fn> --with-trace`: walk a fn's AST, look up each
-- expression's recorded value here, render inline.
--
-- expr_id is globally unique per AST node (IDs are minted in PT2RT), so
-- a flat (trace_id, expr_id) PK works without involving tlid. If the
-- same expr fires multiple times (recursion, loops), the latest value
-- wins via INSERT OR REPLACE — same shape as the in-memory
-- `Execution.traceDvals` Dictionary.

CREATE TABLE trace_expr_values
( trace_id  TEXT NOT NULL
, expr_id   TEXT NOT NULL
, dval_json TEXT NOT NULL
, PRIMARY KEY (trace_id, expr_id)
);
CREATE INDEX idx_trace_expr_values_trace_id ON trace_expr_values(trace_id);
