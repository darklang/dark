CREATE TABLE IF NOT EXISTS trace_loops (
               trace_id  TEXT NOT NULL,
               call_site TEXT NOT NULL,
               passes    INTEGER NOT NULL,
               PRIMARY KEY (trace_id, call_site)
             );
