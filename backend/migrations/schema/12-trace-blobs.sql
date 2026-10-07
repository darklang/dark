-- A trace's values by content hash, owned by the trace, written with it and deleted with it. Two
-- kinds, one table:
--
-- - every logged call's result, once per distinct value. `trace_fn_calls.result` holds the
--   value's hash as TEXT, and readers join here for the bytes (`08-traces.sql` is frozen, so the
--   column keeps its name). A traced type check reads the same items and gets the same small
--   answers thousands of times; one stdlib-wide trace had 8,217 calls and 2,700 distinct results.
-- - the blobs a trace captured: a request body, a file's bytes, anything an effect handed over as
--   a `Blob`.
--
-- They used to be promoted into `package_blobs`, which is shared with package values and user
-- data and which nothing collects, so retention removed the trace and kept the body forever:
-- five traced 400 KB POSTs left 2.0 MB there, and none of it was counted by `trace.maxMb`.
-- Content-addressed like `package_blobs`, so a read that misses there falls back to here
-- (`LibDB.RuntimeTypes.Blob.get`).
CREATE TABLE IF NOT EXISTS trace_blobs (
  trace_id TEXT NOT NULL,
  hash     TEXT NOT NULL,
  bytes    BLOB NOT NULL,
  PRIMARY KEY (trace_id, hash)
);
CREATE INDEX IF NOT EXISTS idx_trace_blobs_hash ON trace_blobs(hash);
