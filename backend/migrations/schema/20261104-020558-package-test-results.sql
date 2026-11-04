-- The `dark test run` result cache: what a test came to, so a pass can be reused.
--
-- Keyed on everything a pass depends on that the test hash does not cover: the runtime and
-- judging code (`runtime_hash`), the branch, and that branch's binding of
-- `Stdlib.Test.Result` (`result_type_hash`). `outcome_json` is the outcome as Dark serialised
-- it; the store does not read it.
--
-- Stores that ran `dark test` before this file existed already have this table, created by the
-- test runner with this exact shape, so the `IF NOT EXISTS` leaves it and its rows alone.
-- A cache, not part of the log: survives a purge (see `hashCoupledSurvivors`).
CREATE TABLE IF NOT EXISTS package_test_results (
  test_hash TEXT NOT NULL,
  runtime_hash TEXT NOT NULL,
  branch_id TEXT NOT NULL,
  result_type_hash TEXT NOT NULL,
  outcome_json TEXT NOT NULL,
  PRIMARY KEY (test_hash, runtime_hash, branch_id, result_type_hash)
);
