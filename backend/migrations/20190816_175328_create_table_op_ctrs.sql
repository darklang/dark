CREATE TABLE IF NOT EXISTS
op_ctrs
(
 browser_id UUID NOT NULL UNIQUE
, ctr INTEGER NOT NULL DEFAULT 0
, timestamp TIMESTAMP NOT NULL DEFAULT NOW()
, locked BOOLEAN NOT NULL);

CREATE INDEX IF NOT EXISTS idx_op_ctrs_browser_id_ctr ON op_ctrs (browser_id, ctr DESC);
CREATE INDEX IF NOT EXISTS idx_op_ctrs_timestamp ON op_ctrs (timestamp ASC)
