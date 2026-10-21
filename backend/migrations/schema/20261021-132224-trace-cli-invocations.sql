-- Saves the source, arguments and options needed to replay a CLI run.
-- Traces.create writes this row with the trace; delete them together.
-- HTTP requests have traces, but no row in this CLI-only table.
-- Older CLI traces also have no row here.
--
-- Columns avoid storing Dval type hashes, which change when package types change.
-- CHECKs reject missing options so replay cannot silently drop a sandbox.
CREATE TABLE IF NOT EXISTS trace_cli_invocations (
  trace_id TEXT PRIMARY KEY,
  kind TEXT NOT NULL CHECK (kind IN ('eval', 'script')),
  source TEXT NOT NULL,
  filename TEXT,               -- scripts only
  args BLOB,                   -- scripts only, see ScriptOrEvalInput.encodeArgs
  allow_harmful INTEGER NOT NULL CHECK (allow_harmful IN (0, 1)),
  sandbox INTEGER CHECK (sandbox IN (0, 1)),                  -- scripts only
  warn_permissions INTEGER CHECK (warn_permissions IN (0, 1)), -- scripts only
  CHECK (
    (kind = 'eval'
     AND filename IS NULL AND args IS NULL
     AND sandbox IS NULL AND warn_permissions IS NULL)
    OR (kind = 'script'
        AND filename IS NOT NULL AND args IS NOT NULL
        AND sandbox IS NOT NULL AND warn_permissions IS NOT NULL))
);
