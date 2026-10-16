-- What a CLI run was invoked with, so a replay uses the same source, arguments and options.
--
-- One row per CLI run, written with its `traces` row by `Traces.create`. A trace with no row
-- here has nothing to replay with: a served request, or a run recorded before this table.
-- Plain columns rather than a Dval, whose stored type hash would go stale when the package
-- type is edited. The CHECKs refuse an incomplete row on write, so a reader never has to
-- guess a missing option (a sandboxed script replayed without its sandbox).
--
-- An existing store gets this table from `Releases.applySchemaTables`, which replays every
-- CREATE TABLE on startup, so it needs no step in backend/src/LibDB/Releases.fs.
--
-- Every path that deletes a trace deletes its row here too, like the other per-trace tables.
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
