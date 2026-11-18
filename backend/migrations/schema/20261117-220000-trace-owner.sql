-- The OS process running a trace: its pid and when that process started (`LibDB.Traces.Owner`).
-- A `running` trace whose process is gone is shown, and kept, as `lost`.
ALTER TABLE traces ADD COLUMN owner_pid INTEGER;
ALTER TABLE traces ADD COLUMN owner_start TEXT;
