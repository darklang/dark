/// Which ops in `package_ops` the fold has applied: every row at or below one mark in the log, less
/// the exceptions in `package_ops_unapplied` (see the schema file that declares both). The SQL every
/// reader and writer shares, so they all ask the same question.
module LibDB.AppliedOps

/// Whether a `package_ops` row is applied, as a condition on it.
let isApplied =
  "(package_ops.rowid <= (SELECT seq FROM package_ops_applied_through)
    AND package_ops.rowid NOT IN (SELECT seq FROM package_ops_unapplied))"

/// The ops still to apply, as <param columns> of each (`p` is the `package_ops` row). Two lookups
/// rather than `NOT isApplied`, which would read the whole log. The CROSS JOIN fixes the join order:
/// left to choose, SQLite scans the log to find the few exceptions.
let pending (columns : string) =
  $"SELECT {columns} FROM package_ops p
    WHERE p.rowid > (SELECT seq FROM package_ops_applied_through)
    UNION ALL
    SELECT {columns} FROM package_ops_unapplied u CROSS JOIN package_ops p ON p.rowid = u.seq"

/// Before an author stores ops it folds itself, in the same transaction: the ops still waiting above
/// the mark become exceptions, so moving the mark past them with `coverAll` keeps them to apply.
let holdPending =
  "INSERT OR IGNORE INTO package_ops_unapplied (seq)
   SELECT rowid FROM package_ops WHERE rowid > (SELECT seq FROM package_ops_applied_through)"

/// After a fold of every op still to apply up to `$last`: the ones it could not read, `$skipped` (a
/// JSON list), are the only exceptions left at or below it.
let foldedThrough =
  "DELETE FROM package_ops_unapplied WHERE seq <= $last;
   UPDATE package_ops_applied_through SET seq = MAX(seq, $last);
   INSERT OR IGNORE INTO package_ops_unapplied (seq) SELECT value FROM json_each($skipped);"

/// Every op in the log now counts as applied, apart from the exceptions.
let coverAll =
  "UPDATE package_ops_applied_through
   SET seq = MAX(seq, (SELECT COALESCE(MAX(rowid), 0) FROM package_ops))"

/// Nothing is applied: the next fold takes the whole log.
let reset =
  "DELETE FROM package_ops_unapplied; UPDATE package_ops_applied_through SET seq = 0;"
