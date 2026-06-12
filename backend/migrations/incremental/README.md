# Incremental migrations (legacy escape hatch — empty by default)

**The schema lives in one place: `../schema.sql`.** It is the single, complete source of truth — every
table, column, and index is defined there in its final form. `schema.sql` is content-hashed and
re-applied on change (durable-canon: regenerable projections are dropped + re-folded from the op log;
the canonical op log / blobs / branch state come through intact). Edit `schema.sql` directly.

**A versioned change that must coordinate across instances goes through the Release migrator**
(`backend/src/LibDB/Releases.fs`): one `Release` step can carry forward SQL (copy-and-swap), an optional
op-format re-serialize, and a projection re-fold — gated by the single Release coordinate that also gates
cross-instance sync. That's the mechanism for future schema/format evolution.

This `incremental/` directory is a rarely-needed escape hatch for an additive backfill on a populated dev
DB you don't want to rebuild. It is **empty by default** and you almost never want a file here — prefer
`schema.sql` (+ a Release step when the change must travel). Files run in lexical order, name-dedup'd via
`system_migrations_v0`; naming is `YYYYMMDD_HHMMSS_<short-tag>.sql`, and a first line of the literal
`--#[no_tx]` skips the wrapping transaction (rare; for DDL SQLite refuses inside one).
