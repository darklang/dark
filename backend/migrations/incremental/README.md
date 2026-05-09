# Incremental migrations

This directory holds per-file additive migrations, run in lexical
order on top of `../schema.sql` and name-dedup'd via
`system_migrations_v0`. Empty by default.

When to add a file here vs editing `schema.sql`:

- **Edit `schema.sql`**: structural redesigns, adding a new table or
  column where rebuilding from source is fine. The file is hashed +
  kill-and-fill'd; data in the affected tables is lost.
- **Add a file here**: data backfills, transforms, additive
  alterations on populated dev/test DBs you don't want to nuke.

File naming: `YYYYMMDD_HHMMSS_<short-tag>.sql` so lexical sort gives
chronological order. First line may be the literal `--#[no_tx]` to
skip the wrapping transaction (rare; for DDL SQLite refuses inside
one).
