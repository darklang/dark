/// Mutable, per-install local config (key/value): the CLI entry-point pointer + per-user settings.
///
/// Deliberately NOT content-addressed and NOT synced. This is local mutable state (Globals) — the entry
/// point Feriel and Stachu each set differently on their own machines — kept separate from the immutable
/// op log by design (sync ships ops, never this table). See notes/fresh-arch/50-mvp-clean.md.
module LibDB.Config

open System.Threading.Tasks
open FSharp.Control.Tasks

open Fumble
open LibDB.Sqlite

open Prelude

/// The value for `key`, or None if unset.
let get (key : string) : Task<string option> =
  Sql.query "SELECT value FROM config_v0 WHERE key = @key"
  |> Sql.parameters [ "key", Sql.string key ]
  |> Sql.executeRowOptionAsync (fun read -> read.string "value")

/// Set `key` to `value` (upsert).
let set (key : string) (value : string) : Task<unit> =
  task {
    let! (_ : int) =
      Sql.query
        """
        INSERT INTO config_v0 (key, value) VALUES (@key, @value)
        ON CONFLICT(key) DO UPDATE SET value = @value
        """
      |> Sql.parameters [ "key", Sql.string key; "value", Sql.string value ]
      |> Sql.executeNonQueryAsync
    return ()
  }
