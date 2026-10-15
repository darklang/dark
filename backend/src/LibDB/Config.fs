/// Mutable, per-install local config (key/value): the current branch, the scheduler knobs, the
/// tracing knobs, the HTTP settings.
///
/// Deliberately NOT content-addressed. This is local mutable state -- what one install chooses
/// for itself -- kept separate from the immutable op log by design.
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

/// The values of several keys in one query, keyed by name; unset keys are absent.
/// For a startup that reads a handful of settings: one round trip, not one per key.
let getMany (keys : List<string>) : Task<Map<string, string>> =
  task {
    let names = keys |> List.mapi (fun i _ -> $"@k{i}") |> String.concat ", "
    let! rows =
      Sql.query $"SELECT key, value FROM config_v0 WHERE key IN ({names})"
      |> Sql.parameters (keys |> List.mapi (fun i k -> $"k{i}", Sql.string k))
      |> Sql.executeAsync (fun read -> read.string "key", read.string "value")
    return Map.ofList rows
  }

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
