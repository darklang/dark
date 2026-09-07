/// Mutable, per-install local config (key/value): the CLI entry-point pointer + per-user settings.
///
/// Deliberately NOT content-addressed and NOT synced. This is local mutable state -- an entry point
/// every install sets for itself -- kept separate from the immutable op log by design (sync ships ops,
/// never this table).
module LibDB.Config

open System.Threading.Tasks
open FSharp.Control.Tasks

open Fumble
open LibDB.Sqlite

open Prelude

/// Keys under this prefix hold CREDENTIALS and are not readable through the general getter.
///
/// The relay write secret lives in config, and `configGet` carries no capability, so without this any
/// Dark the CLI runs could read it -- a package pulled from a peer included, and with it that code can
/// push ops as you, a strictly larger hole than the unguarded transport it would use to do it.
///
/// So the secret never reaches Dark at all: F# attaches it to sync requests and F# scans outgoing ops
/// for it. Dark still decides WHEN to push, which is the part that belongs to it.
let secretPrefix = "sync.secret."

let isSecretKey (key : string) : bool = key.StartsWith secretPrefix


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
