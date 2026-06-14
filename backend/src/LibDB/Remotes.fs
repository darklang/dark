/// Remotes — the registered sync peers.
///
/// Local-only, NOT synced (a remote list is per-instance setup, like git's). Each entry is a
/// (name, url) the tailnet-wide daemon polls — so you can `dark remote add` a peer and have the
/// daemon sync it WITHOUT a manual pull first. Distinct from `sync_cursors` (poll resume state):
/// cursors are created implicitly by pulling; remotes are explicitly registered. The daemon's
/// poll set is the UNION of the two, so neither path regresses.
///
/// The `sync_remotes` table lives in `backend/migrations/schema.sql` (created at startup).
module LibDB.Remotes

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

/// Register (or update) a remote by name. Idempotent upsert — re-adding a name updates its url.
let add (name : string) (url : string) : Task<unit> =
  Sql.query
    """
    INSERT INTO sync_remotes (name, url) VALUES (@name, @url)
    ON CONFLICT(name) DO UPDATE SET url = excluded.url
    """
  |> Sql.parameters [ "name", Sql.string name; "url", Sql.string url ]
  |> Sql.executeStatementAsync

/// Remove a registered remote by name. Returns true if it existed (and was deleted).
let remove (name : string) : Task<bool> =
  task {
    let! before =
      Sql.query "SELECT COUNT(*) AS n FROM sync_remotes WHERE name = @name"
      |> Sql.parameters [ "name", Sql.string name ]
      |> Sql.executeAsync (fun read -> read.int64 "n")
    let existed =
      (match before with
       | n :: _ -> n > 0L
       | [] -> false)
    do!
      Sql.query "DELETE FROM sync_remotes WHERE name = @name"
      |> Sql.parameters [ "name", Sql.string name ]
      |> Sql.executeStatementAsync
    return existed
  }

/// All registered remotes as (name, url), ordered by name for a stable display.
let list () : Task<List<string * string>> =
  Sql.query "SELECT name, url FROM sync_remotes ORDER BY name"
  |> Sql.executeAsync (fun read -> (read.string "name", read.string "url"))

/// Just the pollable urls of registered remotes — what the daemon adds to its poll set.
let urls () : Task<List<string>> =
  Sql.query "SELECT url FROM sync_remotes ORDER BY url"
  |> Sql.executeAsync (fun read -> read.string "url")
