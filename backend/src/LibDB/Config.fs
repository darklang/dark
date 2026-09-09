/// Mutable, per-install local config (key/value): the CLI entry-point pointer + per-user settings.
///
/// Deliberately NOT content-addressed and NOT synced. This is local mutable state -- an entry point
/// every install sets for itself -- kept separate from the immutable op log by design (sync ships ops,
/// never this table).
module LibDB.Config

open System.Threading.Tasks
open FSharp.Control.Tasks

open Fumble
open Microsoft.Data.Sqlite
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


/// Secret-prefixed keys, in a file of their own beside the store.
///
/// Being a separate FILE is the mechanism: `Stdlib.Sqlite` declares only `package-read` for a
/// statement whose path is the store, so a plain SELECT on `config_v0` read the write secret past
/// `configGet`'s guard. A blocklist on the SQL text would be leakier -- a table name can be quoted,
/// aliased or reached through a view.
///
/// From the LIVE store path, so a test that repoints LibDB gets that instance's credentials.
/// Not covered by `dark backups`, which snapshots the store alone.
let credentialsPath () : string =
  let dir =
    match System.IO.Path.GetDirectoryName(currentDbPath : string) with
    | null
    | "" -> "."
    | d -> d
  System.IO.Path.Combine(dir, "credentials.db")

let private credentialsConnString () : string =
  $"Data Source={credentialsPath ()};Mode=ReadWriteCreate;Cache=Private;Pooling=true"

/// Open the credential store, creating file and table on first use. Not a migration: nothing else
/// opens this file, and a store with no secret has no reason to carry an empty one.
let private credentialsConn () : SqliteConnection =
  let conn = new SqliteConnection(credentialsConnString ())
  conn.Open()
  use cmd = conn.CreateCommand()
  cmd.CommandText <-
    "CREATE TABLE IF NOT EXISTS credentials_v0 (key TEXT PRIMARY KEY, value TEXT NOT NULL)"
  cmd.ExecuteNonQuery() |> ignore<int>
  conn

let private getCredential (key : string) : string option =
  use conn = credentialsConn ()
  use cmd = conn.CreateCommand()
  cmd.CommandText <- "SELECT value FROM credentials_v0 WHERE key = @key"
  cmd.Parameters.AddWithValue("@key", key) |> ignore<SqliteParameter>
  match cmd.ExecuteScalar() with
  | null -> None
  | v -> Some(string v)

let private setCredential (key : string) (value : string) : unit =
  use conn = credentialsConn ()
  use cmd = conn.CreateCommand()
  cmd.CommandText <-
    "INSERT INTO credentials_v0 (key, value) VALUES (@key, @value)
     ON CONFLICT(key) DO UPDATE SET value = @value"
  cmd.Parameters.AddWithValue("@key", key) |> ignore<SqliteParameter>
  cmd.Parameters.AddWithValue("@value", value) |> ignore<SqliteParameter>
  cmd.ExecuteNonQuery() |> ignore<int>


/// The value for `key`, or None if unset. Secret keys come from the credential store, everything
/// else from `config_v0`; callers do not choose.
let get (key : string) : Task<string option> =
  if isSecretKey key then
    Task.FromResult(getCredential key)
  else
    Sql.query "SELECT value FROM config_v0 WHERE key = @key"
    |> Sql.parameters [ "key", Sql.string key ]
    |> Sql.executeRowOptionAsync (fun read -> read.string "value")

/// Set `key` to `value` (upsert).
let set (key : string) (value : string) : Task<unit> =
  task {
    if isSecretKey key then
      setCredential key value
    else
      let! (_ : int) =
        Sql.query
          """
          INSERT INTO config_v0 (key, value) VALUES (@key, @value)
          ON CONFLICT(key) DO UPDATE SET value = @value
          """
        |> Sql.parameters [ "key", Sql.string key; "value", Sql.string value ]
        |> Sql.executeNonQueryAsync
      ()

    return ()
  }
