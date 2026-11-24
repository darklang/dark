/// SQLite on a guest's database file, for `Operation.Sqlite`. `Host` has checked the path and
/// the grant before any of this runs; this opens that one file and nothing else.
module LibExecution.HostSqlite

open Microsoft.Data.Sqlite

open Prelude

type Value = HostTypes.SqliteValue
type Answer = HostTypes.SqliteAnswer
type Error = HostTypes.SqliteError

let private failure (e : exn) : Error =
  match e with
  | :? SqliteException as e ->
    { code = e.SqliteExtendedErrorCode; message = e.Message }
  | e -> { code = -1; message = e.Message }

/// `write` opens read-write and creates the file if needed; otherwise read-only, so SQLite
/// itself refuses every write.
let private openAt (write : bool) (path : string) : SqliteConnection =
  let builder = SqliteConnectionStringBuilder()
  builder.DataSource <- path
  builder.Mode <-
    if write then SqliteOpenMode.ReadWriteCreate else SqliteOpenMode.ReadOnly
  // seconds to wait on another writer before SQLITE_BUSY
  builder.DefaultTimeout <- 5
  let connection = new SqliteConnection(builder.ToString())
  connection.Open()
  // ATTACH, and VACUUM INTO (which attaches), would reach any other file
  SQLitePCL.raw.sqlite3_limit (
    connection.Handle,
    SQLitePCL.raw.SQLITE_LIMIT_ATTACHED,
    0
  )
  |> ignore<int>
  connection

let private bind (command : SqliteCommand) (parameters : List<Value>) : unit =
  parameters
  |> List.iteri (fun i value ->
    let value : obj =
      match value with
      | Value.Null -> System.DBNull.Value
      | Value.Integer n -> n
      | Value.Real f -> f
      | Value.Text s -> s
      | Value.Blob b -> b
    command.Parameters.AddWithValue($"?{i + 1}", value) |> ignore<SqliteParameter>)

let private column (reader : SqliteDataReader) (i : int) : string * Value =
  let value =
    match reader.GetValue i with
    | :? int64 as n -> Value.Integer n
    | :? double as f -> Value.Real f
    | :? string as s -> Value.Text s
    | :? (byte[]) as b -> Value.Blob b
    | _ -> Value.Null
  (reader.GetName i, value)

let private rows (reader : SqliteDataReader) : List<List<string * Value>> =
  let rec go acc =
    if reader.Read() then go (List.init reader.FieldCount (column reader) :: acc)
    elif reader.NextResult() then go acc
    else List.rev acc
  go []

/// Runs `sql` on the database at `path`: every statement's rows when not `write`; when
/// `write`, every statement in one transaction, and what they changed.
let run
  (write : bool)
  (path : string)
  (sql : string)
  (parameters : List<Value>)
  : Result<Answer, Error> =
  try
    use connection = openAt write path
    // the path was checked before the open; a link swapped in since is caught here
    if HostSecurity.isPackageDbPath connection.DataSource then
      Error
        { code = 23 // SQLITE_AUTH
          message = $"`{connection.DataSource}` is the host-owned package store" }
    else
      use command = connection.CreateCommand()
      command.CommandText <- sql
      bind command parameters
      if write then
        use transaction = connection.BeginTransaction()
        command.Transaction <- transaction
        let before = SQLitePCL.raw.sqlite3_total_changes connection.Handle
        command.ExecuteNonQuery() |> ignore<int>
        let changed = SQLitePCL.raw.sqlite3_total_changes connection.Handle - before
        let rowid = SQLitePCL.raw.sqlite3_last_insert_rowid connection.Handle
        transaction.Commit()
        Ok { rows = []; rowsAffected = int64 changed; lastInsertRowid = rowid }
      else
        use reader = command.ExecuteReader()
        Ok { rows = rows reader; rowsAffected = 0L; lastInsertRowid = 0L }
  with e ->
    Error(failure e)
