/// A batch of raw SQLite statements sharing one connection and one prepared-command
/// cache.
///
/// This is plumbing, not package logic: nothing here knows what a package op is. It
/// exists because the projection writes run tens of thousands of tiny statements during
/// a cold-start grow, and building a fresh `SqliteCommand` (re-parsing the SQL) for each
/// one is most of the cost. Commands are keyed by SQL text, so a caller that reuses the
/// same template pays the parse once per batch. Dynamically built SQL still caches fine
/// as long as the same call site produces the same text.
///
/// Callers hold the `Ctx` for the life of one outer transaction and dispose it after.
module LibDB.PreparedBatch

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.Data.Sqlite

open Prelude


/// One batch's open connection plus its cache of prepared commands, keyed by SQL text.
type Ctx =
  { conn : SqliteConnection
    cmds : System.Collections.Generic.Dictionary<string, SqliteCommand> }

let newCtx (conn : SqliteConnection) : Ctx =
  { conn = conn
    cmds = System.Collections.Generic.Dictionary<string, SqliteCommand>() }

let disposeCtx (ctx : Ctx) : unit =
  for KeyValue(_, cmd) in ctx.cmds do
    cmd.Dispose()
  ctx.cmds.Clear()


/// Get the prepared command for this SQL, building and preparing it on first sight.
let command (ctx : Ctx) (sql : string) : SqliteCommand =
  match ctx.cmds.TryGetValue(sql) with
  | true, cmd -> cmd
  | false, _ ->
    let cmd = ctx.conn.CreateCommand()
    cmd.CommandText <- sql
    cmd.Prepare()
    ctx.cmds[sql] <- cmd
    cmd


/// Run a non-query statement and return how many rows it affected.
/// `setParams` populates the command's parameters, which are named with `$name`.
let execRows
  (ctx : Ctx)
  (sql : string)
  (setParams : SqliteCommand -> unit)
  : Task<int> =
  task {
    let cmd = command ctx sql
    cmd.Parameters.Clear()
    setParams cmd
    return! cmd.ExecuteNonQueryAsync()
  }


/// Run a non-query statement, ignoring how many rows it affected.
let exec
  (ctx : Ctx)
  (sql : string)
  (setParams : SqliteCommand -> unit)
  : Task<unit> =
  task {
    let! _ = execRows ctx sql setParams
    return ()
  }


/// Read one optional BLOB scalar. Raises if the column holds something else, since
/// that means the schema and the caller disagree rather than that the row is absent.
let bytesOption
  (ctx : Ctx)
  (sql : string)
  (setParams : SqliteCommand -> unit)
  : Task<Option<byte[]>> =
  task {
    let cmd = command ctx sql
    cmd.Parameters.Clear()
    setParams cmd
    let! value = cmd.ExecuteScalarAsync()
    return
      match value with
      | :? (byte[]) as bytes -> Some bytes
      | null -> None
      | value when System.Convert.IsDBNull value -> None
      | value ->
        Exception.raiseInternal
          "Expected a BLOB from a prepared-batch scalar read"
          [ "actualType", value.GetType().FullName ]
  }


/// Bind a parameter. Wraps `AddWithValue` so it always returns unit.
let inline p (cmd : SqliteCommand) (name : string) (value : obj) =
  cmd.Parameters.AddWithValue(name, value) |> ignore<SqliteParameter>


/// Bind a `Guid` as its canonical text representation. Without this, the
/// default Microsoft.Data.Sqlite type mapping is `BLOB(16)`, which does
/// not match the TEXT columns we store branch_id / location_id / etc. as,
/// so foreign-key checks fail with "constraint violated" even though
/// the parent row exists. (Fumble's `Sql.uuid` did this implicitly; we
/// replicate it.)
let inline pUuid (cmd : SqliteCommand) (name : string) (value : System.Guid) =
  p cmd name (string value)


/// Bind a `string option` as either the string or DBNull.
let inline pOpt (cmd : SqliteCommand) (name : string) (value : string option) =
  match value with
  | Some s -> p cmd name (box s)
  | None -> p cmd name (box System.DBNull.Value)
