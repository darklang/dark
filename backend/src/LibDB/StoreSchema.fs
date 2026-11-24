/// The store's tables, as `migrations/schema/*.sql` declares them.
///
/// The schema is replayed against an existing store: `CREATE ... IF NOT EXISTS` adds what the store
/// lacks and leaves the rest alone. Nothing changes the shape of a table that is already there, so a
/// store whose tables differ from the schema is refused (`mismatch`), not migrated.
module LibDB.StoreSchema

open Fumble
open LibDB.Sqlite

open Prelude


let private statements (schemaSql : string) : string[] =
  // Comments FIRST, then split. The schema's comments contain semicolons ("NULL = the branch's
  // draft."), so splitting first cuts statements in half and SQLite reports "incomplete input".
  // No `--` appears inside a string literal in that file, so truncating at one is safe here.
  let stripped =
    schemaSql.Split('\n')
    |> Array.map (fun line ->
      match line.IndexOf "--" with
      | -1 -> line
      | i -> line.Substring(0, i))
    |> String.concat "\n"

  stripped.Split(';')
  |> Array.map (fun st -> st.Trim())
  |> Array.filter (fun st -> st <> "")

let private isTable (st : string) =
  st.StartsWith("CREATE TABLE", System.StringComparison.OrdinalIgnoreCase)

let private run (connString : string) (statements : string[]) : unit =
  use conn = new Microsoft.Data.Sqlite.SqliteConnection(connString)
  conn.Open()
  Telemetry.countBy "sql.schema" (int64 statements.Length)
  for st in statements do
    use cmd = conn.CreateCommand()
    cmd.CommandText <- st
    cmd.ExecuteNonQuery() |> ignore<int>


/// Create the tables, seeded rows and indexes the store lacks, in file order.
let apply (schemaSql : string) : unit = run connString (statements schemaSql)


/// How the store's tables differ from the schema's, if they do. A table the store does not have yet
/// is no difference: `apply` adds it.
let mismatch (schemaSql : string) : Option<string> =
  use fresh = new Microsoft.Data.Sqlite.SqliteConnection("Data Source=:memory:")
  fresh.Open()
  for st in statements schemaSql |> Array.filter isTable do
    use cmd = fresh.CreateCommand()
    cmd.CommandText <- st
    cmd.ExecuteNonQuery() |> ignore<int>

  let freshNames (sql : string) (table : string) : List<string> =
    use cmd = fresh.CreateCommand()
    cmd.CommandText <- sql
    cmd.Parameters.AddWithValue("@t", table)
    |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
    use r = cmd.ExecuteReader()
    [ while r.Read() do
        yield r.GetString 0 ]

  let tablesSql =
    "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE @t"
  let columnsSql = "SELECT name FROM pragma_table_info(@t)"

  freshNames tablesSql "sqlite_%"
  |> List.tryPick (fun table ->
    let want = freshNames columnsSql table |> Set.ofList
    let have =
      Sql.query columnsSql
      |> Sql.parameters [ "t", Sql.string table ]
      |> Sql.executeRows (fun read -> read.string "name")
      |> Set.ofList
    if Set.isEmpty have then
      None
    else
      match
        Set.toList (Set.difference want have), Set.toList (Set.difference have want)
      with
      | [], [] -> None
      | missing :: _, _ -> Some $"the `{table}` table has no `{missing}` column"
      | [], extra :: _ ->
        Some $"the `{table}` table has a `{extra}` column this dark does not know")


/// Whether the store's packages are in another binary format than this build reads. The oldest op
/// stands for the store: a store holds the format of the build that made it.
let formatMismatch () : Option<string> =
  let current = LibSerialization.Binary.BaseFormat.CurrentVersion
  Sql.query "SELECT op_blob FROM package_ops ORDER BY seq LIMIT 1"
  |> Sql.executeRows (fun read -> read.bytes "op_blob")
  |> List.tryPick (fun blob ->
    if blob.Length < 4 then
      None
    else
      let version = System.BitConverter.ToUInt32(blob, 0)
      if version = current then
        None
      else
        Some
          $"its packages are in binary format {version}; this dark reads format {current}")
