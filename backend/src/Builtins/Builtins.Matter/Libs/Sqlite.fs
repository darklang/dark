/// Raw, general-purpose SQLite access from Darklang — the `Stdlib.Sqlite` floor primitive that lets policy
/// live in Dark over an ordinary database. (The internal sync machinery — op-log read/append, blob channel,
/// store path + Release coordinate — lives in `Libs/Sync.fs`, not here.)
///
/// `sqliteExec` (DDL/DML → rows affected) and `sqliteQuery` (SELECT → each row as a typed `Dict<Value>`)
/// are the two primitives; the `.dark` wrappers add the with/without-params convenience so there's one
/// builtin per operation rather than four. Both open a caller-supplied path, so they declare
/// `Needs.fileReadWrite` (the CLI host grants it; a narrowed `dark run` is denied).
///
/// Deferred (not needed yet): scoping the grant to a specific path/glob, binding typed params (params are
/// string-only today; results already carry types via `Value`), an opaque connection handle, and `transact`.
/// CLEANUP(sqlite-scope): add the in-body path/glob capability check.
module Builtins.Matter.Libs.Sqlite

open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

open Microsoft.Data.Sqlite

module Dval = LibExecution.Dval
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution
module Blob = LibExecution.Blob


let private connStr (path : string) : string = $"Data Source={path}"


/// Marshal a SQLite cell (the ADO reader's `obj`) into a typed `Stdlib.Sqlite.Value` DEnum, so
/// ints/reals/blobs keep their types across the round-trip (esp. Bytes for BLOB columns). Mirrors the
/// `Int.fs` ParseError.toDT pattern: build the FQ type name from PackageRefs, then a `DEnum`.
module Value =
  let typeName = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.sqliteValue ())
  let knownType : KnownType = KTCustomType(typeName, [])
  let typeRef : TypeReference = TCustomType(NR.ok typeName, [])

  let toDT (cell : obj) : Dval =
    let (caseName, fields) =
      match cell with
      | null -> "Null", []
      | :? System.DBNull -> "Null", [] // ADO returns DBNull.Value (not null) for a SQL NULL
      | :? int64 as i -> "Int", [ DInt64 i ]
      | :? double as f -> "Real", [ DFloat f ]
      | :? string as s -> "Text", [ DString s ]
      | :? (byte[]) as b -> "Bytes", [ Blob.newEphemeral b ]
      | other -> "Text", [ DString(string other) ]
    DEnum(typeName, typeName, [], caseName, fields)


/// Bind a positional param list to @p0..@pN — parameterized statements, so values can't be SQL-injected.
let private bindParams (cmd : SqliteCommand) (parameters : List<string>) : unit =
  parameters
  |> List.iteri (fun i p ->
    cmd.Parameters.AddWithValue($"@p{i}", box p) |> ignore<SqliteParameter>)


let private paramStrings (dvals : List<Dval>) : List<string> =
  dvals
  |> List.map (fun d ->
    match d with
    | DString s -> s
    | other -> string other)

// exec returns a `Result<Int, String>` (Ok rows-affected / Error message) so a bad path, a locked db, a
// mid-copy failure, etc. surface as a value Dark can handle — never an uncaught throw. This is what lets
// `Sync.pull`/the daemon skip a bad-or-offline peer gracefully.
let private execImpl
  (path : string)
  (sql : string)
  (parameters : List<string>)
  : Ply<Dval> =
  uply {
    try
      use conn = new SqliteConnection(connStr path)
      do! conn.OpenAsync()
      use cmd = conn.CreateCommand()
      cmd.CommandText <- sql
      bindParams cmd parameters
      let! affected = cmd.ExecuteNonQueryAsync()
      return Dval.resultOk KTInt KTString (Dval.int (bigint affected))
    with e ->
      return Dval.resultError KTInt KTString (DString e.Message)
  }

// query mirrors exec: a bad path / malformed SQL surfaces as an Error value, never an uncaught throw.
let private queryImpl
  (path : string)
  (sql : string)
  (parameters : List<string>)
  : Ply<Dval> =
  let rowsKT = KTList(ValueType.Known(KTDict(ValueType.Known Value.knownType)))

  uply {
    try
      use conn = new SqliteConnection(connStr path)
      do! conn.OpenAsync()
      use cmd = conn.CreateCommand()
      cmd.CommandText <- sql
      bindParams cmd parameters
      let! readerObj = cmd.ExecuteReaderAsync()
      use reader = readerObj
      let rows = System.Collections.Generic.List<Dval>()

      let rec loop () : Ply<unit> =
        uply {
          let! hasRow = reader.ReadAsync()
          if hasRow then
            let cells =
              [ for i in 0 .. reader.FieldCount - 1 ->
                  (reader.GetName i, Value.toDT (reader.GetValue i)) ]
            rows.Add(Dval.dict Value.knownType cells)
            return! loop ()
        }

      do! loop ()
      let listDval =
        Dval.list (KTDict(ValueType.Known Value.knownType)) (List.ofSeq rows)
      return Dval.resultOk rowsKT KTString listDval
    with e ->
      return Dval.resultError rowsKT KTString (DString e.Message)
  }


let fns () : List<BuiltInFn> =
  [ { name = fn "sqliteExec" 0
      typeParams = []
      parameters =
        [ Param.make "path" TString "the SQLite file to open"
          Param.make
            "sql"
            TString
            "a statement to run (CREATE/INSERT/UPDATE/DELETE…)"
          Param.make
            "params"
            (TList TString)
            ("values bound to @p0..@pN placeholders, in order (injection-safe); "
             + "[] for none") ]
      returnType = TypeReference.result TInt TString
      description =
        "Opens the SQLite file at <param path> and runs <param sql>, binding "
        + "<param params> to @p0..@pN placeholders (injection-safe; pass [] for "
        + "none). Ok = rows affected; Error = the SQLite message. Never throws."
      fn =
        (function
        | struct (_, _, _, [| DString path; DString sql; DList(_, ps) |]) ->
          execImpl path sql (paramStrings ps)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.fileReadWrite
      deprecated = NotDeprecated }

    { name = fn "sqliteQuery" 0
      typeParams = []
      parameters =
        [ Param.make "path" TString "the SQLite file to open"
          Param.make "sql" TString "a SELECT to run"
          Param.make
            "params"
            (TList TString)
            ("values bound to @p0..@pN placeholders, in order (injection-safe); "
             + "[] for none") ]
      returnType = TypeReference.result (TList(TDict Value.typeRef)) TString
      description =
        "Opens the SQLite file at <param path> and runs the SELECT <param sql>, "
        + "binding <param params> to @p0..@pN placeholders (injection-safe; pass "
        + "[] for none). Ok = each row as a dict of column-name to its typed "
        + "value; Error = the SQLite message. Never throws."
      fn =
        (function
        | struct (_, _, _, [| DString path; DString sql; DList(_, ps) |]) ->
          queryImpl path sql (paramStrings ps)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.fileReadWrite
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
