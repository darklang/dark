/// <summary>
/// Supports anything relating to user-defined Datastores.
/// Namely, this file is responsible for managing such user data, including CRUD and type-checking.
/// </summary>
///
/// <remarks>
/// User data stores live within the `user_data` table, which is broken down by:
/// - Dark version
/// - DB scope (by UUID)
/// - table/store ID
/// - user version (for when the users change the type being stored)
/// </remarks>
module LibDB.UserDB

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.Data.Sqlite
open Fumble
open LibSqlite.Db

open Prelude

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module DvalReprInternalQueryable = LibExecution.DvalReprInternalQueryable

/// Type of DB query operation - determines SQL SELECT and result processing
type DBQueryType =
  | DBQueryAll
  | DBQueryWithKey
  | DBQueryOne
  | DBQueryCount

// Bump this if you make a breaking change to the underlying data format, and
// are migrating user data to the new version
//
// ! you should definitely notify the entire engineering team about this
let currentDarkVersion = 0

let rec dbToDval
  (types : RT.Types)
  (threadID : RT.ThreadID)
  (tst : RT.TypeSymbolTable)
  (db : RT.DB.T)
  (dbValue : string)
  : Ply<RT.Dval> =
  DvalReprInternalQueryable.parseJsonV0 types threadID tst db.typ dbValue

let dvalToDB
  (threadID : RT.ThreadID)
  (types : RT.Types)
  (dv : RT.Dval)
  : Ply<string> =
  DvalReprInternalQueryable.toJsonStringV0 types threadID dv

let rec set
  (exeState : RT.ExecutionState)
  (threadID : RT.ThreadID)
  (upsert : bool)
  (db : RT.DB.T)
  (key : string)
  (dv : RT.Dval)
  : Ply<Result<uuid, RT.RuntimeError.Error>> =
  uply {
    let id = System.Guid.NewGuid()

    let types = exeState.types
    // CLEANUP: the caller should do this type check instead, but we haven't
    // implemented nested types in the DB yet
    //let context = LibExecution.TypeChecker.DBSchemaType(db.name, db.typ)


    // TODO: should we be passing in a TST?
    match! LibExecution.TypeChecker.unify types Map.empty db.typ dv with
    | Error _errPath ->
      // TODO: include path
      return Error(RT.RuntimeError.DBSetOfWrongType(db.typ, RT.Dval.toValueType dv))

    | Ok _ ->
      let upsertQuery =
        if upsert then
          "ON CONFLICT (canvas_id, table_tlid, dark_version, user_version, key) DO UPDATE SET data=EXCLUDED.data, updated_at=datetime('now')"
        else
          ""

      let! data = dvalToDB threadID types dv

      do!
        Sql.query
          $"INSERT INTO user_data_v0
            (id, canvas_id, table_tlid, user_version, dark_version, key, data, updated_at)
          VALUES
            (@id, @dbScope, @tlid, @userVersion, @darkVersion, @key, @data, datetime('now'))
          {upsertQuery}"
        |> Sql.parameters
          [ "id", Sql.uuid id
            "dbScope", Sql.uuid exeState.program.dbScope
            "tlid", Sql.id db.tlid
            "userVersion", Sql.int db.version
            "darkVersion", Sql.int currentDarkVersion
            "key", Sql.string key
            "data", Sql.string data ]
        |> Sql.executeStatementAsync

      return Ok id
  }



and getOption
  (exeState : RT.ExecutionState)
  (threadID : RT.ThreadID)
  (db : RT.DB.T)
  (key : string)
  : Ply<Option<RT.Dval>> =
  uply {
    let types = exeState.types

    let! result =
      Sql.query
        "SELECT data
          FROM user_data_v0
        WHERE table_tlid = @tlid
          AND canvas_id = @dbScope
          AND user_version = @userVersion
          AND dark_version = @darkVersion
          AND key = @key"
      |> Sql.parameters
        [ "tlid", Sql.tlid db.tlid
          "dbScope", Sql.uuid exeState.program.dbScope
          "userVersion", Sql.int db.version
          "darkVersion", Sql.int currentDarkVersion
          "key", Sql.string key ]
      |> Sql.executeRowOptionAsync (fun read -> read.string "data")

    match result with
    | None -> return None
    | Some dval ->
      let tst = Map.empty // OK?
      return! dbToDval types threadID tst db dval |> Ply.map Some
  }


and getMany
  (exeState : RT.ExecutionState)
  (threadID : RT.ThreadID)
  (tst : RT.TypeSymbolTable)
  (db : RT.DB.T)
  (keys : string list)
  : Ply<List<RT.Dval>> =
  uply {
    let types = exeState.types

    // If no keys, return empty list early
    if List.isEmpty keys then
      return []
    else
      // Create parameters for each key
      let keyParams = keys |> List.mapi (fun i key -> ($"key{i}", Sql.string key))

      // Create placeholders for the SQL query
      let keyPlaceholders =
        keys |> List.mapi (fun i _ -> $"@key{i}") |> String.concat ", "

      // Base parameters that are always needed
      let baseParams =
        [ "tlid", Sql.tlid db.tlid
          "dbScope", Sql.uuid exeState.program.dbScope
          "userVersion", Sql.int db.version
          "darkVersion", Sql.int currentDarkVersion ]

      let! result =
        Sql.query
          $"SELECT data
          FROM user_data_v0
          WHERE table_tlid = @tlid
            AND canvas_id = @dbScope
            AND user_version = @userVersion
            AND dark_version = @darkVersion
            AND key IN ({keyPlaceholders})"
        |> Sql.parameters (baseParams @ keyParams)
        |> Sql.executeAsync (fun read -> read.string "data")

      return! result |> List.map (dbToDval types threadID tst db) |> Ply.List.flatten
  }



and getManyWithKeys
  (exeState : RT.ExecutionState)
  (threadID : RT.ThreadID)
  (tst : RT.TypeSymbolTable)
  (db : RT.DB.T)
  (keys : string list)
  : Ply<List<string * RT.Dval>> =
  uply {
    let types = exeState.types

    // If no keys, return empty list early
    if List.isEmpty keys then
      return []
    else
      // Create parameters for each key
      let keyParams = keys |> List.mapi (fun i key -> ($"key{i}", Sql.string key))

      // Create placeholders for the SQL query
      let keyPlaceholders =
        keys |> List.mapi (fun i _ -> $"@key{i}") |> String.concat ", "

      // Base parameters that are always needed
      let baseParams =
        [ "tlid", Sql.tlid db.tlid
          "dbScope", Sql.uuid exeState.program.dbScope
          "userVersion", Sql.int db.version
          "darkVersion", Sql.int currentDarkVersion ]

      let! result =
        Sql.query
          $"SELECT key, data
          FROM user_data_v0
          WHERE table_tlid = @tlid
            AND canvas_id = @dbScope
            AND user_version = @userVersion
            AND dark_version = @darkVersion
            AND key IN ({keyPlaceholders})"
        |> Sql.parameters (baseParams @ keyParams)
        |> Sql.executeAsync (fun read -> (read.string "key", read.string "data"))

      return!
        result
        |> List.map (fun (key, data) ->
          dbToDval types threadID tst db data |> Ply.map (fun dval -> (key, dval)))
        |> Ply.List.flatten
  }



let getAll
  (exeState : RT.ExecutionState)
  (threadID : RT.ThreadID)
  (tst : RT.TypeSymbolTable)
  (db : RT.DB.T)
  : Ply<List<string * RT.Dval>> =
  uply {
    let! result =
      Sql.query
        "SELECT key, data
        FROM user_data_v0
        WHERE table_tlid = @tlid
          AND canvas_id = @dbScope
          AND user_version = @userVersion
          AND dark_version = @darkVersion"
      |> Sql.parameters
        [ "tlid", Sql.tlid db.tlid
          "dbScope", Sql.uuid exeState.program.dbScope
          "userVersion", Sql.int db.version
          "darkVersion", Sql.int currentDarkVersion ]
      |> Sql.executeAsync (fun read -> (read.string "key", read.string "data"))

    return!
      result
      |> List.map (fun (key, data) ->
        dbToDval exeState.types threadID tst db data
        |> Ply.map (fun dval -> (key, dval)))
      |> Ply.List.flatten
  }

let getAllKeys (exeState : RT.ExecutionState) (db : RT.DB.T) : Task<List<string>> =
  Sql.query
    "SELECT key
    FROM user_data_v0
    WHERE table_tlid = @tlid
      AND canvas_id = @dbScope
      AND user_version = @userVersion
      AND dark_version = @darkVersion"
  |> Sql.parameters
    [ "tlid", Sql.tlid db.tlid
      "dbScope", Sql.uuid exeState.program.dbScope
      "userVersion", Sql.int db.version
      "darkVersion", Sql.int currentDarkVersion ]
  |> Sql.executeAsync (fun read -> read.string "key")

let count (exeState : RT.ExecutionState) (db : RT.DB.T) : Task<int> =
  Sql.query
    "SELECT COUNT(*) as count
    FROM user_data_v0
    WHERE table_tlid = @tlid
      AND canvas_id = @dbScope
      AND user_version = @userVersion
      AND dark_version = @darkVersion"
  |> Sql.parameters
    [ "tlid", Sql.tlid db.tlid
      "dbScope", Sql.uuid exeState.program.dbScope
      "userVersion", Sql.int db.version
      "darkVersion", Sql.int currentDarkVersion ]
  |> Sql.executeRowAsync (fun read -> read.int "count")

let delete
  (exeState : RT.ExecutionState)
  (db : RT.DB.T)
  (key : string)
  : Task<unit> =
  Sql.query
    "DELETE
    FROM user_data_v0
    WHERE key = @key
      AND table_tlid = @tlid
      AND canvas_id = @dbScope
      AND user_version = @userVersion
      AND dark_version = @darkVersion"
  |> Sql.parameters
    [ "key", Sql.string key
      "tlid", Sql.tlid db.tlid
      "dbScope", Sql.uuid exeState.program.dbScope
      "userVersion", Sql.int db.version
      "darkVersion", Sql.int currentDarkVersion ]
  |> Sql.executeStatementAsync

let deleteAll (exeState : RT.ExecutionState) (db : RT.DB.T) : Task<unit> =
  //   covered by idx_user_data_current_data_for_tlid
  Sql.query
    "DELETE FROM user_data_v0
    WHERE canvas_id = @dbScope
      AND table_tlid = @tlid
      AND user_version = @userVersion
      AND dark_version = @darkVersion"
  |> Sql.parameters
    [ "tlid", Sql.tlid db.tlid
      "dbScope", Sql.uuid exeState.program.dbScope
      "userVersion", Sql.int db.version
      "darkVersion", Sql.int currentDarkVersion ]
  |> Sql.executeStatementAsync

// -------------------------
// stats/locked/unlocked (not _locking_)
// -------------------------

let statsCount (dbScope : uuid) (db : RT.DB.T) : Task<int> =
  Sql.query
    "SELECT COUNT(*) as count
    FROM user_data_v0
    WHERE table_tlid = @tlid
      AND canvas_id = @dbScope
      AND user_version = @userVersion
      AND dark_version = @darkVersion"
  |> Sql.parameters
    [ "tlid", Sql.tlid db.tlid
      "dbScope", Sql.uuid dbScope
      "userVersion", Sql.int db.version
      "darkVersion", Sql.int currentDarkVersion ]
  |> Sql.executeRowAsync (fun read -> read.int "count")

// Given a [dbScope], return tlids for all unlocked databases -
// a database is unlocked if it has no records, and thus its schema can be
// changed without a migration.

let all (dbScope : uuid) : Task<List<tlid>> =
  Sql.query
    "SELECT tlid
    FROM toplevels_v0
    WHERE canvas_id = @dbScope
      AND tipe = 'db'"
  |> Sql.parameters [ "dbScope", Sql.uuid dbScope ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")


let unlocked (dbScope : uuid) : Task<List<tlid>> =
  // this will need to be fixed when we allow migrations
  // Note: tl.module IS NULL means it's a db; anything else will be
  // HTTP/REPL/CRON/WORKER
  // NOTE: the line `AND tl.account_id = ud.account_id` seems redunant, but
  // it's required to hit the index

  // CLEANUP: do we need table_tlid IS NULL since we're using NOT NULL in the schema?
  Sql.query
    "SELECT tl.tlid
    FROM toplevels_v0 as tl
    LEFT JOIN user_data_v0 as ud
      ON tl.tlid = ud.table_tlid
        AND tl.canvas_id = ud.canvas_id
    WHERE tl.canvas_id = @dbScope
      AND tl.module IS NULL
      AND tl.deleted = 0
      AND ud.table_tlid IS NULL
    GROUP BY tl.tlid"
  |> Sql.parameters [ "dbScope", Sql.uuid dbScope ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")



// -------------------------
// DB schema
// -------------------------

let create (tlid : tlid) (name : string) (typ : PT.TypeReference) : PT.DB.T =
  { tlid = tlid; name = name; typ = typ; version = 0 }

let renameDB (n : string) (db : PT.DB.T) : PT.DB.T = { db with name = n }


// -------------------------
// DBQuery execution
// -------------------------

/// Execute a compiled DB query
/// Called by DB.query/queryWithKey/queryOne/queryCount builtins
let executeCompiledQuery
  (exeState : RT.ExecutionState)
  (vm : RT.VMState)
  (db : RT.DB.T)
  (queryType : DBQueryType)
  (compiledSql : string)
  (paramValues : List<RT.Dval>)
  : Ply<RT.Dval> =
  uply {
    let types = exeState.types
    let threadID = vm.threadID
    let tst = Map.empty // TODO: proper type symbol table

    // Build parameter bindings for the SQL query
    // The compiled SQL uses @p1, @p2, etc.
    let! paramBindings =
      paramValues
      |> List.mapi (fun i dv -> (i, dv))
      |> Ply.List.mapSequentially (fun (i, dv) ->
        uply {
          let paramName = $"p{i + 1}"
          let! sqlValue =
            match dv with
            | RT.DString s -> Ply(Sql.string s)
            | RT.DInt64 n -> Ply(Sql.int64 n)
            | RT.DFloat f -> Ply(Sql.double f)
            | RT.DBool b -> Ply(Sql.bool b)
            | RT.DUnit -> Ply(Sql.string "null")
            | RT.DUuid u -> Ply(Sql.uuid u)
            | RT.DDateTime dt ->
              Ply(Sql.string (LibExecution.DarkDateTime.toIsoString dt))
            | other ->
              // For complex types, convert to JSON string
              uply {
                let! json = dvalToDB threadID types other
                return Sql.string json
              }
          return (paramName, sqlValue)
        })

    // Base parameters for the table filtering
    let baseParams =
      [ "tlid", Sql.tlid db.tlid
        "dbScope", Sql.uuid exeState.program.dbScope
        "userVersion", Sql.int db.version
        "darkVersion", Sql.int currentDarkVersion ]

    let allParams = baseParams @ paramBindings

    // Determine what to SELECT and build the query
    match queryType with
    | DBQueryAll ->
      let! results =
        Sql.query
          $"SELECT data
            FROM user_data_v0
            WHERE table_tlid = @tlid
              AND canvas_id = @dbScope
              AND user_version = @userVersion
              AND dark_version = @darkVersion
              AND ({compiledSql})"
        |> Sql.parameters allParams
        |> Sql.executeAsync (fun read -> read.string "data")

      let! dvals =
        results |> List.map (dbToDval types threadID tst db) |> Ply.List.flatten

      return
        dvals
        |> LibExecution.TypeChecker.DvalCreator.list
          threadID
          LibExecution.ValueType.unknownDbTODO

    | DBQueryWithKey ->
      let! results =
        Sql.query
          $"SELECT key, data
            FROM user_data_v0
            WHERE table_tlid = @tlid
              AND canvas_id = @dbScope
              AND user_version = @userVersion
              AND dark_version = @darkVersion
              AND ({compiledSql})"
        |> Sql.parameters allParams
        |> Sql.executeAsync (fun read -> (read.string "key", read.string "data"))

      let! kvPairs =
        results
        |> List.map (fun (key, data) ->
          dbToDval types threadID tst db data |> Ply.map (fun dval -> (key, dval)))
        |> Ply.List.flatten

      return
        LibExecution.TypeChecker.DvalCreator.dict
          threadID
          LibExecution.ValueType.unknownDbTODO
          kvPairs

    | DBQueryOne ->
      let! results =
        Sql.query
          $"SELECT data
            FROM user_data_v0
            WHERE table_tlid = @tlid
              AND canvas_id = @dbScope
              AND user_version = @userVersion
              AND dark_version = @darkVersion
              AND ({compiledSql})
            LIMIT 2"
        |> Sql.parameters allParams
        |> Sql.executeAsync (fun read -> read.string "data")

      match results with
      | [ single ] ->
        let! dval = dbToDval types threadID tst db single
        return
          LibExecution.TypeChecker.DvalCreator.optionSome
            threadID
            LibExecution.ValueType.unknownDbTODO
            dval
      | _ ->
        // None if zero or more than one result
        return
          LibExecution.TypeChecker.DvalCreator.optionNone
            LibExecution.ValueType.unknownDbTODO

    | DBQueryCount ->
      let! count =
        Sql.query
          $"SELECT COUNT(*) as count
            FROM user_data_v0
            WHERE table_tlid = @tlid
              AND canvas_id = @dbScope
              AND user_version = @userVersion
              AND dark_version = @darkVersion
              AND ({compiledSql})"
        |> Sql.parameters allParams
        |> Sql.executeRowAsync (fun read -> read.int "count")

      return RT.DInt64(int64 count)
  }
