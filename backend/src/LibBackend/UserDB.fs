/// <summary>
/// Supports anything relating to Datastores on a user canvas.
/// Namely, this file is responsible for managing this user data, including CRUD and type checking.
/// </summary>
///
/// <remarks>
/// User data stores live within the `user_data` table, which is broken down by:
/// - Dark version
/// - canvas (by ID)
/// - table/store ID
/// - user version (for when the users change the type being stored)
/// </remarks>
module LibBackend.UserDB

open System.Threading.Tasks
open FSharp.Control.Tasks
open Npgsql.FSharp
open Npgsql
open Db

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module Errors = LibExecution.Errors
module DvalReprInternalQueryable = LibExecution.DvalReprInternalQueryable

// Bump this if you make a breaking change to the underlying data format, and
// are migrating user data to the new version
//
// ! you should definitely notify the entire engineering team about this
let currentDarkVersion = 0

type Uuid = System.Guid

let rec dbToDval
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  (dbValue : string)
  : RT.Dval =
  let availableTypes = RT.ExecutionState.availableTypes state
  DvalReprInternalQueryable.parseJsonV0 availableTypes db.typ dbValue

let rec dvalToDB (state : RT.ExecutionState) (db : RT.DB.T) (dv : RT.Dval) : string =
  let availableTypes = RT.ExecutionState.availableTypes state
  DvalReprInternalQueryable.toJsonStringV0 availableTypes db.typ dv

let rec set
  (state : RT.ExecutionState)
  (upsert : bool)
  (db : RT.DB.T)
  (key : string)
  (dv : RT.Dval)
  : Task<Uuid> =
  let id = System.Guid.NewGuid()

  let availableTypes = RT.ExecutionState.availableTypes state

  match LibExecution.TypeChecker.unify [ key ] availableTypes db.typ dv with
  | Error err ->
    let msg = LibExecution.TypeChecker.Error.toString err
    Exception.raiseCode msg
  | Ok _ -> ()

  let upsertQuery =
    if upsert then
      "ON CONFLICT ON CONSTRAINT user_data_key_uniq DO UPDATE SET data = EXCLUDED.data"
    else
      ""

  Sql.query
    $"INSERT INTO user_data_v0
       (id, canvas_id, table_tlid, user_version, dark_version, key, data)
       VALUES (@id, @canvasID, @tlid, @userVersion, @darkVersion, @key, @data)
       {upsertQuery}"
  |> Sql.parameters [ "id", Sql.uuid id
                      "canvasID", Sql.uuid state.program.canvasID
                      "tlid", Sql.id db.tlid
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion
                      "key", Sql.string key
                      "data", Sql.jsonb (dvalToDB state db dv) ]
  |> Sql.executeStatementAsync
  |> Task.map (fun () -> id)



and getOption
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  (key : string)
  : Task<Option<RT.Dval>> =
  task {
    let! result =
      Sql.query
        "SELECT data
           FROM user_data_v0
          WHERE table_tlid = @tlid
            AND canvas_id = @canvasID
            AND user_version = @userVersion
            AND dark_version = @darkVersion
            AND key = @key"
      |> Sql.parameters [ "tlid", Sql.tlid db.tlid
                          "canvasID", Sql.uuid state.program.canvasID
                          "userVersion", Sql.int db.version
                          "darkVersion", Sql.int currentDarkVersion
                          "key", Sql.string key ]
      |> Sql.executeRowOptionAsync (fun read -> read.string "data")
    return Option.map (dbToDval state db) result
  }


and getMany
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  (keys : string list)
  : Task<List<RT.Dval>> =
  Sql.query
    "SELECT data
       FROM user_data_v0
      WHERE table_tlid = @tlid
        AND canvas_id = @canvasID
        AND user_version = @userVersion
        AND dark_version = @darkVersion
        AND key = ANY (@keys)"
  |> Sql.parameters [ "tlid", Sql.tlid db.tlid
                      "canvasID", Sql.uuid state.program.canvasID
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion
                      "keys", Sql.stringArray (Array.ofList keys) ]
  |> Sql.executeAsync (fun read -> read.string "data")
  |> Task.map (List.map (dbToDval state db))



and getManyWithKeys
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  (keys : string list)
  : Task<List<string * RT.Dval>> =
  Sql.query
    "SELECT key, data
       FROM user_data_v0
      WHERE table_tlid = @tlid
        AND canvas_id = @canvasID
        AND user_version = @userVersion
        AND dark_version = @darkVersion
        AND key = ANY (@keys)"
  |> Sql.parameters [ "tlid", Sql.tlid db.tlid
                      "canvasID", Sql.uuid state.program.canvasID
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion
                      "keys", Sql.stringArray (Array.ofList keys) ]
  |> Sql.executeAsync (fun read -> (read.string "key", read.string "data"))
  |> Task.map (List.map (fun (key, data) -> key, dbToDval state db data))



let getAll
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  : Task<List<string * RT.Dval>> =
  Sql.query
    "SELECT key, data
       FROM user_data_v0
      WHERE table_tlid = @tlid
        AND canvas_id = @canvasID
        AND user_version = @userVersion
        AND dark_version = @darkVersion"
  |> Sql.parameters [ "tlid", Sql.tlid db.tlid
                      "canvasID", Sql.uuid state.program.canvasID
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion ]
  |> Sql.executeAsync (fun read -> (read.string "key", read.string "data"))
  |> Task.map (List.map (fun (key, data) -> key, dbToDval state db data))

// Reusable function that provides the template for the SqlCompiler query functions
let doQuery
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  (b : RT.LambdaImpl)
  (queryFor : string)
  : Task<Sql.SqlProps> =
  task {
    let paramName =
      match b.parameters with
      | [ (_, name) ] -> name
      | _ -> Exception.raiseInternal "wrong number of args" [ "args", b.parameters ]

    let! sql, vars =
      SqlCompiler.compileLambda state b.symtable paramName db.typ b.body

    return
      Sql.query
        $"SELECT {queryFor}
            FROM user_data_v0
            WHERE table_tlid = @tlid
              AND canvas_id = @canvasID
              AND user_version = @userVersion
              AND dark_version = @darkVersion
              AND {sql}"
      |> Sql.parameters (
        vars
        @ [ "tlid", Sql.tlid db.tlid
            "canvasID", Sql.uuid state.program.canvasID
            "userVersion", Sql.int db.version
            "darkVersion", Sql.int currentDarkVersion ]
      )
  }

let query
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  (b : RT.LambdaImpl)
  : Task<List<string * RT.Dval>> =
  task {
    let! query = doQuery state db b "key, data"
    let! results =
      query |> Sql.executeAsync (fun read -> (read.string "key", read.string "data"))

    return results |> List.map (fun (key, data) -> (key, dbToDval state db data))
  }

let queryValues
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  (b : RT.LambdaImpl)
  : Task<List<RT.Dval>> =
  task {
    let! query = doQuery state db b "data"

    let! results = query |> Sql.executeAsync (fun read -> read.string "data")

    return results |> List.map (dbToDval state db)
  }

let queryCount
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  (b : RT.LambdaImpl)
  : Task<int> =
  task {
    let! query = doQuery state db b "COUNT(*)"
    return! query |> Sql.executeRowAsync (fun read -> read.int "count")
  }

let getAllKeys (state : RT.ExecutionState) (db : RT.DB.T) : Task<List<string>> =
  Sql.query
    "SELECT key
     FROM user_data_v0
     WHERE table_tlid = @tlid
     AND canvas_id = @canvasID
     AND user_version = @userVersion
     AND dark_version = @darkVersion"
  |> Sql.parameters [ "tlid", Sql.tlid db.tlid
                      "canvasID", Sql.uuid state.program.canvasID
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion ]
  |> Sql.executeAsync (fun read -> read.string "key")

let count (state : RT.ExecutionState) (db : RT.DB.T) : Task<int> =
  Sql.query
    "SELECT COUNT(*)
     FROM user_data_v0
     WHERE table_tlid = @tlid
       AND canvas_id = @canvasID
       AND user_version = @userVersion
       AND dark_version = @darkVersion"
  |> Sql.parameters [ "tlid", Sql.tlid db.tlid
                      "canvasID", Sql.uuid state.program.canvasID
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion ]
  |> Sql.executeRowAsync (fun read -> read.int "count")

let delete (state : RT.ExecutionState) (db : RT.DB.T) (key : string) : Task<unit> =
  Sql.query
    "DELETE
     FROM user_data_v0
     WHERE key = @key
       AND table_tlid = @tlid
       AND canvas_id = @canvasID
       AND user_version = @userVersion
       AND dark_version = @darkVersion"
  |> Sql.parameters [ "key", Sql.string key
                      "tlid", Sql.tlid db.tlid
                      "canvasID", Sql.uuid state.program.canvasID
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion ]
  |> Sql.executeStatementAsync

let deleteAll (state : RT.ExecutionState) (db : RT.DB.T) : Task<unit> =
  //   covered by idx_user_data_current_data_for_tlid
  Sql.query
    "DELETE FROM user_data_v0
     WHERE canvas_id = @canvasID
       AND table_tlid = @tlid
       AND user_version = @userVersion
       AND dark_version = @darkVersion"
  |> Sql.parameters [ "tlid", Sql.tlid db.tlid
                      "canvasID", Sql.uuid state.program.canvasID
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion ]
  |> Sql.executeStatementAsync

// -------------------------
// stats/locked/unlocked (not _locking_)
// -------------------------
// let statsPluck
//   (canvasID : CanvasID)
//   (db : RT.DB.T)
//   : Task<Option<RT.Dval * string>> =
//   task {
//     let! result =
//       Sql.query
//         "SELECT data, key
//         FROM user_data_v0
//         WHERE table_tlid = @tlid
//           AND canvas_id = @canvasID
//           AND user_version = @userVersion
//           AND dark_version = @darkVersion
//         ORDER BY created_at DESC
//         LIMIT 1"
//       |> Sql.parameters [ "tlid", Sql.tlid db.tlid
//                           "canvasID", Sql.uuid canvasID
//                           "userVersion", Sql.int db.version
//                           "darkVersion", Sql.int currentDarkVersion ]
//       |> Sql.executeRowOptionAsync (fun read ->
//         (read.string "data", read.string "key"))
//     return result |> Option.map (fun (data, key) -> (dbToDval state db data, key))
//   }

let statsCount (canvasID : CanvasID) (db : RT.DB.T) : Task<int> =
  Sql.query
    "SELECT COUNT(*)
     FROM user_data_v0
     WHERE table_tlid = @tlid
       AND canvas_id = @canvasID
       AND user_version = @userVersion
       AND dark_version = @darkVersion"
  |> Sql.parameters [ "tlid", Sql.tlid db.tlid
                      "canvasID", Sql.uuid canvasID
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion ]
  |> Sql.executeRowAsync (fun read -> read.int "count")

// Given a [canvasID], return tlids for all unlocked databases -
// a database is unlocked if it has no records, and thus its schema can be
// changed without a migration.

let all (canvasID : CanvasID) : Task<List<tlid>> =
  Sql.query
    "SELECT tlid
       FROM toplevel_oplists_v0
      WHERE canvas_id = @canvasID
        AND tipe = 'db'"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")


let unlocked (canvasID : CanvasID) : Task<List<tlid>> =
  // this will need to be fixed when we allow migrations
  // Note: tl.module IS NULL means it's a db; anything else will be
  // HTTP/REPL/CRON/WORKER
  // NOTE: the line `AND tl.account_id = ud.account_id` seems redunant, but
  // it's required to hit the index

  // CLEANUP: do we need table_tlid IS NULL since we're using NOT NULL in the schema?
  Sql.query
    "SELECT tl.tlid
     FROM toplevel_oplists_v0 as tl
     LEFT JOIN user_data_v0 as ud
            ON tl.tlid = ud.table_tlid
           AND tl.canvas_id = ud.canvas_id
     WHERE tl.canvas_id = @canvasID
       AND tl.module IS NULL
       AND tl.deleted = false
       AND ud.table_tlid IS NULL
     GROUP BY tl.tlid"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")


// -------------------------
// DB schema
// -------------------------

let create (tlid : tlid) (name : string) (typ : PT.TypeReference) : PT.DB.T =
  { tlid = tlid; name = name; typ = typ; version = 0 }

let renameDB (n : string) (db : PT.DB.T) : PT.DB.T = { db with name = n }
