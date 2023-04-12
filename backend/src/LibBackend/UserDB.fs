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

let rec toObj (db : RT.DB.T) (obj : string) : RT.Dval =
  // TYPESCLEANUP
  Exception.raiseInternal "toObj" []
// let pObj =
//   match DvalReprInternalQueryable.parseJsonV0 fieldTypes obj with
//   | RT.DDict o -> o
//   | _ -> Exception.raiseInternal "failed format, expected DDict" [ "actual", obj ]
// let typeChecked = typeCheck db pObj
// RT.DDict typeChecked


// TODO: Unify with TypeChecker.fs
and typeCheck (db : RT.DB.T) (dval : RT.Dval) : RT.Dval =
  Exception.raiseInternal "typecheck" ["db", db; "dval", dval]
  // let sameKeys = tipeKeys = objKeys

  // if sameKeys then
  //   Map.mapWithIndex
  //     (fun key value ->
  //       let col =
  //         Map.get key cols
  //         |> Exception.unwrapOptionInternal
  //              "Could not find Col"
  //              [ "name", key; "cols", cols ]
  //       match col, value with
  //       | RT.TInt, RT.DInt _ -> value
  //       | RT.TFloat, RT.DFloat _ -> value
  //       | RT.TString, RT.DString _ -> value
  //       | RT.TChar, RT.DChar _ -> value
  //       | RT.TBool, RT.DBool _ -> value
  //       | RT.TDateTime, RT.DDateTime _ -> value
  //       // CLEANUP use the inner type
  //       | RT.TList _, RT.DList _ -> value
  //       | RT.TPassword, RT.DPassword _ -> value
  //       | RT.TUuid, RT.DUuid _ -> value
  //       | RT.TDict _, RT.DDict _ -> value
  //       | RT.TUnit, RT.DUnit -> value // allow nulls for now
  //       | expectedType, valueOfActualType ->
  //         Exception.raiseCode (
  //           Errors.typeErrorMsg key expectedType valueOfActualType
  //         ))
  //     obj
  // else
  //   let missingKeys = Set.difference tipeKeys objKeys

  //   let missingMsg =
  //     "Expected but did not find: ["
  //     + (missingKeys |> Set.toList |> String.concat ", ")
  //     + "]"

  //   let extraKeys = Set.difference objKeys tipeKeys

  //   let extraMsg =
  //     "Found but did not expect: ["
  //     + (extraKeys |> Set.toList |> String.concat ", ")
  //     + "]"

  //   match (Set.isEmpty missingKeys, Set.isEmpty extraKeys) with
  //   | false, false -> Exception.raiseCode $"{missingMsg} & {extraMsg}"
  //   | false, true -> Exception.raiseCode missingMsg
  //   | true, false -> Exception.raiseCode extraMsg
  //   | true, true ->
  //     Exception.raiseCode
  //       "Type checker error! Deduced expected and actual did not unify, but could not find any examples!"


and set
  (state : RT.ExecutionState)
  (upsert : bool)
  (db : RT.DB.T)
  (key : string)
  (dv : RT.Dval)
  : Task<Uuid> =
  let id = System.Guid.NewGuid()
  let merged = typeCheck db dv

  let availableTypes = RT.ExecutionState.availableTypes state

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
                      "data",
                      Sql.jsonb (
                        DvalReprInternalQueryable.toJsonStringV0
                          availableTypes
                          db.typ
                          merged
                      ) ]
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
    return Option.map (toObj db) result
  }


and getMany
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  (keys : string list)
  : Task<List<RT.Dval>> =
  task {
    let! results =
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
    return results |> List.map (fun (data) -> (toObj db data))
  }


and getManyWithKeys
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  (keys : string list)
  : Task<List<string * RT.Dval>> =
  task {
    let! results =
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
    return results |> List.map (fun (key, data) -> (key, toObj db data))
  }


let getAll
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  : Task<List<string * RT.Dval>> =
  task {
    let! results =
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
    return results |> List.map (fun (key, data) -> (key, toObj db data))
  }

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
    let! results = doQuery state db b "key, data"
    let! results =
      results
      |> Sql.executeAsync (fun read -> (read.string "key", read.string "data"))

    return results |> List.map (fun (key, data) -> (key, toObj db data))
  }

let queryValues
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  (b : RT.LambdaImpl)
  : Task<List<RT.Dval>> =
  task {
    let! results = doQuery state db b "data"

    let! results = results |> Sql.executeAsync (fun read -> (read.string "data"))

    return results |> List.map (toObj db)
  }

let queryCount
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  (b : RT.LambdaImpl)
  : Task<int> =
  task {
    let! results = doQuery state db b "COUNT(*)"
    return! results |> Sql.executeRowAsync (fun read -> read.int "count")
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
let statsPluck
  (canvasID : CanvasID)
  (db : RT.DB.T)
  : Task<Option<RT.Dval * string>> =
  task {
    let! result =
      Sql.query
        "SELECT data, key
        FROM user_data_v0
        WHERE table_tlid = @tlid
          AND canvas_id = @canvasID
          AND user_version = @userVersion
          AND dark_version = @darkVersion
        ORDER BY created_at DESC
        LIMIT 1"
      |> Sql.parameters [ "tlid", Sql.tlid db.tlid
                          "canvasID", Sql.uuid canvasID
                          "userVersion", Sql.int db.version
                          "darkVersion", Sql.int currentDarkVersion ]
      |> Sql.executeRowOptionAsync (fun read ->
        (read.string "data", read.string "key"))
    return result |> Option.map (fun (data, key) -> (toObj db data, key))
  }

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
