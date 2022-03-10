module LibBackend.UserDB

// Anything relating to Datastores on a user canvas

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

// Bump this if you make a breaking change to the underlying data format, and
// are migrating user data to the new version
//
// ! you should definitely notify the entire engineering team about this
let currentDarkVersion = 0

type Uuid = System.Guid

let rec queryExactFields
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  (queryObj : RT.DvalMap)
  : Task<List<string * RT.Dval>> =
  Sql.query
    "SELECT key, data
     FROM user_data
     WHERE table_tlid = @tlid
     AND user_version = @userVersion
     AND dark_version = @darkVersion
     AND canvas_id = @canvasID
     AND data @> @fields"
  |> Sql.parameters [ "tlid", Sql.tlid db.tlid
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion
                      "canvasID", Sql.uuid state.program.canvasID
                      "fields", Sql.queryableDvalMap queryObj ]
  |> Sql.executeAsync (fun read ->
    (read.string "key", read.string "data" |> toObj db))


// Handle the DB hacks while converting this into a DVal
and toObj (db : RT.DB.T) (obj : string) : RT.Dval =
  let pObj =
    match LibExecution.DvalReprInternal.ofInternalQueryableV1 obj with
    | RT.DObj o ->
      // <HACK 1>: some legacy objects were allowed to be saved with `id`
      // keys _in_ the data object itself. they got in the datastore on
      // the `update` of an already present object as `update` did not
      // remove the magic `id` field which had been injected on fetch. we
      // need to remove magic `id` if we fetch them otherwise they will
      // not type check on the way out any more and will not work.  if
      // they are re-saved with `update` they will have their ids
      // removed.  we consider an `id` key on the map to be a "magic" one
      // if it is present in the map but not in the schema of the object.
      // this is a deliberate weakening of our schema checker to deal
      // with this case.
      if not (List.includes "id" (db.cols |> List.map Tuple2.first)) then
        Map.remove "id" o
      else
        o
    // </HACK 1>
    | x -> Exception.raiseInternal "failed format, expected DObj" [ "actual", obj ]
  // <HACK 2>: because it's hard to migrate at the moment, we need to have
  // default values when someone adds a col. We can remove this when the
  // migrations work properly. Structured like this so that hopefully we
  // only have to remove this small part.
  let defaultKeys = db.cols |> List.map (fun (k, _) -> (k, RT.DNull)) |> Map
  let merged = Map.mergeFavoringLeft pObj defaultKeys
  // </HACK 2>
  let typeChecked = typeCheck db merged
  RT.DObj typeChecked


// TODO: Unify with Type_checker.ml
and typeCheck (db : RT.DB.T) (obj : RT.DvalMap) : RT.DvalMap =
  let cols = Map.ofList db.cols
  let tipeKeys = cols |> Map.keys |> Set.ofList
  let objKeys = obj |> Map.keys |> Set.ofList
  let sameKeys = tipeKeys = objKeys

  if sameKeys then
    Map.mapWithIndex
      (fun key value ->
        match (Map.get key cols |> Option.unwrapUnsafe, value) with
        | RT.TInt, RT.DInt _ -> value
        | RT.TFloat, RT.DFloat _ -> value
        | RT.TStr, RT.DStr _ -> value
        | RT.TBool, RT.DBool _ -> value
        | RT.TDate, RT.DDate _ -> value
        // CLEANUP use the inner type
        | RT.TList _, RT.DList _ -> value
        | RT.TPassword, RT.DPassword _ -> value
        | RT.TUuid, RT.DUuid _ -> value
        | RT.TDict _, RT.DObj _ -> value
        | RT.TRecord _, RT.DObj _ -> value
        | _, RT.DNull -> value // allow nulls for now
        | expectedType, valueOfActualType ->
          Errors.throw (Errors.typeErrorMsg key expectedType valueOfActualType))
      obj
  else
    let missingKeys = Set.difference tipeKeys objKeys

    let missingMsg =
      "Expected but did not find: ["
      + (missingKeys |> Set.toList |> String.concat ", ")
      + "]"

    let extraKeys = Set.difference objKeys tipeKeys

    let extraMsg =
      "Found but did not expect: ["
      + (extraKeys |> Set.toList |> String.concat ", ")
      + "]"

    match (Set.isEmpty missingKeys, Set.isEmpty extraKeys) with
    | false, false -> Errors.throw $"{missingMsg} & {extraMsg}"
    | false, true -> Errors.throw missingMsg
    | true, false -> Errors.throw extraMsg
    | true, true ->
      Errors.throw
        "Type checker error! Deduced expected and actual did not unify, but could not find any examples!"


and set
  (state : RT.ExecutionState)
  (upsert : bool)
  (db : RT.DB.T)
  (key : string)
  (vals : RT.DvalMap)
  : Task<Uuid> =
  let id = System.Guid.NewGuid()
  let merged = typeCheck db vals

  let upsertQuery =
    if upsert then
      "ON CONFLICT ON CONSTRAINT user_data_key_uniq DO UPDATE SET data = EXCLUDED.data"
    else
      ""

  Sql.query
    $"INSERT INTO user_data
       (id, account_id, canvas_id, table_tlid, user_version, dark_version, key, data)
       VALUES (@id, @accountID, @canvasID, @tlid, @userVersion, @darkVersion, @key, @data)
       {upsertQuery}"
  |> Sql.parameters [ "id", Sql.uuid id
                      "accountID", Sql.uuid state.program.accountID
                      "canvasID", Sql.uuid state.program.canvasID
                      "tlid", Sql.id db.tlid
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion
                      "key", Sql.string key
                      "data", Sql.queryableDvalMap merged ]
  |> Sql.executeStatementAsync
  |> Task.map (fun () -> id)



and getOption
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  (key : string)
  : Task<Option<RT.Dval>> =
  Sql.query
    "SELECT data
       FROM user_data
       WHERE table_tlid = @tlid
         AND account_id = @accountID
         AND canvas_id = @canvasID
         AND user_version = @userVersion
         AND dark_version = @darkVersion
         AND key = @key"
  |> Sql.parameters [ "tlid", Sql.tlid db.tlid
                      "accountID", Sql.uuid state.program.accountID
                      "canvasID", Sql.uuid state.program.canvasID
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion
                      "key", Sql.string key ]
  |> Sql.executeRowOptionAsync (fun read -> read.string "data" |> toObj db)

// CLEANUP: this is identical to getManyWithKeys, remove the key
and getMany
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  (keys : string list)
  : Task<List<string * RT.Dval>> =
  Sql.query
    "SELECT key, data
     FROM user_data
     WHERE table_tlid = @tlid
       AND account_id = @accountID
       AND canvas_id = @canvasID
       AND user_version = @userVersion
       AND dark_version = @darkVersion
       AND key = ANY (@keys)"
  |> Sql.parameters [ "tlid", Sql.tlid db.tlid
                      "accountID", Sql.uuid state.program.accountID
                      "canvasID", Sql.uuid state.program.canvasID
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion
                      "keys", Sql.stringArray (Array.ofList keys) ]
  |> Sql.executeAsync (fun read ->
    (read.string "key", read.string "data" |> toObj db))


and getManyWithKeys
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  (keys : string list)
  : Task<List<string * RT.Dval>> =
  Sql.query
    "SELECT key, data
     FROM user_data
     WHERE table_tlid = @tlid
     AND account_id = @accountID
     AND canvas_id = @canvasID
     AND user_version = @userVersion
     AND dark_version = @darkVersion
     AND key = ANY (@keys)"
  |> Sql.parameters [ "tlid", Sql.tlid db.tlid
                      "accountID", Sql.uuid state.program.accountID
                      "canvasID", Sql.uuid state.program.canvasID
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion
                      "keys", Sql.stringArray (Array.ofList keys) ]
  |> Sql.executeAsync (fun read ->
    (read.string "key", read.string "data" |> toObj db))


let getAll
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  : Task<List<string * RT.Dval>> =
  Sql.query
    "SELECT key, data
     FROM user_data
     WHERE table_tlid = @tlid
     AND account_id = @accountID
     AND canvas_id = @canvasID
     AND user_version = @userVersion
     AND dark_version = @darkVersion"
  |> Sql.parameters [ "tlid", Sql.tlid db.tlid
                      "accountID", Sql.uuid state.program.accountID
                      "canvasID", Sql.uuid state.program.canvasID
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion ]
  |> Sql.executeAsync (fun read ->
    (read.string "key", read.string "data" |> toObj db))

// Reusable function that provides the template for the SqlCompiler query functions
let doQuery
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  (b : RT.LambdaImpl)
  (queryFor : string)
  : Task<Sql.SqlProps> =
  task {
    let dbFields = Map.ofList db.cols

    let paramName =
      match b.parameters with
      | [ (_, name) ] -> name
      | _ -> Exception.raiseInternal "wrong number of args" [ "args", b.parameters ]

    let! sql, vars =
      SqlCompiler.compileLambda state b.symtable paramName dbFields b.body

    return
      Sql.query
        $"SELECT {queryFor}
            FROM user_data
            WHERE table_tlid = @tlid
              AND account_id = @accountID
              AND canvas_id = @canvasID
              AND user_version = @userVersion
              AND dark_version = @darkVersion
              AND {sql}"
      |> Sql.parameters (
        vars
        @ [ "tlid", Sql.tlid db.tlid
            "accountID", Sql.uuid state.program.accountID
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

    return!
      results
      |> Sql.executeAsync (fun read ->
        (read.string "key", read.string "data" |> toObj db))
  }

let queryValues
  (state : RT.ExecutionState)
  (db : RT.DB.T)
  (b : RT.LambdaImpl)
  : Task<List<RT.Dval>> =
  task {
    let! results = doQuery state db b "data"

    return!
      results |> Sql.executeAsync (fun read -> (read.string "data" |> toObj db))
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
     FROM user_data
     WHERE table_tlid = @tlid
     AND account_id = @accountID
     AND canvas_id = @canvasID
     AND user_version = @userVersion
     AND dark_version = @darkVersion"
  |> Sql.parameters [ "tlid", Sql.tlid db.tlid
                      "accountID", Sql.uuid state.program.accountID
                      "canvasID", Sql.uuid state.program.canvasID
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion ]
  |> Sql.executeAsync (fun read -> read.string "key")

let count (state : RT.ExecutionState) (db : RT.DB.T) : Task<int> =
  Sql.query
    "SELECT COUNT(*)
     FROM user_data
     WHERE table_tlid = @tlid
       AND account_id = @accountID
       AND canvas_id = @canvasID
       AND user_version = @userVersion
       AND dark_version = @darkVersion"
  |> Sql.parameters [ "tlid", Sql.tlid db.tlid
                      "accountID", Sql.uuid state.program.accountID
                      "canvasID", Sql.uuid state.program.canvasID
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion ]
  |> Sql.executeRowAsync (fun read -> read.int "count")

let delete (state : RT.ExecutionState) (db : RT.DB.T) (key : string) : Task<unit> =
  Sql.query
    "DELETE
     FROM user_data
     WHERE key = @key
       AND table_tlid = @tlid
       AND account_id = @accountID
       AND canvas_id = @canvasID
       AND user_version = @userVersion
       AND dark_version = @darkVersion"
  |> Sql.parameters [ "key", Sql.string key
                      "tlid", Sql.tlid db.tlid
                      "accountID", Sql.uuid state.program.accountID
                      "canvasID", Sql.uuid state.program.canvasID
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion ]
  |> Sql.executeStatementAsync

let deleteAll (state : RT.ExecutionState) (db : RT.DB.T) : Task<unit> =
  //   covered by idx_user_data_current_data_for_tlid
  Sql.query
    "DELETE FROM user_data
     WHERE account_id = @accountID
       AND canvas_id = @canvasID
       AND table_tlid = @tlid
       AND user_version = @userVersion
       AND dark_version = @darkVersion"
  |> Sql.parameters [ "tlid", Sql.tlid db.tlid
                      "accountID", Sql.uuid state.program.accountID
                      "canvasID", Sql.uuid state.program.canvasID
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion ]
  |> Sql.executeStatementAsync

// -------------------------
// stats/locked/unlocked (not _locking_)
// -------------------------
let statsPluck
  (canvasID : CanvasID)
  (ownerID : UserID)
  (db : RT.DB.T)
  : Task<Option<RT.Dval * string>> =
  Sql.query
    "SELECT data, key
     FROM user_data
     WHERE table_tlid = @tlid
       AND account_id = @accountID
       AND canvas_id = @canvasID
       AND user_version = @userVersion
       AND dark_version = @darkVersion
     ORDER BY created_at DESC
     LIMIT 1"
  |> Sql.parameters [ "tlid", Sql.tlid db.tlid
                      "accountID", Sql.uuid ownerID
                      "canvasID", Sql.uuid canvasID
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion ]
  |> Sql.executeRowOptionAsync (fun read ->
    toObj db (read.string "data"), read.string "key")

let statsCount (canvasID : CanvasID) (ownerID : UserID) (db : RT.DB.T) : Task<int> =
  Sql.query
    "SELECT COUNT(*)
     FROM user_data
     WHERE table_tlid = @tlid
       AND account_id = @accountID
       AND canvas_id = @canvasID
       AND user_version = @userVersion
       AND dark_version = @darkVersion"
  |> Sql.parameters [ "tlid", Sql.tlid db.tlid
                      "accountID", Sql.uuid ownerID
                      "canvasID", Sql.uuid canvasID
                      "userVersion", Sql.int db.version
                      "darkVersion", Sql.int currentDarkVersion ]
  |> Sql.executeRowAsync (fun read -> read.int "count")

// Given a [canvasID] and an [accountID], return tlids for all unlocked databases -
// a database is unlocked if it has no records, and thus its schema can be
// changed without a migration.
//
// [ownerID] is needed here because we'll use it in the DB JOIN; we could
// pass in a whole canvas and get [canvasID] and [accountID] from that, but
// that would require loading the canvas, which is undesirable for performance
// reasons
let unlocked (ownerID : UserID) (canvasID : CanvasID) : Task<List<tlid>> =
  // this will need to be fixed when we allow migrations
  // Note: tl.module IS NULL means it's a db; anything else will be
  // HTTP/REPL/CRON/WORKER or a legacy space
  // NOTE: the line `AND tl.account_id = ud.account_id` seems redunant, but
  // it's required to hit the index
  Sql.query
    "SELECT tl.tlid
     FROM toplevel_oplists as tl
     LEFT JOIN user_data as ud
            ON tl.tlid = ud.table_tlid
           AND tl.canvas_id = ud.canvas_id
           AND tl.account_id = ud.account_id
     WHERE tl.canvas_id = @canvasID
       AND tl.account_id = @accountID
       AND tl.module IS NULL
       AND tl.deleted = false
       AND ud.table_tlid IS NULL
     GROUP BY tl.tlid"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "accountID", Sql.uuid ownerID ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")


// -------------------------
// DB schema
// -------------------------

let create (tlid : tlid) (name : string) (pos : pos) : PT.DB.T =
  { tlid = tlid; pos = pos; name = name; nameID = tlid; cols = []; version = 0 }


let create2 (tlid : tlid) (name : string) (pos : pos) (nameID : id) : PT.DB.T =
  { tlid = tlid; name = name; nameID = nameID; pos = pos; cols = []; version = 0 }

let renameDB (n : string) (db : PT.DB.T) : PT.DB.T = { db with name = n }

let addCol colid typeid (db : PT.DB.T) : PT.DB.T =
  { db with
      cols = db.cols @ [ { name = ""; typ = None; nameID = colid; typeID = typeid } ] }

let setColName id name (db : PT.DB.T) : PT.DB.T =
  let set (col : PT.DB.Col) =
    if col.nameID = id then { col with name = name } else col

  { db with cols = List.map set db.cols }

let setColType (id : id) (typ : PT.DType) (db : PT.DB.T) =
  let set (col : PT.DB.Col) =
    if col.typeID = id then { col with typ = Some typ } else col

  { db with cols = List.map set db.cols }

let deleteCol id (db : PT.DB.T) =
  { db with
      cols = List.filter (fun col -> col.nameID <> id && col.typeID <> id) db.cols }


// let create_migration rbid rfid cols (db : PT.DB.T) =
//   match db.active_migration with
//   | Some migration ->
//       db
//   | None ->
//       let max_version =
//         db.old_migrations
//         |> List.map ~f:(fun m -> m.version)
//         |> List.fold_left ~init:0 ~f:max
//       in
//       { db with
//         active_migration =
//           Some
//             { starting_version = db.version
//             ; version = max_version + 1
//             ; cols
//             ; state = DBMigrationInitialized
//             ; rollback = Libshared.FluidExpression.EBlank rbid
//             ; rollforward = Libshared.FluidExpression.EBlank rfid } }
//
//
// let add_col_to_migration nameid typeid (db : PT.DB.T) =
//   match db.active_migration with
//   | None ->
//       db
//   | Some migration ->
//       let mutated_migration =
//         {migration with cols = migration.cols @ [(Blank nameid, Blank typeid)]}
//       in
//       {db with active_migration = Some mutated_migration}
//
//
// let set_col_name_in_migration id name (db : PT.DB.T) =
//   match db.active_migration with
//   | None ->
//       db
//   | Some migration ->
//       let set col =
//         match col with
//         | Blank hid, tipe when hid = id ->
//             (Filled (hid, name), tipe)
//         | Filled (nameid, oldname), tipe when nameid = id ->
//             (Filled (nameid, name), tipe)
//         | _ ->
//             col
//       in
//       let newcols = List.map ~f:set migration.cols in
//       let mutated_migration = {migration with cols = newcols} in
//       {db with active_migration = Some mutated_migration}
//
//
// let set_col_type_in_migration id tipe (db : RT.DB.T) =
//   match db.active_migration with
//   | None ->
//       db
//   | Some migration ->
//       let set col =
//         match col with
//         | name, Blank blankid when blankid = id ->
//             (name, Filled (blankid, tipe))
//         | name, Filled (tipeid, oldtipe) when tipeid = id ->
//             (name, Filled (tipeid, tipe))
//         | _ ->
//             col
//       in
//       let newcols = List.map ~f:set migration.cols in
//       let mutated_migration = {migration with cols = newcols} in
//       {db with active_migration = Some mutated_migration}
//
//
// let abandon_migration (db : RT.DB.T) =
//   match db.active_migration with
//   | None ->
//       db
//   | Some migration ->
//       let mutated_migration = {migration with state = DBMigrationAbandoned} in
//       let db2 =
//         {db with old_migrations = db.old_migrations @ [mutated_migration]}
//       in
//       {db2 with active_migration = None}
//
//
// let delete_col_in_migration id (db : PT.DB.T) =
//   match db.active_migration with
//   | None ->
//       db
//   | Some migration ->
//       let newcols =
//         List.filter migration.cols ~f:(fun col ->
//             match col with
//             | Blank nid, _ when nid = id ->
//                 false
//             | Filled (nid, _), _ when nid = id ->
//                 false
//             | _ ->
//                 true)
//       in
//       let mutated_migration = {migration with cols = newcols} in
//       {db with active_migration = Some mutated_migration}
//

let placeholder = 0
