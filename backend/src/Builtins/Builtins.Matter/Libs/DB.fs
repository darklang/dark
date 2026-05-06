/// Builtin functions for accessing and manipulating user datastores
module Builtins.Matter.Libs.DB

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open Fumble
open LibDB.Sqlite

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module TypeChecker = LibExecution.TypeChecker
module Builtin = LibExecution.Builtin
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module NR = LibExecution.RuntimeTypes.NameResolution

module UserDB = LibDB.UserDB
module Db = LibDB.Sqlite
module RTQueryCompiler = LibExecution.RTQueryCompiler
module Toplevels = LibCloud.Toplevels
module Serialize = LibCloud.Serialize
module PackageLocation = LibDB.PackageLocation

let tvar v = TVariable v

let dbType v = TDB(tvar v)

let valParam v = Param.make "val" (tvar v) ""
let keyParam = Param.make "key" TString ""
let keysParam = Param.make "keys" (TList TString) ""

let tableParam v = Param.make "table" (dbType v) ""

/// A function param that goes from `TVariable v` to `TBool`, to be used as a filter
let queryFilterParam v =
  Param.makeWithArgs
    "filter"
    (TFn(NEList.singleton (TVariable v), TBool))
    ""
    [ "value" ]


/// Collect all LoadValue instructions from lambda instructions and resolve them
let resolveLoadValues
  (exeState : ExecutionState)
  (lambdaImpl : LambdaImpl)
  : Ply.Ply<Map<FQValueName.FQValueName, Dval>> =
  uply {
    // Collect all value references from LoadValue instructions
    let valueRefs =
      lambdaImpl.instructions.instructions
      |> List.choose (function
        | Instruction.LoadValue(_, valueName) -> Some valueName
        | _ -> None)
      |> List.distinct

    // Resolve each value
    let! resolved =
      valueRefs
      |> Ply.List.mapSequentially (fun valueName ->
        uply {
          match valueName with
          | FQValueName.Builtin builtinName ->
            // Builtin values - look up in builtIn values
            match Map.tryFind builtinName exeState.values.builtIn with
            | Some v -> return Some(valueName, v.body)
            | None -> return None
          | FQValueName.Package pkgId ->
            let! pkg = exeState.values.package pkgId
            match pkg with
            | Some v -> return Some(valueName, v.body)
            | None -> return None
        })

    return resolved |> List.choose (fun x -> x) |> Map.ofList
  }


/// Look up a lambda from the execution cache by its exprId
let lookupLambdaImpl (exeState : ExecutionState) (exprId : id) : LambdaImpl =
  match exeState.lambdaInstrCache.TryGetValue exprId with
  | true, impl -> impl
  | false, _ ->
    Exception.raiseInternal "Lambda not found in cache" [ "exprId", exprId ]


/// Compile a lambda to SQL for use in DB queries
/// Raises RuntimeErrorException on error
let compileQueryLambda
  (exeState : ExecutionState)
  (appLambda : ApplicableLambda)
  : Ply.Ply<LibExecution.RTQueryCompiler.CompiledQuery> =
  uply {
    let lambdaImpl = lookupLambdaImpl exeState appLambda.exprId
    let! resolvedValues = resolveLoadValues exeState lambdaImpl

    match
      RTQueryCompiler.compileLambda
        exeState
        lambdaImpl
        appLambda.closedRegisters
        resolvedValues
    with
    | Error err ->
      let fullMessage = RTQueryCompiler.errorTemplate + err
      return raiseUntargetedRTE (RuntimeError.Error.SqlCompiler fullMessage)
    | Ok compiled -> return compiled
  }


let fns () : List<BuiltInFn> =
  [ { name = fn "dbSet" 0
      typeParams = []
      parameters = [ valParam "a"; keyParam; tableParam "a" ]
      returnType = tvar "a"
      description =
        "Upsert <param val> into <param table>, accessible by <param key>"
      fn =
        (function
        | exeState, vm, _, [ value; DString key; DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]

            let! id = UserDB.set exeState vm.threadID true db key value

            match id with
            | Ok _id -> return value
            | Error rte -> return raiseUntargetedRTE rte
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "dbGet" 0
      typeParams = []
      parameters = [ keyParam; tableParam "a" ]
      returnType = TypeReference.option (tvar "a")
      description = "Finds a value in <param table> by <param key>"
      fn =
        (function
        | exeState, vm, _, [ DString key; DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            let! result = UserDB.getOption exeState vm.threadID db key
            return TypeChecker.DvalCreator.option vm.threadID VT.unknownDbTODO result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "dbGetMany" 0
      typeParams = []
      parameters = [ keysParam; tableParam "a" ]
      returnType = TypeReference.option (TList(tvar "a"))
      description =
        "Finds many values in <param table> by <param keys>. If all <param keys> are found, returns Some a list of [values], otherwise returns None (to ignore missing keys, use DB.etExisting)"
      fn =
        let valueType = VT.unknownDbTODO
        let optType = KTList valueType
        (function
        | exeState, vm, _, [ DList(_, keys); DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]

            let tst = Map.empty // TODO idk if this is reasonable

            let! items =
              keys
              |> List.map (function
                | DString s -> s
                | dv -> Exception.raiseInternal "keys aren't strings" [ "key", dv ])
              |> UserDB.getMany exeState vm.threadID tst db

            if List.length items = List.length keys then
              return
                items
                |> TypeChecker.DvalCreator.list vm.threadID valueType
                |> Dval.optionSome optType
            else
              return Dval.optionNone optType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "dbGetExisting" 0
      typeParams = []
      parameters = [ keysParam; tableParam "a" ]
      returnType = TList(tvar "a")
      description =
        "Finds many values in <param table> by <param keys> (ignoring any missing items), returning a {{ [value] }} list of values"
      fn =
        (function
        | exeState, vm, _, [ DList(_, keys); DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]

            let tst = Map.empty // TODO idk if this is reasonable

            let! result =
              keys
              |> List.map (function
                | DString s -> s
                | dv -> Exception.raiseInternal "keys aren't strings" [ "key", dv ])
              |> UserDB.getMany exeState vm.threadID tst db
            return
              result |> TypeChecker.DvalCreator.list vm.threadID VT.unknownDbTODO
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "dbGetManyWithKeys" 0
      typeParams = []
      parameters = [ keysParam; tableParam "a" ]
      returnType = TDict(tvar "a")
      description =
        "Finds many values in <param table> by <param keys>, returning a {{ {key:{value}, key2: {value2} } }} object of keys and values"
      fn =
        (function
        | exeState, vm, _, [ DList(_, keys); DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]

            let tst = Map.empty // TODO idk if this is reasonable

            let! result =
              keys
              |> List.map (function
                | DString s -> s
                | dv -> Exception.raiseInternal "keys aren't strings" [ "key", dv ])
              |> UserDB.getManyWithKeys exeState vm.threadID tst db
            return TypeChecker.DvalCreator.dict vm.threadID VT.unknownDbTODO result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "dbDelete" 0
      typeParams = []
      parameters = [ keyParam; tableParam "a" ]
      returnType = TUnit
      description = "Delete <param key> from <param table>"
      fn =
        (function
        | exeState, _, _, [ DString key; DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            do! UserDB.delete exeState db key
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "dbDeleteAll" 0
      typeParams = []
      parameters = [ tableParam "a" ]
      returnType = TUnit
      description = "Delete everything from <param table>"
      fn =
        (function
        | exeState, _, _, [ DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            do! UserDB.deleteAll exeState db
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "dbGetAll" 0
      typeParams = []
      parameters = [ tableParam "a" ]
      returnType = TList(tvar "a")
      description = "Fetch all the values in <param table>"
      fn =
        (function
        | exeState, vm, _, [ DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            let tst = Map.empty // TODO idk if this is reasonable
            let! results = UserDB.getAll exeState vm.threadID tst db
            return
              results
              |> List.map snd
              |> TypeChecker.DvalCreator.list vm.threadID VT.unknownDbTODO
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "dbGetAllWithKeys" 0
      typeParams = []
      parameters = [ tableParam "a" ]
      returnType = TDict(tvar "a")
      description =
        "Fetch all the values in <param table>. Returns an object with key: value. ie. {key : value, key2: value2}"
      fn =
        (function
        | exeState, vm, _, [ DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            let tst = Map.empty // TODO idk if this is reasonable
            let! result = UserDB.getAll exeState vm.threadID tst db
            return TypeChecker.DvalCreator.dict vm.threadID VT.unknownDbTODO result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "dbCount" 0
      typeParams = []
      parameters = [ tableParam "a" ]
      returnType = TInt64
      description = "Return the number of items stored in <param table>"
      fn =
        (function
        | exeState, _, _, [ DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            let! (count : int) = UserDB.count exeState db
            return count |> int64 |> DInt64
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "dbGenerateKey" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Returns a random key suitable for use as a DB key"
      fn =
        (function
        | _, _, _, [ DUnit ] -> System.Guid.NewGuid() |> string |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "dbKeys" 0
      typeParams = []
      parameters = [ tableParam "a" ]
      returnType = TList TString
      description =
        "Fetch all the keys of entries in <param table>. Returns an list with strings"
      fn =
        (function
        | exeState, _, _, [ DDB dbname ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            let! results = UserDB.getAllKeys exeState db
            return results |> List.map DString |> Dval.list KTString
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "dbQuery" 0
      typeParams = []
      parameters = [ tableParam "a"; queryFilterParam "a" ]
      returnType = TList(tvar "a")
      description =
        "Fetch all the values from <param table> for which filter returns true.
        Note that this does not check every value in <param table>, but rather is optimized to find data with indexes.
        Errors at compile-time if Dark's compiler does not support the code in question."
      fn =
        (function
        | exeState, vm, _, [ DDB dbname; DApplicable(AppLambda appLambda) ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            let! compiled = compileQueryLambda exeState appLambda
            return!
              UserDB.executeCompiledQuery
                exeState
                vm
                db
                UserDB.DBQueryAll
                compiled.sql
                compiled.paramValues
          }
        | _ -> incorrectArgs ())
      sqlSpec = QueryFunction
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "dbQueryWithKey" 0
      typeParams = []
      parameters = [ tableParam "a"; queryFilterParam "a" ]
      returnType = TDict(tvar "a")
      description =
        "Fetch all the values from <param table> for which filter returns true, returning {key : value} as a dict."
      fn =
        (function
        | exeState, vm, _, [ DDB dbname; DApplicable(AppLambda appLambda) ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            let! compiled = compileQueryLambda exeState appLambda
            return!
              UserDB.executeCompiledQuery
                exeState
                vm
                db
                UserDB.DBQueryWithKey
                compiled.sql
                compiled.paramValues
          }
        | _ -> incorrectArgs ())
      sqlSpec = QueryFunction
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "dbQueryOne" 0
      typeParams = []
      parameters = [ tableParam "a"; queryFilterParam "a" ]
      returnType = TypeReference.option (tvar "a")
      description =
        "Fetch exactly one value from <param table> for which filter returns true. Returns Some if exactly one found, None otherwise."
      fn =
        (function
        | exeState, vm, _, [ DDB dbname; DApplicable(AppLambda appLambda) ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            let! compiled = compileQueryLambda exeState appLambda
            return!
              UserDB.executeCompiledQuery
                exeState
                vm
                db
                UserDB.DBQueryOne
                compiled.sql
                compiled.paramValues
          }
        | _ -> incorrectArgs ())
      sqlSpec = QueryFunction
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "dbQueryCount" 0
      typeParams = []
      parameters = [ tableParam "a"; queryFilterParam "a" ]
      returnType = TInt64
      description =
        "Return the number of items from <param table> for which filter returns true."
      fn =
        (function
        | exeState, vm, _, [ DDB dbname; DApplicable(AppLambda appLambda) ] ->
          uply {
            let db = exeState.program.dbs[dbname]
            let! compiled = compileQueryLambda exeState appLambda
            return!
              UserDB.executeCompiledQuery
                exeState
                vm
                db
                UserDB.DBQueryCount
                compiled.sql
                compiled.paramValues
          }
        | _ -> incorrectArgs ())
      sqlSpec = QueryFunction
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    // ---------------
    // DB management — create / list / drop. Operate on the implicit
    // scope (`exeState.program.scopeID`) rather than taking it explicitly.
    // ---------------

    { name = fn "dbCreate" 0
      typeParams = []
      parameters =
        [ Param.make "dbName" TString "Name of the database"
          Param.make
            "typeHash"
            (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
            "Hash of the type stored in this DB" ]
      returnType = TypeReference.result TUInt64 TString
      description = "Creates a new database"
      fn =
        (function
        | exeState, _, _, [ DString dbName; typeHashDval ] ->
          let typeHash = PT2DT.Hash.fromDT typeHashDval
          let scopeID = exeState.program.scopeID
          uply {
            let! existing =
              Sql.query
                "SELECT COUNT(*) as cnt FROM toplevels_v0
                 WHERE scope_id = @scopeID
                   AND tipe = 'db'
                   AND name = @name
                   AND deleted = 0"
              |> Sql.parameters
                [ "scopeID", Sql.uuid scopeID; "name", Sql.string dbName ]
              |> Sql.executeRowAsync (fun read -> read.int "cnt")

            if existing > 0 then
              return
                Dval.resultError
                  KTUInt64
                  KTString
                  (DString $"A database named '{dbName}' already exists")
            else
              let tlid = gid ()
              let db : PT.DB.T =
                { tlid = tlid
                  name = dbName
                  version = 0
                  typ =
                    PT.TypeReference.TCustomType(
                      { originalName = []
                        location = None
                        resolved = Ok(PT.FQTypeName.Package typeHash) },
                      []
                    ) }

              let toplevel = PT.Toplevel.TLDB db
              do! Toplevels.saveTLIDs scopeID [ (toplevel, Serialize.NotDeleted) ]
              return Dval.resultOk KTUInt64 KTString (DUInt64 tlid)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "dbListAll" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch for resolving type names" ]
      returnType = TList(TTuple(TString, TString, []))
      description = "Returns a list of (name, typeName) tuples for all DBs"
      fn =
        (function
        | exeState, _, _, [ DUuid branchId ] ->
          let scopeID = exeState.program.scopeID
          uply {
            let! app = Toplevels.loadAllDBs scopeID
            let pm = LibDB.PackageManager.pt
            let! dbs =
              app.dbs
              |> Map.values
              |> Ply.List.mapSequentially (fun (db : PT.DB.T) ->
                uply {
                  let! typeName =
                    match db.typ with
                    | PT.TypeReference.TCustomType({ resolved = Ok(PT.FQTypeName.Package typeID) },
                                                   _) ->
                      uply {
                        let! locs = pm.getTypeLocations branchId typeID
                        match locs with
                        | location :: _ -> return PackageLocation.toFQN location
                        | [] -> return typeID.ToString()
                      }
                    | _ -> Ply "unknown"
                  return DTuple(DString db.name, DString typeName, [])
                })
            return Dval.list (KTTuple(VT.string, VT.string, [])) dbs
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "dbDrop" 0
      typeParams = []
      parameters =
        [ Param.make "dbName" TString "Name of the database to drop" ]
      returnType = TypeReference.result TUnit TString
      description = "Drops (deletes) all databases with the given name"
      fn =
        (function
        | exeState, _, _, [ DString dbName ] ->
          let scopeID = exeState.program.scopeID
          uply {
            let! matchingTlids =
              Sql.query
                "SELECT tlid FROM toplevels_v0
                 WHERE scope_id = @scopeID
                   AND tipe = 'db'
                   AND name = @name
                   AND deleted = 0"
              |> Sql.parameters
                [ "scopeID", Sql.uuid scopeID; "name", Sql.string dbName ]
              |> Sql.executeAsync (fun read -> read.tlid "tlid")

            match matchingTlids with
            | [] ->
              return
                Dval.resultError
                  KTUnit
                  KTString
                  (DString $"Database not found: {dbName}")
            | _ ->
              do!
                matchingTlids
                |> Task.iterInParallel (fun tlid ->
                  Toplevels.deleteToplevelForever scopeID tlid)
              do!
                matchingTlids
                |> Task.iterInParallel (fun tlid ->
                  Sql.query
                    "DELETE FROM user_data_v0
                     WHERE scope_id = @scopeID AND table_tlid = @tlid"
                  |> Sql.parameters
                    [ "scopeID", Sql.uuid scopeID; "tlid", Sql.id tlid ]
                  |> Sql.executeStatementAsync)
              return Dval.resultOk KTUnit KTString DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    // ---------------
    // Infra: SQLite-level introspection. Reports per-table row counts +
    // approximate disk-byte share. Useful for "why is data.db big" forensics.
    // ---------------

    { name = fn "infraTableStats" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList(TTuple(TString, TInt64, [ TInt64 ]))
      description =
        "Returns (tableName, rowCount, approxDiskBytes) tuples for every
         non-internal table. diskBytes is approximate (page-count * page-size
         apportioned by row share)."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let! rows = LibDB.Sqlite.tableStats ()
            return
              rows
              |> List.map (fun r ->
                DTuple(DString r.relation, DInt64 r.rows, [ DInt64 r.diskBytes ]))
              |> Dval.list (KTTuple(VT.string, VT.int64, [ VT.int64 ]))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any } ]

let builtins () = Builtin.make [] (fns ())
