/// Builtin functions for accessing and manipulating user datastores
module BuiltinCloudExecution.Libs.DB

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval
module TypeChecker = LibExecution.TypeChecker

module UserDB = LibCloud.UserDB
module Db = LibCloud.Db


let varA = TVariable "a"

let dbType = TDB varA

let valType = varA
let valParam = Param.make "val" valType ""
let keyParam = Param.make "key" TString ""
let keysParam = Param.make "keys" (TList TString) ""
let tableParam = Param.make "table" dbType ""
let queryParam =
  Param.makeWithArgs "filter" (TFn(NEList.singleton varA, TBool)) "" [ "value" ]

let handleUnexpectedExceptionDuringQuery
  (state : ExecutionState)
  (dbname : string)
  (query : LambdaImpl)
  (e : System.Exception)
  : Dval =
  match e with
  | RuntimeErrorException _ -> Exception.reraise e
  | e ->
    state.reportException
      state
      [ "dbName", dbname; "lambda", query; "db", state.program.dbs[dbname] ]
      e
    LibCloud.SqlCompiler.error "An error occurred while querying the Datastore"

let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let fn = fn [ "DB" ]

let fns : List<BuiltInFn> =
  [ { name = fn "set" 0
      typeParams = []
      parameters = [ valParam; keyParam; tableParam ]
      returnType = valType
      description =
        "Upsert <param val> into <param table>, accessible by <param key>"
      fn =
        (function
        | state, _, [ value; DString key; DDB dbname ] ->
          uply {
            let db = state.program.dbs[dbname]

            let! id = UserDB.set state true db key value

            match id with
            | Ok _id -> return value
            | Error rte -> return raiseRTE None rte
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "get" 0
      typeParams = []
      parameters = [ keyParam; tableParam ]
      returnType = TypeReference.option valType
      description = "Finds a value in <param table> by <param key>"
      fn =
        (function
        | state, _, [ DString key; DDB dbname ] ->
          uply {
            let db = state.program.dbs[dbname]
            let! result = UserDB.getOption state db key
            return TypeChecker.DvalCreator.option VT.unknownDbTODO result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "getMany" 0
      typeParams = []
      parameters = [ keysParam; tableParam ]
      returnType = TypeReference.option (TList valType)
      description =
        "Finds many values in <param table> by <param keys>. If all <param keys> are found, returns Some a list of [values], otherwise returns None (to ignore missing keys, use DB.etExisting)"
      fn =
        let valueType = VT.unknownDbTODO
        let optType = KTList valueType
        (function
        | state, _, [ DList(_, keys); DDB dbname ] ->
          uply {
            let db = state.program.dbs[dbname]

            let! items =
              keys
              |> List.map (function
                | DString s -> s
                | dv -> Exception.raiseInternal "keys aren't strings" [ "key", dv ])
              |> UserDB.getMany state db

            if List.length items = List.length keys then
              return
                items
                |> TypeChecker.DvalCreator.list valueType
                |> Dval.optionSome optType
            else
              return Dval.optionNone optType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "getExisting" 0
      typeParams = []
      parameters = [ keysParam; tableParam ]
      returnType = TList valType
      description =
        "Finds many values in <param table> by <param keys> (ignoring any missing items), returning a {{ [value] }} list of values"
      fn =
        (function
        | state, _, [ DList(_, keys); DDB dbname ] ->
          uply {
            let db = state.program.dbs[dbname]

            let! result =
              keys
              |> List.map (function
                | DString s -> s
                | dv -> Exception.raiseInternal "keys aren't strings" [ "key", dv ])
              |> UserDB.getMany state db
            return result |> TypeChecker.DvalCreator.list VT.unknownDbTODO
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "getManyWithKeys" 0
      typeParams = []
      parameters = [ keysParam; tableParam ]
      returnType = TDict valType
      description =
        "Finds many values in <param table> by <param keys>, returning a {{ {key:{value}, key2: {value2} } }} object of keys and values"
      fn =
        (function
        | state, _, [ DList(_, keys); DDB dbname ] ->
          uply {
            let db = state.program.dbs[dbname]
            let! result =
              keys
              |> List.map (function
                | DString s -> s
                | dv -> Exception.raiseInternal "keys aren't strings" [ "key", dv ])
              |> UserDB.getManyWithKeys state db
            return TypeChecker.DvalCreator.dict VT.unknownDbTODO result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "delete" 0
      typeParams = []
      parameters = [ keyParam; tableParam ]
      returnType = TUnit
      description = "Delete <param key> from <param table>"
      fn =
        (function
        | state, _, [ DString key; DDB dbname ] ->
          uply {
            let db = state.program.dbs[dbname]
            do! UserDB.delete state db key
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "deleteAll" 0
      typeParams = []
      parameters = [ tableParam ]
      returnType = TUnit
      description = "Delete everything from <param table>"
      fn =
        (function
        | state, _, [ DDB dbname ] ->
          uply {
            let db = state.program.dbs[dbname]
            do! UserDB.deleteAll state db
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "getAll" 0
      typeParams = []
      parameters = [ tableParam ]
      returnType = TList valType
      description = "Fetch all the values in <param table>"
      fn =
        (function
        | state, _, [ DDB dbname ] ->
          uply {
            let db = state.program.dbs[dbname]
            let! results = UserDB.getAll state db
            return
              results
              |> List.map snd
              |> TypeChecker.DvalCreator.list VT.unknownDbTODO
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "getAllWithKeys" 0
      typeParams = []
      parameters = [ tableParam ]
      returnType = TDict valType
      description =
        "Fetch all the values in <param table>. Returns an object with key: value. ie. {key : value, key2: value2}"
      fn =
        (function
        | state, _, [ DDB dbname ] ->
          uply {
            let db = state.program.dbs[dbname]
            let! result = UserDB.getAll state db
            return TypeChecker.DvalCreator.dict VT.unknownDbTODO result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "count" 0
      typeParams = []
      parameters = [ tableParam ]
      returnType = TInt
      description = "Return the number of items stored in <param table>"
      fn =
        (function
        | state, _, [ DDB dbname ] ->
          uply {
            let db = state.program.dbs[dbname]
            let! (count : int) = UserDB.count state db
            return count |> int64 |> DInt
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "generateKey" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Returns a random key suitable for use as a DB key"
      fn =
        (function
        | _, _, [ DUnit ] -> System.Guid.NewGuid() |> string |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "keys" 0
      typeParams = []
      parameters = [ tableParam ]
      returnType = TList TString
      description =
        "Fetch all the keys of entries in <param table>. Returns an list with strings"
      fn =
        (function
        | state, _, [ DDB dbname ] ->
          uply {
            let db = state.program.dbs[dbname]
            let! results = UserDB.getAllKeys state db
            return results |> List.map DString |> Dval.list KTString
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "query" 0
      typeParams = []
      parameters = [ tableParam; queryParam ]
      returnType = TList valType
      description =
        "Fetch all the values from <param table> for which filter returns true. Note that this does not check every value in <param table>, but rather is optimized to find data with indexes. Errors at compile-time if Dark's compiler does not support the code in question."
      fn =
        (function
        | state, _, [ DDB dbname; DFnVal(Lambda b) ] ->
          uply {
            try
              let db = state.program.dbs[dbname]
              let! results = UserDB.queryValues state db b
              match results with
              | Ok results ->
                return results |> TypeChecker.DvalCreator.list VT.unknownDbTODO
              | Error rte -> return raiseUntargetedRTE rte
            with e ->
              return handleUnexpectedExceptionDuringQuery state dbname b e
          }
        | _ -> incorrectArgs ())
      sqlSpec = QueryFunction
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "queryWithKey" 0
      typeParams = []
      parameters = [ tableParam; queryParam ]
      returnType = TDict valType
      description =
        "Fetch all the values from <param table> for which filter returns true, returning {key : value} as an dict. Note that this does not check every value in <param table>, but rather is optimized to find data with indexes. Errors at compile-time if Dark's compiler does not support the code in question."
      fn =
        (function
        | state, _, [ DDB dbname; DFnVal(Lambda b) ] ->
          uply {
            try
              let db = state.program.dbs[dbname]
              let! results = UserDB.query state db b
              match results with
              | Ok results ->
                return TypeChecker.DvalCreator.dict VT.unknownDbTODO results
              | Error rte -> return raiseUntargetedRTE rte
            with e ->
              return handleUnexpectedExceptionDuringQuery state dbname b e
          }
        | _ -> incorrectArgs ())
      sqlSpec = QueryFunction
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "queryOne" 0
      typeParams = []
      parameters = [ tableParam; queryParam ]
      returnType = TypeReference.option valType
      description =
        "Fetch exactly one value from <param table> for which filter returns true. Note that this does not check every value in <param table>, but rather is optimized to find data with indexes.  If there is exactly one value, it returns Some value and if there is none or more than 1 found, it returns None. Errors at compile-time if Dark's compiler does not support the code in question."
      fn =
        let optType = VT.unknownDbTODO
        (function
        | state, _, [ DDB dbname; DFnVal(Lambda b) ] ->
          uply {
            try
              let db = state.program.dbs[dbname]
              let! results = UserDB.query state db b

              match results with
              | Ok [ (_, v) ] -> return TypeChecker.DvalCreator.optionSome optType v
              | Ok _ -> return TypeChecker.DvalCreator.optionNone optType
              | Error rte -> return raiseUntargetedRTE rte
            with e ->
              return handleUnexpectedExceptionDuringQuery state dbname b e
          }
        | _ -> incorrectArgs ())
      sqlSpec = QueryFunction
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "queryOneWithKey" 0
      typeParams = []
      parameters = [ tableParam; queryParam ]
      returnType = TypeReference.option (TTuple(TString, valType, []))
      description =
        "Fetch exactly one value from <param table> for which filter returns true. Note that this does not check every value in <param table>, but rather is optimized to find data with indexes. If there is exactly one key/value pair, it returns Some {key: value} and if there is none or more than 1 found, it returns None. Errors at compile-time if Dark's compiler does not support the code in question."
      fn =
        let optType = VT.tuple VT.string VT.unknownDbTODO []
        (function
        | state, _, [ DDB dbname; DFnVal(Lambda b) ] ->
          uply {
            try
              let db = state.program.dbs[dbname]
              let! results = UserDB.query state db b

              match results with
              | Ok [ (key, dv) ] ->
                return
                  TypeChecker.DvalCreator.optionSome
                    optType
                    (DTuple(DString key, dv, []))
              | Ok _ -> return TypeChecker.DvalCreator.optionNone optType
              | Error rte -> return raiseUntargetedRTE rte
            with e ->
              return handleUnexpectedExceptionDuringQuery state dbname b e
          }
        | _ -> incorrectArgs ())
      sqlSpec = QueryFunction
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "queryCount" 0
      typeParams = []
      parameters = [ tableParam; queryParam ]
      returnType = TInt
      description =
        "Return the number of items from <param table> for which filter returns true. Note that this does not check every value in <param table>, but rather is optimized to find data with indexes. Errors at compile-time if Dark's compiler does not support the code in question."
      fn =
        (function
        | state, _, [ DDB dbname; DFnVal(Lambda b) ] ->
          uply {
            try
              let db = state.program.dbs[dbname]
              let! result = UserDB.queryCount state db b
              match result with
              | Ok result -> return Dval.int result
              | Error rte -> return raiseUntargetedRTE rte
            with e ->
              return handleUnexpectedExceptionDuringQuery state dbname b e
          }
        | _ -> incorrectArgs ())
      sqlSpec = QueryFunction
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
