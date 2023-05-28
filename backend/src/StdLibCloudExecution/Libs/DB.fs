/// StdLib functions for accessing and manipulating user datastores
module StdLibCloudExecution.Libs.DB

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

module Errors = LibExecution.Errors

module UserDB = LibBackend.UserDB

module Db = LibBackend.Db


let varA = TVariable "a"

let dbType = TDB varA

let valType = varA
let valParam = Param.make "val" valType ""
let keyParam = Param.make "key" TString ""
let keysParam = Param.make "keys" (TList TString) ""
let tableParam = Param.make "table" dbType ""
let queryParam = Param.makeWithArgs "filter" (TFn([ varA ], TBool)) "" [ "value" ]

let handleUnexpectedExceptionDuringQuery
  (state : ExecutionState)
  (dbname : string)
  (query : LambdaImpl)
  (e : System.Exception)
  : Dval =
  match e with
  | :? CodeException -> e.Reraise()
  | e ->
    state.reportException
      state
      [ "dbName", dbname; "lambda", query; "db", state.program.dbs[dbname] ]
      e
    LibBackend.SqlCompiler.error "An error occurred while querying the Datastore"

let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn "DB" "set" 0
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
            let! _id = UserDB.set state true db key value
            return value
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DB" "get" 0
      typeParams = []
      parameters = [ keyParam; tableParam ]
      returnType = TOption valType
      description = "Finds a value in <param table> by <param key>"
      fn =
        (function
        | state, _, [ DString key; DDB dbname ] ->
          uply {
            let db = state.program.dbs[dbname]
            let! result = UserDB.getOption state db key
            return Dval.option result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DB" "getMany" 0
      typeParams = []
      parameters = [ keysParam; tableParam ]
      returnType = TOption(TList valType)
      description =
        "Finds many values in <param table> by <param keys>. If all <param keys> are found, returns Just a list of [values], otherwise returns Nothing (to ignore missing keys, use DB.etExisting)"
      fn =
        (function
        | state, _, [ DList keys; DDB dbname ] ->
          uply {
            let db = state.program.dbs[dbname]

            let skeys =
              List.map
                (function
                | DString s -> s
                | t -> Errors.argumentWasntType (TList TString) "keys" t)
                keys

            let! items = UserDB.getMany state db skeys

            if List.length items = List.length skeys then
              return items |> DList |> Some |> DOption
            else
              return DOption None
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DB" "getExisting" 0
      typeParams = []
      parameters = [ keysParam; tableParam ]
      returnType = TList valType
      description =
        "Finds many values in <param table> by <param keys> (ignoring any missing items), returning a {{ [value] }} list of values"
      fn =
        (function
        | state, _, [ DList keys; DDB dbname ] ->
          uply {
            let db = state.program.dbs[dbname]

            let skeys =
              List.map
                (function
                | DString s -> s
                | t -> Errors.argumentWasntType (TList TString) "keys" t)
                keys

            let! result = UserDB.getMany state db skeys
            return result |> DList
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DB" "getManyWithKeys" 0
      typeParams = []
      parameters = [ keysParam; tableParam ]
      returnType = TDict valType
      description =
        "Finds many values in <param table> by <param keys>, returning a {{ {key:{value}, key2: {value2} } }} object of keys and values"
      fn =
        (function
        | state, _, [ DList keys; DDB dbname ] ->
          uply {
            let db = state.program.dbs[dbname]

            let skeys =
              List.map
                (function
                | DString s -> s
                | t -> Errors.argumentWasntType (TList TString) "keys" t)
                keys

            let! result = UserDB.getManyWithKeys state db skeys
            return result |> Map.ofList |> DDict
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DB" "delete" 0
      typeParams = []
      parameters = [ keyParam; tableParam ]
      returnType = TUnit
      description = "Delete <param key> from <param table>"
      fn =
        (function
        | state, _, [ DString key; DDB dbname ] ->
          uply {
            let db = state.program.dbs[dbname]
            let! _result = UserDB.delete state db key
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DB" "deleteAll" 0
      typeParams = []
      parameters = [ tableParam ]
      returnType = TUnit
      description = "Delete everything from <param table>"
      fn =
        (function
        | state, _, [ DDB dbname ] ->
          uply {
            let db = state.program.dbs[dbname]
            let! _result = UserDB.deleteAll state db
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DB" "getAll" 0
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
            return results |> List.map snd |> Dval.list
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DB" "getAllWithKeys" 0
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
            return result |> Map.ofList |> DDict
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DB" "count" 0
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


    { name = fn "DB" "generateKey" 0
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


    { name = fn "DB" "keys" 0
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
            return results |> List.map (fun k -> DString k) |> DList
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DB" "query" 0
      typeParams = []
      parameters = [ tableParam; queryParam ]
      returnType = TList valType
      description =
        "Fetch all the values from <param table> for which filter returns true. Note that this does not check every value in <param table>, but rather is optimized to find data with indexes. Errors at compile-time if Dark's compiler does not support the code in question."
      fn =
        (function
        | state, _, [ DDB dbname; DFnVal (Lambda b) ] ->
          uply {
            try
              let db = state.program.dbs[dbname]
              let! results = UserDB.queryValues state db b
              return results |> Dval.list
            with
            | e -> return handleUnexpectedExceptionDuringQuery state dbname b e
          }
        | _ -> incorrectArgs ())
      sqlSpec = QueryFunction
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DB" "queryWithKey" 0
      typeParams = []
      parameters = [ tableParam; queryParam ]
      returnType = TDict valType
      description =
        "Fetch all the values from <param table> for which filter returns true, returning {key : value} as an dict. Note that this does not check every value in <param table>, but rather is optimized to find data with indexes. Errors at compile-time if Dark's compiler does not support the code in question."
      fn =
        (function
        | state, _, [ DDB dbname; DFnVal (Lambda b) ] ->
          uply {
            try
              let db = state.program.dbs[dbname]
              let! results = UserDB.query state db b
              return results |> Map.ofList |> DDict
            with
            | e -> return handleUnexpectedExceptionDuringQuery state dbname b e
          }
        | _ -> incorrectArgs ())
      sqlSpec = QueryFunction
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DB" "queryOne" 0
      typeParams = []
      parameters = [ tableParam; queryParam ]
      returnType = TOption valType
      description =
        "Fetch exactly one value from <param table> for which filter returns true. Note that this does not check every value in <param table>, but rather is optimized to find data with indexes.  If there is exactly one value, it returns Just value and if there is none or more than 1 found, it returns Nothing. Errors at compile-time if Dark's compiler does not support the code in question."
      fn =
        (function
        | state, _, [ DDB dbname; DFnVal (Lambda b) ] ->
          uply {
            try
              let db = state.program.dbs[dbname]
              let! results = UserDB.query state db b

              match results with
              | [ (_, v) ] -> return Dval.optionJust v
              | _ -> return DOption None
            with
            | e -> return handleUnexpectedExceptionDuringQuery state dbname b e
          }
        | _ -> incorrectArgs ())
      sqlSpec = QueryFunction
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DB" "queryOneWithKey" 0
      typeParams = []
      parameters = [ tableParam; queryParam ]
      returnType = TOption(TTuple(TString, valType, []))
      description =
        "Fetch exactly one value from <param table> for which filter returns true. Note that this does not check every value in <param table>, but rather is optimized to find data with indexes. If there is exactly one key/value pair, it returns Just {key: value} and if there is none or more than 1 found, it returns Nothing. Errors at compile-time if Dark's compiler does not support the code in question."
      fn =
        (function
        | state, _, [ DDB dbname; DFnVal (Lambda b) ] ->
          uply {
            try
              let db = state.program.dbs[dbname]
              let! results = UserDB.query state db b

              match results with
              | [ (key, dv) ] -> return Dval.optionJust (DTuple(DString key, dv, []))
              | _ -> return DOption None
            with
            | e -> return handleUnexpectedExceptionDuringQuery state dbname b e
          }
        | _ -> incorrectArgs ())
      sqlSpec = QueryFunction
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DB" "queryCount" 0
      typeParams = []
      parameters = [ tableParam; queryParam ]
      returnType = TInt
      description =
        "Return the number of items from <param table> for which filter returns true. Note that this does not check every value in <param table>, but rather is optimized to find data with indexes. Errors at compile-time if Dark's compiler does not support the code in question."
      fn =
        (function
        | state, _, [ DDB dbname; DFnVal (Lambda b) ] ->
          uply {
            try
              let db = state.program.dbs[dbname]
              let! result = UserDB.queryCount state db b
              return Dval.int result
            with
            | e -> return handleUnexpectedExceptionDuringQuery state dbname b e
          }
        | _ -> incorrectArgs ())
      sqlSpec = QueryFunction
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
