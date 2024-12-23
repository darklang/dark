/// Builtin functions for accessing and manipulating user datastores
module BuiltinCloudExecution.Libs.DB

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module TypeChecker = LibExecution.TypeChecker
module Builtin = LibExecution.Builtin

module UserDB = LibCloud.UserDB
module Db = LibCloud.Db


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

// let handleUnexpectedExceptionDuringQuery
//   (exeState : ExecutionState)
//   (dbname : string)
//   (query : LambdaImpl)
//   (e : System.Exception)
//   : Dval =
//   match e with
//   | RuntimeErrorException _ -> Exception.reraise e
//   | e ->
//     exeState.reportException
//       exeState
//       [ "dbName", dbname; "lambda", query; "db", exeState.program.dbs[dbname] ]
//       e
//     LibCloud.SqlCompiler.error "An error occurred while querying the Datastore"

let fns : List<BuiltInFn> =
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
      deprecated = NotDeprecated }


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
      deprecated = NotDeprecated }


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
      deprecated = NotDeprecated }


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
      deprecated = NotDeprecated }


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
      deprecated = NotDeprecated }


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
      deprecated = NotDeprecated }


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
      deprecated = NotDeprecated }


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
      deprecated = NotDeprecated }


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
      deprecated = NotDeprecated }


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
      deprecated = NotDeprecated }


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
      deprecated = NotDeprecated }


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
      deprecated = NotDeprecated }


    // { name = fn "dbQuery" 0
    //   typeParams = []
    //   parameters = [ tableParam "a"; queryFilterParam "a" ]
    //   returnType = TList(tvar "a")
    //   description =
    //     "Fetch all the values from <param table> for which filter returns true.
    //     Note that this does not check every value in <param table>, but rather is optimized to find data with indexes.
    //     Errors at compile-time if Dark's compiler does not support the code in question."
    //   fn =
    //     (function
    //     | exeState, _, [ DDB dbname; DFnVal(Lambda b) ] ->
    //       uply {
    //         try
    //           let db = exeState.program.dbs[dbname]
    //           let! results = UserDB.queryValues exeState db b
    //           match results with
    //           | Ok results ->
    //             return
    //               results
    //               |> TypeChecker.DvalCreator.list
    //                 exeState.tracing.callStack
    //                 VT.unknownDbTODO
    //           | Error rte -> return raiseUntargetedRTE rte
    //         with e ->
    //           return handleUnexpectedExceptionDuringQuery exeState dbname b e
    //       }
    //     | _ -> incorrectArgs ())
    //   sqlSpec = QueryFunction
    //   previewable = Impure
    //   deprecated = NotDeprecated }


    // { name = fn "dbQueryWithKey" 0
    //   typeParams = []
    //   parameters = [ tableParam "a"; queryFilterParam "a" ]
    //   returnType = TDict(tvar "a")
    //   description =
    //     "Fetch all the values from <param table> for which filter returns true, returning {key : value} as an dict. Note that this does not check every value in <param table>, but rather is optimized to find data with indexes. Errors at compile-time if Dark's compiler does not support the code in question."
    //   fn =
    //     (function
    //     | exeState, _, [ DDB dbname; DFnVal(Lambda b) ] ->
    //       uply {
    //         try
    //           let db = exeState.program.dbs[dbname]
    //           let! results = UserDB.query exeState db b
    //           match results with
    //           | Ok results ->
    //             return TypeChecker.DvalCreator.dict VT.unknownDbTODO results
    //           | Error rte -> return raiseUntargetedRTE rte
    //         with e ->
    //           return handleUnexpectedExceptionDuringQuery exeState dbname b e
    //       }
    //     | _ -> incorrectArgs ())
    //   sqlSpec = QueryFunction
    //   previewable = Impure
    //   deprecated = NotDeprecated }


    // { name = fn "dbQueryOne" 0
    //   typeParams = []
    //   parameters = [ tableParam "a"; queryFilterParam "a" ]
    //   returnType = TypeReference.option (tvar "a")
    //   description =
    //     "Fetch exactly one value from <param table> for which filter returns true. Note that this does not check every value in <param table>, but rather is optimized to find data with indexes.  If there is exactly one value, it returns Some value and if there is none or more than 1 found, it returns None. Errors at compile-time if Dark's compiler does not support the code in question."
    //   fn =
    //     let optType = VT.unknownDbTODO
    //     (function
    //     | exeState, _, [ DDB dbname; DFnVal(Lambda b) ] ->
    //       uply {
    //         try
    //           let db = exeState.program.dbs[dbname]
    //           let! results = UserDB.query exeState db b

    //           match results with
    //           | Ok [ (_, v) ] ->
    //             return
    //               TypeChecker.DvalCreator.optionSome
    //                 exeState.tracing.callStack
    //                 optType
    //                 v
    //           | Ok _ -> return TypeChecker.DvalCreator.optionNone optType
    //           | Error rte -> return raiseUntargetedRTE rte
    //         with e ->
    //           return handleUnexpectedExceptionDuringQuery exeState dbname b e
    //       }
    //     | _ -> incorrectArgs ())
    //   sqlSpec = QueryFunction
    //   previewable = Impure
    //   deprecated = NotDeprecated }


    // { name = fn "dbQueryOneWithKey" 0
    //   typeParams = []
    //   parameters = [ tableParam "a"; queryFilterParam "a" ]
    //   returnType = TypeReference.option (TTuple(TString, tvar "a", []))
    //   description =
    //     "Fetch exactly one value from <param table> for which filter returns true. Note that this does not check every value in <param table>, but rather is optimized to find data with indexes. If there is exactly one key/value pair, it returns Some {key: value} and if there is none or more than 1 found, it returns None. Errors at compile-time if Dark's compiler does not support the code in question."
    //   fn =
    //     let optType = VT.tuple VT.string VT.unknownDbTODO []
    //     (function
    //     | exeState, _, [ DDB dbname; DFnVal(Lambda b) ] ->
    //       uply {
    //         try
    //           let db = exeState.program.dbs[dbname]
    //           let! results = UserDB.query exeState db b

    //           match results with
    //           | Ok [ (key, dv) ] ->
    //             return
    //               TypeChecker.DvalCreator.optionSome
    //                 exeState.tracing.callStack
    //                 optType
    //                 (DTuple(DString key, dv, []))
    //           | Ok _ -> return TypeChecker.DvalCreator.optionNone optType
    //           | Error rte -> return raiseUntargetedRTE rte
    //         with e ->
    //           return handleUnexpectedExceptionDuringQuery exeState dbname b e
    //       }
    //     | _ -> incorrectArgs ())
    //   sqlSpec = QueryFunction
    //   previewable = Impure
    //   deprecated = NotDeprecated }


    // { name = fn "dbQueryCount" 0
    //   typeParams = []
    //   parameters = [ tableParam "a"; queryFilterParam "a" ]
    //   returnType = TInt64
    //   description =
    //     "Return the number of items from <param table> for which filter returns true. Note that this does not check every value in <param table>, but rather is optimized to find data with indexes. Errors at compile-time if Dark's compiler does not support the code in question."
    //   fn =
    //     (function
    //     | exeState, _, [ DDB dbname; DFnVal(Lambda b) ] ->
    //       uply {
    //         try
    //           let db = exeState.program.dbs[dbname]
    //           let! result = UserDB.queryCount exeState db b
    //           match result with
    //           | Ok result -> return Dval.int64 result
    //           | Error rte -> return raiseUntargetedRTE rte
    //         with e ->
    //           return handleUnexpectedExceptionDuringQuery exeState dbname b e
    //       }
    //     | _ -> incorrectArgs ())
    //   sqlSpec = QueryFunction
    //   previewable = Impure
    //   deprecated = NotDeprecated }
    ]

let builtins = Builtin.make [] fns
