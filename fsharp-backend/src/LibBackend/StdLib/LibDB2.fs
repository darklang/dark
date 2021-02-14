module LibBackend.StdLib.LibDB2

open Prelude
open LibExecution.RuntimeTypes

module UserDB = LibBackend.UserDB
module Errors = LibExecution.Errors
module Db = LibBackend.Db

let fn = FQFnName.stdlibName

let err (str : string) = Value(Dval.errStr str)

let removedFunction = LibExecution.Errors.removedFunction
let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let dbType = TDB varA

let valParam = Param.make "val" varA ""
let keyParam = Param.make "key" TStr ""
let keysParam = Param.make "keys" (TList TStr) ""
let tableParam = Param.make "table" dbType ""
let specParam = Param.make "spec" TAny ""
let queryParam = Param.make "filter" (TFn([ varA ], TBool)) ""

let fns : List<BuiltInFn> =
  [ { name = fn "DB" "set" 1
      parameters = [ valParam; keyParam; tableParam ]
      returnType = varA
      description = "Upsert `val` into `table`, accessible by `key`"
      fn =
        InProcess
          (function
          | state, [ DObj value; DStr key; DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]
                let! _id = UserDB.set state true db key value
                return DObj value
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DB" "add" 0
      parameters = [ valParam; tableParam ]
      returnType = TStr
      description =
        "Add `val` as a new entry into `table`, using a newly generated key. Returns the generated key."
      fn =
        InProcess
          (function
          | state, [ DObj value; DDB dbname ] ->
              taskv {
                let key = System.Guid.NewGuid() |> toString
                let db = state.dbs.[dbname]
                let! _id = UserDB.set state true db key value
                return DStr(key)
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "DB" "set" 1) }
    { name = fn "DB" "get" 1
      parameters = [ keyParam; tableParam ]
      returnType = TOption TAny
      description = "Finds a value in `table` by `key"
      fn =
        InProcess
          (function
          | state, [ DStr key; DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]
                let! result = UserDB.getOption state db key
                return Dval.option result
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "DB" "get" 2) }
    // ; { name = fn "DB" "get" 2
//   ; parameters = [keyParam; tableParam]
//   ; returnType = TOption
//   ; description = "Finds a value in `table` by `key"
//   ; fn =
//         InProcess (function
//         | state, [DStr key; DDB dbname] -> taskv {
//             let key = Unicode_string.to_string key in
//             let db = state.dbs.[dbname]
//             UserDB.get_option state db key |> Dval.option
//           }
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotQueryable
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
    { name = fn "DB" "getMany" 1
      parameters = [ keysParam; tableParam ]
      returnType = TList TAny
      description =
        "Finds many values in `table` by `keys, returning a [[key, value]] list of lists"
      fn =
        InProcess
          (function
          | state, [ DList keys; DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]

                let skeys =
                  List.map
                    (function
                    | DStr s -> s
                    | t -> Errors.argumentWasnt "a list of strings" "keys" t)
                    keys

                let! result = UserDB.getMany state db skeys

                return
                  result |> List.map (fun (k, v) -> DList [ DStr k; v ]) |> DList
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DB" "getMany" 2
      parameters = [ keysParam; tableParam ]
      returnType = TList TAny
      description =
        "Finds many values in `table` by `keys, returning a [value] list of values"
      fn =
        InProcess
          (function
          | state, [ DList keys; DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]

                let skeys =
                  List.map
                    (function
                    | DStr s -> s
                    | t -> Errors.argumentWasnt "a list of strings" "keys" t)
                    keys

                let! result = UserDB.getMany state db skeys
                return result |> List.map (fun (_, v) -> v) |> DList
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "DB" "getMany" 3) }
    { name = fn "DB" "getMany" 3
      parameters = [ keysParam; tableParam ]
      returnType = TOption varA
      description =
        "Finds many values in `table` by `keys`. If all `keys` are found, returns Just a list of [values], otherwise returns Nothing (to ignore missing keys, use DB::getExisting)"
      fn =
        InProcess
          (function
          | state, [ DList keys; DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]

                let skeys =
                  List.map
                    (function
                    | DStr s -> s
                    | t -> Errors.argumentWasnt "a list of strings" "keys" t)
                    keys

                let! items = UserDB.getMany state db skeys

                if List.length items = List.length skeys then
                  return
                    items |> List.map (fun (_, v) -> v) |> DList |> Some |> DOption
                else
                  return DOption None
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DB" "getExisting" 0
      parameters = [ keysParam; tableParam ]
      returnType = TList TAny
      description =
        "Finds many values in `table` by `keys` (ignoring any missing items), returning a [value] list of values"
      fn =
        InProcess
          (function
          | state, [ DList keys; DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]

                let skeys =
                  List.map
                    (function
                    | DStr s -> s
                    | t -> Errors.argumentWasnt "a list of strings" "keys" t)
                    keys

                let! result = UserDB.getMany state db skeys
                return result |> List.map (fun (_, v) -> v) |> DList
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DB" "getManyWithKeys" 0
      parameters = [ keysParam; tableParam ]
      returnType = TList TAny
      description =
        "Finds many values in `table` by `keys, returning a [[key, value]] list of lists"
      fn =
        InProcess
          (function
          | state, [ DList keys; DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]

                let skeys =
                  List.map
                    (function
                    | DStr s -> s
                    | t -> Errors.argumentWasnt "a list of strings" "keys" t)
                    keys

                let! result = UserDB.getManyWithKeys state db skeys

                return
                  result |> List.map (fun (k, v) -> DList [ DStr k; v ]) |> DList
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "DB" "getManyWithKeys" 1) }
    { name = fn "DB" "getManyWithKeys" 1
      parameters = [ keysParam; tableParam ]
      returnType = TDict varA
      description =
        "Finds many values in `table` by `keys, returning a {key:{value}, key2: {value2}} object of keys and values"
      fn =
        InProcess
          (function
          | state, [ DList keys; DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]

                let skeys =
                  List.map
                    (function
                    | DStr s -> s
                    | t -> Errors.argumentWasnt "a list of strings" "keys" t)
                    keys

                let! result = UserDB.getManyWithKeys state db skeys
                return result |> Map.ofList |> DObj
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }
    // ; { name = fn "DB" "delete" 1
//   ; parameters = [keyParam; tableParam]
//   ; returnType = TNull
//   ; description = "Delete `key` from `table`"
//   ; fn =
//         InProcess (function
//         | state, [DStr key; DDB dbname] -> taskv {
//             let db = state.dbs.[dbname]
//             let key = Unicode_string.to_string key in
//             UserDB.delete state db key ;
//             DNull
//           }
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotQueryable
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "DB" "deleteAll" 1
//   ; parameters = [tableParam]
//   ; returnType = TNull
//   ; description = "Delete everything from `table`"
//   ; fn =
//         InProcess (function
//         | state, [DDB dbname] -> taskv {
//             let db = state.dbs.[dbname]
//             UserDB.delete_all state db ;
//             DNull
//           }
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotQueryable
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
    { name = fn "DB" "query" 1
      parameters = [ specParam; tableParam ]
      returnType = TList TAny // heterogenous list
      description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has, returning a [[key, value]] list of lists"
      fn =
        InProcess
          (function
          | state, [ DObj fields; DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]
                let! results = UserDB.queryExactFields state db fields

                return
                  results |> List.map (fun (k, v) -> DList [ DStr k; v ]) |> DList
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "DB" "query" 2) }
    { name = fn "DB" "query" 2

      parameters = [ specParam; tableParam ]
      returnType = TList varA
      description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has, returning a list of values"
      fn =
        InProcess
          (function
          | state, [ DObj fields; DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]
                let! results = UserDB.queryExactFields state db fields
                return results |> List.map (fun (k, v) -> v) |> Dval.list
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "DB" "query" 3) }
    { name = fn "DB" "query" 3
      parameters = [ specParam; tableParam ]
      returnType = TList varA
      description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has, returning a list of values"
      fn =
        InProcess
          (function
          | state, [ (DObj fields); DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]
                let! results = UserDB.queryExactFields state db fields
                return results |> List.map (fun (k, v) -> v) |> Dval.list
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "DB" "queryExactFields" 0) }
    // ; { name = fn "DB" "queryExactFields" 0
//   ; parameters = [Param.make "spec" TObj; tableParam]
//   ; returnType = TList TAny
//   ; description =
//       "Fetch all the values from `table` which have the same fields and values that `spec` has, returning a list of values. Previously called DB::query_v3"
//   ; fn =
//         InProcess (function
//         | state, [(DObj _ as obj); DDB dbname] -> taskv {
//             let db = state.dbs.[dbname]
//             UserDB.query_exact_fields state db obj
//             |> List.map (fun (k, v) -> v)
//             |> Dval.to_list
//           }
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotQueryable
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
    { name = fn "DB" "queryWithKey" 1
      parameters = [ specParam; tableParam ]
      returnType = TList TAny
      description = "Fetch all the values from `table` which have the same fields and values that `spec` has
          , returning a [[key, value]] list of lists"
      fn =
        InProcess
          (function
          | state, [ DObj fields; DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]

                let! result = UserDB.queryExactFields state db fields

                return
                  result |> List.map (fun (k, v) -> DList [ DStr k; v ]) |> DList
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "DB" "queryExactFieldsWithKey" 0) }
    { name = fn "DB" "queryWithKey" 2
      parameters = [ specParam; tableParam ]
      returnType = TDict varA
      description = "Fetch all the values from `table` which have the same fields and values that `spec` has
          , returning {key : value} as an object"
      fn =
        InProcess
          (function
          | state, [ DObj fields; DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]
                let! result = UserDB.queryExactFields state db fields
                return result |> Map.ofList |> DObj
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "DB" "queryExactFieldsWithKey" 0) }
    // ; { name = fn "DB" "queryExactFieldsWithKey" 0
//   ; parameters = [Param.make "spec" TObj; tableParam]
//   ; returnType = TObj
//   ; description =
//       "Fetch all the values from `table` which have the same fields and values that `spec` has
//       , returning {key : value} as an object. Previous called DB::queryWithKey_v2"
//   ; fn =
//         InProcess (function
//         | state, [(DObj _ as obj); DDB dbname] -> taskv {
//             let db = state.dbs.[dbname]
//             UserDB.query_exact_fields state db obj
//             |> DvalMap.from_list
//             |> DObj
//           }
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotQueryable
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
    { name = fn "DB" "queryOne" 1
      parameters = [ specParam; tableParam ]
      returnType = TOption varA
      description =
        "Fetch exactly one value from `table` which have the same fields and values that `spec` has. If there is exactly one value, it returns Just value and if there is none or more than 1 found, it returns Nothing"
      fn =
        InProcess
          (function
          | state, [ DObj fields; DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]
                let! results = UserDB.queryExactFields state db fields

                match results with
                | [ (_, v) ] -> return DOption(Some v)
                | _ -> return DOption None
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "DB" "queryOne" 2) }
    // ; { name = fn "DB" "queryOne" 2
//   ; parameters = [Param.make "spec" TObj; tableParam]
//   ; returnType = TOption
//   ; description =
//       "Fetch exactly one value from `table` which have the same fields and values that `spec` has. If there is exactly one value, it returns Just value and if there is none or more than 1 found, it returns Nothing"
//   ; fn =
//         InProcess (function
//         | state, [(DObj _ as obj); DDB dbname] -> taskv {
//             let results =
//               let db = state.dbs.[dbname]
//               UserDB.query_exact_fields state db obj
//             in
//             ( match results with
//             | [(_, v)] ->
//                 Dval.to_opt_just v
//             | _ ->
//                 DOption OptNothing )
//           }
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotQueryable
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
//   (* see queryOneExactFields *)
// ; { name = fn "DB" "queryOneWithExactFields" 0
//   ; parameters = [Param.make "spec" TObj; tableParam]
//   ; returnType = TOption
//   ; description =
//       "Fetch exactly one value from `table` which have the same fields and values that `spec` has. If there is exactly one value, it returns Just value and if there is none or more than 1 found, it returns Nothing. Previously called DB::queryOne_v2"
//   ; fn =
//         InProcess (function
//         | state, [(DObj _ as obj); DDB dbname] -> taskv {
//             let results =
//               let db = state.dbs.[dbname]
//               UserDB.query_exact_fields state db obj
//             in
//             ( match results with
//             | [(_, v)] ->
//                 Dval.to_opt_just v
//             | _ ->
//                 DOption OptNothing )
//           }
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotQueryable
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
    { name = fn "DB" "queryOneWithKey" 1
      parameters = [ specParam; tableParam ]
      returnType = TOption varA
      description =
        "Fetch exactly one value from `table` which have the same fields and values that `spec` has. Returns Nothing if none or more than 1 found"
      fn =
        InProcess
          (function
          | state, [ DObj fields; DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]
                let! results = UserDB.queryExactFields state db fields

                match results with
                | [ (k, v) ] -> return (DOption(Some(DList [ DStr k; v ])))
                | _ -> return (DOption None)
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "DB" "queryOneWithKey" 2) }
    { name = fn "DB" "queryOneWithKey" 2
      parameters = [ specParam; tableParam ]
      returnType = TOption varA
      description =
        "Fetch exactly one value from `table` which have the same fields and values that `spec` has. If there is exactly one key/value pair, it returns Just {key: value} and if there is none or more than 1 found, it returns Nothing"
      fn =
        InProcess
          (function
          | state, [ DObj fields; DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]
                let! results = UserDB.queryExactFields state db fields

                match results with
                | [ (k, v) ] -> return DOption(Some(DObj(Map.ofList [ (k, v) ])))
                | _ -> return DOption None
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "DB" "queryOneExactFieldsWithKey" 0) }
    // ; { name = fn "DB" "queryOneWithExactFieldsWithKey" 0
//   ; parameters = [Param.make "spec" TObj; tableParam]
//   ; returnType = TOption
//   ; description =
//       "Fetch exactly one value from `table` which have the same fields and values that `spec` has. If there is exactly one key/value pair, it returns Just {key: value} and if there is none or more than 1 found, it returns Nothing. Previously called DB::queryOnewithKey_v2"
//   ; fn =
//         InProcess (function
//         | state, [(DObj _ as obj); DDB dbname] -> taskv {
//             let results =
//               let db = state.dbs.[dbname]
//               UserDB.query_exact_fields state db obj
//             in
//             ( match results with
//             | [(k, v)] ->
//                 DOption (OptJust (DObj (DvalMap.singleton k v)))
//             | _ ->
//                 DOption OptNothing )
//           }
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotQueryable
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
    { name = fn "DB" "getAll" 1
      parameters = [ tableParam ]
      returnType = TList TAny
      description = "Fetch all the values in `table`. Returns a list of lists such that the inner
          lists are pairs of [key, value]. ie. [[key, value], [key, value]]"
      fn =
        InProcess
          (function
          | state, [ DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]
                let! results = UserDB.getAll state db

                return
                  results |> List.map (fun (k, v) -> DList [ DStr k; v ]) |> DList
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "DB" "getAll" 2) }
    { name = fn "DB" "getAll" 2
      parameters = [ tableParam ]
      returnType = TList TAny
      description = "Fetch all the values in `table`."
      fn =
        InProcess
          (function
          | state, [ DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]
                let! results = UserDB.getAll state db
                return results |> List.map (fun (_, v) -> v) |> DList
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DB" "getAll" 3
      parameters = [ tableParam ]
      returnType = TList TAny
      description = "Fetch all the values in `table`."
      fn =
        InProcess
          (function
          | state, [ DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]
                let! results = UserDB.getAll state db
                return results |> List.map (fun (_, v) -> v) |> Dval.list
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DB" "getAllWithKeys" 1
      parameters = [ tableParam ]
      returnType = TList TAny
      description = "Fetch all the values in `table`. Returns a list of lists such that the inner
                     lists are pairs of [key, value]. ie. [[key, value], [key, value]]"
      fn =
        InProcess
          (function
          | state, [ DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]
                let! result = UserDB.getAll state db

                return
                  result |> List.map (fun (k, v) -> DList [ DStr k; v ]) |> DList
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "DB" "getAllWithKeys" 2) }
    { name = fn "DB" "getAllWithKeys" 2
      parameters = [ tableParam ]
      returnType = TDict(varA)
      description =
        "Fetch all the values in `table`. Returns an object with key: value. ie. {key : value, key2: value2}"
      fn =
        InProcess
          (function
          | state, [ DDB dbname ] ->
              taskv {
                let db = state.dbs.[dbname]
                let! result = UserDB.getAll state db
                return result |> Map.ofList |> DObj
              }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }
    // ; { name = fn "DB" "count" 0
//   ; parameters = [tableParam]
//   ; returnType = TInt
//   ; description = "Return the number of items stored in `table`."
//   ; fn =
//         InProcess (function
//         | state, [DDB dbname] -> taskv {
//             let db = state.dbs.[dbname]
//             UserDB.count state db |> Dval.dint
//           }
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotQueryable
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; (* previously called `DB::keys` *)
//   { name = fn "DB" "schemaFields" 1
//   ; parameters = [tableParam]
//   ; returnType = TList TAny
//   ; description = "Fetch all the fieldNames in `table`"
//   ; fn =
//         InProcess (function
//         | state, [DDB dbname] -> taskv {
//             let db = state.dbs.[dbname]
//             UserDB.cols_for db
//             |> List.map (fun (k, v) ->  k)
//             |> DList
//           }
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotQueryable
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "DB" "schema" 1
//   ; parameters = [tableParam]
//   ; returnType = TObj
//   ; description =
//       "Returns an `Obj` representing { fieldName: fieldType } in `table`"
//   ; fn =
//         InProcess (function
//         | state, [DDB dbname] -> taskv {
//             let db = state.dbs.[dbname]
//             UserDB.cols_for db
//             |> List.map (fun (k, v) ->
//                    (k,  (Dval.tipe_to_string v)))
//             |> Dval.to_dobj_exn
//           }
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotQueryable
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "DB" "generateKey" 0
//   ; parameters = []
//   ; returnType = TStr
//   ; description = "Returns a random key suitable for use as a DB key"
//   ; fn =
//         InProcess (function
//         | _, [] ->
//             Uuidm.v `V4 |> Uuidm.to_string |> DStr
//           }
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotQueryable
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "DB" "keys" 1
//   ; parameters = [tableParam]
//   ; returnType = TList TAny
//   ; description =
//       "Fetch all the keys of entries in `table`. Returns an list with strings"
//   ; fn =
//         InProcess (function
//         | state, [DDB dbname] -> taskv {
//             let db = state.dbs.[dbname]
//             UserDB.get_all_keys state db
//             |> List.map (fun k -> DStr k)
//             |> DList
//           }
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotQueryable
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "DB" "query" 4
//   ; parameters = [tableParam; Param.make "filter" TBlock ["value"]]
//   ; returnType = TList TAny
//   ; description =
//       "Fetch all the values from `table` for which filter returns true. Note that this does not check every value in `table`, but rather is optimized to find data with indexes. Errors at compile-time if Dark's compiler does not support the code in question."
//   ; fn =
//         InProcess (function
//         | state, [DDB dbname; DFnVal b] -> taskv {
//           ( try
//               let db = state.dbs.[dbname]
//               UserDB.query state db b
//               |> List.map (fun (k, v) -> v)
//               |> Dval.to_list
//             with Db.DBQueryException _ as e ->
//               DError (SourceNone, Db.dbQueryExceptionToString e) )
//           }
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotQueryable
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "DB" "queryWithKey" 3
//   ; parameters = [tableParam; Param.make "filter" TBlock ["value"]]
//   ; returnType = TObj
//   ; description =
//       "Fetch all the values from `table` for which filter returns true, returning {key : value} as an object. Note that this does not check every value in `table`, but rather is optimized to find data with indexes. Errors at compile-time if Dark's compiler does not support the code in question."
//   ; fn =
//         InProcess (function
//         | state, [DDB dbname; DFnVal b] -> taskv {
//           ( try
//               let db = state.dbs.[dbname]
//               UserDB.query state db b |> DvalMap.from_list |> DObj
//             with Db.DBQueryException _ as e ->
//               DError (SourceNone, Db.dbQueryExceptionToString e) )
//           }
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotQueryable
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "DB" "queryOne" 3
//   ; parameters = [tableParam; Param.make "filter" TBlock ["value"]]
//   ; returnType = TList TAny
//   ; description =
//       "Fetch exactly one value from `table` for which filter returns true. Note that this does not check every value in `table`, but rather is optimized to find data with indexes.  If there is exactly one value, it returns Just value and if there is none or more than 1 found, it returns Nothing. Errors at compile-time if Dark's compiler does not support the code in question."
//   ; fn =
//         InProcess (function
//         | state, [DDB dbname; DFnVal b] -> taskv {
//           ( try
//               let db = state.dbs.[dbname]
//               let results = UserDB.query state db b in
//               match results with
//               | [(_, v)] ->
//                   Dval.to_opt_just v
//               | _ ->
//                   DOption OptNothing
//             with Db.DBQueryException _ as e ->
//               DError (SourceNone, Db.dbQueryExceptionToString e) )
//           }
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotQueryable
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DB" "queryOne" 4
      parameters = [ tableParam; queryParam ]
      returnType = TOption varA
      description =
        "Fetch exactly one value from `table` for which filter returns true. Note that this does not check every value in `table`, but rather is optimized to find data with indexes.  If there is exactly one value, it returns Just value and if there is none or more than 1 found, it returns Nothing. Errors at compile-time if Dark's compiler does not support the code in question."
      fn =
        InProcess
          (function
          | state, [ DDB dbname; DFnVal (Lambda b) ] ->
              taskv {
                try
                  let db = state.dbs.[dbname]
                  let! results = UserDB.query state db b

                  match results with
                  | [ (_, v) ] -> return Dval.optionJust v
                  | _ -> return DOption None
                with
                | Db.FakeValFoundInQuery dv -> return dv
                | Db.DBQueryException _ as e ->
                    return (Dval.errStr (Db.dbQueryExceptionToString e))
              }
          | _ -> incorrectArgs ())
      sqlSpec = QueryFunction
      previewable = Impure
      deprecated = NotDeprecated } ]
// ; { name = fn "DB" "queryOneWithKey" 3
//   ; parameters = [tableParam; Param.make "filter" TBlock ["value"]]
//   ; returnType = TOption
//   ; description =
//       "Fetch exactly one value from `table` for which filter returns true. Note that this does not check every value in `table`, but rather is optimized to find data with indexes. If there is exactly one key/value pair, it returns Just {key: value} and if there is none or more than 1 found, it returns Nothing. Errors at compile-time if Dark's compiler does not support the code in question."
//   ; fn =
//         InProcess (function
//         | state, [DDB dbname; DFnVal b] -> taskv {
//           ( try
//               let db = state.dbs.[dbname]
//               let results = UserDB.query state db b in
//               match results with
//               | [(k, v)] ->
//                   DOption (OptJust (DObj (DvalMap.singleton k v)))
//               | _ ->
//                   DOption OptNothing
//             with Db.DBQueryException _ as e ->
//               DError (SourceNone, Db.dbQueryExceptionToString e) )
//           }
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotQueryable
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "DB" "queryCount" 0
//   ; parameters = [tableParam; Param.make "filter" TBlock ["value"]]
//   ; returnType = TInt
//   ; description =
//       "Return the number of items from `table` for which filter returns true. Note that this does not check every value in `table`, but rather is optimized to find data with indexes. Errors at compile-time if Dark's compiler does not support the code in question."
//   ; fn =
//         InProcess (function
//         | state, [DDB dbname; DFnVal b] -> taskv {
//           ( try
//               let db = state.dbs.[dbname]
//               UserDB.query_count state db b |> Dval.dint
//             with Db.DBQueryException _ as e ->
//               DError (SourceNone, Db.dbQueryExceptionToString e) )
//           }
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotQueryable
// ; previewable = Impure
//   ; deprecated = NotDeprecated } ]
//
