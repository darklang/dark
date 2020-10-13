open Core_kernel
open Libexecution.Lib
open Libexecution.Runtime
open Libexecution.Types.RuntimeT
module Exception = Libexecution.Exception
module Dval = Libexecution.Dval
module Unicode_string = Libexecution.Unicode_string

let find_db = Libexecution.Ast.find_db

let fns : fn list =
  [ { name = fn "DB" "set" 1

    ; parameters = [Param.make "val" TObj; Param.make "key" TStr; Param.make "table" TDB]
    ; returnType = TObj
    ; description = "Upsert `val` into `table`, accessible by `key`"
    ; fn =

          (function
          | state, [DObj value; DStr key; DDB dbname] ->
              let db = find_db state.dbs dbname in
              let key = Unicode_string.to_string key in
              ignore (User_db.set ~state true db key value) ;
              DObj value
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "add" 0

    ; parameters = [Param.make "val" TObj; Param.make "table" TDB]
    ; returnType = TStr
    ; description =
        "Add `val` as a new entry into `table`, using a newly generated key. Returns the generated key."
    ; fn =

          (function
          | state, [DObj value; DDB dbname] ->
              let key = Uuidm.v `V4 |> Uuidm.to_string in
              let db = find_db state.dbs dbname in
              ignore (User_db.set ~state true db key value) ;
              Dval.dstr_of_string_exn key
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "get" 1

    ; parameters = [Param.make "key" TStr; Param.make "table" TDB]
    ; returnType = TOption
    ; description = "Finds a value in `table` by `key"
    ; fn =

          (function
          | state, [DStr key; DDB dbname] ->
              let key = Unicode_string.to_string key in
              let db = find_db state.dbs dbname in
              User_db.get_option ~state db key |> Dval.dopt_of_option
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "get" 2

    ; parameters = [Param.make "key" TStr; Param.make "table" TDB]
    ; returnType = TOption
    ; description = "Finds a value in `table` by `key"
    ; fn =

          (function
          | state, [DStr key; DDB dbname] ->
              let key = Unicode_string.to_string key in
              let db = find_db state.dbs dbname in
              User_db.get_option ~state db key |> Dval.dopt_of_option
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "getMany" 1

    ; parameters = [Param.make "keys" TList; Param.make "table" TDB]
    ; returnType = TList
    ; description =
        "Finds many values in `table` by `keys, returning a [[key, value]] list of lists"
    ; fn =

          (function
          | state, [DList keys; DDB dbname] ->
              let db = find_db state.dbs dbname in
              let skeys =
                List.map
                  (function
                    | DStr s ->
                        Unicode_string.to_string s
                    | t ->
                        Exception.code "Expected a string, got: "
                        ^ (t |> Dval.tipe_of |> Dval.tipe_to_string))
                  keys
              in
              User_db.get_many ~state db skeys
              |> List.map (fun (k, v) ->
                     DList [Dval.dstr_of_string_exn k; v])
              |> DList
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "getMany" 2

    ; parameters = [Param.make "keys" TList; Param.make "table" TDB]
    ; returnType = TList
    ; description =
        "Finds many values in `table` by `keys, returning a [value] list of values"
    ; fn =

          (function
          | state, [DList keys; DDB dbname] ->
              let db = find_db state.dbs dbname in
              let skeys =
                List.map
                  (function
                    | DStr s ->
                        Unicode_string.to_string s
                    | t ->
                        Exception.code "Expected a string, got: "
                        ^ (t |> Dval.tipe_of |> Dval.tipe_to_string))
                  keys
              in
              User_db.get_many ~state db skeys
              |> List.map (fun (_, v) -> v)
              |> DList
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "getMany" 3

    ; parameters = [Param.make "keys" TList; Param.make "table" TDB]
    ; returnType = TOption
    ; description =
        "Finds many values in `table` by `keys`. If all `keys` are found, returns Just a list of [values], otherwise returns Nothing (to ignore missing keys, use DB::getExisting)"
    ; fn =

          (function
          | state, [DList keys; DDB dbname] ->
              let db = find_db state.dbs dbname in
              let skeys =
                List.map
                  (function
                    | DStr s ->
                        Unicode_string.to_string s
                    | t ->
                        Exception.code "Expected a string, got: "
                        ^ (t |> Dval.tipe_of |> Dval.tipe_to_string))
                  keys
              in
              let items = User_db.get_many ~state db skeys in
              if List.length items = List.length skeys
              then
                List.map items (fun (_, v) -> v) |> DList |> OptJust |> DOption
              else DOption OptNothing
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "getExisting" 0

    ; parameters = [Param.make "keys" TList; Param.make "table" TDB]
    ; returnType = TList
    ; description =
        "Finds many values in `table` by `keys` (ignoring any missing items), returning a [value] list of values"
    ; fn =

          (function
          | state, [DList keys; DDB dbname] ->
              let db = find_db state.dbs dbname in
              let skeys =
                List.map
                  (function
                    | DStr s ->
                        Unicode_string.to_string s
                    | t ->
                        Exception.code "Expected a string, got: "
                        ^ (t |> Dval.tipe_of |> Dval.tipe_to_string))
                  keys
              in
              User_db.get_many ~state db skeys
              |> List.map (fun (_, v) -> v)
              |> DList
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "getManyWithKeys" 0

    ; parameters = [Param.make "keys" TList; Param.make "table" TDB]
    ; returnType = TList
    ; description =
        "Finds many values in `table` by `keys, returning a [[key, value]] list of lists"
    ; fn =

          (function
          | state, [DList keys; DDB dbname] ->
              let db = find_db state.dbs dbname in
              let skeys =
                List.map
                  (function
                    | DStr s ->
                        Unicode_string.to_string s
                    | t ->
                        Exception.code "Expected a string, got: "
                        ^ (t |> Dval.tipe_of |> Dval.tipe_to_string))
                  keys
              in
              User_db.get_many_with_keys ~state db skeys
              |> List.map (fun (k, v) ->
                     DList [Dval.dstr_of_string_exn k; v])
              |> DList
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "getManyWithKeys" 1

    ; parameters = [Param.make "keys" TList; Param.make "table" TDB]
    ; returnType = TObj
    ; description =
        "Finds many values in `table` by `keys, returning a {key:{value}, key2: {value2}} object of keys and values"
    ; fn =

          (function
          | state, [DList keys; DDB dbname] ->
              let db = find_db state.dbs dbname in
              let skeys =
                List.map
                  (function
                    | DStr s ->
                        Unicode_string.to_string s
                    | t ->
                        Exception.code "Expected a string, got: "
                        ^ (t |> Dval.tipe_of |> Dval.tipe_to_string))
                  keys
              in
              User_db.get_many_with_keys ~state db skeys
              |> DvalMap.from_list
              |> DObj
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "delete" 1

    ; parameters = [Param.make "key" TStr; Param.make "table" TDB]
    ; returnType = TNull
    ; description = "Delete `key` from `table`"
    ; fn =

          (function
          | state, [DStr key; DDB dbname] ->
              let db = find_db state.dbs dbname in
              let key = Unicode_string.to_string key in
              User_db.delete ~state db key ;
              DNull
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "deleteAll" 1

    ; parameters = [Param.make "table" TDB]
    ; returnType = TNull
    ; description = "Delete everything from `table`"
    ; fn =

          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.delete_all state db ;
              DNull
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "query" 1

    ; parameters = [Param.make "spec" TObj; Param.make "table" TDB]
    ; returnType = TList
    ; description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has
        , returning a [[key, value]] list of lists"
    ; fn =

          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.query_exact_fields ~state db obj
              |> List.map (fun (k, v) ->
                     DList [Dval.dstr_of_string_exn k; v])
              |> DList
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) (* see query_v2 *) }
  ; { name = fn "DB" "query" 2

    ; parameters = [Param.make "spec" TObj; Param.make "table" TDB]
    ; returnType = TList
    ; description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has, returning a list of values"
    ; fn =

          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.query_exact_fields ~state db obj
              |> List.map (fun (k, v) -> v)
              |> Dval.to_list
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
    (* see queryExactFields *)
  ; { name = fn "DB" "query" 3

    ; parameters = [Param.make "spec" TObj; Param.make "table" TDB]
    ; returnType = TList
    ; description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has, returning a list of values"
    ; fn =

          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.query_exact_fields ~state db obj
              |> List.map (fun (k, v) -> v)
              |> Dval.to_list
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "queryExactFields" 0

    ; parameters = [Param.make "spec" TObj; Param.make "table" TDB]
    ; returnType = TList
    ; description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has, returning a list of values. Previously called DB::query_v3"
    ; fn =

          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.query_exact_fields ~state db obj
              |> List.map (fun (k, v) -> v)
              |> Dval.to_list
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "queryWithKey" 1

    ; parameters = [Param.make "spec" TObj; Param.make "table" TDB]
    ; returnType = TList
    ; description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has
        , returning a [[key, value]] list of lists"
    ; fn =

          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.query_exact_fields ~state db obj
              |> List.map (fun (k, v) ->
                     DList [Dval.dstr_of_string_exn k; v])
              |> DList
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
    (* see queryExactFieldsWithKey *)
  ; { name = fn "DB" "queryWithKey" 2

    ; parameters = [Param.make "spec" TObj; Param.make "table" TDB]
    ; returnType = TObj
    ; description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has
        , returning {key : value} as an object"
    ; fn =

          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.query_exact_fields ~state db obj
              |> DvalMap.from_list
              |> DObj
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "queryExactFieldsWithKey" 0

    ; parameters = [Param.make "spec" TObj; Param.make "table" TDB]
    ; returnType = TObj
    ; description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has
        , returning {key : value} as an object. Previous called DB::queryWithKey_v2"
    ; fn =

          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.query_exact_fields ~state db obj
              |> DvalMap.from_list
              |> DObj
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "queryOne" 1

    ; parameters = [Param.make "spec" TObj; Param.make "table" TDB]
    ; returnType = TOption
    ; description =
        "Fetch exactly one value from `table` which have the same fields and values that `spec` has. If there is exactly one value, it returns Just value and if there is none or more than 1 found, it returns Nothing"
    ; fn =

          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let results =
                let db = find_db state.dbs dbname in
                User_db.query_exact_fields ~state db obj
              in
              ( match results with
              | [(_, v)] ->
                  DOption (OptJust v)
              | _ ->
                  DOption OptNothing )
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "queryOne" 2

    ; parameters = [Param.make "spec" TObj; Param.make "table" TDB]
    ; returnType = TOption
    ; description =
        "Fetch exactly one value from `table` which have the same fields and values that `spec` has. If there is exactly one value, it returns Just value and if there is none or more than 1 found, it returns Nothing"
    ; fn =

          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let results =
                let db = find_db state.dbs dbname in
                User_db.query_exact_fields ~state db obj
              in
              ( match results with
              | [(_, v)] ->
                  Dval.to_opt_just v
              | _ ->
                  DOption OptNothing )
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
    (* see queryOneExactFields *)
  ; { name = fn "DB" "queryOneWithExactFields" 0

    ; parameters = [Param.make "spec" TObj; Param.make "table" TDB]
    ; returnType = TOption
    ; description =
        "Fetch exactly one value from `table` which have the same fields and values that `spec` has. If there is exactly one value, it returns Just value and if there is none or more than 1 found, it returns Nothing. Previously called DB::queryOne_v2"
    ; fn =

          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let results =
                let db = find_db state.dbs dbname in
                User_db.query_exact_fields ~state db obj
              in
              ( match results with
              | [(_, v)] ->
                  Dval.to_opt_just v
              | _ ->
                  DOption OptNothing )
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "queryOneWithKey" 1

    ; parameters = [Param.make "spec" TObj; Param.make "table" TDB]
    ; returnType = TOption
    ; description =
        "Fetch exactly one value from `table` which have the same fields and values that `spec` has. Returns Nothing if none or more than 1 found"
    ; fn =

          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let results =
                let db = find_db state.dbs dbname in
                User_db.query_exact_fields ~state db obj
              in
              ( match results with
              | [(k, v)] ->
                  DOption (OptJust (DList [Dval.dstr_of_string_exn k; v]))
              | _ ->
                  DOption OptNothing )
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "queryOneWithKey" 2

    ; parameters = [Param.make "spec" TObj; Param.make "table" TDB]
    ; returnType = TOption
    ; description =
        "Fetch exactly one value from `table` which have the same fields and values that `spec` has. If there is exactly one key/value pair, it returns Just {key: value} and if there is none or more than 1 found, it returns Nothing"
    ; fn =

          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let results =
                let db = find_db state.dbs dbname in
                User_db.query_exact_fields ~state db obj
              in
              ( match results with
              | [(k, v)] ->
                  DOption (OptJust (DObj (DvalMap.singleton k v)))
              | _ ->
                  DOption OptNothing )
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
    (* see queryOneExactFieldsWithKey *)
  ; { name = fn "DB" "queryOneWithExactFieldsWithKey" 0

    ; parameters = [Param.make "spec" TObj; Param.make "table" TDB]
    ; returnType = TOption
    ; description =
        "Fetch exactly one value from `table` which have the same fields and values that `spec` has. If there is exactly one key/value pair, it returns Just {key: value} and if there is none or more than 1 found, it returns Nothing. Previously called DB::queryOnewithKey_v2"
    ; fn =

          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let results =
                let db = find_db state.dbs dbname in
                User_db.query_exact_fields ~state db obj
              in
              ( match results with
              | [(k, v)] ->
                  DOption (OptJust (DObj (DvalMap.singleton k v)))
              | _ ->
                  DOption OptNothing )
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "getAll" 1

    ; parameters = [Param.make "table" TDB]
    ; returnType = TList
    ; description =
        "Fetch all the values in `table`. Returns a list of lists such that the inner
        lists are pairs of [key, value]. ie. [[key, value], [key, value]]"
    ; fn =

          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.get_all ~state db
              |> List.map (fun (k, v) ->
                     DList [Dval.dstr_of_string_exn k; v])
              |> DList
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "getAll" 2

    ; parameters = [Param.make "table" TDB]
    ; returnType = TList
    ; description = "Fetch all the values in `table`."
    ; fn =

          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.get_all ~state db
              |> List.map (fun (k, v) -> v)
              |> DList
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "getAll" 3

    ; parameters = [Param.make "table" TDB]
    ; returnType = TList
    ; description = "Fetch all the values in `table`."
    ; fn =

          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.get_all ~state db
              |> List.map (fun (k, v) -> v)
              |> Dval.to_list
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "getAllWithKeys" 1

    ; parameters = [Param.make "table" TDB]
    ; returnType = TList
    ; description =
        "Fetch all the values in `table`. Returns a list of lists such that the inner
        lists are pairs of [key, value]. ie. [[key, value], [key, value]]"
    ; fn =

          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.get_all ~state db
              |> List.map (fun (k, v) ->
                     DList [Dval.dstr_of_string_exn k; v])
              |> DList
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "getAllWithKeys" 2

    ; parameters = [Param.make "table" TDB]
    ; returnType = TObj
    ; description =
        "Fetch all the values in `table`. Returns an object with key: value. ie. {key : value, key2: value2}"
    ; fn =

          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.get_all ~state db |> DvalMap.from_list |> DObj
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "count" 0

    ; parameters = [Param.make "table" TDB]
    ; returnType = TInt
    ; description = "Return the number of items stored in `table`."
    ; fn =

          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.count ~state db |> Dval.dint
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; (* previously called `DB::keys` *)
    { name = fn "DB" "schemaFields" 1

    ; parameters = [Param.make "table" TDB]
    ; returnType = TList
    ; description = "Fetch all the fieldNames in `table`"
    ; fn =

          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.cols_for db
              |> List.map (fun (k, v) -> Dval.dstr_of_string_exn k)
              |> DList
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "schema" 1

    ; parameters = [Param.make "table" TDB]
    ; returnType = TObj
    ; description =
        "Returns an `Obj` representing { fieldName: fieldType } in `table`"
    ; fn =

          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.cols_for db
              |> List.map (fun (k, v) ->
                     (k, Dval.dstr_of_string_exn (Dval.tipe_to_string v)))
              |> Dval.to_dobj_exn
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "generateKey" 0

    ; parameters = []
    ; returnType = TStr
    ; description = "Returns a random key suitable for use as a DB key"
    ; fn =

          (function
          | _, [] ->
              Uuidm.v `V4 |> Uuidm.to_string |> Dval.dstr_of_string_exn
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "keys" 1

    ; parameters = [Param.make "table" TDB]
    ; returnType = TList
    ; description =
        "Fetch all the keys of entries in `table`. Returns an list with strings"
    ; fn =

          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.get_all_keys ~state db
              |> List.map (fun k -> Dval.dstr_of_string_exn k)
              |> DList
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "query" 4

    ; parameters = [Param.make "table" TDB; Param.make "filter" TBlock ["value"]]
    ; returnType = TList
    ; description =
        "Fetch all the values from `table` for which filter returns true. Note that this does not check every value in `table`, but rather is optimized to find data with indexes. Errors at compile-time if Dark's compiler does not support the code in question."
    ; fn =

          (function
          | state, [DDB dbname; DLambda b] ->
            ( try
                let db = find_db state.dbs dbname in
                User_db.query ~state db b
                |> List.map (fun (k, v) -> v)
                |> Dval.to_list
              with Db.DBQueryException _ as e ->
                DError (SourceNone, Db.dbQueryExceptionToString e) )
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "queryWithKey" 3

    ; parameters = [Param.make "table" TDB; Param.make "filter" TBlock ["value"]]
    ; returnType = TObj
    ; description =
        "Fetch all the values from `table` for which filter returns true, returning {key : value} as an object. Note that this does not check every value in `table`, but rather is optimized to find data with indexes. Errors at compile-time if Dark's compiler does not support the code in question."
    ; fn =

          (function
          | state, [DDB dbname; DLambda b] ->
            ( try
                let db = find_db state.dbs dbname in
                User_db.query ~state db b |> DvalMap.from_list |> DObj
              with Db.DBQueryException _ as e ->
                DError (SourceNone, Db.dbQueryExceptionToString e) )
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "queryOne" 3

    ; parameters = [Param.make "table" TDB; Param.make "filter" TBlock ["value"]]
    ; returnType = TList
    ; description =
        "Fetch exactly one value from `table` for which filter returns true. Note that this does not check every value in `table`, but rather is optimized to find data with indexes.  If there is exactly one value, it returns Just value and if there is none or more than 1 found, it returns Nothing. Errors at compile-time if Dark's compiler does not support the code in question."
    ; fn =

          (function
          | state, [DDB dbname; DLambda b] ->
            ( try
                let db = find_db state.dbs dbname in
                let results = User_db.query ~state db b in
                match results with
                | [(_, v)] ->
                    Dval.to_opt_just v
                | _ ->
                    DOption OptNothing
              with Db.DBQueryException _ as e ->
                DError (SourceNone, Db.dbQueryExceptionToString e) )
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "queryOne" 4

    ; parameters = [Param.make "table" TDB; Param.make "filter" TBlock ["value"]]
    ; returnType = TOption
    ; description =
        "Fetch exactly one value from `table` for which filter returns true. Note that this does not check every value in `table`, but rather is optimized to find data with indexes.  If there is exactly one value, it returns Just value and if there is none or more than 1 found, it returns Nothing. Errors at compile-time if Dark's compiler does not support the code in question."
    ; fn =

          (function
          | state, [DDB dbname; DLambda b] ->
            ( try
                let db = find_db state.dbs dbname in
                let results = User_db.query ~state db b in
                match results with
                | [(_, v)] ->
                    Dval.to_opt_just v
                | _ ->
                    DOption OptNothing
              with Db.DBQueryException _ as e ->
                DError (SourceNone, Db.dbQueryExceptionToString e) )
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "queryOneWithKey" 3

    ; parameters = [Param.make "table" TDB; Param.make "filter" TBlock ["value"]]
    ; returnType = TOption
    ; description =
        "Fetch exactly one value from `table` for which filter returns true. Note that this does not check every value in `table`, but rather is optimized to find data with indexes. If there is exactly one key/value pair, it returns Just {key: value} and if there is none or more than 1 found, it returns Nothing. Errors at compile-time if Dark's compiler does not support the code in question."
    ; fn =

          (function
          | state, [DDB dbname; DLambda b] ->
            ( try
                let db = find_db state.dbs dbname in
                let results = User_db.query ~state db b in
                match results with
                | [(k, v)] ->
                    DOption (OptJust (DObj (DvalMap.singleton k v)))
                | _ ->
                    DOption OptNothing
              with Db.DBQueryException _ as e ->
                DError (SourceNone, Db.dbQueryExceptionToString e) )
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated }
  ; { name = fn "DB" "queryCount" 0

    ; parameters = [Param.make "table" TDB; Param.make "filter" TBlock ["value"]]
    ; returnType = TInt
    ; description =
        "Return the number of items from `table` for which filter returns true. Note that this does not check every value in `table`, but rather is optimized to find data with indexes. Errors at compile-time if Dark's compiler does not support the code in question."
    ; fn =

          (function
          | state, [DDB dbname; DLambda b] ->
            ( try
                let db = find_db state.dbs dbname in
                User_db.query_count ~state db b |> Dval.dint
              with Db.DBQueryException _ as e ->
                DError (SourceNone, Db.dbQueryExceptionToString e) )
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated } ]
