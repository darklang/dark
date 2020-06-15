open Core_kernel
open Libexecution.Lib
open Libexecution.Runtime
open Libexecution.Types.RuntimeT
module Exception = Libexecution.Exception
module Dval = Libexecution.Dval
module Unicode_string = Libexecution.Unicode_string

let find_db = Libexecution.Ast.find_db

let fns : Libexecution.Types.fluid_expr fn list =
  [ { prefix_names = ["DB::set_v1"]
    ; infix_names = []
    ; parameters = [par "val" TObj; par "key" TStr; par "table" TDB]
    ; return_type = TObj
    ; description = "Upsert `val` into `table`, accessible by `key`"
    ; func =
        InProcess
          (function
          | state, [DObj value; DStr key; DDB dbname] ->
              let db = find_db state.dbs dbname in
              let key = Unicode_string.to_string key in
              ignore (User_db.set ~state ~upsert:true db key value) ;
              DObj value
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::add_v0"]
    ; infix_names = []
    ; parameters = [par "val" TObj; par "table" TDB]
    ; return_type = TStr
    ; description =
        "Add `val` as a new entry into `table`, using a newly generated key. Returns the generated key."
    ; func =
        InProcess
          (function
          | state, [DObj value; DDB dbname] ->
              let key = Uuidm.v `V4 |> Uuidm.to_string in
              let db = find_db state.dbs dbname in
              ignore (User_db.set ~state ~upsert:true db key value) ;
              Dval.dstr_of_string_exn key
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::get_v1"]
    ; infix_names = []
    ; parameters = [par "key" TStr; par "table" TDB]
    ; return_type = TOption
    ; description = "Finds a value in `table` by `key"
    ; func =
        InProcess
          (function
          | state, [DStr key; DDB dbname] ->
              let key = Unicode_string.to_string key in
              let db = find_db state.dbs dbname in
              User_db.get_option ~state db key |> Dval.dopt_of_option
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::get_v2"]
    ; infix_names = []
    ; parameters = [par "key" TStr; par "table" TDB]
    ; return_type = TOption
    ; description = "Finds a value in `table` by `key"
    ; func =
        InProcess
          (function
          | state, [DStr key; DDB dbname] ->
              let key = Unicode_string.to_string key in
              let db = find_db state.dbs dbname in
              User_db.get_option ~state db key |> Dval.dopt_of_option
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::getMany_v1"]
    ; infix_names = []
    ; parameters = [par "keys" TList; par "table" TDB]
    ; return_type = TList
    ; description =
        "Finds many values in `table` by `keys, returning a [[key, value]] list of lists"
    ; func =
        InProcess
          (function
          | state, [DList keys; DDB dbname] ->
              let db = find_db state.dbs dbname in
              let skeys =
                List.map
                  ~f:(function
                    | DStr s ->
                        Unicode_string.to_string s
                    | t ->
                        Exception.code "Expected a string, got: "
                        ^ (t |> Dval.tipe_of |> Dval.tipe_to_string))
                  keys
              in
              User_db.get_many ~state db skeys
              |> List.map ~f:(fun (k, v) ->
                     DList [Dval.dstr_of_string_exn k; v])
              |> DList
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::getMany_v2"]
    ; infix_names = []
    ; parameters = [par "keys" TList; par "table" TDB]
    ; return_type = TList
    ; description =
        "Finds many values in `table` by `keys, returning a [value] list of values"
    ; func =
        InProcess
          (function
          | state, [DList keys; DDB dbname] ->
              let db = find_db state.dbs dbname in
              let skeys =
                List.map
                  ~f:(function
                    | DStr s ->
                        Unicode_string.to_string s
                    | t ->
                        Exception.code "Expected a string, got: "
                        ^ (t |> Dval.tipe_of |> Dval.tipe_to_string))
                  keys
              in
              User_db.get_many ~state db skeys
              |> List.map ~f:(fun (_, v) -> v)
              |> DList
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::getMany_v3"]
    ; infix_names = []
    ; parameters = [par "keys" TList; par "table" TDB]
    ; return_type = TOption
    ; description =
        "Finds many values in `table` by `keys`. If all `keys` are found, returns Just a list of [values], otherwise returns Nothing (to ignore missing keys, use DB::getExisting)"
    ; func =
        InProcess
          (function
          | state, [DList keys; DDB dbname] ->
              let db = find_db state.dbs dbname in
              let skeys =
                List.map
                  ~f:(function
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
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::getExisting"]
    ; infix_names = []
    ; parameters = [par "keys" TList; par "table" TDB]
    ; return_type = TList
    ; description =
        "Finds many values in `table` by `keys` (ignoring any missing items), returning a [value] list of values"
    ; func =
        InProcess
          (function
          | state, [DList keys; DDB dbname] ->
              let db = find_db state.dbs dbname in
              let skeys =
                List.map
                  ~f:(function
                    | DStr s ->
                        Unicode_string.to_string s
                    | t ->
                        Exception.code "Expected a string, got: "
                        ^ (t |> Dval.tipe_of |> Dval.tipe_to_string))
                  keys
              in
              User_db.get_many ~state db skeys
              |> List.map ~f:(fun (_, v) -> v)
              |> DList
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::getManyWithKeys"]
    ; infix_names = []
    ; parameters = [par "keys" TList; par "table" TDB]
    ; return_type = TList
    ; description =
        "Finds many values in `table` by `keys, returning a [[key, value]] list of lists"
    ; func =
        InProcess
          (function
          | state, [DList keys; DDB dbname] ->
              let db = find_db state.dbs dbname in
              let skeys =
                List.map
                  ~f:(function
                    | DStr s ->
                        Unicode_string.to_string s
                    | t ->
                        Exception.code "Expected a string, got: "
                        ^ (t |> Dval.tipe_of |> Dval.tipe_to_string))
                  keys
              in
              User_db.get_many_with_keys ~state db skeys
              |> List.map ~f:(fun (k, v) ->
                     DList [Dval.dstr_of_string_exn k; v])
              |> DList
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::getManyWithKeys_v1"]
    ; infix_names = []
    ; parameters = [par "keys" TList; par "table" TDB]
    ; return_type = TObj
    ; description =
        "Finds many values in `table` by `keys, returning a {key:{value}, key2: {value2}} object of keys and values"
    ; func =
        InProcess
          (function
          | state, [DList keys; DDB dbname] ->
              let db = find_db state.dbs dbname in
              let skeys =
                List.map
                  ~f:(function
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
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::delete_v1"]
    ; infix_names = []
    ; parameters = [par "key" TStr; par "table" TDB]
    ; return_type = TNull
    ; description = "Delete `key` from `table`"
    ; func =
        InProcess
          (function
          | state, [DStr key; DDB dbname] ->
              let db = find_db state.dbs dbname in
              let key = Unicode_string.to_string key in
              User_db.delete ~state db key ;
              DNull
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::deleteAll_v1"]
    ; infix_names = []
    ; parameters = [par "table" TDB]
    ; return_type = TNull
    ; description = "Delete everything from `table`"
    ; func =
        InProcess
          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.delete_all state db ;
              DNull
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::query_v1"]
    ; infix_names = []
    ; parameters = [par "spec" TObj; par "table" TDB]
    ; return_type = TList
    ; description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has
        , returning a [[key, value]] list of lists"
    ; func =
        InProcess
          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.query_exact_fields ~state db obj
              |> List.map ~f:(fun (k, v) ->
                     DList [Dval.dstr_of_string_exn k; v])
              |> DList
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true (* see query_v2 *) }
  ; { prefix_names = ["DB::query_v2"]
    ; infix_names = []
    ; parameters = [par "spec" TObj; par "table" TDB]
    ; return_type = TList
    ; description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has, returning a list of values"
    ; func =
        InProcess
          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.query_exact_fields ~state db obj
              |> List.map ~f:(fun (k, v) -> v)
              |> Dval.to_list
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
    (* see queryExactFields *)
  ; { prefix_names = ["DB::query_v3"]
    ; infix_names = []
    ; parameters = [par "spec" TObj; par "table" TDB]
    ; return_type = TList
    ; description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has, returning a list of values"
    ; func =
        InProcess
          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.query_exact_fields ~state db obj
              |> List.map ~f:(fun (k, v) -> v)
              |> Dval.to_list
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::queryExactFields"]
    ; infix_names = []
    ; parameters = [par "spec" TObj; par "table" TDB]
    ; return_type = TList
    ; description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has, returning a list of values. Previously called DB::query_v3"
    ; func =
        InProcess
          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.query_exact_fields ~state db obj
              |> List.map ~f:(fun (k, v) -> v)
              |> Dval.to_list
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::queryWithKey_v1"]
    ; infix_names = []
    ; parameters = [par "spec" TObj; par "table" TDB]
    ; return_type = TList
    ; description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has
        , returning a [[key, value]] list of lists"
    ; func =
        InProcess
          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.query_exact_fields ~state db obj
              |> List.map ~f:(fun (k, v) ->
                     DList [Dval.dstr_of_string_exn k; v])
              |> DList
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
    (* see queryExactFieldsWithKey *)
  ; { prefix_names = ["DB::queryWithKey_v2"]
    ; infix_names = []
    ; parameters = [par "spec" TObj; par "table" TDB]
    ; return_type = TObj
    ; description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has
        , returning {key : value} as an object"
    ; func =
        InProcess
          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.query_exact_fields ~state db obj
              |> DvalMap.from_list
              |> DObj
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::queryExactFieldsWithKey"]
    ; infix_names = []
    ; parameters = [par "spec" TObj; par "table" TDB]
    ; return_type = TObj
    ; description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has
        , returning {key : value} as an object. Previous called DB::queryWithKey_v2"
    ; func =
        InProcess
          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.query_exact_fields ~state db obj
              |> DvalMap.from_list
              |> DObj
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::queryOne_v1"]
    ; infix_names = []
    ; parameters = [par "spec" TObj; par "table" TDB]
    ; return_type = TOption
    ; description =
        "Fetch exactly one value from `table` which have the same fields and values that `spec` has. If there is exactly one value, it returns Just value and if there is none or more than 1 found, it returns Nothing"
    ; func =
        InProcess
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
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::queryOne_v2"]
    ; infix_names = []
    ; parameters = [par "spec" TObj; par "table" TDB]
    ; return_type = TOption
    ; description =
        "Fetch exactly one value from `table` which have the same fields and values that `spec` has. If there is exactly one value, it returns Just value and if there is none or more than 1 found, it returns Nothing"
    ; func =
        InProcess
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
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
    (* see queryOneExactFields *)
  ; { prefix_names = ["DB::queryOneWithExactFields"]
    ; infix_names = []
    ; parameters = [par "spec" TObj; par "table" TDB]
    ; return_type = TOption
    ; description =
        "Fetch exactly one value from `table` which have the same fields and values that `spec` has. If there is exactly one value, it returns Just value and if there is none or more than 1 found, it returns Nothing. Previously called DB::queryOne_v2"
    ; func =
        InProcess
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
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::queryOneWithKey_v1"]
    ; infix_names = []
    ; parameters = [par "spec" TObj; par "table" TDB]
    ; return_type = TOption
    ; description =
        "Fetch exactly one value from `table` which have the same fields and values that `spec` has. Returns Nothing if none or more than 1 found"
    ; func =
        InProcess
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
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::queryOneWithKey_v2"]
    ; infix_names = []
    ; parameters = [par "spec" TObj; par "table" TDB]
    ; return_type = TOption
    ; description =
        "Fetch exactly one value from `table` which have the same fields and values that `spec` has. If there is exactly one key/value pair, it returns Just {key: value} and if there is none or more than 1 found, it returns Nothing"
    ; func =
        InProcess
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
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
    (* see queryOneExactFieldsWithKey *)
  ; { prefix_names = ["DB::queryOneWithExactFieldsWithKey"]
    ; infix_names = []
    ; parameters = [par "spec" TObj; par "table" TDB]
    ; return_type = TOption
    ; description =
        "Fetch exactly one value from `table` which have the same fields and values that `spec` has. If there is exactly one key/value pair, it returns Just {key: value} and if there is none or more than 1 found, it returns Nothing. Previously called DB::queryOnewithKey_v2"
    ; func =
        InProcess
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
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::getAll_v1"]
    ; infix_names = []
    ; parameters = [par "table" TDB]
    ; return_type = TList
    ; description =
        "Fetch all the values in `table`. Returns a list of lists such that the inner
        lists are pairs of [key, value]. ie. [[key, value], [key, value]]"
    ; func =
        InProcess
          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.get_all ~state db
              |> List.map ~f:(fun (k, v) ->
                     DList [Dval.dstr_of_string_exn k; v])
              |> DList
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::getAll_v2"]
    ; infix_names = []
    ; parameters = [par "table" TDB]
    ; return_type = TList
    ; description = "Fetch all the values in `table`."
    ; func =
        InProcess
          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.get_all ~state db
              |> List.map ~f:(fun (k, v) -> v)
              |> DList
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::getAll_v3"]
    ; infix_names = []
    ; parameters = [par "table" TDB]
    ; return_type = TList
    ; description = "Fetch all the values in `table`."
    ; func =
        InProcess
          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.get_all ~state db
              |> List.map ~f:(fun (k, v) -> v)
              |> Dval.to_list
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::getAllWithKeys_v1"]
    ; infix_names = []
    ; parameters = [par "table" TDB]
    ; return_type = TList
    ; description =
        "Fetch all the values in `table`. Returns a list of lists such that the inner
        lists are pairs of [key, value]. ie. [[key, value], [key, value]]"
    ; func =
        InProcess
          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.get_all ~state db
              |> List.map ~f:(fun (k, v) ->
                     DList [Dval.dstr_of_string_exn k; v])
              |> DList
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::getAllWithKeys_v2"]
    ; infix_names = []
    ; parameters = [par "table" TDB]
    ; return_type = TObj
    ; description =
        "Fetch all the values in `table`. Returns an object with key: value. ie. {key : value, key2: value2}"
    ; func =
        InProcess
          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.get_all ~state db |> DvalMap.from_list |> DObj
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::count"]
    ; infix_names = []
    ; parameters = [par "table" TDB]
    ; return_type = TInt
    ; description = "Return the number of items stored in `table`."
    ; func =
        InProcess
          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.count ~state db |> Dval.dint
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; (* previously called `DB::keys` *)
    { prefix_names = ["DB::schemaFields_v1"]
    ; infix_names = []
    ; parameters = [par "table" TDB]
    ; return_type = TList
    ; description = "Fetch all the fieldNames in `table`"
    ; func =
        InProcess
          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.cols_for db
              |> List.map ~f:(fun (k, v) -> Dval.dstr_of_string_exn k)
              |> DList
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::schema_v1"]
    ; infix_names = []
    ; parameters = [par "table" TDB]
    ; return_type = TObj
    ; description =
        "Returns an `Obj` representing { fieldName: fieldType } in `table`"
    ; func =
        InProcess
          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.cols_for db
              |> List.map ~f:(fun (k, v) ->
                     (k, Dval.dstr_of_string_exn (Dval.tipe_to_string v)))
              |> Dval.to_dobj_exn
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::generateKey"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TStr
    ; description = "Returns a random key suitable for use as a DB key"
    ; func =
        InProcess
          (function
          | _, [] ->
              Uuidm.v `V4 |> Uuidm.to_string |> Dval.dstr_of_string_exn
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::keys_v1"]
    ; infix_names = []
    ; parameters = [par "table" TDB]
    ; return_type = TList
    ; description =
        "Fetch all the keys of entries in `table`. Returns an list with strings"
    ; func =
        InProcess
          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.get_all_keys ~state db
              |> List.map ~f:(fun k -> Dval.dstr_of_string_exn k)
              |> DList
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::query_v4"]
    ; infix_names = []
    ; parameters = [par "table" TDB; par "filter" TBlock ~args:["value"]]
    ; return_type = TList
    ; description =
        "Fetch all the values from `table` for which filter returns true. Note that this does not check every value in `table`, but rather is optimized to find data with indexes. Errors at compile-time if Dark's compiler does not support the code in question."
    ; func =
        InProcess
          (function
          | state, [DDB dbname; DBlock b] ->
            ( try
                let db = find_db state.dbs dbname in
                User_db.query ~state db b
                |> List.map ~f:(fun (k, v) -> v)
                |> Dval.to_list
              with Db.DBQueryException _ as e ->
                DError (SourceNone, Db.dbQueryExceptionToString e) )
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::queryWithKey_v3"]
    ; infix_names = []
    ; parameters = [par "table" TDB; par "filter" TBlock ~args:["value"]]
    ; return_type = TObj
    ; description =
        "Fetch all the values from `table` for which filter returns true, returning {key : value} as an object. Note that this does not check every value in `table`, but rather is optimized to find data with indexes. Errors at compile-time if Dark's compiler does not support the code in question."
    ; func =
        InProcess
          (function
          | state, [DDB dbname; DBlock b] ->
            ( try
                let db = find_db state.dbs dbname in
                User_db.query ~state db b |> DvalMap.from_list |> DObj
              with Db.DBQueryException _ as e ->
                DError (SourceNone, Db.dbQueryExceptionToString e) )
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::queryOne_v3"]
    ; infix_names = []
    ; parameters = [par "table" TDB; par "filter" TBlock ~args:["value"]]
    ; return_type = TList
    ; description =
        "Fetch exactly one value from `table` for which filter returns true. Note that this does not check every value in `table`, but rather is optimized to find data with indexes.  If there is exactly one value, it returns Just value and if there is none or more than 1 found, it returns Nothing. Errors at compile-time if Dark's compiler does not support the code in question."
    ; func =
        InProcess
          (function
          | state, [DDB dbname; DBlock b] ->
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
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::queryOne_v4"]
    ; infix_names = []
    ; parameters = [par "table" TDB; par "filter" TBlock ~args:["value"]]
    ; return_type = TOption
    ; description =
        "Fetch exactly one value from `table` for which filter returns true. Note that this does not check every value in `table`, but rather is optimized to find data with indexes.  If there is exactly one value, it returns Just value and if there is none or more than 1 found, it returns Nothing. Errors at compile-time if Dark's compiler does not support the code in question."
    ; func =
        InProcess
          (function
          | state, [DDB dbname; DBlock b] ->
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
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::queryOneWithKey_v3"]
    ; infix_names = []
    ; parameters = [par "table" TDB; par "filter" TBlock ~args:["value"]]
    ; return_type = TOption
    ; description =
        "Fetch exactly one value from `table` for which filter returns true. Note that this does not check every value in `table`, but rather is optimized to find data with indexes. If there is exactly one key/value pair, it returns Just {key: value} and if there is none or more than 1 found, it returns Nothing. Errors at compile-time if Dark's compiler does not support the code in question."
    ; func =
        InProcess
          (function
          | state, [DDB dbname; DBlock b] ->
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
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DB::queryCount"]
    ; infix_names = []
    ; parameters = [par "table" TDB; par "filter" TBlock ~args:["value"]]
    ; return_type = TInt
    ; description =
        "Return the number of items from `table` for which filter returns true. Note that this does not check every value in `table`, but rather is optimized to find data with indexes. Errors at compile-time if Dark's compiler does not support the code in question."
    ; func =
        InProcess
          (function
          | state, [DDB dbname; DBlock b] ->
            ( try
                let db = find_db state.dbs dbname in
                User_db.query_count ~state db b |> Dval.dint
              with Db.DBQueryException _ as e ->
                DError (SourceNone, Db.dbQueryExceptionToString e) )
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false } ]
