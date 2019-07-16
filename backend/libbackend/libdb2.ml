open Core_kernel
open Libexecution.Lib
open Libexecution.Runtime
open Libexecution.Types.RuntimeT
module Exception = Libexecution.Exception
module Dval = Libexecution.Dval
module Unicode_string = Libexecution.Unicode_string

let find_db = Libdb.find_db

let fns : shortfn list =
  [ { pns = ["DB::set_v1"]
    ; ins = []
    ; p = [par "val" TObj; par "key" TStr; par "table" TDB]
    ; r = TObj
    ; d = "Upsert `val` into `table`, accessible by `key`"
    ; f =
        InProcess
          (function
          | state, [DObj value; DStr key; DDB dbname] ->
              let db = find_db state.dbs dbname in
              let key = Unicode_string.to_string key in
              ignore (User_db.set ~state ~upsert:true db key value) ;
              DObj value
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::add_v0"]
    ; ins = []
    ; p = [par "val" TObj; par "table" TDB]
    ; r = TStr
    ; d =
        "Add `val` as a new entry into `table`, using a newly generated key. Returns the generated key."
    ; f =
        InProcess
          (function
          | state, [DObj value; DDB dbname] ->
              let key = Uuidm.v `V4 |> Uuidm.to_string in
              let db = find_db state.dbs dbname in
              ignore (User_db.set ~state ~upsert:true db key value) ;
              Dval.dstr_of_string_exn key
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::get_v1"]
    ; ins = []
    ; p = [par "key" TStr; par "table" TDB]
    ; r = TOption
    ; d = "Finds a value in `table` by `key"
    ; f =
        InProcess
          (function
          | state, [DStr key; DDB dbname] ->
            ( try
                let key = Unicode_string.to_string key in
                let db = find_db state.dbs dbname in
                DOption (OptJust (User_db.get ~state db key))
              with
            | Exception.DarkException e when e.tipe = Exception.DarkStorage ->
                DOption OptNothing
            | e ->
                raise e )
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::getMany_v1"]
    ; ins = []
    ; p = [par "keys" TList; par "table" TDB]
    ; r = TList
    ; d =
        "Finds many values in `table` by `keys, returning a [[key, value]] list of lists"
    ; f =
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
          | args ->
              fail args)
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::getMany_v2"]
    ; ins = []
    ; p = [par "keys" TList; par "table" TDB]
    ; r = TList
    ; d =
        "Finds many values in `table` by `keys, returning a [value] list of values"
    ; f =
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
              User_db.get_many_v2 ~state db skeys
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::getManyWithKeys"]
    ; ins = []
    ; p = [par "keys" TList; par "table" TDB]
    ; r = TList
    ; d =
        "Finds many values in `table` by `keys, returning a [[key, value]] list of lists"
    ; f =
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
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::delete_v1"]
    ; ins = []
    ; p = [par "key" TStr; par "table" TDB]
    ; r = TNull
    ; d = "Delete `key` from `table`"
    ; f =
        InProcess
          (function
          | state, [DStr key; DDB dbname] ->
              let db = find_db state.dbs dbname in
              let key = Unicode_string.to_string key in
              User_db.delete ~state db key ;
              DNull
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::deleteAll_v1"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TNull
    ; d = "Delete everything from `table`"
    ; f =
        InProcess
          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.delete_all state db ;
              DNull
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::query_v1"]
    ; ins = []
    ; p = [par "spec" TObj; par "table" TDB]
    ; r = TList
    ; d =
        "Fetch all the values from `table` which have the same fields and values that `spec` has
        , returning a [[key, value]] list of lists"
    ; f =
        InProcess
          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.query ~state db obj
          | args ->
              fail args)
    ; ps = false
    ; dep = true (* see query_v2 *) }
  ; { pns = ["DB::query_v2"]
    ; ins = []
    ; p = [par "spec" TObj; par "table" TDB]
    ; r = TList
    ; d =
        "Fetch all the values from `table` which have the same fields and values that `spec` has, returning a list of values"
    ; f =
        InProcess
          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let results =
                let db = find_db state.dbs dbname in
                User_db.query ~state db obj
              in
              ( match results with
              | DList xs ->
                  xs
                  |> List.map ~f:(function
                         | DList [x; y] ->
                             y
                         | _ ->
                             Exception.internal "bad format from User_db.query" )
                  |> DList
              | _ ->
                  Exception.internal "bad format from User_db.query" )
          | args ->
              fail args)
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::query_v3"]
    ; ins = []
    ; p = [par "spec" TObj; par "table" TDB]
    ; r = TList
    ; d =
        "Fetch all the values from `table` which have the same fields and values that `spec` has, returning a list of values"
    ; f =
        InProcess
          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let results =
                let db = find_db state.dbs dbname in
                User_db.query ~state db obj
              in
              ( match results with
              | DList xs ->
                  xs
                  |> List.map ~f:(function
                         | DList [x; y] ->
                             y
                         | _ ->
                             Exception.internal "bad format from User_db.query" )
                  |> Dval.to_list
              | _ ->
                  Exception.internal "bad format from User_db.query" )
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::queryWithKey_v1"]
    ; ins = []
    ; p = [par "spec" TObj; par "table" TDB]
    ; r = TList
    ; d =
        "Fetch all the values from `table` which have the same fields and values that `spec` has
        , returning a [[key, value]] list of lists"
    ; f =
        InProcess
          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.query ~state db obj
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::queryOne_v1"]
    ; ins = []
    ; p = [par "spec" TObj; par "table" TDB]
    ; r = TOption
    ; d =
        "Fetch exactly one value from `table` which have the same fields and values that `spec` has. Returns Nothing if none or more than 1 found"
    ; f =
        InProcess
          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let results =
                let db = find_db state.dbs dbname in
                User_db.query ~state db obj
              in
              ( match results with
              | DList [res] ->
                ( match res with
                | DList [_; v] ->
                    DOption (OptJust v)
                | _ ->
                    Exception.internal
                      "Bad format from query in queryOneWithKey_v1" )
              | _ ->
                  DOption OptNothing )
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::queryOneWithKey_v1"]
    ; ins = []
    ; p = [par "spec" TObj; par "table" TDB]
    ; r = TOption
    ; d =
        "Fetch exactly one value from `table` which have the same fields and values that `spec` has. Returns Nothing if none or more than 1 found"
    ; f =
        InProcess
          (function
          | state, [(DObj _ as obj); DDB dbname] ->
              let results =
                let db = find_db state.dbs dbname in
                User_db.query ~state db obj
              in
              ( match results with
              | DList [res] ->
                  DOption (OptJust res)
              | _ ->
                  DOption OptNothing )
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::getAll_v1"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TList
    ; d =
        "Fetch all the values in `table`. Returns a list of lists such that the inner
        lists are pairs of [key, value]. ie. [[key, value], [key, value]]"
    ; f =
        InProcess
          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.get_all ~state db
          | args ->
              fail args)
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::getAll_v2"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TList
    ; d = "Fetch all the values in `table`."
    ; f =
        InProcess
          (function
          | state, [DDB dbname] ->
              let results =
                let db = find_db state.dbs dbname in
                User_db.get_all ~state db
              in
              ( match results with
              | DList xs ->
                  xs
                  |> List.map ~f:(function
                         | DList [x; y] ->
                             y
                         | _ ->
                             Exception.internal
                               "bad format from User_db.get_all" )
                  |> DList
              | _ ->
                  Exception.internal "bad format from User_db.get_all" )
          | args ->
              fail args)
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::getAll_v3"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TList
    ; d = "Fetch all the values in `table`."
    ; f =
        InProcess
          (function
          | state, [DDB dbname] ->
              let results =
                let db = find_db state.dbs dbname in
                User_db.get_all ~state db
              in
              ( match results with
              | DList xs ->
                  xs
                  |> List.map ~f:(function
                         | DList [x; y] ->
                             y
                         | _ ->
                             Exception.internal "bad format from User_db.query" )
                  |> Dval.to_list
              | _ ->
                  Exception.internal "bad format from User_db.query" )
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::getAllWithKeys_v1"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TList
    ; d =
        "Fetch all the values in `table`. Returns a list of lists such that the inner
        lists are pairs of [key, value]. ie. [[key, value], [key, value]]"
    ; f =
        InProcess
          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.get_all ~state db
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::count"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TInt
    ; d = "Return the number of items stored in `table`."
    ; f =
        InProcess
          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.count ~state db |> Dval.dint
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; (* previously called `DB::keys` *)
    { pns = ["DB::schemaFields_v1"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TList
    ; d = "Fetch all the fieldNames in `table`"
    ; f =
        InProcess
          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.cols_for db
              |> List.map ~f:(fun (k, v) -> Dval.dstr_of_string_exn k)
              |> DList
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::schema_v1"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TObj
    ; d = "Returns an `Obj` representing { fieldName: fieldType } in `table`"
    ; f =
        InProcess
          (function
          | state, [DDB dbname] ->
              let db = find_db state.dbs dbname in
              User_db.cols_for db
              |> List.map ~f:(fun (k, v) ->
                     (k, Dval.dstr_of_string_exn (Dval.tipe_to_string v)) )
              |> Dval.to_dobj_exn
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::generateKey"]
    ; ins = []
    ; p = []
    ; r = TStr
    ; d = "Returns a random key suitable for use as a DB key"
    ; f =
        InProcess
          (function
          | _, [] ->
              Uuidm.v `V4 |> Uuidm.to_string |> Dval.dstr_of_string_exn
          | args ->
              fail args)
    ; ps = false
    ; dep = false } ]
