open Core_kernel
open Libexecution

open Lib
open Runtime
open Types.RuntimeT


let fns : Lib.shortfn list = [
  { pns = ["DB::set_v1"]
  ; ins = []
  ; p = [par "val" TObj; par "key" TStr; par "table" TDB]
  ; r = TObj
  ; d = "Upsert `val` into `table`, accessible by `key`"
  ; f = InProcess
        (function
          | (state, [DObj value; DStr key; DDB db]) ->
            let _ = User_db.insert ~state ~upsert:true db key value in
            DObj value
          | args -> fail args)
  ; pr = None
  ; ps = false
  }
  ;

  { pns = ["DB::get_v1"]
  ; ins = []
  ; p = [par "key" TStr; par "table" TDB]
  ; r = TObj
  ; d = "Finds a value in `table` by `key"
  ; f = InProcess
        (function
          | (state, [DStr key; DDB db]) ->
            User_db.fetch_by_key state db key
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["DB::delete_v1"]
  ; ins = []
  ; p = [par "key" TStr; par "table" TDB]
  ; r = TNull
  ; d = "Delete `value` from `table`"
  ; f = InProcess
        (function
          | (state, [DStr key; DDB db]) ->
            User_db.delete state db key;
            DNull
          | args -> fail args)
  ; pr = None
  ; ps = false
  }
  ;

  { pns = ["DB::deleteAll_v1"]
  ; ins = []
  ; p = [par "table" TDB]
  ; r = TNull
  ; d = "Delete everything from `table`"
  ; f = InProcess
        (function
          | (state, [DDB db]) ->
            User_db.delete_all state db;
            DNull
          | args -> fail args)
  ; pr = None
  ; ps = false
  }
  ;

  { pns = ["DB::query_v1"]
  ; ins = []
  ; p = [par "spec" TObj; par "table" TDB]
  ; r = TList
  ; d = "Fetch all the values from `table` which have the same fields and values that `spec` has"
  ; f = InProcess
        (function
          | (state, [DObj map; DDB db]) ->
            map
            |> DvalMap.to_alist
            |> User_db.fetch_by_many state db
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["DB::fetchAll_v1"]
  ; ins = []
  ; p = [par "table" TDB]
  ; r = TList
  ; d = "Fetch all the values in `table`. Returns a list of lists such that the inner
        lists are pairs of [key, value]. ie. [[key, value], [key, value]]"
  ; f = InProcess
        (function
          | (state, [DDB db]) ->
            User_db.fetch_all state db
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  (* previously called `DB::keys` *)
  { pns = ["DB::schemaFields_v1"]
  ; ins = []
  ; p = [par "table" TDB]
  ; r = TList
  ; d = "Fetch all the fieldNames in `table`"
  ; f = InProcess
        (function
          | (_, [DDB db]) ->
            User_db.cols_for db
            |> List.map ~f:(fun (k,v) -> DStr k)
            |> DList
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["DB::schema_v1"]
  ; ins = []
  ; p = [par "table" TDB]
  ; r = TObj
  ; d = "Returns an `Obj` representing { fieldName: fieldType } in `table`"
  ; f = InProcess
        (function
          | (_, [DDB db]) ->
            User_db.cols_for db
            |> List.map ~f:(fun (k,v) -> (k, DStr (Dval.tipe_to_string v)))
            |> Dval.to_dobj
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
]
