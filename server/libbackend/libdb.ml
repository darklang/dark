open Core_kernel
open Libexecution
open Libexecution

open Lib
open Runtime
open Types.RuntimeT


let fns : Lib.shortfn list = [

  { pns = ["DB::insert"]
  ; ins = []
  ; p = [par "val" TObj; par "table" TDB]
  ; r = TObj
  ; d = "Insert `val` into `table`"
  ; f = InProcess
        (function
          | (state, [DObj value; DDB db]) ->
            let id = User_db.insert state db value in
            DObj (Map.set value "id" (DID id))
          | args -> fail args)
  ; pr = None
  ; ps = false
  }
  ;

  { pns = ["DB::delete"]
  ; ins = []
  ; p = [par "value" TObj; par "table" TDB]
  ; r = TNull
  ; d = "Delete `value` from `table`"
  ; f = InProcess
        (function
          | (state, [DObj vals; DDB db]) ->
            User_db.delete state db vals;
            DNull
          | args -> fail args)
  ; pr = None
  ; ps = false
  }
  ;


  { pns = ["DB::deleteAll"]
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



  { pns = ["DB::update"]
  ; ins = []
  ; p = [par "value" TObj; par "table" TDB]
  ; r = TNull
  ; d = "Update `table` value which has the same ID as `value`"
  ; f = InProcess
        (function
          | (state, [DObj vals; DDB db]) ->
            User_db.update state db vals;
            DObj vals
          | args -> fail args)
  ; pr = None
  ; ps = false
  }
  ;

  { pns = ["DB::fetchBy"]
  ; ins = []
  ; p = [par "value" TAny; par "field" TStr; par "table" TDB]
  ; r = TList
  ; d = "Fetch all the values in `table` whose `field` is `value`"
  ; f = InProcess
        (function
          | (state, [value; DStr field; DDB db]) ->
            User_db.fetch_by state db field value
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["DB::fetchOneBy"]
  ; ins = []
  ; p = [par "value" TAny; par "field" TStr; par "table" TDB]
  ; r = TAny
  ; d = "Fetch exactly one value in `table` whose `field` is `value`"
  ; f = InProcess
        (function
          | (state, [value; DStr field; DDB db]) ->
            let result = User_db.fetch_by state db field value in
            (match result with
             | DList (x :: xs) -> x
               (* TODO(ian): Maybe/Option *)
             | _ -> DNull)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["DB::fetchByMany"]
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

  { pns = ["DB::fetchOneByMany"]
  ; ins = []
  ; p = [par "spec" TObj; par "table" TDB]
  ; r = TAny
  ; d = "Fetch exactly one value from `table`, which have the samea fields and values that `spec` has"
  ; f = InProcess
        (function
          | (state, [DObj map; DDB db]) ->
            let result =
              User_db.fetch_by_many state db (DvalMap.to_alist map)
            in
            (match result with
             | DList (x :: xs) -> x
               (* TODO(ian): Maybe/Option *)
             | _ -> DNull)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;



  { pns = ["DB::fetchAll"]
  ; ins = []
  ; p = [par "table" TDB]
  ; r = TList
  ; d = "Fetch all the values in `table`"
  ; f = InProcess
        (function
          | (state, [DDB db]) ->
            User_db.fetch_all state db
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["DB::keys"]
  ; ins = []
  ; p = [par "table" TDB]
  ; r = TList
  ; d = "Fetch all the keys in `table`"
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

  { pns = ["DB::schema"]
  ; ins = []
  ; p = [par "table" TDB]
  ; r = TObj
  ; d = "Fetch all the values in `table`"
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
  ;

]

