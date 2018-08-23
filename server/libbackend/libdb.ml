open Core_kernel
open Libexecution

open Lib
open Runtime
open Types.RuntimeT

let fetch_by_field ~state fieldname fieldvalue db =
  if fieldname = "id"
  then
    let skey =
      (match fieldvalue with
        | DID id -> Uuidm.to_string id
        | DStr s -> s
        | x ->
          Exception.user
            ("Expected an ID or a String at 'id' but got: "
            ^ (x |> Dval.tipe_of |> Dval.tipe_to_string)))
    in
    User_db.get_many ~state ~magic:true db [skey]
  else
    User_db.query_by_one ~state ~magic:true db fieldname fieldvalue


let fns : Lib.shortfn list = [

  { pns = ["DB::insert"]
  ; ins = []
  ; p = [par "val" TObj; par "table" TDB]
  ; r = TObj
  ; d = "DEPRECATED: Insert `val` into `table`"
  ; f = InProcess
        (function
          | (state, [DObj value; DDB db]) ->
            let key = Util.create_uuid () in
            ignore
              (User_db.set
                 ~state
                 ~magic:true
                 ~upsert:false
                 db (Uuidm.to_string key) value);
            DObj (Map.set value "id" (DID key))
          | args -> fail args)
  ; pr = None
  ; ps = false
  ; dep = true
  }
  ;

  { pns = ["DB::delete"]
  ; ins = []
  ; p = [par "value" TObj; par "table" TDB]
  ; r = TNull
  ; d = "DEPRECATED: Delete `value` from `table`"
  ; f = InProcess
        (function
          | (state, [DObj vals; DDB db]) ->
            let key =
              match Map.find_exn vals "id" with
              | DID id -> Uuidm.to_string id
              | _ -> Exception.internal "expected a UUID` at magic `id` field in deprecated delete"
            in
            User_db.delete state db key;
            DNull
          | args -> fail args)
  ; pr = None
  ; ps = false
  ; dep = true
  }
  ;


  { pns = ["DB::deleteAll"]
  ; ins = []
  ; p = [par "table" TDB]
  ; r = TNull
  ; d = "DEPRECATED: Delete everything from `table`"
  ; f = InProcess
        (function
          | (state, [DDB db]) ->
            User_db.delete_all state db;
            DNull
          | args -> fail args)
  ; pr = None
  ; ps = false
  ; dep = true
  }
  ;



  { pns = ["DB::update"]
  ; ins = []
  ; p = [par "value" TObj; par "table" TDB]
  ; r = TNull
  ; d = "DEPRECATED: Update `table` value which has the same ID as `value`"
  ; f = InProcess
        (function
          | (state, [DObj vals; DDB db]) ->
            User_db.update state db vals;
            DObj vals
          | args -> fail args)
  ; pr = None
  ; ps = false
  ; dep = true
  }
  ;

  { pns = ["DB::fetchBy"]
  ; ins = []
  ; p = [par "value" TAny; par "field" TStr; par "table" TDB]
  ; r = TList
  ; d = "DEPRECATED: Fetch all the values in `table` whose `field` is `value`"
  ; f = InProcess
        (function
          | (state, [value; DStr field; DDB db]) ->
            let result = fetch_by_field ~state field value db in
            User_db.coerce_dlist_of_kv_pairs_to_legacy_object result
          | args -> fail args)
  ; pr = None
  ; ps = true
  ; dep = true
  }
  ;

  { pns = ["DB::fetchOneBy"]
  ; ins = []
  ; p = [par "value" TAny; par "field" TStr; par "table" TDB]
  ; r = TAny
  ; d = "DEPRECATED: Fetch exactly one value in `table` whose `field` is `value`"
  ; f = InProcess
        (function
          | (state, [value; DStr field; DDB db]) ->
            let result = fetch_by_field ~state field value db in
            (match result with
             | DList (x :: xs) ->
               (match x with
                | DList pair -> User_db.coerce_key_value_pair_to_legacy_object pair
                | _ -> Exception.internal "bad fetch")
             | _ -> DNull)
          | args -> fail args)
  ; pr = None
  ; ps = true
  ; dep = true
  }
  ;

  { pns = ["DB::fetchByMany"]
  ; ins = []
  ; p = [par "spec" TObj; par "table" TDB]
  ; r = TList
  ; d = "DEPRECATED: Fetch all the values from `table` which have the same fields and values that `spec` has"
  ; f = InProcess
        (function
          | (state, [DObj map; DDB db]) ->
            map
            |> DvalMap.to_alist
            |> User_db.query ~state ~magic:true db
            |> User_db.coerce_dlist_of_kv_pairs_to_legacy_object
          | args -> fail args)
  ; pr = None
  ; ps = true
  ; dep = true
  }
  ;

  { pns = ["DB::fetchOneByMany"]
  ; ins = []
  ; p = [par "spec" TObj; par "table" TDB]
  ; r = TAny
  ; d = "DEPRECATED: Fetch exactly one value from `table`, which have the samea fields and values that `spec` has"
  ; f = InProcess
        (function
          | (state, [DObj map; DDB db]) ->
            let result =
              User_db.query ~state ~magic:true db (DvalMap.to_alist map)
            in
            (match result with
             | DList (x :: xs) ->
               (match x with
                | DList pair -> User_db.coerce_key_value_pair_to_legacy_object pair
                | _ -> Exception.internal "bad fetch")
               (* TODO(ian): Maybe/Option *)
             | _ -> DNull)
          | args -> fail args)
  ; pr = None
  ; ps = true
  ; dep = true
  }
  ;



  { pns = ["DB::fetchAll"]
  ; ins = []
  ; p = [par "table" TDB]
  ; r = TList
  ; d = "DEPRECATED: Fetch all the values in `table`"
  ; f = InProcess
        (function
          | (state, [DDB db]) ->
            let result = User_db.get_all ~state ~magic:true db in
            User_db.coerce_dlist_of_kv_pairs_to_legacy_object result
          | args -> fail args)
  ; pr = None
  ; ps = true
  ; dep = true
  }
  ;

  { pns = ["DB::keys"]
  ; ins = []
  ; p = [par "table" TDB]
  ; r = TList
  ; d = "DEPRECATED: Fetch all the keys in `table`"
  ; f = InProcess
        (function
          | (_, [DDB db]) ->
            User_db.cols_for db
            |> List.map ~f:(fun (k,v) -> DStr k)
            |> DList
          | args -> fail args)
  ; pr = None
  ; ps = true
  ; dep = true
  }
  ;

  { pns = ["DB::schema"]
  ; ins = []
  ; p = [par "table" TDB]
  ; r = TObj
  ; d = "DEPRECATED: Fetch all the values in `table`"
  ; f = InProcess
        (function
          | (_, [DDB db]) ->
            User_db.cols_for db
            |> List.map ~f:(fun (k,v) -> (k, DStr (Dval.tipe_to_string v)))
            |> Dval.to_dobj
          | args -> fail args)
  ; pr = None
  ; ps = true
  ; dep = true
  }
  ;

]

