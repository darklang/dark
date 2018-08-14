open Core_kernel
open Libexecution

open Lib
open Runtime
open Types.RuntimeT

let coerce_key_value_pair_to_legacy_object pair =
  let id =
    (match List.hd_exn pair with
     | DStr s ->
       (match Uuidm.of_string s with
        | Some id -> DID id
        | None -> DStr s)
     | _ ->
       Exception.internal "Error with backport; bad type for key")
  in
  let value =
    (match List.last_exn pair with
     | DObj o -> o
     | _ ->
       Exception.internal "bad format from fetch; expected object")
  in
  DObj (Map.set ~key:"id" ~data:id value)

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
    User_db.get state db skey
  else
    User_db.fetch_by state db fieldname fieldvalue


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
                 ~upsert:false
                 db (Uuidm.to_string key) value);
            DObj (Map.set value "id" (DID key))
          | args -> fail args)
  ; pr = None
  ; ps = false
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
            (match result with
             | DList pairs ->
               pairs
               |> List.map
                 ~f:(function
                     | DList pair ->
                       coerce_key_value_pair_to_legacy_object pair
                     | _ ->
                        Exception.internal
                          "bad format from internal fetch, unable to coerce to legacy format")
               |> DList
             | _ -> Exception.internal "bad fetch in legacy fetchBy")
          | args -> fail args)
  ; pr = None
  ; ps = true
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
                | DList pair -> coerce_key_value_pair_to_legacy_object pair
                | _ -> Exception.internal "bad fetch")
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
  ; d = "DEPRECATED: Fetch all the values from `table` which have the same fields and values that `spec` has"
  ; f = InProcess
        (function
          | (state, [DObj map; DDB db]) ->
            map
            |> DvalMap.to_alist
            |> User_db.fetch_by_many state db
            |>
            (function
              | DList pairs ->
                pairs
                |> List.map
                  ~f:(function
                      | DList pair ->
                        coerce_key_value_pair_to_legacy_object pair
                      | _ ->
                        Exception.internal
                          "bad format from internal fetch, unable to coerce to legacy format")
                |> DList
              | _ -> Exception.internal "bad fetch in legacy fetchBy")
          | args -> fail args)
  ; pr = None
  ; ps = true
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
              User_db.fetch_by_many state db (DvalMap.to_alist map)
            in
            (match result with
             | DList (x :: xs) ->
               (match x with
                | DList pair -> coerce_key_value_pair_to_legacy_object pair
                | _ -> Exception.internal "bad fetch")
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
  ; d = "DEPRECATED: Fetch all the values in `table`"
  ; f = InProcess
        (function
          | (state, [DDB db]) ->
            (match User_db.get_all state db with
             | DList pairs ->
               pairs
               |> List.map
                 ~f:(function
                     | DList pair ->
                       coerce_key_value_pair_to_legacy_object pair
                     | _ ->
                        Exception.internal
                          "bad format from internal fetch, unable to coerce to legacy format")
               |> DList
             | _ ->
               Exception.internal
                 "bad format from internal fetch, unable to coerce to legacy format")
          | args -> fail args)
  ; pr = None
  ; ps = true
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
  }
  ;

]

