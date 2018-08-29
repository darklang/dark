open Core_kernel
open Libexecution

open Lib
open Runtime
open Types.RuntimeT


let replacements = [

  ( "DB::set_v1"
    , InProcess
        (function
          | (state, [DObj value; DStr key; DDB db]) ->
            ignore (User_db.set ~state ~magic:false ~upsert:true db key value);
            DObj value
          | args -> fail args))

  ;
  ( "DB::get_v1"
  , InProcess
      (function
          | (state, [DStr key; DDB db]) ->
            (try
               DOption (OptJust (User_db.get ~state ~magic:false db key))
             with
             | Exception.DarkException e when e.tipe = Exception.DarkStorage ->
               DOption OptNothing
             | e -> raise e)
          | args -> fail args))

  ;
  ( "DB::getMany_v1"
  , InProcess
      (function
          | (state, [DList keys; DDB db]) ->
            let skeys =
              List.map
                ~f:(function
                    | DStr s -> s
                    | t ->
                      Exception.user
                        "Expected a string, got: "
                        ^ (t |> Dval.tipe_of |> Dval.tipe_to_string))
                keys
            in
            User_db.get_many ~state ~magic:false db skeys
          | args -> fail args))

  ;
  ( "DB::delete_v1"
  , InProcess
      (function
          | (state, [DStr key; DDB db]) ->
            User_db.delete ~state db key;
            DNull
          | args -> fail args))

  ;
  ( "DB::deleteAll_v1"
  , InProcess
      (function
          | (state, [DDB db]) ->
            User_db.delete_all state db;
            DNull
          | args -> fail args))

  ;
  ( "DB::query_v1"
  , InProcess
      (function
          | (state, [DObj map; DDB db]) ->
            map
            |> DvalMap.to_alist
            |> User_db.query ~state ~magic:false db
          | args -> fail args))

  ;
  ( "DB::getAll_v1"
  , InProcess
      (function
          | (state, [DDB db]) ->
            User_db.get_all ~state ~magic:false db
          | args -> fail args))

  (* previously called `DB::keys` *)
  ;
  ( "DB::schemaFields_v1"
  , InProcess
      (function
          | (_, [DDB db]) ->
            User_db.cols_for db
            |> List.map ~f:(fun (k,v) -> DStr k)
            |> DList
          | args -> fail args))

  ;
  ( "DB::schema_v1"
  , InProcess
      (function
          | (_, [DDB db]) ->
            User_db.cols_for db
            |> List.map ~f:(fun (k,v) -> (k, DStr (Dval.tipe_to_string v)))
            |> Dval.to_dobj
          | args -> fail args))

]
