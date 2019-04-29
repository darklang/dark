open Core_kernel
open Libexecution
open Lib
open Runtime
open Types.RuntimeT

let find_db (dbs : DbT.db list) (name : string) : DbT.db =
  dbs
  |> List.filter ~f:(fun db ->
         match db.name with
         | Partial _ | Blank _ ->
             false
         | Filled (_, dbname) ->
             dbname = name )
  |> List.hd_exn


let fetch_by_field ~state fieldname fieldvalue db =
  if Unicode_string.equal fieldname (Unicode_string.of_string_exn "id")
  then
    let skey =
      match fieldvalue with
      | DID id ->
          Uuidm.to_string id
      | DStr s ->
          Unicode_string.to_string s
      | x ->
          Exception.user
            ( "Expected an ID or a String at 'id' but got: "
            ^ (x |> Dval.tipe_of |> Dval.tipe_to_string) )
    in
    User_db.get_many ~state db [skey]
  else
    User_db.query_by_one
      ~state
      db
      (Unicode_string.to_string fieldname)
      fieldvalue


let replacements =
  [ ( "DB::insert"
    , InProcess
        (function
        | state, [DObj value; DDB dbname] ->
            let db = find_db state.dbs dbname in
            let key = Util.create_uuid () in
            ignore
              (User_db.set ~state ~upsert:false db (Uuidm.to_string key) value) ;
            DObj (Map.set value "id" (DID key))
        | args ->
            fail args) )
  ; ( "DB::delete"
    , InProcess
        (function
        | state, [DObj vals; DDB dbname] ->
            let db = find_db state.dbs dbname in
            let key =
              match Map.find_exn vals "id" with
              | DID id ->
                  Uuidm.to_string id
              | _ ->
                  Exception.internal
                    "expected a UUID` at magic `id` field in deprecated delete"
            in
            User_db.delete state db key ;
            DNull
        | args ->
            fail args) )
  ; ( "DB::deleteAll"
    , InProcess
        (function
        | state, [DDB dbname] ->
            let db = find_db state.dbs dbname in
            User_db.delete_all state db ;
            DNull
        | args ->
            fail args) )
  ; ( "DB::update"
    , InProcess
        (function
        | state, [DObj vals; DDB dbname] ->
            let db = find_db state.dbs dbname in
            User_db.update state db vals ;
            DObj vals
        | args ->
            fail args) )
  ; ( "DB::fetchBy"
    , InProcess
        (function
        | state, [value; DStr field; DDB dbname] ->
            let db = find_db state.dbs dbname in
            let result = fetch_by_field ~state field value db in
            User_db.coerce_dlist_of_kv_pairs_to_legacy_object result
        | args ->
            fail args) )
  ; ( "DB::fetchOneBy"
    , InProcess
        (function
        | state, [value; DStr field; DDB dbname] ->
            let db = find_db state.dbs dbname in
            let result = fetch_by_field ~state field value db in
            ( match result with
            | DList (x :: xs) ->
              ( match x with
              | DList pair ->
                  User_db.coerce_key_value_pair_to_legacy_object pair
              | _ ->
                  Exception.internal "bad fetch" )
            | _ ->
                DNull )
        | args ->
            fail args) )
  ; ( "DB::fetchByMany"
    , InProcess
        (function
        | state, [(DObj _ as obj); DDB dbname] ->
            let db = find_db state.dbs dbname in
            obj
            |> User_db.query ~state db
            |> User_db.coerce_dlist_of_kv_pairs_to_legacy_object
        | args ->
            fail args) )
  ; ( "DB::fetchOneByMany"
    , InProcess
        (function
        | state, [(DObj _ as obj); DDB dbname] ->
            let db = find_db state.dbs dbname in
            let result = User_db.query ~state db obj in
            ( match result with
            | DList (x :: xs) ->
              ( match x with
              | DList pair ->
                  User_db.coerce_key_value_pair_to_legacy_object pair
              | _ ->
                  Exception.internal "bad fetch" )
            (* TODO(ian): Maybe/Option *)
            | _ ->
                DNull )
        | args ->
            fail args) )
  ; ( "DB::fetchAll"
    , InProcess
        (function
        | state, [DDB dbname] ->
            let db = find_db state.dbs dbname in
            let result = User_db.get_all ~state db in
            User_db.coerce_dlist_of_kv_pairs_to_legacy_object result
        | args ->
            fail args) )
  ; ( "DB::keys"
    , InProcess
        (function
        | state, [DDB dbname] ->
            let db = find_db state.dbs dbname in
            User_db.cols_for db
            |> List.map ~f:(fun (k, v) -> Dval.dstr_of_string_exn k)
            |> DList
        | args ->
            fail args) )
  ; ( "DB::schema"
    , InProcess
        (function
        | state, [DDB dbname] ->
            let db = find_db state.dbs dbname in
            User_db.cols_for db
            |> List.map ~f:(fun (k, v) ->
                   (k, Dval.dstr_of_string_exn (Dval.tipe_to_string v)) )
            |> Dval.to_dobj_exn
        | args ->
            fail args) ) ]
