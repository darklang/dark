open Core_kernel
open Libexecution.Lib
open Libexecution.Runtime
open Libexecution.Types.RuntimeT
module Exception = Libexecution.Exception
module Dval = Libexecution.Dval
module Unicode_string = Libexecution.Unicode_string

let find_db = Libdb.find_db

let replacements =
  [ ( "DB::add_v0"
    , InProcess
        (function
        | state, [DObj value; DDB dbname] ->
            let key = Uuidm.v `V4 |> Uuidm.to_string in
            let db = find_db state.dbs dbname in
            ignore (User_db.set ~state ~magic:false ~upsert:true db key value) ;
            Dval.dstr_of_string_exn key
        | args ->
            fail args) )
  ; ( "DB::set_v1"
    , InProcess
        (function
        | state, [DObj value; DStr key; DDB dbname] ->
            let db = find_db state.dbs dbname in
            let key = Unicode_string.to_string key in
            ignore (User_db.set ~state ~magic:false ~upsert:true db key value) ;
            DObj value
        | args ->
            fail args) )
  ; ( "DB::get_v1"
    , InProcess
        (function
        | state, [DStr key; DDB dbname] ->
          ( try
              let key = Unicode_string.to_string key in
              let db = find_db state.dbs dbname in
              DOption (OptJust (User_db.get ~state ~magic:false db key))
            with
          | Exception.DarkException e when e.tipe = Exception.DarkStorage ->
              DOption OptNothing
          | e ->
              raise e )
        | args ->
            fail args) )
  ; ( "DB::getMany_v1"
    , InProcess
        (function
        | state, [DList keys; DDB dbname] ->
            let db = find_db state.dbs dbname in
            let skeys =
              List.map
                ~f:(function
                  | DStr s ->
                      Unicode_string.to_string s
                  | t ->
                      Exception.user "Expected a string, got: "
                      ^ (t |> Dval.tipe_of |> Dval.tipe_to_string))
                keys
            in
            User_db.get_many ~state ~magic:false db skeys
        | args ->
            fail args) )
  ; ( "DB::delete_v1"
    , InProcess
        (function
        | state, [DStr key; DDB dbname] ->
            let db = find_db state.dbs dbname in
            let key = Unicode_string.to_string key in
            User_db.delete ~state db key ;
            DNull
        | args ->
            fail args) )
  ; ( "DB::deleteAll_v1"
    , InProcess
        (function
        | state, [DDB dbname] ->
            let db = find_db state.dbs dbname in
            User_db.delete_all state db ;
            DNull
        | args ->
            fail args) )
  ; ( "DB::query_v1"
    , InProcess
        (function
        | state, [(DObj _ as obj); DDB dbname] ->
            let db = find_db state.dbs dbname in
            User_db.query ~state ~magic:false db obj
        | args ->
            fail args) )
  ; ( "DB::query_v2"
    , InProcess
        (function
        | state, [(DObj _ as obj); DDB dbname] ->
            let results =
              let db = find_db state.dbs dbname in
              User_db.query ~state ~magic:false db obj
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
            fail args) )
  ; ( "DB::queryWithKey_v1"
    , InProcess
        (function
        | state, [(DObj _ as obj); DDB dbname] ->
            let db = find_db state.dbs dbname in
            User_db.query ~state ~magic:false db obj
        | args ->
            fail args) )
  ; ( "DB::queryOne_v1"
    , InProcess
        (function
        | state, [(DObj _ as obj); DDB dbname] ->
            let results =
              let db = find_db state.dbs dbname in
              User_db.query ~state ~magic:false db obj
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
            fail args) )
  ; ( "DB::queryOneWithKey_v1"
    , InProcess
        (function
        | state, [(DObj _ as obj); DDB dbname] ->
            let results =
              let db = find_db state.dbs dbname in
              User_db.query ~state ~magic:false db obj
            in
            ( match results with
            | DList [res] ->
                DOption (OptJust res)
            | _ ->
                DOption OptNothing )
        | args ->
            fail args) )
  ; ( "DB::getAll_v1"
    , InProcess
        (function
        | state, [DDB dbname] ->
            let db = find_db state.dbs dbname in
            User_db.get_all ~state ~magic:false db
        | args ->
            fail args) )
  ; ( "DB::getAll_v2"
    , InProcess
        (function
        | state, [DDB dbname] ->
            let results =
              let db = find_db state.dbs dbname in
              User_db.get_all ~state ~magic:false db
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
            fail args) )
  ; ( "DB::getAllWithKeys_v1"
    , InProcess
        (function
        | state, [DDB dbname] ->
            let db = find_db state.dbs dbname in
            User_db.get_all ~state ~magic:false db
        | args ->
            fail args) )
  ; ( "DB::count"
    , InProcess
        (function
        | state, [DDB dbname] ->
            let db = find_db state.dbs dbname in
            User_db.count ~state db |> DInt
        | args ->
            fail args) )
  ; (* previously called `DB::keys` *)
    ( "DB::schemaFields_v1"
    , InProcess
        (function
        | state, [DDB dbname] ->
            let db = find_db state.dbs dbname in
            User_db.cols_for db
            |> List.map ~f:(fun (k, v) -> Dval.dstr_of_string_exn k)
            |> DList
        | args ->
            fail args) )
  ; ( "DB::schema_v1"
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
