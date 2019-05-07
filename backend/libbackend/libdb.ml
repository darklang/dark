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
    , InProcess (fun _ -> Exception.user "DB::insert is DEPRECATED") )
  ; ( "DB::delete"
    , InProcess (fun _ -> Exception.user "DB::delete is DEPRECATED") )
  ; ( "DB::deleteAll"
    , InProcess (fun _ -> Exception.user "DB::deleteAll is DEPRECATED") )
  ; ( "DB::update"
    , InProcess (fun _ -> Exception.user "DB::update is DEPRECATED") )
  ; ( "DB::fetchBy"
    , InProcess (fun _ -> Exception.user "DB::fetchBy is DEPRECATED") )
  ; ( "DB::fetchOneBy"
    , InProcess (fun _ -> Exception.user "DB::fetchOneBy is DEPRECATED") )
  ; ( "DB::fetchByMany"
    , InProcess (fun _ -> Exception.user "DB::fetchByMany is DEPRECATED") )
  ; ( "DB::fetchOneByMany"
    , InProcess (fun _ -> Exception.user "DB::fetchOneByMany is DEPRECATED") )
  ; ( "DB::fetchAll"
    , InProcess (fun _ -> Exception.user "DB::fetchAll is DEPRECATED") )
  ; ("DB::keys", InProcess (fun _ -> Exception.user "DB::keys is DEPRECATED"))
  ; ( "DB::schema"
    , InProcess (fun _ -> Exception.user "DB::schema is DEPRECATED") ) ]
