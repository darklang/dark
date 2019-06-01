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


let replacements =
  [ ( "DB::insert"
    , InProcess (fun _ -> Exception.code "DB::insert is DEPRECATED") )
  ; ( "DB::delete"
    , InProcess (fun _ -> Exception.code "DB::delete is DEPRECATED") )
  ; ( "DB::deleteAll"
    , InProcess (fun _ -> Exception.code "DB::deleteAll is DEPRECATED") )
  ; ( "DB::update"
    , InProcess (fun _ -> Exception.code "DB::update is DEPRECATED") )
  ; ( "DB::fetchBy"
    , InProcess (fun _ -> Exception.code "DB::fetchBy is DEPRECATED") )
  ; ( "DB::fetchOneBy"
    , InProcess (fun _ -> Exception.code "DB::fetchOneBy is DEPRECATED") )
  ; ( "DB::fetchByMany"
    , InProcess (fun _ -> Exception.code "DB::fetchByMany is DEPRECATED") )
  ; ( "DB::fetchOneByMany"
    , InProcess (fun _ -> Exception.code "DB::fetchOneByMany is DEPRECATED") )
  ; ( "DB::fetchAll"
    , InProcess (fun _ -> Exception.code "DB::fetchAll is DEPRECATED") )
  ; ("DB::keys", InProcess (fun _ -> Exception.code "DB::keys is DEPRECATED"))
  ; ( "DB::schema"
    , InProcess (fun _ -> Exception.code "DB::schema is DEPRECATED") ) ]
