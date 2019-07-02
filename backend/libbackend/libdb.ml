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


let fns : Lib.shortfn list =
  [ { pns = ["DB::insert"]
    ; ins = []
    ; p = [par "val" TObj; par "table" TDB]
    ; r = TObj
    ; d = "Insert `val` into `table`"
    ; f = InProcess (fun _ -> Exception.code "DB::insert is DEPRECATED")
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::delete"]
    ; ins = []
    ; p = [par "value" TObj; par "table" TDB]
    ; r = TNull
    ; d = "Delete `value` from `table`"
    ; f = InProcess (fun _ -> Exception.code "DB::delete is DEPRECATED")
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::deleteAll"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TNull
    ; d = "Delete everything from `table`"
    ; f = InProcess (fun _ -> Exception.code "DB::deleteAll is DEPRECATED")
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::update"]
    ; ins = []
    ; p = [par "value" TObj; par "table" TDB]
    ; r = TNull
    ; d = "Update `table` value which has the same ID as `value`"
    ; f = InProcess (fun _ -> Exception.code "DB::update is DEPRECATED")
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::fetchBy"]
    ; ins = []
    ; p = [par "value" TAny; par "field" TStr; par "table" TDB]
    ; r = TList
    ; d = "Fetch all the values in `table` whose `field` is `value`"
    ; f = InProcess (fun _ -> Exception.code "DB::fetchBy is DEPRECATED")
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::fetchOneBy"]
    ; ins = []
    ; p = [par "value" TAny; par "field" TStr; par "table" TDB]
    ; r = TAny
    ; d = "Fetch exactly one value in `table` whose `field` is `value`"
    ; f = InProcess (fun _ -> Exception.code "DB::fetchOneBy is DEPRECATED")
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::fetchByMany"]
    ; ins = []
    ; p = [par "spec" TObj; par "table" TDB]
    ; r = TList
    ; d =
        "Fetch all the values from `table` which have the same fields and values that `spec` has"
    ; f = InProcess (fun _ -> Exception.code "DB::fetchByMany is DEPRECATED")
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::fetchOneByMany"]
    ; ins = []
    ; p = [par "spec" TObj; par "table" TDB]
    ; r = TAny
    ; d =
        "Fetch exactly one value from `table`, which have the same fields and values that `spec` has"
    ; f = InProcess (fun _ -> Exception.code "DB::fetchOneByMany is DEPRECATED")
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::fetchAll"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TList
    ; d = "Fetch all the values in `table`"
    ; f = InProcess (fun _ -> Exception.code "DB::fetchAll is DEPRECATED")
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::keys"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TList
    ; d = "Fetch all the keys in `table`"
    ; f = InProcess (fun _ -> Exception.code "DB::keys is DEPRECATED")
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::schema"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TObj
    ; d = "Fetch all the values in `table`"
    ; f = InProcess (fun _ -> Exception.code "DB::schema is DEPRECATED")
    ; ps = false
    ; dep = true } ]
