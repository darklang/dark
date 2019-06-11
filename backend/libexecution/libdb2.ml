open Core_kernel
open Lib
open Runtime
open Types.RuntimeT

let fns : Lib.shortfn list =
  [ { pns = ["DB::set_v1"]
    ; ins = []
    ; p = [par "val" TObj; par "key" TStr; par "table" TDB]
    ; r = TObj
    ; d = "Upsert `val` into `table`, accessible by `key`"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::add_v0"]
    ; ins = []
    ; p = [par "val" TObj; par "table" TDB]
    ; r = TStr
    ; d =
        "Add `val` as a new entry into `table`, using a newly generated key. Returns the generated key."
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::get_v1"]
    ; ins = []
    ; p = [par "key" TStr; par "table" TDB]
    ; r = TOption
    ; d = "Finds a value in `table` by `key"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::getMany_v1"]
    ; ins = []
    ; p = [par "keys" TList; par "table" TDB]
    ; r = TList
    ; d =
        "Finds many values in `table` by `keys, returning a [[key, value]] list of lists"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::delete_v1"]
    ; ins = []
    ; p = [par "key" TStr; par "table" TDB]
    ; r = TNull
    ; d = "Delete `key` from `table`"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::deleteAll_v1"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TNull
    ; d = "Delete everything from `table`"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::query_v1"]
    ; ins = []
    ; p = [par "spec" TObj; par "table" TDB]
    ; r = TList
    ; d =
        "Fetch all the values from `table` which have the same fields and values that `spec` has
        , returning a [[key, value]] list of lists"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true (* see query_v2 *) }
  ; { pns = ["DB::query_v2"]
    ; ins = []
    ; p = [par "spec" TObj; par "table" TDB]
    ; r = TList
    ; d =
        "Fetch all the values from `table` which have the same fields and values that `spec` has, returning a list of values"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::query_v3"]
    ; ins = []
    ; p = [par "spec" TObj; par "table" TDB]
    ; r = TList
    ; d =
        "Fetch all the values from `table` which have the same fields and values that `spec` has, returning a list of values"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::queryWithKey_v1"]
    ; ins = []
    ; p = [par "spec" TObj; par "table" TDB]
    ; r = TList
    ; d =
        "Fetch all the values from `table` which have the same fields and values that `spec` has
        , returning a [[key, value]] list of lists"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::queryOne_v1"]
    ; ins = []
    ; p = [par "spec" TObj; par "table" TDB]
    ; r = TOption
    ; d =
        "Fetch exactly one value from `table` which have the same fields and values that `spec` has. Returns Nothing if none or more than 1 found"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::queryOneWithKey_v1"]
    ; ins = []
    ; p = [par "spec" TObj; par "table" TDB]
    ; r = TList
    ; d =
        "Fetch exactly one value table` which have the same fields and values that `spec` has, returning a [[key, value]] list of lists"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::getAll_v1"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TList
    ; d =
        "Fetch all the values in `table`. Returns a list of lists such that the inner
        lists are pairs of [key, value]. ie. [[key, value], [key, value]]"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::getAll_v2"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TList
    ; d = "Fetch all the values in `table`."
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::getAll_v3"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TList
    ; d = "Fetch all the values in `table`."
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::getAllWithKeys_v1"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TList
    ; d =
        "Fetch all the values in `table`. Returns a list of lists such that the inner
        lists are pairs of [key, value]. ie. [[key, value], [key, value]]"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::count"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TInt
    ; d = "Return the number of items stored in `table`."
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; (* previously called `DB::keys` *)
    { pns = ["DB::schemaFields_v1"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TList
    ; d = "Fetch all the fieldNames in `table`"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::schema_v1"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TObj
    ; d = "Returns an `Obj` representing { fieldName: fieldType } in `table`"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["DB::generateKey"]
    ; ins = []
    ; p = []
    ; r = TStr
    ; d = "Returns a random key suitable for use as a DB key"
    ; f =
        InProcess
          (function
          | _, [] ->
              Uuidm.v `V4 |> Uuidm.to_string |> Dval.dstr_of_string_exn
          | args ->
              fail args)
    ; ps = false
    ; dep = false } ]
