open Core_kernel
open Lib
open Runtime
open Types.RuntimeT

let fns : Lib.shortfn list =
  [ { pns = ["DB::insert"]
    ; ins = []
    ; p = [par "val" TObj; par "table" TDB]
    ; r = TObj
    ; d = "DEPRECATED: Insert `val` into `table`"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::delete"]
    ; ins = []
    ; p = [par "value" TObj; par "table" TDB]
    ; r = TNull
    ; d = "DEPRECATED: Delete `value` from `table`"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::deleteAll"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TNull
    ; d = "DEPRECATED: Delete everything from `table`"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::update"]
    ; ins = []
    ; p = [par "value" TObj; par "table" TDB]
    ; r = TNull
    ; d = "DEPRECATED: Update `table` value which has the same ID as `value`"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::fetchBy"]
    ; ins = []
    ; p = [par "value" TAny; par "field" TStr; par "table" TDB]
    ; r = TList
    ; d =
        "DEPRECATED: Fetch all the values in `table` whose `field` is `value`"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::fetchOneBy"]
    ; ins = []
    ; p = [par "value" TAny; par "field" TStr; par "table" TDB]
    ; r = TAny
    ; d =
        "DEPRECATED: Fetch exactly one value in `table` whose `field` is `value`"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::fetchByMany"]
    ; ins = []
    ; p = [par "spec" TObj; par "table" TDB]
    ; r = TList
    ; d =
        "DEPRECATED: Fetch all the values from `table` which have the same fields and values that `spec` has"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::fetchOneByMany"]
    ; ins = []
    ; p = [par "spec" TObj; par "table" TDB]
    ; r = TAny
    ; d =
        "DEPRECATED: Fetch exactly one value from `table`, which have the samea fields and values that `spec` has"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::fetchAll"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TList
    ; d = "DEPRECATED: Fetch all the values in `table`"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::keys"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TList
    ; d = "DEPRECATED: Fetch all the keys in `table`"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["DB::schema"]
    ; ins = []
    ; p = [par "table" TDB]
    ; r = TObj
    ; d = "DEPRECATED: Fetch all the values in `table`"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true } ]
