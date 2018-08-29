open Core_kernel

open Lib
open Runtime
open Types.RuntimeT


let fns : Lib.shortfn list = [
  { pns = ["DB::set_v1"]
  ; ins = []
  ; p = [par "val" TObj; par "key" TStr; par "table" TDB]
  ; r = TObj
  ; d = "Upsert `val` into `table`, accessible by `key`"
  ; f = NotClientAvailable
  ; pr = None
  ; ps = false
  ; dep = false
  }
  ;

  { pns = ["DB::get_v1"]
  ; ins = []
  ; p = [par "key" TStr; par "table" TDB]
  ; r = TOption
  ; d = "Finds a value in `table` by `key"
  ; f = NotClientAvailable
  ; pr = None
  ; ps = true
  ; dep = false
  }
  ;

  { pns = ["DB::getMany_v1"]
  ; ins = []
  ; p = [par "keys" TList; par "table" TDB]
  ; r = TList
  ; d = "Finds many values in `table` by `keys, returning a [[key, value]] list of lists"
  ; f = NotClientAvailable
  ; pr = None
  ; ps = true
  ; dep = false
  }
  ;

  { pns = ["DB::delete_v1"]
  ; ins = []
  ; p = [par "key" TStr; par "table" TDB]
  ; r = TNull
  ; d = "Delete `key` from `table`"
  ; f = NotClientAvailable
  ; pr = None
  ; ps = false
  ; dep = false
  }
  ;

  { pns = ["DB::deleteAll_v1"]
  ; ins = []
  ; p = [par "table" TDB]
  ; r = TNull
  ; d = "Delete everything from `table`"
  ; f = NotClientAvailable
  ; pr = None
  ; ps = false
  ; dep = false
  }
  ;

  { pns = ["DB::query_v1"]
  ; ins = []
  ; p = [par "spec" TObj; par "table" TDB]
  ; r = TList
  ; d = "Fetch all the values from `table` which have the same fields and values that `spec` has
        , returning a [[key, value]] list of lists"
  ; f = NotClientAvailable
  ; pr = None
  ; ps = true
  ; dep = false
  }
  ;

  { pns = ["DB::getAll_v1"]
  ; ins = []
  ; p = [par "table" TDB]
  ; r = TList
  ; d = "Fetch all the values in `table`. Returns a list of lists such that the inner
        lists are pairs of [key, value]. ie. [[key, value], [key, value]]"
  ; f = NotClientAvailable
  ; pr = None
  ; ps = true
  ; dep = false
  }
  ;

  (* previously called `DB::keys` *)
  { pns = ["DB::schemaFields_v1"]
  ; ins = []
  ; p = [par "table" TDB]
  ; r = TList
  ; d = "Fetch all the fieldNames in `table`"
  ; f = NotClientAvailable
  ; pr = None
  ; ps = true
  ; dep = false
  }
  ;

  { pns = ["DB::schema_v1"]
  ; ins = []
  ; p = [par "table" TDB]
  ; r = TObj
  ; d = "Returns an `Obj` representing { fieldName: fieldType } in `table`"
  ; f = NotClientAvailable
  ; pr = None
  ; ps = true
  ; dep = false
  }
]
