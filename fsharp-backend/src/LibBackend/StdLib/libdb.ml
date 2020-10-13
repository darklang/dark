open Core_kernel
open Libexecution
open Lib
open Runtime
open Types.RuntimeT

let fns : fn list =
  [ { prefix_names = ["DB::insert"]
    ; infix_names = []
    ; parameters = [par "val" TObj; par "table" TDB]
    ; return_type = TObj
    ; description = "Insert `val` into `table`"
    ; func = InProcess (fun _ -> Exception.code "DB::insert is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::delete"]
    ; infix_names = []
    ; parameters = [par "value" TObj; par "table" TDB]
    ; return_type = TNull
    ; description = "Delete `value` from `table`"
    ; func = InProcess (fun _ -> Exception.code "DB::delete is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::deleteAll"]
    ; infix_names = []
    ; parameters = [par "table" TDB]
    ; return_type = TNull
    ; description = "Delete everything from `table`"
    ; func = InProcess (fun _ -> Exception.code "DB::deleteAll is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::update"]
    ; infix_names = []
    ; parameters = [par "value" TObj; par "table" TDB]
    ; return_type = TNull
    ; description = "Update `table` value which has the same ID as `value`"
    ; func = InProcess (fun _ -> Exception.code "DB::update is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::fetchBy"]
    ; infix_names = []
    ; parameters = [par "value" TAny; par "field" TStr; par "table" TDB]
    ; return_type = TList
    ; description = "Fetch all the values in `table` whose `field` is `value`"
    ; func = InProcess (fun _ -> Exception.code "DB::fetchBy is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::fetchOneBy"]
    ; infix_names = []
    ; parameters = [par "value" TAny; par "field" TStr; par "table" TDB]
    ; return_type = TAny
    ; description =
        "Fetch exactly one value in `table` whose `field` is `value`"
    ; func = InProcess (fun _ -> Exception.code "DB::fetchOneBy is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::fetchByMany"]
    ; infix_names = []
    ; parameters = [par "spec" TObj; par "table" TDB]
    ; return_type = TList
    ; description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has"
    ; func = InProcess (fun _ -> Exception.code "DB::fetchByMany is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::fetchOneByMany"]
    ; infix_names = []
    ; parameters = [par "spec" TObj; par "table" TDB]
    ; return_type = TAny
    ; description =
        "Fetch exactly one value from `table`, which have the same fields and values that `spec` has"
    ; func =
        InProcess (fun _ -> Exception.code "DB::fetchOneByMany is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::fetchAll"]
    ; infix_names = []
    ; parameters = [par "table" TDB]
    ; return_type = TList
    ; description = "Fetch all the values in `table`"
    ; func = InProcess (fun _ -> Exception.code "DB::fetchAll is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::keys"]
    ; infix_names = []
    ; parameters = [par "table" TDB]
    ; return_type = TList
    ; description = "Fetch all the keys in `table`"
    ; func = InProcess (fun _ -> Exception.code "DB::keys is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DB::schema"]
    ; infix_names = []
    ; parameters = [par "table" TDB]
    ; return_type = TObj
    ; description = "Fetch all the values in `table`"
    ; func = InProcess (fun _ -> Exception.code "DB::schema is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = true } ]
