open Core_kernel
open Libexecution
open Lib
open Runtime
open Types.RuntimeT

let fns : fn list =
  [ { name = fn "DB" "insert" 0

    ; parameters = [Param.make "val" TObj; Param.make "table" TDB]
    ; return_type = TObj
    ; description = "Insert `val` into `table`"
    ; func = InProcess (fun _ -> Exception.code "DB::insert is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "delete" 0

    ; parameters = [Param.make "value" TObj; Param.make "table" TDB]
    ; return_type = TNull
    ; description = "Delete `value` from `table`"
    ; func = InProcess (fun _ -> Exception.code "DB::delete is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "deleteAll" 0

    ; parameters = [Param.make "table" TDB]
    ; return_type = TNull
    ; description = "Delete everything from `table`"
    ; func = InProcess (fun _ -> Exception.code "DB::deleteAll is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "update" 0

    ; parameters = [Param.make "value" TObj; Param.make "table" TDB]
    ; return_type = TNull
    ; description = "Update `table` value which has the same ID as `value`"
    ; func = InProcess (fun _ -> Exception.code "DB::update is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "fetchBy" 0

    ; parameters = [Param.make "value" TAny; Param.make "field" TStr; Param.make "table" TDB]
    ; return_type = TList
    ; description = "Fetch all the values in `table` whose `field` is `value`"
    ; func = InProcess (fun _ -> Exception.code "DB::fetchBy is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "fetchOneBy" 0

    ; parameters = [Param.make "value" TAny; Param.make "field" TStr; Param.make "table" TDB]
    ; return_type = TAny
    ; description =
        "Fetch exactly one value in `table` whose `field` is `value`"
    ; func = InProcess (fun _ -> Exception.code "DB::fetchOneBy is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "fetchByMany" 0

    ; parameters = [Param.make "spec" TObj; Param.make "table" TDB]
    ; return_type = TList
    ; description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has"
    ; func = InProcess (fun _ -> Exception.code "DB::fetchByMany is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "fetchOneByMany" 0

    ; parameters = [Param.make "spec" TObj; Param.make "table" TDB]
    ; return_type = TAny
    ; description =
        "Fetch exactly one value from `table`, which have the same fields and values that `spec` has"
    ; func =
        InProcess (fun _ -> Exception.code "DB::fetchOneByMany is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "fetchAll" 0

    ; parameters = [Param.make "table" TDB]
    ; return_type = TList
    ; description = "Fetch all the values in `table`"
    ; func = InProcess (fun _ -> Exception.code "DB::fetchAll is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "keys" 0

    ; parameters = [Param.make "table" TDB]
    ; return_type = TList
    ; description = "Fetch all the keys in `table`"
    ; func = InProcess (fun _ -> Exception.code "DB::keys is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "schema" 0

    ; parameters = [Param.make "table" TDB]
    ; return_type = TObj
    ; description = "Fetch all the values in `table`"
    ; func = InProcess (fun _ -> Exception.code "DB::schema is DEPRECATED")
    ; preview_safety = Unsafe
    ; deprecated = ReplacedBy(fn "" "" 0) } ]
