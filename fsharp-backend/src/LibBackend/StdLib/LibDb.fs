open Core_kernel
open Libexecution
open Lib
open Runtime
open Types.RuntimeT

let fns : fn list =
  [ { name = fn "DB" "insert" 0

    ; parameters = [Param.make "val" TObj; Param.make "table" TDB]
    ; returnType = TObj
    ; description = "Insert `val` into `table`"
    ; fn =  (fun _ -> Exception.code "DB::insert is DEPRECATED")
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "delete" 0

    ; parameters = [Param.make "value" TObj; Param.make "table" TDB]
    ; returnType = TNull
    ; description = "Delete `value` from `table`"
    ; fn =  (fun _ -> Exception.code "DB::delete is DEPRECATED")
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "deleteAll" 0

    ; parameters = [Param.make "table" TDB]
    ; returnType = TNull
    ; description = "Delete everything from `table`"
    ; fn =  (fun _ -> Exception.code "DB::deleteAll is DEPRECATED")
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "update" 0

    ; parameters = [Param.make "value" TObj; Param.make "table" TDB]
    ; returnType = TNull
    ; description = "Update `table` value which has the same ID as `value`"
    ; fn =  (fun _ -> Exception.code "DB::update is DEPRECATED")
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "fetchBy" 0

    ; parameters = [Param.make "value" TAny; Param.make "field" TStr; Param.make "table" TDB]
    ; returnType = TList
    ; description = "Fetch all the values in `table` whose `field` is `value`"
    ; fn =  (fun _ -> Exception.code "DB::fetchBy is DEPRECATED")
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "fetchOneBy" 0

    ; parameters = [Param.make "value" TAny; Param.make "field" TStr; Param.make "table" TDB]
    ; returnType = TAny
    ; description =
        "Fetch exactly one value in `table` whose `field` is `value`"
    ; fn =  (fun _ -> Exception.code "DB::fetchOneBy is DEPRECATED")
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "fetchByMany" 0

    ; parameters = [Param.make "spec" TObj; Param.make "table" TDB]
    ; returnType = TList
    ; description =
        "Fetch all the values from `table` which have the same fields and values that `spec` has"
    ; fn =  (fun _ -> Exception.code "DB::fetchByMany is DEPRECATED")
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "fetchOneByMany" 0

    ; parameters = [Param.make "spec" TObj; Param.make "table" TDB]
    ; returnType = TAny
    ; description =
        "Fetch exactly one value from `table`, which have the same fields and values that `spec` has"
    ; fn =
         (fun _ -> Exception.code "DB::fetchOneByMany is DEPRECATED")
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "fetchAll" 0

    ; parameters = [Param.make "table" TDB]
    ; returnType = TList
    ; description = "Fetch all the values in `table`"
    ; fn =  (fun _ -> Exception.code "DB::fetchAll is DEPRECATED")
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "keys" 0

    ; parameters = [Param.make "table" TDB]
    ; returnType = TList
    ; description = "Fetch all the keys in `table`"
    ; fn =  (fun _ -> Exception.code "DB::keys is DEPRECATED")
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "DB" "schema" 0

    ; parameters = [Param.make "table" TDB]
    ; returnType = TObj
    ; description = "Fetch all the values in `table`"
    ; fn =  (fun _ -> Exception.code "DB::schema is DEPRECATED")
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) } ]
