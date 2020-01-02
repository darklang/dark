open Types
open Prelude
open Tc

let findByNameInList (name : string) (functions : function_ list) : function_ =
  functions
  |> List.find ~f:(fun f -> f.fnName = name)
  |> recoverOpt
       "findByNameInList"
       ~debug:name
       ~default:
         { fnName = "fnLookupError"
         ; fnParameters = []
         ; fnDescription = "default, fn error"
         ; fnReturnTipe = TError
         ; fnPreviewExecutionSafe = true
         ; fnInfix = false
         ; fnDeprecated = false }
