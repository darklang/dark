open Prelude

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
         ; fnPreviewSafety = Safe
         ; fnInfix = false
         ; fnDeprecated = false
         ; fnOrigin =
             (* whenever this happens it's almost certainly a user
              * function *)
             UserFunction }
