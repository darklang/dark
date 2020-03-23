open Prelude

(* Returns the function named `name`. Returns Nothing if the function
  * can't be found - this shouldn't happen in theory but often does
  * in practice; for example, someone might delete a function and
  * then do a local undo. *)
let findByNameInList (name : string) (functions : function_ list) :
    function_ option =
  List.find functions ~f:(fun f -> f.fnName = name)
