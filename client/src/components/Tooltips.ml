(* NOTE: This will change in the future to pretty tool tips, this is just an inbetween state *)
open Prelude

let update (msg : toolTipMsg) : modification =
  let tooltip = match msg with Open tooltip -> tooltip | Close -> None in
  Many
    [ ReplaceAllModificationsWithThisOne
        (fun m -> ({m with tooltip}, Tea.Cmd.none)) ]
