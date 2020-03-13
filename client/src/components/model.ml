open Types

let updateError (fn : Error.t -> Error.t) ((m, cmd) : model * msg Tea.Cmd.t) =
  ({m with error = fn m.error}, cmd)


let updateErrorMod (fn : Error.t -> Error.t) : modification =
  ReplaceAllModificationsWithThisOne (fun m -> updateError fn (m, Tea.Cmd.none))
