open Types

let updateError = (fn: Error.t => Error.t, (m, cmd): (model, Tea.Cmd.t<msg>)) => (
  {...m, error: fn(m.error)},
  cmd,
)

let updateErrorMod = (fn: Error.t => Error.t): modification => ReplaceAllModificationsWithThisOne(
  m => updateError(fn, (m, Tea.Cmd.none)),
)
