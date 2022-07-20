let updateError = (fn: Error.t => Error.t, (m, cmd): (AppTypes.model, AppTypes.cmd)) => (
  {...m, error: fn(m.error)},
  cmd,
)

let updateErrorMod = (
  fn: Error.t => Error.t,
): AppTypes.modification => ReplaceAllModificationsWithThisOne(
  m => updateError(fn, (m, Tea.Cmd.none)),
)
