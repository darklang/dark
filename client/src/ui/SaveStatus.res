@ppx.deriving(show)
type rec t =
  | Saved
  | Saving
  // This does not mean the value has not been saved - it may not be necessary to save it
  | NotSaving
