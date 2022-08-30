// LoadStatus is a value to represent that something is being or has been loaded.

// This is deliberately simple, same as SaveStatus
//
// It is mostly used to indicate in the UI whether something is loading. If something
// has finished loading, the value is Success, with a value: that is the server-side
// value.
//
// LoadStatus is not a good fit to be a Msg, or to be a store of value, or to store
// errors.
//
// It is also not intended to be used as a value store for a form - a form is
// editable and so the state should be elsewhere so it can be updated. In this case,
// the old value can be useful to know if the user has changed it.
//
// It does not include an unintialized state -- use an Option to represent that.

@ppx.deriving(show)
type rec t<'result> =
  | Loading
  | Success('result)
  | Error
