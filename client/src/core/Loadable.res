// Deprecated, use loadStatus.t instead. Loadable included the storage of the value,
// which makes it non-generic, but also stores more info than we need (which gets in the way)
@ppx.deriving(show)
type rec t<'result, 'error> =
  | NotInitialized
  | Loading(option<'result>)
  | Success('result)
  | Error('error)
