// open Tc

module FullstoryJs = {
  @val @scope(("window", "Dark", "fullstory")) external _setConsent: bool => unit = "setConsent"

  let setConsent = (allow: bool): Tea.Cmd.t<'msg> => Tea.Cmd.call(_ => _setConsent(allow))
}

@ppx.deriving(show) type rec t = {recordConsent: option<bool>}

@ppx.deriving(show)
type rec msg = SetRecordConsent(bool)

module Intent = {
  @ppx.deriving(show)
  type rec t<'msg> = RecordConsent(Tea.Cmd.t<'msg>)

  let map = (RecordConsent(cmd): t<'msg>, f: 'msg1 => 'msg2): t<'msg2> => RecordConsent(
    Tea.Cmd.map(f, cmd),
  )
}

let title = "Privacy"

let default = {recordConsent: None}

let update = (_: t, msg: msg): (t, Intent.t<msg>) => {
  switch msg {
  | SetRecordConsent(allow) => (
      {recordConsent: Some(allow)},
      RecordConsent(FullstoryJs.setConsent(allow)),
    )
  }
}
