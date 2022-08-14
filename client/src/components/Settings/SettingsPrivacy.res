// open Tc

// TODO describe file/module

module FullstoryJs = {
  // I assume this is some JS interop calling `window.Dark.fullstory`
  // hmmm: find the ReScript documentatino for this
  @val
  @scope(("window", "Dark", "fullstory"))
  external _setConsent: bool => unit = "setConsent"

  let setConsent = (allow: bool): 'cmd => Tea.Cmd.call(_ => _setConsent(allow))
}

@ppx.deriving(show)
type rec t = {recordConsent: option<bool>}

@ppx.deriving(show)
type rec msg = SetRecordConsent(bool)

let title = "Privacy"

let default = {recordConsent: None}

@ppx.deriving(show)
type rec effect<'cmd> = RecordConsent('cmd)

let update = (_: t, msg: msg): (t, effect<'cmd>) => {
  switch msg {
  | SetRecordConsent(allow) => (
      {recordConsent: Some(allow)},
      RecordConsent(FullstoryJs.setConsent(allow)),
    )
  }
}
