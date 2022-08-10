// open Tc

@ppx.deriving(show) type rec t = {recordConsent: option<bool>}

@ppx.deriving(show)
type rec msg = SetRecordConsent(bool)

let title = "Privacy"

let default = {recordConsent: None}

@ppx.deriving(show)
type rec effect = RecordConsent(bool)

let update = (_: t, msg: msg): (t, effect) => {
  switch msg {
  | SetRecordConsent(allow) => ({recordConsent: Some(allow)}, RecordConsent(allow))
  }
}
