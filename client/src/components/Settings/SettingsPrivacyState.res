// open Tc

@ppx.deriving(show) type rec t = {recordConsent: option<bool>}

@ppx.deriving(show)
type rec msg =
  | InitRecordConsent(option<bool>)
  | SetRecordConsent(bool)

let title = "Privacy"

let default = {recordConsent: None}

let init = () => default
