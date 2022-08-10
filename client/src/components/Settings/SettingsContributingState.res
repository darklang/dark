// open Tc

module Utils = SettingsUtils

let title = "Contributing"
@ppx.deriving(show)
type rec t = {url: Utils.formField}

type rec msg =
  | UpdateTunnelForm(string)
  | SubmitTunnelForm

let update = (s: t, msg: msg): t =>
  switch msg {
  | UpdateTunnelForm(value) => {url: {value: value, error: None}}
  | SubmitTunnelForm => s
  }
