// open Tc

module Utils = SettingsUtils

let title = "Contributing"
@ppx.deriving(show)
type rec t = {tunnelUrl: Utils.formField}

let default = {tunnelUrl: Utils.defaultFormField}

@ppx.deriving(show)
type rec msg =
  | UpdateTunnelForm(string)
  | SubmitTunnelForm

@ppx.deriving(show)
type rec effect<'cmd> = Reload('cmd)

let update = (s: t, msg: msg): (t, option<effect<'cmd>>) =>
  switch msg {
  | UpdateTunnelForm(value) => ({tunnelUrl: {value: value, error: None}}, None)

  | SubmitTunnelForm =>
    module L = Webapi.Dom.Location
    // let location = Tea.Navigation.getLocation()
    let location = Webapi.Dom.location
    let search = location->L.search
    let newSearch = `${search}&x=y`

    (s, Some(Reload(Tea.Cmd.call(_ => L.setSearch(location, newSearch)))))
  }
