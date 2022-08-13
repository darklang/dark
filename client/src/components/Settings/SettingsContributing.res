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
    let setLocationFn = _ => {
      module L = Webapi.Dom.Location
      let location = Webapi.Dom.location // This is the window.dom.location object

      let search = location->L.search
      let search = if search == "" {
        ""
      } else {
        search ++ "&"
      }
      let tunnelUrl = Js.Global.encodeURIComponent(s.tunnelUrl.value)
      let newSearch = `${search}localhost-assets=${tunnelUrl}`

      L.setSearch(location, newSearch)
    }
    // Work out the URL late in case anything has changed since creating it
    (s, Some(Reload(Tea.Cmd.call(setLocationFn))))
  }
