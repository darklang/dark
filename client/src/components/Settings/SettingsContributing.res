// open Tc
open BaseTypes

module Utils = SettingsUtils

let title = "Contributing"
@ppx.deriving(show)
type rec t = {tunnelHost: Utils.formField}

let default = {tunnelHost: Utils.defaultFormField}

@ppx.deriving(show)
type rec msg =
  | UpdateTunnelHostInput(string)
  | SubmitTunnelHostForm
  | RegisterTunnelHostAPICallback(Tea_result.t<APITunnelHost.t, Prelude.Tea.Http.error<string>>)

@ppx.deriving(show)
type rec effect<'cmd> = Reload('cmd) | RegisterTunnelHostAPICall(option<string>)

let update = (s: t, msg: msg): (t, option<effect<'cmd>>) =>
  switch msg {
  | UpdateTunnelHostInput(value) => ({tunnelHost: {value: value, error: None}}, None)

  | SubmitTunnelHostForm =>
    let param = if s.tunnelHost.value == "" {
      None
    } else {
      Some(s.tunnelHost.value)
    }
    (s, Some(RegisterTunnelHostAPICall(param)))

  | RegisterTunnelHostAPICallback(result) =>
    let setLocation = _ => {
      // Instantly reload with the new url
      module L = Webapi.Dom.Location
      let location = Webapi.Dom.location // This is the window.dom.location object

      let search = location->L.search
      let search = if search == "" {
        ""
      } else {
        search ++ "&"
      }
      let newSearch = `${search}use-assets-tunnel`

      L.setSearch(location, newSearch)
    }

    switch result {
    | Tea.Result.Ok(true) => (s, Some(Reload(Tea.Cmd.call(setLocation))))
    | Tea.Result.Ok(false) => ({tunnelHost: {...s.tunnelHost, error: Some("Invalid url")}}, None)
    | Tea.Result.Error(e) => (
        {tunnelHost: {...s.tunnelHost, error: Some(Tea.Http.string_of_error(e))}},
        None,
      )
    }
  }
