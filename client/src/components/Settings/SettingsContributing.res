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

let tunnelQueryParamName = "use-assets-tunnel"

module USP = Webapi.Url.URLSearchParams

let getTunnelQueryParam = (): bool => {
  module L = Webapi.Dom.Location
  Webapi.Dom.location->L.search->USP.make->USP.has(tunnelQueryParamName)
}

let modifySearchParamsAndReload = (f: Webapi.Url.URLSearchParams.t => unit): unit => {
  module L = Webapi.Dom.Location

  let location = Webapi.Dom.location
  let searchParams = location->L.search->USP.make
  f(searchParams)
  L.setSearch(location, searchParams->USP.toString)
}

let setTunnelQueryParam = (host: string): unit => {
  modifySearchParamsAndReload(params => USP.set(params, tunnelQueryParamName, host))
}

let clearTunnelQueryParam = (): unit => {
  modifySearchParamsAndReload(params => USP.delete(params, tunnelQueryParamName))
}

let update = (s: t, msg: msg): (t, option<effect<'cmd>>) =>
  switch msg {
  | UpdateTunnelHostInput(value) => ({tunnelHost: {value, error: None}}, None)

  | SubmitTunnelHostForm =>
    let param = if s.tunnelHost.value == "" {
      None
    } else {
      Some(s.tunnelHost.value)
    }
    (s, Some(RegisterTunnelHostAPICall(param)))

  | RegisterTunnelHostAPICallback(result) =>
    // Instantly reload with the new url
    let setLocation = _ => setTunnelQueryParam("")

    switch result {
    | Tea.Result.Ok(true) => (s, Some(Reload(Tea.Cmd.call(setLocation))))
    | Tea.Result.Ok(false) => ({tunnelHost: {...s.tunnelHost, error: Some("Invalid url")}}, None)
    | Tea.Result.Error(e) => (
        {tunnelHost: {...s.tunnelHost, error: Some(Tea.Http.string_of_error(e))}},
        None,
      )
    }
  }
