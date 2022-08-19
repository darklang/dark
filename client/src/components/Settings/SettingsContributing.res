// open Tc

module Utils = SettingsUtils

// -------------------
// Tunnel URL form
// -------------------

// This is a single input box that we save/load via API
module TunnelHost = {
  // UX:
  // - when we load the page, we should load the tunnelHost setting
  //   - if we previously loaded it, use that value while loading
  //   - show an animation while loading
  //   - do not show the input until the load is completed
  //   - if loading fails, try again 2 more times
  //   - if loading completely fails, show the user, ask them to reload
  // - keep the loaded value so we know if we have to save
  // - when editing, update the model
  //   - when moving off the input, validate and show an error
  //   - when editing, clear the loading/saving indicators
  // - have a Save button
  //   - when saving, show indicator
  //   - after saving success, show the success icon until we edit again
  //   - if saving successful, show "reload" if in tunnel mode
  //   - if saving fails, show the error (same place)

  @ppx.deriving(show)
  type rec original = Original(string)
  @ppx.deriving(show)
  type rec current = Current(string)
  @ppx.deriving(show)
  type rec error = FormError(string)

  @ppx.deriving(show)
  type rec t =
    | Loading
    | LoadError(error, int)
    | Unedited(original)
    | Edited(original, current)
    | ValidationError(original, current, error)
    | Saving(original, current)
    | SaveError(original, current, error)

  let default = Loading

  @ppx.deriving(show)
  type rec msg =
    | Initialize
    | LoadApiCallback(Tea.Result.t<string, Tea.Http.error<string>>)
    | SaveAPICallback(Tea.Result.t<unit, Tea.Http.error<string>>)
    | InputUnfocus
    | InputEdit
    | Submit

  @ppx.deriving(show)
  type rec effect<'cmd> = Cmd('cmd)

  let loadTunnelUrl = "https://editor.darklang.com/api/tunnel"

  let update = (state: t, msg: msg): (t, effect<'cmd>) =>
    switch (state, msg) {
    | (Loading, Initialize) => Cmd(ApiFramework.get(loadTunnelUrl))
    | _ => (state, ())

    // | UpdateTunnelHostInput(value) => ({tunnelHost: {value: value, error: None}}, None)

    // | SubmitTunnelHostForm =>
    //   let param = if s.tunnelHost.value == "" {
    //     None
    //   } else {
    //     Some(s.tunnelHost.value)
    //   }
    //   (s, Some(RegisterTunnelHostAPICall(param)))
    }
}

// -------------------
// Toggle to use assets
// -------------------
module UseAssets = {
  @ppx.deriving(show)
  type rec t =
    | UseTunnelAssets
    | UseProductionAssets

  let default = UseProductionAssets

  @ppx.deriving(show)
  type rec msg = Set(t)

  @ppx.deriving(show)
  type rec effect = unit

  let update = (state: t, msg: msg): (t, effect) => (state, ())
}

let title = "Contributing"

@ppx.deriving(show)
type rec t = {tunnelHost: TunnelHost.t, useAssets: UseAssets.t}

let default = {tunnelHost: TunnelHost.default, useAssets: UseAssets.default}

@ppx.deriving(show)
type rec msg =
  | TunnelHostMsg(TunnelHost.msg)
  | UseAssetsMsg(UseAssets.msg)

@ppx.deriving(show)
type rec effect<'cmd> = TunnelHostEffect(TunnelHost.effect) | UseAssetsEffect(UseAssets.effect)
// Reload('cmd) | RegisterTunnelHostAPICall(option<string>)

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

let setTunnelQueryParam = (): unit => {
  modifySearchParamsAndReload(params => USP.set(params, tunnelQueryParamName, ""))
}

let clearTunnelQueryParam = (): unit => {
  modifySearchParamsAndReload(params => USP.delete(params, tunnelQueryParamName))
}

let update = (s: t, msg: msg): (t, option<effect<'cmd>>) =>
  switch msg {
  | UseAssetsMsg(msg) =>
    let (useAssets, effect) = UseAssets.update(s.useAssets, msg)
    ({...s, useAssets: useAssets}, Some(UseAssetsEffect(effect)))
  | TunnelHostMsg(msg) =>
    let (tunnelHost, effect) = TunnelHost.update(s.tunnelHost, msg)
    ({...s, tunnelHost: tunnelHost}, Some(TunnelHostEffect(effect)))

  // | UpdateTunnelHostInput(value) => ({tunnelHost: {value: value, error: None}}, None)

  // | SubmitTunnelHostForm =>
  //   let param = if s.tunnelHost.value == "" {
  //     None
  //   } else {
  //     Some(s.tunnelHost.value)
  //   }
  //   (s, Some(RegisterTunnelHostAPICall(param)))

  // | RegisterTunnelHostAPICallback(result) =>
  //   // Instantly reload with the new url
  //   switch result {
  //   | Tea.Result.Ok(true) => (s, Some(Reload(Tea.Cmd.call(_ => setTunnelQueryParam()))))
  //   | Tea.Result.Ok(false) => ({tunnelHost: {...s.tunnelHost, error: Some("Invalid url")}}, None)
  //   | Tea.Result.Error(e) => (
  //       {tunnelHost: {...s.tunnelHost, error: Some(Tea.Http.string_of_error(e))}},
  //       None,
  //     )
  //   }
  }
