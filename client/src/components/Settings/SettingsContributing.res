// open Tc
open Belt_extended
module Utils = SettingsUtils

// -------------------
// Tunnel URL form
// -------------------

// This is a single input box that we save/load via API
module TunnelHost = {
  // UX:
  // - when we load the page, we should load the tunnelHost setting
  //   - (maybe) if we previously loaded it, use that value while loading
  //   - show an animation while loading
  //   - do not show the input box until the load is completed
  // - show an indicator when the value is unsaved
  // - when editing, update the model
  //   - when moving off the input, validate and show an error
  //   - don't save until the user presses save
  // - have a Save button
  //   - when saving, show indicator
  //   - after saving success, show the success icon until we edit again
  //   - if saving successful, show "reload" if in tunnel mode
  //   - if saving fails, show the error (same place)

  @ppx.deriving(show)
  type rec saveStatus =
    | Saved
    | Saving
    | NotSaving

  @ppx.deriving(show)
  type rec loadStatus =
    | Loading
    | Loaded(Belt.Result.t<string, unit>)

  @ppx.deriving(show)
  type rec t = {
    loadStatus: loadStatus, // if not initialized, don't use it
    value: option<string>, // only overwrite if user has not entered
    error: option<string>,
    saveStatus: saveStatus,
  }

  @ppx.deriving(show)
  type rec msg =
    | LoadAPICallback(Tea.Result.t<string, Tea.Http.error<string>>)
    | InputEdit(string)
    | InputUnfocus
    | Submit
    // Track parameter in case user has changed it
    | SaveAPICallback(string, Tea.Result.t<unit, Tea.Http.error<string>>)

  @ppx.deriving(show)
  type rec effect<'cmd> = Cmd('cmd) | NoEffect

  let init = () => (
    {loadStatus: Loading, saveStatus: NotSaving, value: None, error: None},
    Cmd(NoEffect),
  )

  let loadTunnelUrl = "https://editor.darklang.com/api/tunnel"

  let validate = (host: string): result<string, string> => Ok(host)

  let update = (state: t, msg: msg): (t, effect<'cmd>) => {
    let loadCmd = NoEffect // Some(Cmd(ApiFramework.get(loadTunnelUrl)))
    let saveCmd = NoEffect // Some(Cmd(ApiFramework.get(loadTunnelUrl)))

    switch msg {
    | LoadAPICallback(Error(e)) =>
      let errorStr = Tea.Http.string_of_error(e)
      ({...state, error: Some(`Load error: ${errorStr}`), loadStatus: Loaded(Error())}, NoEffect)

    | LoadAPICallback(Ok(original)) => ({...state, loadStatus: Loaded(Ok(original))}, NoEffect)

    | InputEdit(tunnelHost) => ({...state, value: Some(tunnelHost)}, NoEffect)

    | InputUnfocus =>
      switch validate(state.value->Belt.Option.getWithDefault("")) {
      | Ok(validated) => ({...state, error: None, value: Some(validated)}, NoEffect)
      | Error(e) => ({...state, error: Some(e)}, NoEffect)
      }

    | Submit =>
      switch validate(state.value->Belt.Option.getWithDefault("")) {
      | Ok(validated) => ({...state, saveStatus: Saving}, saveCmd)
      | Error(e) => ({...state, error: Some(e)}, NoEffect)
      }

    | SaveAPICallback(_, Error(e)) =>
      let error = Some(Tea.Http.string_of_error(e))
      ({...state, error, saveStatus: NotSaving}, NoEffect)

    | SaveAPICallback(savedValue, Ok()) => ({...state, error: None, saveStatus: Saved}, NoEffect)
    }
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
type rec effect<'cmd> =
  TunnelHostEffect(TunnelHost.effect<'cmd>) | UseAssetsEffect(UseAssets.effect)
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

let update = (s: t, msg: msg): (t, effect<'cmd>) =>
  switch msg {
  | UseAssetsMsg(msg) =>
    let (useAssets, effect) = UseAssets.update(s.useAssets, msg)
    ({...s, useAssets}, UseAssetsEffect(effect))
  | TunnelHostMsg(msg) =>
    let (tunnelHost, effect) = TunnelHost.update(s.tunnelHost, msg)
    ({...s, tunnelHost}, TunnelHostEffect(effect))

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
