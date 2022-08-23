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
  type rec values = option<string>

  @ppx.deriving(show)
  type rec saveStatus =
    | Saved
    | Saving
    | NotSaving

  @ppx.deriving(show)
  type rec loadStatus =
    | Loading
    | Loaded(Belt.Result.t<values, unit>)

  @ppx.deriving(show)
  type rec t = {
    loadStatus: loadStatus, // if not initialized, don't use it
    value: option<string>, // only overwrite if user has not entered
    error: option<string>,
    saveStatus: saveStatus,
  }

  @ppx.deriving(show)
  type rec msg =
    | LoadAPICallback(Tea.Result.t<values, Tea.Http.error<string>>)
    | InputEdit(string)
    | InputUnfocus
    | Submit
    // Track parameter in case user has changed it
    | SaveAPICallback(values, Tea.Result.t<bool, Tea.Http.error<string>>)

  module API = {
    let endpoint = "https://editor.darklang.com/api/tunnel"

    module Get = {
      type rec t = values
      let decode = (j: Js.Json.t): t => {
        open Json.Decode
        optional(string, j)
      }

      let callback = (v: Tea.Result.t<values, Tea.Http.error<string>>) => {
        LoadAPICallback(v)
      }

      let load = clientData =>
        APIFramework.editorGet(endpoint, ~callback, ~decoder=decode, clientData)
    }

    module Post = {
      module Params = {
        type rec t = values
        let encode = (v: values): Js.Json.t => {
          open Json.Encode
          nullable(string, v)
        }
      }
      type rec t = bool
      let decode = (j: Js.Json.t): t => {
        open Json.Decode
        bool(j)
      }

      let callback = (params, v: Tea.Result.t<t, Tea.Http.error<string>>) => {
        SaveAPICallback(params, v)
      }

      let save = params =>
        APIFramework.editorGet(endpoint, ~callback=callback(params), ~decoder=decode)
    }
  }

  module Intent = {
    @ppx.deriving(show)
    type rec t<'msg> = Cmd(APIFramework.Callable.t<'msg>) | NoIntent

    let map = (i: t<'msg>, f: 'msg1 => 'msg2): t<'msg2> => {
      switch i {
      | Cmd(c) => Cmd(APIFramework.Callable.map(c, f))
      | NoIntent => NoIntent
      }
    }
  }

  let default = {loadStatus: Loading, saveStatus: NotSaving, value: None, error: None}

  let validate = (host: string): result<values, string> => {
    if host == "" {
      Ok(None)
    } else {
      Ok(Some(host))
    }
  }

  let init = (clientData: APIFramework.clientData) => API.Get.load(clientData)

  let update = (state: t, msg: msg): (t, Intent.t<'msg>) => {
    let saveCmd = params => Intent.Cmd(API.Post.save(params))

    switch msg {
    | LoadAPICallback(Error(e)) =>
      let errorStr = Tea.Http.string_of_error(e)
      ({...state, error: Some(`Load error: ${errorStr}`), loadStatus: Loaded(Error())}, NoIntent)

    | LoadAPICallback(Ok(original)) => ({...state, loadStatus: Loaded(Ok(original))}, NoIntent)

    | InputEdit(tunnelHost) => ({...state, value: Some(tunnelHost)}, NoIntent)

    | InputUnfocus =>
      switch validate(state.value->Belt.Option.getWithDefault("")) {
      | Ok(validated) => ({...state, error: None, value: validated}, NoIntent)
      | Error(e) => ({...state, error: Some(e)}, NoIntent)
      }

    | Submit =>
      switch validate(state.value->Belt.Option.getWithDefault("")) {
      | Ok(validated) => ({...state, value: validated, saveStatus: Saving}, saveCmd(validated))
      | Error(e) => ({...state, error: Some(e)}, NoIntent)
      }

    | SaveAPICallback(_, Error(e)) =>
      let error = Some(Tea.Http.string_of_error(e))
      ({...state, error, saveStatus: NotSaving}, NoIntent)

    | SaveAPICallback(savedValue, Ok(true)) => (
        {...state, loadStatus: Loaded(Ok(savedValue)), error: None, saveStatus: Saved},
        NoIntent,
      )

    | SaveAPICallback(_, Ok(false)) => (
        {...state, error: Some("server rejected value"), saveStatus: NotSaving},
        NoIntent,
      )
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
  type rec intent = unit

  let update = (state: t, _msg: msg): (t, intent) => (state, ())
}

let title = "Contributing"

@ppx.deriving(show)
type rec t = {tunnelHost: TunnelHost.t, useAssets: UseAssets.t}

@ppx.deriving(show)
type rec msg =
  | TunnelHostMsg(TunnelHost.msg)
  | UseAssetsMsg(UseAssets.msg)

module Intent = {
  @ppx.deriving(show)
  type rec t<'msg> = TunnelHostIntent(TunnelHost.Intent.t<'msg>) | UseAssetsIntent(UseAssets.intent)

  let map = (i: t<'msg>, f: 'msg1 => 'msg2): t<'msg2> => {
    switch i {
    | TunnelHostIntent(i) => TunnelHostIntent(TunnelHost.Intent.map(i, f))
    | UseAssetsIntent(i) => UseAssetsIntent(i)
    }
  }
}

let default = {tunnelHost: TunnelHost.default, useAssets: UseAssets.default}

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

let init = clientData => Tea.Cmd.map(msg => TunnelHostMsg(msg), TunnelHost.init(clientData))

let update = (s: t, msg: msg): (t, Intent.t<msg>) =>
  switch msg {
  | UseAssetsMsg(msg) =>
    let (useAssets, intent) = UseAssets.update(s.useAssets, msg)
    ({...s, useAssets}, UseAssetsIntent(intent))
  | TunnelHostMsg(msg) =>
    let (tunnelHost, intent) = TunnelHost.update(s.tunnelHost, msg)
    let intent = TunnelHost.Intent.map(intent, msg => TunnelHostMsg(msg))
    ({...s, tunnelHost}, TunnelHostIntent(intent))

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
