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
  //   - show an animation while loading
  // - (TODO) show an indicator when the value is unsaved
  // - when editing, update the model
  //   - when moving off the input, validate and show an error
  //   - don't save until the user presses save
  // - have a Save button
  //   - when saving, show indicator
  //   - after saving success, show the success icon until we edit again
  //   - if saving fails, show the error (same place)

  @ppx.deriving(show)
  type rec values = option<string>

  @ppx.deriving(show)
  type rec t = {
    loadStatus: LoadStatus.t<values>, // if not initialized, don't use it
    value: option<string>, // only overwrite if user has not entered
    error: option<string>,
    saveStatus: SaveStatus.t,
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
    module JD = Json.Decode
    module JE = Json.Encode
    let endpoint = "tunnel"

    let load = clientData =>
      APIFramework.editorGet(
        endpoint,
        ~callback=v => LoadAPICallback(v),
        ~decoder=JD.optional(JD.string),
        clientData,
      )

    let save = params =>
      APIFramework.editorPost(
        endpoint,
        ~userAPI=true,
        ~params,
        ~encoder=JE.nullable(JE.string),
        ~callback=v => SaveAPICallback(params, v),
        ~decoder=JD.bool,
      )
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

  let default = {
    loadStatus: LoadStatus.Loading,
    saveStatus: NotSaving,
    value: None,
    error: None,
  }

  let validate = (host: string): result<values, string> => {
    let host = String.trim(host)
    if host == "" {
      Ok(None)
    } else if Tc.String.startsWith(host, ~prefix="http://") {
      Ok(Some(Tc.String.dropLeft(~count=7, host)))
    } else if Tc.String.startsWith(host, ~prefix="https://") {
      Ok(Some(Tc.String.dropLeft(~count=8, host)))
    } else {
      Ok(Some(host))
    }
  }

  let init = (clientData: APIFramework.clientData) => API.load(clientData)

  let update = (state: t, msg: msg): (t, Intent.t<'msg>) => {
    switch msg {
    | LoadAPICallback(Error(e)) =>
      let errorStr = Tea.Http.string_of_error(e)
      ({...state, error: Some(`Load error: ${errorStr}`), loadStatus: LoadStatus.Error}, NoIntent)

    | LoadAPICallback(Ok(original)) => (
        {...state, value: original, loadStatus: LoadStatus.Success(original)},
        NoIntent,
      )

    | InputEdit(tunnelHost) =>
      let saveStatus = switch state.saveStatus {
      | Saved => SaveStatus.NotSaving
      | Saving | NotSaving => state.saveStatus
      }
      ({...state, value: Some(tunnelHost), saveStatus: saveStatus}, NoIntent)

    | InputUnfocus =>
      switch validate(state.value->Belt.Option.getWithDefault("")) {
      | Ok(validated) => ({...state, error: None, value: validated}, NoIntent)
      | Error(e) => ({...state, error: Some(e)}, NoIntent)
      }

    | Submit =>
      let saveCmd = params => Intent.Cmd(API.save(params))
      switch validate(state.value->Belt.Option.getWithDefault("")) {
      | Ok(validated) => ({...state, value: validated, saveStatus: Saving}, saveCmd(validated))
      | Error(e) => ({...state, error: Some(e)}, NoIntent)
      }

    | SaveAPICallback(_, Error(e)) =>
      let error = Some(Tea.Http.string_of_error(e))
      ({...state, error: error, saveStatus: NotSaving}, NoIntent)

    | SaveAPICallback(savedValue, Ok(true)) => (
        {...state, loadStatus: LoadStatus.Success(savedValue), error: None, saveStatus: Saved},
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
  // Unlike most parts of the app, this component works by directly getting and
  // setting a query param in the URL

  @ppx.deriving(show)
  type rec t =
    | UseTunnelAssets
    | UseProductionAssets

  @ppx.deriving(show)
  type rec msg = Toggle | ReadValue

  @ppx.deriving(show)
  type rec intent = unit

  module QueryParam = {
    module USP = Webapi.Url.URLSearchParams
    module L = Webapi.Dom.Location

    let name = "use-assets-tunnel"

    let get = (): t => {
      if Webapi.Dom.location->L.search->USP.make->USP.has(name) {
        UseTunnelAssets
      } else {
        UseProductionAssets
      }
    }

    let modifyAndReload = (f: USP.t => unit): unit => {
      let location = Webapi.Dom.location
      let searchParams = location->L.search->USP.make
      f(searchParams)
      L.setSearch(location, searchParams->USP.toString)
    }

    let set = (): unit => {
      modifyAndReload(params => USP.set(params, name, ""))
    }

    let clear = (): unit => {
      modifyAndReload(params => USP.delete(params, name))
    }
  }

  let default = UseProductionAssets

  let init = () => Tea.Cmd.msg(ReadValue)

  let update = (state: t, msg: msg): (t, intent) =>
    switch msg {
    | ReadValue => (QueryParam.get(), ())
    | Toggle =>
      switch state {
      | UseTunnelAssets =>
        QueryParam.clear() // reloads page
        (UseProductionAssets, ())
      | UseProductionAssets =>
        QueryParam.set() // reloads page
        (UseTunnelAssets, ())
      }
    }
}

// -------------------
// Toggle to show contributor UI
// -------------------
module Tools = {
  @ppx.deriving(show)
  type rec t = {showSidebarPanel: bool}

  @ppx.deriving(show)
  type rec msg = SetSidebarPanel(bool)

  @ppx.deriving(show)
  type rec intent = unit

  let default = {showSidebarPanel: false}

  let toSaved = ({showSidebarPanel}: t) => {
    open Json.Encode
    object_(list{("showSidebarPanel", bool(showSidebarPanel))})
  }
  let fromSaved = (j: Js.Json.t) => {
    open Json.Decode
    {
      showSidebarPanel: field("showSidebarPanel", bool, j),
    }
  }

  let init = () => Tea.Cmd.none

  let update = (_: t, msg: msg): (t, intent) =>
    switch msg {
    | SetSidebarPanel(v) => ({showSidebarPanel: v}, ())
    }
}

let title = "Contributing"

@ppx.deriving(show)
type rec t = {tunnelHost: TunnelHost.t, useAssets: UseAssets.t, tools: Tools.t}

@ppx.deriving(show)
type rec msg =
  | TunnelHostMsg(TunnelHost.msg)
  | UseAssetsMsg(UseAssets.msg)
  | ToolsMsg(Tools.msg)

module Intent = {
  @ppx.deriving(show)
  type rec t<'msg> =
    | TunnelHostIntent(TunnelHost.Intent.t<'msg>)
    | UseAssetsIntent(UseAssets.intent)
    | ToolsIntent(Tools.intent)

  let map = (i: t<'msg>, f: 'msg1 => 'msg2): t<'msg2> => {
    switch i {
    | TunnelHostIntent(i) => TunnelHostIntent(TunnelHost.Intent.map(i, f))
    | UseAssetsIntent(i) => UseAssetsIntent(i)
    | ToolsIntent(i) => ToolsIntent(i)
    }
  }
}

let default = {
  tunnelHost: TunnelHost.default,
  useAssets: UseAssets.default,
  tools: Tools.default,
}

let toSaved = ({tools, _}: t) => {
  open Json.Encode
  object_(list{("tools", Tools.toSaved(tools))})
}

let fromSaved = (j: Js.Json.t) => {
  open Json.Decode
  {
    ...default,
    tools: field("tools", Tools.fromSaved, j),
  }
}

let init = clientData =>
  Tea.Cmd.batch(list{
    Tea.Cmd.map(msg => TunnelHostMsg(msg), TunnelHost.init(clientData)),
    Tea.Cmd.map(msg => UseAssetsMsg(msg), UseAssets.init()),
  })

let update = (s: t, msg: msg): (t, Intent.t<msg>) =>
  switch msg {
  | UseAssetsMsg(msg) =>
    let (useAssets, intent) = UseAssets.update(s.useAssets, msg)
    ({...s, useAssets: useAssets}, UseAssetsIntent(intent))
  | TunnelHostMsg(msg) =>
    let (tunnelHost, intent) = TunnelHost.update(s.tunnelHost, msg)
    let intent = TunnelHost.Intent.map(intent, msg => TunnelHostMsg(msg))
    ({...s, tunnelHost: tunnelHost}, TunnelHostIntent(intent))
  | ToolsMsg(msg) =>
    let (tools, intent) = Tools.update(s.tools, msg)
    ({...s, tools: tools}, ToolsIntent(intent))
  }
