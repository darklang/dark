// open Tc

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
  type rec error = LoadError(string) | SaveError(string) | ValidationError(string)
  @ppx.deriving(show)
  type rec original = Original(string)
  @ppx.deriving(show)
  type rec local = Local(string)

  @ppx.deriving(show)
  type rec t =
    | Loading
    // Track original to show whether value has been saved
    | Editing(option<error>, original, local)
    // Track original in case saving fails
    | Saving(original, local)
    // local and server value are the same
    | Saved(string)

  let default = Loading

  @ppx.deriving(show)
  type rec msg =
    | Initialize
    | LoadAPICallback(Tea.Result.t<string, Tea.Http.error<string>>)
    | InputEdit(string)
    | InputUnfocus
    | Submit
    // Track parameter in case user has changed it
    | SaveAPICallback(string, Tea.Result.t<unit, Tea.Http.error<string>>)

  @ppx.deriving(show)
  type rec effect<'cmd> = Cmd('cmd) | NoEffect

  let loadTunnelUrl = "https://editor.darklang.com/api/tunnel"

  let validate = (_host: string): option<error> => None

  let rec update = (state: t, msg: msg): (t, effect<'cmd>) => {
    let loadCmd = NoEffect // Some(Cmd(ApiFramework.get(loadTunnelUrl)))

    switch msg {
    | Initialize => (state, loadCmd)

    | LoadAPICallback(Error(e)) =>
      Recover.asserT("Loading callback occurred in another state", state == Loading)
      // TODO this shouldnt happen unless the user is offline so report

      let errorStr = Tea.Http.string_of_error(e)
      switch state {
      // load error
      | Loading => (Editing(Some(LoadError(errorStr)), Original(""), Local("")), NoEffect)

      // load errors that come later tell us nothing, so ignore them
      | Editing(_)
      | Saving(_)
      | Saved(_) => (state, NoEffect)
      }

    | LoadAPICallback(Ok(tunnelHost)) =>
      Recover.asserT("Loading callback occurred in another state", state == Loading)
      switch state {
      // We expect to be in Loading
      | Loading => (Editing(None, Original(tunnelHost), Local(tunnelHost)), NoEffect)

      // Unintended states: update the original anyway
      | Editing(err, _, local) => (Editing(err, Original(tunnelHost), local), NoEffect)
      | Saving(_, local) => (Saving(Original(tunnelHost), local), NoEffect)

      // this could be a very slow response to the original request, or we might be
      // reloading the form but didn't update the state. The latter is more likely.
      | Saved(saved) =>
        if saved == tunnelHost {
          (state, NoEffect)
        } else {
          update(Loading, msg)
        }
      }

    | InputEdit(tunnelHost) =>
      Recover.asserT("Editing while loading", state != Loading)
      switch state {
      // The users shouldn't be able to edit here
      | Loading => (Editing(None, Original(""), Local(tunnelHost)), NoEffect)
      | Editing(error, orig, _) => (Editing(error, orig, Local(tunnelHost)), NoEffect)

      // User is allows type while saving
      | Saving(orig, _) => (Editing(None, orig, Local(tunnelHost)), NoEffect)
      | Saved(orig) => (Editing(None, Original(orig), Local(tunnelHost)), NoEffect)
      }

    | InputUnfocus =>
      switch state {
      | Editing(_, original, Local(local)) => (
          Editing(validate(local), original, Local(local)),
          NoEffect,
        )

      | Loading
      | Saving(_)
      | Saved(_) => (state, NoEffect)
      }

    | Submit =>
      switch state {
      | Loading => (state, NoEffect)
      | Editing(_) => (state, NoEffect)
      | Saving(_) => (state, NoEffect)
      | Saved(_) => (state, NoEffect)
      }

    | SaveAPICallback(_, Error(e)) =>
      let error = Some(SaveError(Tea.Http.string_of_error(e)))
      switch state {
      // ignore as we aren't expecting it
      | Loading => (state, NoEffect)
      | Editing(_) => (state, NoEffect)
      | Saving(original, local) => (Editing(error, original, local), NoEffect)
      | Saved(_) => (state, NoEffect)
      }

    | SaveAPICallback(savedValue, Ok()) =>
      switch state {
      | Loading => (Saved(savedValue), NoEffect)
      | Editing(_, Original(_), Local(local)) =>
        if savedValue == local {
          (Saved(local), NoEffect)
        } else {
          (Editing(None, Original(savedValue), Local(local)), NoEffect)
        }
      | Saving(original, Local(local)) =>
        if savedValue == local {
          (Saved(savedValue), NoEffect)
        } else {
          (Editing(None, original, Local(local)), NoEffect)
        }
      | Saved(_) => (Saved(savedValue), NoEffect)
      }
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
    ({...s, useAssets: useAssets}, UseAssetsEffect(effect))
  | TunnelHostMsg(msg) =>
    let (tunnelHost, effect) = TunnelHost.update(s.tunnelHost, msg)
    ({...s, tunnelHost: tunnelHost}, TunnelHostEffect(effect))

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
