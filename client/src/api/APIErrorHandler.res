open Prelude

// Tea
module Cmd = Tea.Cmd
module Http = Tea.Http

type t = APIError.t

let sendRollbar = (m: AppTypes.model, e: t): unit => {
  let customContext = Json_encode_extended.object_(list{
    ("httpResponse", Encoders.httpError(e.originalError)),
    ("parameters", Option.unwrap(~default=Js.Json.null, e.requestParams)),
    ("cursorState", AppTypes.CursorState.encode(m.cursorState)),
  })
  Rollbar.send(APIError.msg(e), APIError.urlOf(e), customContext)
}

let handle = (m: AppTypes.model, apiError: t): (AppTypes.model, AppTypes.cmd) => {
  let now = Js.Date.now() |> Js.Date.fromFloat
  let shouldReload = {
    let buildHashMismatch =
      APIError.serverVersionOf(apiError)
      |> Option.map(~f=hash => hash != m.buildHash)
      |> Option.unwrap(~default=false)

    let reloadAllowed = switch m.lastReload {
    | Some(time) =>
      // if 60 seconds have elapsed
      Js.Date.getTime(time) +. 60000.0 > Js.Date.getTime(now)
    | None => true
    }

    // Reload if it's an auth failure or the frontend is out of date
    APIError.isBadAuth(apiError) || (buildHashMismatch && reloadAllowed)
  }

  let ignore = {
    // Ignore when using Ngrok
    let usingTunnel =
      m.settings.contributingSettings.useAssets == SettingsContributing.UseAssets.UseTunnelAssets
    // This message is deep in the server code and hard to pull
    // out, so just ignore for now
    Js.log("Already at latest redo - ignoring server error")
    let redoError = String.includes(
      APIError.msg(apiError),
      ~substring="(client): Already at latest redo",
    )

    redoError || usingTunnel
  }

  let cmd = if shouldReload {
    let m = {...m, lastReload: Some(now)}
    // Previously, this was two calls to Tea_task.nativeBinding. But
    // only the first got called, unclear why
    Cmd.call(_ => {
      SavedSettings.save(m)
      SavedUserSettings.save(m)
      Webapi.Dom.location->Webapi.Dom.Location.reloadWithForce
    })
  } else if !ignore && APIError.shouldRollbar(apiError) {
    Cmd.call(_ => sendRollbar(m, apiError))
  } else {
    Cmd.none
  }

  let newM = {
    let error = if APIError.shouldDisplayToUser(apiError) && !ignore {
      Error.set(APIError.msg(apiError), m.error)
    } else {
      m.error
    }

    let lastReload = if shouldReload {
      Some(now)
    } else {
      m.lastReload
    }
    {...m, error: error, lastReload: lastReload}
  }
  (newM, cmd)
}
