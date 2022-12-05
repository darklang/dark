open Prelude

let send = ErrorReporting.Rollbar.send

let init = ErrorReporting.Rollbar.init

let displayAndReportError = (m, message, url, custom): (AppTypes.model, AppTypes.cmd) => {
  let url = switch url {
  | Some(url) => " (" ++ (url ++ ")")
  | None => ""
  }
  let custom = switch custom {
  | Some(c) => ": " ++ c
  | None => ""
  }
  let msg = message ++ (url ++ custom)
  // Reload on bad csrf
  if String.includes(msg, ~substring="Bad CSRF") {
    Webapi.Dom.location->Webapi.Dom.Location.reload
  }
  (m, Tea.Cmd.call(_ => send(msg, None, Js.Json.null))) |> Model.updateError(Error.set(msg))
}
