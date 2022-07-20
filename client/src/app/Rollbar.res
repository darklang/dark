open Prelude

let send = ErrorReporting.Rollbar.send

let init = ErrorReporting.Rollbar.init

let customContext = (e: apiError, state: AppTypes.CursorState.t): Js.Json.t => {
  let parameters = Option.unwrap(~default=Js.Json.null, e.requestParams)
  Json_encode_extended.object_(list{
    ("httpResponse", Encoders.httpError(e.originalError)),
    ("parameters", parameters),
    ("cursorState", AppTypes.CursorState.encode(state)),
  })
}

let sendAPIError = (m: AppTypes.model, e: apiError): unit =>
  send(APIError.msg(e), APIError.urlOf(e), customContext(e, m.cursorState))

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
    Native.Location.reload(true)
  }
  (m, Tea.Cmd.call(_ => send(msg, None, Js.Json.null))) |> Model.updateError(Error.set(msg))
}
