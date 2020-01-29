open Prelude

let send = Unshared.Rollbar.send

let init = Unshared.Rollbar.init

let customContext (e : apiError) (state : cursorState) : Js.Json.t =
  let parameters = Option.withDefault ~default:Js.Json.null e.requestParams in
  Json_encode_extended.object_
    [ ("httpResponse", Encoders.httpError e.originalError)
    ; ("parameters", parameters)
    ; ("cursorState", Encoders.cursorState state) ]


let sendAPIError (m : model) (e : apiError) : unit =
  send (APIError.msg e) (APIError.urlOf e) (customContext e m.cursorState)
