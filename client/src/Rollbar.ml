open Tc
open Types

let send = Native.Rollbar.send

let init = Native.Rollbar.init

let customContext (e : apiError) (state : cursorState) : Js.Json.t =
  let parameters = Option.withDefault ~default:Js.Json.null e.requestParams in
  Json_encode_extended.object_
    [ ("httpResponse", Encoders.httpError e.originalError)
    ; ("parameters", parameters)
    ; ("cursorState", Encoders.cursorState state)
    ]


let sendApiError (m : model) (e : apiError) : unit =
  send (ApiError.msg e) (ApiError.urlOf e) (customContext e m.cursorState)
