open Prelude

let send = Unshared.Rollbar.send

let init = Unshared.Rollbar.init

let customContext (e : apiError) (state : cursorState) : Js.Json.t =
  let parameters = Option.unwrap ~default:Js.Json.null e.requestParams in
  Json_encode_extended.object_
    [ ("httpResponse", Encoders.httpError e.originalError)
    ; ("parameters", parameters)
    ; ("cursorState", Encoders.cursorState state) ]


let sendAPIError (m : model) (e : apiError) : unit =
  send (APIError.msg e) (APIError.urlOf e) (customContext e m.cursorState)


let displayAndReportError m message url custom : model * msg Tea.Cmd.t =
  let url = match url with Some url -> " (" ^ url ^ ")" | None -> "" in
  let custom = match custom with Some c -> ": " ^ c | None -> "" in
  let msg = message ^ url ^ custom in
  (* Reload on bad csrf *)
  if String.includes msg ~substring:"Bad CSRF" then Native.Location.reload true ;
  (m, Tea.Cmd.call (fun _ -> send msg None Js.Json.null))
  |> Model.updateError (Error.set msg)
