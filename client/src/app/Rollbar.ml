open Prelude

let send = Unshared.Rollbar.send

let init = Unshared.Rollbar.init

let displayAndReportError m message url custom : model * msg Tea.Cmd.t =
  let url = match url with Some url -> " (" ^ url ^ ")" | None -> "" in
  let custom = match custom with Some c -> ": " ^ c | None -> "" in
  let msg = message ^ url ^ custom in
  (* Reload on bad csrf *)
  if String.contains msg ~substring:"Bad CSRF" then Native.Location.reload true ;
  (m, Tea.Cmd.call (fun _ -> send msg None Js.Json.null))
  |> Model.updateError (Error.set msg)
