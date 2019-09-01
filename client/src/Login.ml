open Types
open Prelude

(* open Tc *)
module Events = Tea.Html2.Events
module Cmd = Tea.Cmd

let isLoggedIn (l : loginState) : bool = l.loggedIn <> Some false

let logout (l : loginState) : loginState = {l with loggedIn = Some false}

let login (l : loginState) : loginState = {l with loggedIn = Some true}

(* ------------- *)
(* state *)
(* ------------- *)

let encoder (l : loginState) : Js.Json.t =
  let open Json_encode_extended in
  object_
    [ ("username", string l.loginFormUsername)
    ; ("password", string l.loginFormPassword) ]


let decoder (_ : Js.Json.t) : unit = ()

let loginRPC (l : loginState) : msg Cmd.t =
  let request =
    Tea.Http.request
      { method' = "POST"
      ; headers = [Header ("Content-type", "application/json")]
      ; url = "/login"
      ; body = Web.XMLHttpRequest.StringBody (Json.stringify (encoder l))
      ; expect = Tea.Http.expectStringResponse (fun _ -> Ok ())
      ; timeout = None
      ; withCredentials = false }
  in
  Tea.Http.send (fun x -> LoginRPCCallback x) request


let update (l : loginState) (msg : loginMsg) : loginState * msg Cmd.t =
  match msg with
  | LoginFormSubmit ->
      (l, loginRPC l)
  | LoginFormSetUsername username ->
      ({l with loginFormUsername = username}, Cmd.none)
  | LoginFormSetPassword password ->
      ({l with loginFormPassword = password}, Cmd.none)


let viewLoginForm (l : loginState) : msg Html.html =
  Html.form
    ~key:"login"
    [ Html.class' "login-page"
    ; ViewEntry.onSubmit ~key:"login" (fun _ ->
          Js.log "submit" ;
          Login LoginFormSubmit ) ]
    [ Html.label
        [Html.class' "username"]
        [ Html.span [Html.class' "label"] [Html.text "Username"]
        ; Html.input'
            [Events.onInput (fun u -> Login (LoginFormSetUsername u))]
            [Html.text l.loginFormUsername] ]
    ; Html.label
        [Html.class' "password"]
        [ Html.span [Html.class' "label"] [Html.text "Password"]
        ; Html.input'
            [ Events.onInput (fun pw -> Login (LoginFormSetPassword pw))
            ; Html.type' "password" ]
            [Html.text l.loginFormPassword] ]
    ; Html.input' [Html.type' "submit"; Html.class' "submit"] [] ]
