open Tea
open Tea.Mouse
open App

open Keyboard
open Toplevel
open Location

open Porting

open Analysis
open Errors
open Window

(* DEFINE TYPES *)
type model = 
  { selected_toplevel : Toplevel.model (* TODO : make. optional *)
  ; last_key_event: Keyboard.key_event
  }

type msg =
  Toplevel_msg of Toplevel.msg
  | Key_event of Keyboard.key_event
  | Mouse_click of Mouse.position
  | Mouse_move of Mouse.position
  | Mouse_down of Mouse.position
  | Mouse_up of Mouse.position
  | Analysis_result of Analysis.analysis_result
  | Display_error of string
  | Window_resize of Window.size
  | Window_visible of bool
[@@bs.deriving {accessors}]

(* let msg2Mod model = function
  | GlobalClick mevt -> NoChange
  | NothingClick mevt -> NoChange

let updateMod mod m cmd = m, cmd 
*)

let update m msg =
  match msg with
  Toplevel_msg toplevel_msg -> m, Cmd.none
  | Key_event key_event ->

    if Keyboard.isLoc key_event
    then Js.log (Location.hashString);

    if Keyboard.isError key_event
    then Rollbar.send "send error to rollbar";

    if key_event.key_code = 65 && key_event.alt
    then Analysis.RequestAnalysis.send [123;342;4534];

    { m with last_key_event = key_event }, Cmd.none
  | Mouse_click position ->
    Js.log "click"; Js.log position;
    m, Cmd.none
  | Analysis_result analysis_result ->
    Js.log "Analysis_result"; Js.log analysis_result; m, Cmd.none
  | Display_error msg -> Js.log msg; m, Cmd.none
  | Window_resize window_resize -> Js.log msg; m, Cmd.none
  | _ -> m, Cmd.none

let view m =
  let open Html in
  div
    [ class' "canvas" ]
    [ Toplevel.view m.selected_toplevel |> map toplevel_msg
    ]

let init () =
  { selected_toplevel = Toplevel.new_http_handler "some stuff happening"
  ; last_key_event = Keyboard.nullEvent ()
  }, Cmd.none

let subscriptions model = Sub.batch
  [ Keyboard.downs key_event
  ; Mouse.clicks mouse_click
  ; Mouse.downs mouse_down
  ; Mouse.ups mouse_up
  ; Mouse.moves mouse_move
  ; Analysis.ReceiveAnalysis.listen analysis_result
  ; Errors.DisplayClientError.listen display_error
  ; Window.OnResize.listen window_resize 
  ; Window.OnFocusChange.listen window_visible
  ]

let main =
  App.standardProgram
    { init
    ; update
    ; view
    ; subscriptions = subscriptions
    }