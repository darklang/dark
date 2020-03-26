open Tc
module Html = Tea.Html

type displayMessage =
  | DidCopy
  | FailedCopy
  | DidAutosave
  | DidSaveCanvasInfo
  | DidSendInvite
  | ErrorOnlyOneFeatureFlag
  | CmdPaletteMoved
[@@deriving show]

(** this is a Types.vPos *)
type pos =
  { x : int
  ; y : int }
[@@deriving show]

module State = struct
  type t =
    { message : displayMessage option
    ; pos : pos option }
  [@@deriving show]

  let empty = {message = None; pos = None}

  let displayString = function
    | {message = Some DidCopy; _} ->
        "Copied!"
    | {message = Some FailedCopy; _} ->
        "Could not copy, try again after clicking this handler."
    | {message = Some DidAutosave; _} ->
        "Dark saves automatically!"
    | {message = Some ErrorOnlyOneFeatureFlag; _} ->
        "Only one Feature Flag per-handler/function is supported at this time"
    | {message = Some DidSaveCanvasInfo; _} ->
        "Canvas Info saved!"
    | {message = Some DidSendInvite; _} ->
        "Sent!"
    | {message = Some CmdPaletteMoved; _} ->
        "Command Palette has been moved to Ctrl-\\"
    | {message = None; _} ->
        ""
end

type msg = ClearToast [@@deriving show]

let update msg : State.t = match msg with ClearToast -> State.empty

let show ?(pos : pos option) (m : displayMessage) : State.t =
  {message = Some m; pos}


let view (s : State.t) : 'a Vdom.t =
  let styleOverrides =
    match s.pos with
    | Some {x; y} ->
        Html.styles
          [ ("top", string_of_int (y - 10) ^ "px")
          ; ("left", string_of_int (x + 10) ^ "px") ]
    | None ->
        Vdom.noProp
  in
  Html.div
    [ Html.classList [("toast", true); ("show", Option.isSome s.message)]
    ; Tea.Html.onMsg "animationEnd" ClearToast
    ; styleOverrides ]
    [State.displayString s |> Html.text]
