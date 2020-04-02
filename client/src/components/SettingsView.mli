type t

type formField =
  { value : string
  ; error : string option }
[@@deriving show]

type inviteFields = {email : formField}

(* type settingsTab = *)
(*   | CanvasInfo *)
(*   | UserSettings *)
(*   | InviteUser of inviteFields *)
(* [@@deriving show] *)

type loadCanvasInfoAPIResult =
  { canvasDescription : string
  ; shippedDate : string }
[@@deriving show]

type msg =
  | CloseSettingsView of settingsTab
  | SetSettingsView of
      ((string * string list * string list * Js.Date.t)[@opaque])
  | OpenSettingsView of settingsTab
  | SwitchSettingsTabs of settingsTab
  | UpdateInviteForm of string
  | UpdateCanvasDescription of string
  | SetCanvasDeployStatus of bool
  | SubmitForm
  | TriggerSendInviteCallback of
      (unit, (string Tea.Http.error[@opaque])) Tea.Result.t
  | TriggerUpdateCanvasInfoCallback of
      (unit, (string Tea.Http.error[@opaque])) Tea.Result.t
  | TriggerGetCanvasInfoCallback of
      (loadCanvasInfoAPIResult, (string Tea.Http.error[@opaque])) Tea.Result.t
[@@deriving show]

(* ToastEffect of (Toast.t -> Toast.t) *)
type effect =
  | ToastShow of string
  | NewCursor of cursorState
  | APIError of apiError

val update : t -> msg -> t * effect list

type updateCanvasInfoPayload =
  { canvasName : string
  ; canvasDescription : string
  ; canvasShipped : string
  ; canvasCreation : string }
[@@deriving show]

val toUpdateCanvasInfoPayload : t -> updateCanvasInfoPayload

val view : t -> msg Html.t
