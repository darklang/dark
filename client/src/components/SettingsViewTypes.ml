open Tc

let opaque msg fmt _ =
  Format.pp_print_string fmt ("<opaque:" ^ msg ^ ">") ;
  ()


type formField =
  { value : string
  ; error : string option }
[@@deriving show]

type inviteFields = {email : formField}

and settingsTab =
  | NewCanvas
  | CanvasInfo
  | UserSettings
  | InviteUser of inviteFields
[@@deriving show]

type inviteFormMessage =
  { email : string
  ; inviterUsername : string
  ; inviterName : string }
[@@deriving show]

type updateCanvasInfo =
  { canvasName : string
  ; canvasDescription : string
  ; canvasShipped : string
  ; canvasCreation : string }
[@@deriving show]

type canvasInformation =
  { canvasName : string
  ; canvasDescription : string
  ; shippedDate : Js.Date.t option [@opaque]
  ; createdAt : Js.Date.t option [@opaque] }
[@@deriving show]

type settingsViewState =
  { opened : bool
  ; tab : settingsTab
  ; canvasList : string list
  ; username : string
  ; orgs : string list
  ; orgCanvasList : string list (* This is org canvases, not orgs themselves *)
  ; loading : bool
  ; canvasInformation : canvasInformation }
[@@deriving show]

type loadCanvasInfoAPIResult =
  { canvasDescription : string
  ; shippedDate : string }
[@@deriving show]

type settingsMsg =
  | CloseSettingsView of settingsTab
  | SetSettingsView of
      ((string * string list * string * string list * string list * Js.Date.t)
      [@opaque])
  | OpenSettingsView of settingsTab
  | SwitchSettingsTabs of settingsTab
  | UpdateInviteForm of string
  | UpdateCanvasDescription of string
  | SetCanvasDeployStatus of bool
  | SubmitForm
  | TriggerSendInviteCallback of
      (unit, (string Tea.Http.error[@opaque])) Tea.Result.t
      [@printer opaque "TriggerSendInviteCallback"]
  | TriggerUpdateCanvasInfoCallback of
      (unit, (string Tea.Http.error[@opaque])) Tea.Result.t
      [@printer opaque "TriggerUpdateCanvasInfoCallback"]
  | TriggerGetCanvasInfoCallback of
      (loadCanvasInfoAPIResult, (string Tea.Http.error[@opaque])) Tea.Result.t
      [@printer opaque "LoadPackagesAPICallback"]
[@@deriving show]

let settingsTabToText (tab : settingsTab) : string =
  match tab with
  | NewCanvas ->
      "new-canvas"
  | CanvasInfo ->
      "about"
  | UserSettings ->
      "canvases"
  | InviteUser _ ->
      "share"


let defaultInviteFields : inviteFields = {email = {value = ""; error = None}}

let settingsTabFromText (tab : string) : settingsTab =
  match String.toLower tab with
  | "new-canvas" ->
      NewCanvas
  | "canvases" ->
      UserSettings
  | "share" ->
      InviteUser defaultInviteFields
  | "about" | _ ->
      CanvasInfo
