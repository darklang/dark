let opaque msg fmt _ =
  Format.pp_print_string fmt ("<opaque:" ^ msg ^ ">") ;
  ()


type formField =
  { value : string
  ; error : string option }
[@@deriving show]

type inviteFields = {email : formField}

and settingsTab =
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
  { canvasDescription : string
  ; shippedDate : Js.Date.t option [@opaque]
  ; createdAt : Js.Date.t option [@opaque] }
[@@deriving show]

type settingsViewState =
  { opened : bool
  ; tab : settingsTab
  ; canvas_list : string list
  ; org_list : string list
  ; loading : bool
  ; canvasInformation : canvasInformation }
[@@deriving show]

type loadCanvasInfoAPIResult =
  { canvasDescription : string
  ; shippedDate : string }
[@@deriving show]

type settingsMsg =
  | CloseSettingsView
  | SetSettingsView of (string list * string list * Js.Date.t [@opaque])
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
