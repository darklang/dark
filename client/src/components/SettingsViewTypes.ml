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
  ; canvasShipped : string }
[@@deriving show]

type canvasInformation =
  { canvas_description : string
  ; shipped_date : string option
  ; created_at : string }
[@@deriving show]

type settingsViewState =
  { opened : bool
  ; tab : settingsTab
  ; canvas_list : string list
  ; org_list : string list
  ; loading : bool
  ; canvas_information : canvasInformation }
[@@deriving show]

type settingsMsg =
  | ToggleSettingsView of bool * settingsTab option
  | SwitchSettingsTabs of settingsTab
  | UpdateInviteForm of string
  | UpdateCanvasDescription of string
  | ToggleCanvasDeployStatus
  | SubmitForm
  | SetSettingsView of string list * string list * string
  | TriggerSendInviteCallback of
      (unit, (string Tea.Http.error[@opaque])) Tea.Result.t
      [@printer opaque "TriggerSendInviteCallback"]
  | TriggerUpdateCanvasInfoCallback of
      (unit, (string Tea.Http.error[@opaque])) Tea.Result.t
      [@printer opaque "TriggerUpdateCanvasInfoCallback"]
[@@deriving show]
