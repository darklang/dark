let opaque msg fmt _ =
  Format.pp_print_string fmt ("<opaque:" ^ msg ^ ">") ;
  ()


type formField =
  { value : string
  ; error : string option }
[@@deriving show]

type inviteFields = {email : formField}

and settingsTab =
  | UserSettings
  | InviteUser of inviteFields
[@@deriving show]

type inviteFormMessage =
  { email : string
  ; inviterUsername : string
  ; inviterName : string }
[@@deriving show]

type settingsViewState =
  { opened : bool
  ; tab : settingsTab
  ; canvas_list : string list
  ; org_list : string list
  ; loading : bool }
[@@deriving show]

type settingsMsg =
  | ToggleSettingsView of bool * settingsTab option
  | SwitchSettingsTabs of settingsTab
  | UpdateInviteForm of string
  | SubmitForm
  | TriggerSendInviteCallback of
      (unit, (string Tea.Http.error[@opaque])) Tea.Result.t
      [@printer opaque "TriggerSendInviteCallback"]
[@@deriving show]
