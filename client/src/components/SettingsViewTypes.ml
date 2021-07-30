open Tc

let opaque msg fmt _ =
  Format.pp_print_string fmt ("<opaque:" ^ msg ^ ">") ;
  ()


type formField =
  { value : string
  ; error : string option }
[@@ppx.deriving show]

type inviteFields = {email : formField}

and settingsTab =
  | NewCanvas
  | UserSettings
  | InviteUser of inviteFields
  | Privacy
[@@ppx.deriving show]

type inviteFormMessage =
  { email : string
  ; inviterUsername : string
  ; inviterName : string }
[@@ppx.deriving show]

type privacySettings = {recordConsent : bool option} [@@ppx.deriving show]

type settingsViewState =
  { opened : bool
  ; tab : settingsTab
  ; canvasList : string list
  ; username : string
  ; orgs : string list
  ; orgCanvasList : string list (* This is org canvases, not orgs themselves *)
  ; loading : bool
  ; privacy : privacySettings }
[@@ppx.deriving show]

type settingsMsg =
  | CloseSettingsView of settingsTab
  | SetSettingsView of
      ((string list * string * string list * string list)[@opaque])
  | OpenSettingsView of settingsTab
  | SwitchSettingsTabs of settingsTab
  | UpdateInviteForm of string
  | SubmitForm
  | TriggerSendInviteCallback of
      (unit, (string Tea.Http.error[@opaque])) Tea.Result.t
      [@printer opaque "TriggerSendInviteCallback"]
  | InitRecordConsent of bool option
  | SetRecordConsent of bool
[@@ppx.deriving show]

let settingsTabToText (tab : settingsTab) : string =
  match tab with
  | NewCanvas ->
      "new-canvas"
  | UserSettings ->
      "canvases"
  | InviteUser _ ->
      "share"
  | Privacy ->
      "privacy"


let defaultInviteFields : inviteFields = {email = {value = ""; error = None}}

let settingsTabFromText (tab : string) : settingsTab =
  match String.toLowercase tab with
  | "new-canvas" ->
      NewCanvas
  | "canvases" ->
      UserSettings
  | "share" ->
      InviteUser defaultInviteFields
  | "privacy" ->
      Privacy
  | _ ->
      UserSettings
