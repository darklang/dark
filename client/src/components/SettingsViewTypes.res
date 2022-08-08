open Tc

@warning("-3")
let opaque = (msg, fmt, _) => {
  Format.pp_print_string(fmt, "<opaque:" ++ (msg ++ ">"))
  ()
}

@ppx.deriving(show)
type rec formField = {
  value: string,
  error: option<string>,
}

type rec inviteFields = {email: formField}

@ppx.deriving(show)
and settingsTab =
  | NewCanvas
  | UserSettings
  | InviteUser(inviteFields)
  | Privacy
  | Editor

@ppx.deriving(show)
type rec inviteFormMessage = {
  email: string,
  inviterUsername: string,
  inviterName: string,
}

@ppx.deriving(show) type rec privacySettings = {recordConsent: option<bool>}

@ppx.deriving(show)
type rec settingsViewState = {
  opened: bool,
  tab: settingsTab,
  canvasList: list<string>,
  username: string,
  orgs: list<string>,
  orgCanvasList: list<string> /* This is org canvases, not orgs themselves */,
  loading: bool,
  privacy: privacySettings,
  isContributor: bool
}

@ppx.deriving(show)
type rec settingsMsg =
  | CloseSettingsView(settingsTab)
  | SetSettingsView(@opaque (list<string>, string, list<string>, list<string>, bool))
  | OpenSettingsView(settingsTab)
  | SwitchSettingsTabs(settingsTab)
  | UpdateInviteForm(string)
  | SubmitForm
  | @printer(opaque("TriggerSendInviteCallback"))
  TriggerSendInviteCallback(Tea.Result.t<unit, @opaque Tea.Http.error<string>>)
  | InitRecordConsent(option<bool>)
  | SetRecordConsent(bool)
  | SetIsContributor(bool)

let settingsTabToText = (tab: settingsTab): string =>
  switch tab {
  | NewCanvas => "new-canvas"
  | UserSettings => "canvases"
  | InviteUser(_) => "share"
  | Privacy => "privacy"
  | Editor => "editor"
  }

let defaultInviteFields: inviteFields = {email: {value: "", error: None}}

let settingsTabFromText = (tab: string): settingsTab =>
  switch String.toLowercase(tab) {
  | "new-canvas" => NewCanvas
  | "canvases" => UserSettings
  | "share" => InviteUser(defaultInviteFields)
  | "privacy" => Privacy
  | "editor" => Editor
  | _ => UserSettings
  }
