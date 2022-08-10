open Tc

module Tab = {
  @ppx.deriving(show)
  type rec t =
    | Canvases
    | Invite
    | Privacy
  // | Contributing

  let toText = (tab: t): string =>
    switch tab {
    | Canvases => "canvases"
    | Invite => "share"
    | Privacy => "privacy"
    // | Contributing => "contributing"
    }

  let parse = (tab: string): t =>
    switch String.toLowercase(tab) {
    | "canvases" => Canvases
    | "share" => Invite
    | "privacy" => Privacy
    // | "contributing" => Contributing
    | _ => Canvases
    }
}

@ppx.deriving(show)
type rec t = {
  opened: bool,
  tab: Tab.t,
  canvasesSettings: SettingsCanvasesState.t,
  inviteSettings: SettingsInvite.t,
  // contributingSettings: SettingsContributing.t,
  privacySettings: SettingsPrivacyState.t,
}

let default = {
  opened: false,
  tab: Canvases,
  canvasesSettings: SettingsCanvasesState.default,
  privacySettings: SettingsPrivacyState.default,
  inviteSettings: SettingsInvite.default,
  // contributingSettings: SettingsContributing.default,
}

@ppx.deriving(show)
type rec msg =
  | Close(Tab.t)
  | Open(Tab.t)
  | SwitchTab(Tab.t)
  | CanvasesMsg(SettingsCanvasesState.msg)
  | PrivacyMsg(SettingsPrivacyState.msg)
  | InviteMsg(SettingsInvite.msg)
// | ContributingMsg(SettingsContributing.msg)

@ppx.deriving(show)
type rec effect =
  | OpenSettings(Tab.t)
  | CloseSettings
  | SetSettingsTab(Tab.t)
  | PrivacyEffect(SettingsPrivacyState.effect)
  | InviteEffect(option<SettingsInvite.effect>)

let setInviter = (state: t, username: string, name: string): t => {
  ...state,
  inviteSettings: SettingsInvite.setInviter(state.inviteSettings, username, name),
}

let setCanvasesInfo = (
  state: t,
  canvasList: list<string>,
  username: string,
  orgs: list<string>,
  orgCanvasList: list<string>,
): t => {
  ...state,
  canvasesSettings: SettingsCanvasesState.setInfo(
    state.canvasesSettings,
    canvasList,
    username,
    orgs,
    orgCanvasList,
  ),
}
