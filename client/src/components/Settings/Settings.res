open Tc

module Tab = {
  @ppx.deriving(show)
  type rec t =
    | Canvases
    | Invite
    | Privacy
    | Contributing

  let toText = (tab: t): string =>
    switch tab {
    | Canvases => "canvases"
    | Invite => "share"
    | Privacy => "privacy"
    | Contributing => "contributing"
    }

  let parse = (tab: string): t =>
    switch String.toLowercase(tab) {
    | "canvases" => Canvases
    | "share" => Invite
    | "privacy" => Privacy
    | "contributing" => Contributing
    | _ => Canvases
    }
}

@ppx.deriving(show)
type rec t = {
  opened: bool,
  tab: Tab.t,
  canvasesSettings: SettingsCanvases.t,
  inviteSettings: SettingsInvite.t,
  contributingSettings: SettingsContributing.t,
  privacySettings: SettingsPrivacy.t,
}

let default = {
  opened: false,
  tab: Canvases,
  canvasesSettings: SettingsCanvases.default,
  privacySettings: SettingsPrivacy.default,
  inviteSettings: SettingsInvite.default,
  contributingSettings: SettingsContributing.default,
}

let toSaved = (s: t): Js.Json.t => {
  open Json.Encode
  object_(list{("contributingSettings", SettingsContributing.toSaved(s.contributingSettings))})
}

let fromSaved = (j: Js.Json.t) => {
  open Json.Decode
  {
    ...default,
    contributingSettings: field("contributingSettings", SettingsContributing.fromSaved, j),
  }
}

@ppx.deriving(show)
type rec msg =
  | Close(Tab.t)
  | Open(Tab.t)
  | SwitchTab(Tab.t)
  | CanvasesMsg(SettingsCanvases.msg)
  | PrivacyMsg(SettingsPrivacy.msg)
  | InviteMsg(SettingsInvite.msg)
  | ContributingMsg(SettingsContributing.msg)

let init = (clientData: APIFramework.clientData, tab: Tab.t): Tea.Cmd.t<msg> => {
  let settings = switch tab {
  | Contributing => SettingsContributing.init(clientData)
  | Canvases
  | Privacy
  | Invite => Tea.Cmd.none
  }
  Tea.Cmd.map(msg => ContributingMsg(msg), settings)
}

module Intent = {
  @ppx.deriving(show)
  type rec t<'msg> =
    | OpenSettings(Tab.t, APIFramework.Callable.t<'msg>)
    | CloseSettings
    | SetSettingsTab(Tab.t, APIFramework.Callable.t<'msg>)
    | PrivacyIntent(SettingsPrivacy.Intent.t<'msg>)
    | InviteIntent(option<SettingsInvite.intent>)
    | ContributingIntent(SettingsContributing.Intent.t<'msg>)

  let map = (i: t<'msg1>, f: 'msg1 => 'msg2): t<'msg2> => {
    switch i {
    | OpenSettings(tab, c) => OpenSettings(tab, APIFramework.Callable.map(c, f))
    | CloseSettings => CloseSettings
    | SetSettingsTab(tab, c) => SetSettingsTab(tab, APIFramework.Callable.map(c, f))
    | PrivacyIntent(i) => PrivacyIntent(SettingsPrivacy.Intent.map(i, f))
    | InviteIntent(i) => InviteIntent(i)
    | ContributingIntent(i) => ContributingIntent(SettingsContributing.Intent.map(i, f))
    }
  }
}

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
  canvasesSettings: SettingsCanvases.setInfo(
    state.canvasesSettings,
    canvasList,
    username,
    orgs,
    orgCanvasList,
  ),
}

let update = (state: t, msg: msg): (t, option<Intent.t<msg>>) =>
  switch msg {
  | Open(tab) =>
    let tabInit = clientData => init(clientData, tab)
    ({...state, opened: true, tab: tab}, Some(OpenSettings(tab, tabInit)))

  | Close(_) => ({...state, opened: false}, Some(CloseSettings))

  | SwitchTab(tab) =>
    let tabInit = clientData => init(clientData, tab)
    ({...state, tab: tab}, Some(SetSettingsTab(tab, tabInit)))

  | CanvasesMsg(msg) => {
      let newSettings = SettingsCanvases.update(state.canvasesSettings, msg)
      ({...state, canvasesSettings: newSettings}, None)
    }

  | PrivacyMsg(msg) => {
      let (newSettings, intent) = SettingsPrivacy.update(state.privacySettings, msg)
      let intent = SettingsPrivacy.Intent.map(intent, msg => PrivacyMsg(msg))
      ({...state, privacySettings: newSettings}, Some(PrivacyIntent(intent)))
    }

  | InviteMsg(msg) => {
      let (newSettings, intent) = SettingsInvite.update(state.inviteSettings, msg)
      ({...state, inviteSettings: newSettings}, Some(InviteIntent(intent)))
    }

  | ContributingMsg(msg) => {
      let (newSettings, intent) = SettingsContributing.update(state.contributingSettings, msg)
      let intent = SettingsContributing.Intent.map(intent, msg => ContributingMsg(msg))
      ({...state, contributingSettings: newSettings}, Some(ContributingIntent(intent)))
    }
  }
