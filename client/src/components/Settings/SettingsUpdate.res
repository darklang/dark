// open Tc

module T = SettingsState

let update = (state: T.t, msg: T.msg): (T.t, option<T.effect>) =>
  switch msg {
  | Open(tab) => ({...state, opened: true, tab: tab}, Some(OpenSettings(tab)))
  | Close(_) => ({...state, opened: false}, Some(CloseSettings))
  | SwitchTab(tab) => ({...state, tab: tab}, Some(SetSettingsTab(tab)))

  | CanvasesMsg(msg) => {
      let newSettings = SettingsCanvases.update(state.canvasesSettings, msg)
      ({...state, canvasesSettings: newSettings}, None)
    }
  | PrivacyMsg(msg) => {
      let (newSettings, effect) = SettingsPrivacy.update(state.privacySettings, msg)
      ({...state, privacySettings: newSettings}, Some(PrivacyEffect(effect)))
    }
  | InviteMsg(msg) => {
      let (newSettings, effect) = SettingsInvite.update(state.inviteSettings, msg)
      ({...state, inviteSettings: newSettings}, Some(InviteEffect(effect)))
    }
  }
