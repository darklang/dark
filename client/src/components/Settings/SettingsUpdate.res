// open Tc

module T = SettingsState

let update = (state: T.t, msg: T.msg): T.t =>
  switch msg {
  | Init(canvasesData, inviteData) => {
      ...state,
      canvasesSettings: SettingsCanvasesState.init(canvasesData),
      inviteSettings: SettingsInviteState.init(inviteData),
      privacySettings: SettingsPrivacyState.init(),
      // contributingSettings: SettingsContributing.init(),
    }
  | Open(tab) => {...state, opened: true, tab: tab}
  | Close(_) => {...state, opened: false}
  | SwitchTab(tab) => {...state, tab: tab}

  | CanvasesMsg(msg) => {
      ...state,
      canvasesSettings: SettingsCanvasesUpdate.update(state.canvasesSettings, msg),
    }
  | PrivacyMsg(msg) => {
      ...state,
      privacySettings: SettingsPrivacyUpdate.update(state.privacySettings, msg),
    }
  | InviteMsg(msg) => {
      ...state,
      inviteSettings: SettingsInviteUpdate.update(state.inviteSettings, msg),
    }
  // | ContributingMsg(msg) => SettingsContributing.update(msg)
  }

let getModifications = (m: AppTypes.model, msg: T.msg): list<AppTypes.modification> =>
  switch msg {
  | InviteMsg(SettingsInviteState.TriggerSendCallback(Error(err))) => list{
      SettingsViewUpdate(msg),
      HandleAPIError(
        APIError.make(
          ~context="TriggerSendInviteCallback",
          ~importance=ImportantError,
          ~reload=false,
          err,
        ),
      ),
    }
  | Open(tab) => list{
      SettingsViewUpdate(msg),
      ReplaceAllModificationsWithThisOne(
        m => {
          let cmd = Url.navigateTo(SettingsModal(tab))
          ({...m, cursorState: Deselected, currentPage: SettingsModal(tab)}, cmd)
        },
      ),
    }
  | InviteMsg(SettingsInviteState.TriggerSendCallback(Ok())) => list{
      SettingsViewUpdate(msg),
      ReplaceAllModificationsWithThisOne(
        m => ({...m, toast: {toastMessage: Some("Sent!"), toastPos: None}}, Tea.Cmd.none),
      ),
    }
  | Close(_) => list{
      SettingsViewUpdate(msg),
      ReplaceAllModificationsWithThisOne(
        m => ({...m, canvasProps: {...m.canvasProps, enablePan: true}}, Tea.Cmd.none),
      ),
      Deselect,
      MakeCmd(Url.navigateTo(Architecture)),
    }
  | SwitchTab(tab) => list{
      SettingsViewUpdate(msg),
      ReplaceAllModificationsWithThisOne(
        m => {
          let cmd = Url.navigateTo(SettingsModal(tab))
          ({...m, currentPage: SettingsModal(tab)}, cmd)
        },
      ),
    }
  | InviteMsg(SettingsInviteState.Submit) =>
    let (isInvalid, newForm) = SettingsInviteUpdate.validateForm(m.settingsView.inviteSettings)
    if isInvalid {
      list{
        SettingsViewUpdate(msg),
        ReplaceAllModificationsWithThisOne(
          m => ({...m, settingsView: {...m.settingsView, inviteSettings: newForm}}, Tea.Cmd.none),
        ),
      }
    } else {
      list{
        SettingsViewUpdate(msg),
        ReplaceAllModificationsWithThisOne(
          m => {
            let (newState, _effect) = SettingsInviteUpdate.submitForm(m.settingsView.inviteSettings)
            ({...m, settingsView: {...m.settingsView, inviteSettings: newState}}, Tea.Cmd.none)
          },
        ),
      }
    }
  | PrivacyMsg(SettingsPrivacyState.SetRecordConsent(allow)) => list{
      SettingsViewUpdate(msg),
      MakeCmd(FullstoryView.FullstoryJs.setConsent(allow)),
    }
  | _ => list{SettingsViewUpdate(msg)}
  }
