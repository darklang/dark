// open Tc

module T = SettingsState

let update = (state: T.t, msg: T.msg): (T.t, AppTypes.modification) =>
  switch msg {
  | Open(tab) => (
      {...state, opened: true, tab: tab},
      ReplaceAllModificationsWithThisOne(
        m => {
          let cmd = Url.navigateTo(SettingsModal(tab))
          ({...m, cursorState: Deselected, currentPage: SettingsModal(tab)}, cmd)
        },
      ),
    )

  | Close(_) => (
      {...state, opened: false},
      Many(list{
        ReplaceAllModificationsWithThisOne(
          m => ({...m, canvasProps: {...m.canvasProps, enablePan: true}}, Tea.Cmd.none),
        ),
        Deselect,
        MakeCmd(Url.navigateTo(Architecture)),
      }),
    )

  | SwitchTab(tab) => (
      {...state, tab: tab},
      ReplaceAllModificationsWithThisOne(
        m => {
          let cmd = Url.navigateTo(SettingsModal(tab))
          ({...m, currentPage: SettingsModal(tab)}, cmd)
        },
      ),
    )

  | CanvasesMsg(msg) => {
      let (newSettings, mods) = SettingsCanvasesUpdate.update(state.canvasesSettings, msg)
      ({...state, canvasesSettings: newSettings}, mods)
    }
  | PrivacyMsg(msg) => {
      let (newSettings, mods) = SettingsPrivacyUpdate.update(state.privacySettings, msg)
      ({...state, privacySettings: newSettings}, mods)
    }
  | InviteMsg(msg) => {
      let (newSettings, mods) = SettingsInviteUpdate.update(state.inviteSettings, msg)
      ({...state, inviteSettings: newSettings}, mods)
    }
  }
