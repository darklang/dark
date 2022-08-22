open Prelude

// Dark
module Html = Tea.Html
module Attrs = Tea.Attrs
module Events = Tea.Html.Events
module Cmd = Tea.Cmd

module K = FluidKeyboard
module T = Settings

module Msg = AppTypes.Msg
type msg = AppTypes.msg

let fontAwesome = ViewUtils.fontAwesome

let allTabs: list<T.Tab.t> = list{T.Tab.Canvases, Privacy, Invite, Contributing}

// View

let settingsTabToText = (tab: T.Tab.t): string =>
  switch tab {
  | Canvases => SettingsCanvases.title
  | Invite => SettingsInvite.title
  | Privacy => SettingsPrivacy.title
  | Contributing => SettingsContributing.title
  }

let settingsTabToHtml = (state: T.t): list<Html.html<msg>> => {
  switch state.tab {
  | Canvases => SettingsCanvasesView.view(state.canvasesSettings)
  | Invite => SettingsInviteView.view(state.inviteSettings)
  | Privacy => SettingsPrivacyView.view(state.privacySettings)
  | Contributing => SettingsContributingView.view(state.contributingSettings)
  }
}

let tabTitleView = (tab: T.Tab.t): Html.html<msg> => {
  let tabTitle = (t: T.Tab.t) => {
    let isSameTab = tab == t

    Html.h3(
      list{
        Attrs.classList(list{("tab-title", true), ("selected", isSameTab)}),
        ViewUtils.eventNoPropagation(~key="close-settings-modal", "click", _ => Msg.SettingsMsg(
          SwitchTab(t),
        )),
      },
      list{Html.text(settingsTabToText(t))},
    )
  }

  Html.div(list{Attrs.class'("settings-tab-titles")}, List.map(allTabs, ~f=tabTitle))
}

let settingViewWrapper = (acc: T.t): Html.html<msg> => {
  let tabView = settingsTabToHtml(acc)
  Html.div(
    list{Attrs.class'("settings-tab-wrapper")},
    list{Html.h1(list{}, list{Html.text("Settings")}), tabTitleView(acc.tab), ...tabView},
  )
}

let onKeydown = (evt: Web.Node.event): option<AppTypes.msg> =>
  K.eventToKeyEvent(evt) |> Option.andThen(~f=e =>
    switch e {
    | {K.key: K.Enter, _} =>
      Some(AppTypes.Msg.SettingsMsg(Settings.InviteMsg(SettingsInvite.Submit)))
    | _ => None
    }
  )

let html = (m: AppTypes.model): Html.html<msg> => {
  let svs = m.settingsView
  let closingBtn = Html.div(
    list{
      Attrs.class'("close-btn"),
      ViewUtils.eventNoPropagation(~key="close-settings-modal", "click", _ => Msg.SettingsMsg(
        Close(svs.tab),
      )),
    },
    list{fontAwesome("times")},
  )

  Html.div(
    list{
      Attrs.class'("settings modal-overlay"),
      ViewUtils.nothingMouseEvent("mousedown"),
      ViewUtils.nothingMouseEvent("mouseup"),
      ViewUtils.eventNoPropagation(~key="close-setting-modal", "click", _ => Msg.SettingsMsg(
        Close(svs.tab),
      )),
    },
    list{
      Html.div(
        list{
          Attrs.class'("modal"),
          ViewUtils.nothingMouseEvent("click"),
          ViewUtils.eventNoPropagation(~key="ept", "mouseenter", _ => EnablePanning(false)),
          ViewUtils.eventNoPropagation(~key="epf", "mouseleave", _ => EnablePanning(true)),
          Events.onCB("keydown", "keydown", onKeydown),
        },
        list{settingViewWrapper(svs), closingBtn},
      ),
    },
  )
}
