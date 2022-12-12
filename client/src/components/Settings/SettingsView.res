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
        Attrs.classList(list{
          (%twc("h-full m-0 -mb-0.5 px-2.5 py-0 text-grey2 cursor-pointer"), true),
          (%twc("text-white3 border-solid border-b-3 border-b-grey8 font-heading"), isSameTab),
        }),
        EventListeners.eventNoPropagation(
          ~key="close-settings-modal",
          "click",
          _ => Msg.SettingsMsg(SwitchTab(t)),
        ),
      },
      list{Html.text(settingsTabToText(t))},
    )
  }

  Html.div(
    list{Attrs.class(%twc("flex w-full border-solid border-b-3 border-grey1"))},
    List.map(allTabs, ~f=tabTitle),
  )
}

let settingViewWrapper = (acc: T.t): Html.html<msg> => {
  let tabView = settingsTabToHtml(acc)
  Html.div(
    list{
      Attrs.class(
        %twc(
          "w-[calc(100%-1.875rem)] flex flex-col h-full overflow-auto my-2.5 mr-1 ml-9 scrollbar-none"
        ),
      ),
    },
    list{
      Html.h1(list{Attrs.class(%twc("font-heading"))}, list{Html.text("Settings")}),
      tabTitleView(acc.tab),
      ...tabView,
    },
  )
}

let onKeydown = (evt: Dom.event): option<AppTypes.msg> =>
  K.eventToKeyEvent(evt) |> Option.andThen(~f=e =>
    switch e {
    | {K.key: K.Enter, _} =>
      Some(AppTypes.Msg.SettingsMsg(Settings.InviteMsg(SettingsInvite.Submit)))
    | _ => None
    }
  )

let html = (m: AppTypes.model): Html.html<msg> => {
  let s = m.settings
  let closingBtn = Html.div(
    list{
      Attrs.class(
        %twc(
          "relative self-start w-8 cursor-pointer flex items-center justify-center mt-1.5 text-grey8 hover:text-grey2"
        ),
      ),
      EventListeners.eventNoPropagation(~key="close-settings-modal", "click", _ => Msg.SettingsMsg(
        Close(s.tab),
      )),
    },
    list{Icons.fontAwesome("times")},
  )

  Html.div(
    list{
      Attrs.class(
        %twc(
          "z-100 fixed top-0 left-0 w-full h-full bg-grey1/80 flex items-center justify-center select-text"
        ),
      ),
      EventListeners.nothingMouseEvent("mousedown"),
      EventListeners.nothingMouseEvent("mouseup"),
      EventListeners.eventNoPropagation(~key="close-setting-modal", "click", _ => Msg.SettingsMsg(
        Close(s.tab),
      )),
    },
    list{
      Html.div(
        list{
          Attrs.class(
            %twc(
              "h-3/4 w-17/20 max-w-3.5xl bg-black2 rounded-lg text-white3 flex items-center justify-center"
            ),
          ),
          EventListeners.nothingMouseEvent("click"),
          EventListeners.eventNoPropagation(~key="ept", "mouseenter", _ => Msg.EnablePanning(
            false,
          )),
          EventListeners.eventNoPropagation(~key="epf", "mouseleave", _ => Msg.EnablePanning(true)),
          Events.onCB(~key="", "keydown", Obj.magic(onKeydown)),
        },
        list{settingViewWrapper(s), closingBtn},
      ),
    },
  )
}
