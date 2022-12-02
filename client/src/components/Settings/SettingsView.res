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
          (%twc("h-full m-0 mb-[-3px] px-2.5 py-0 text-grey2 cursor-pointer"), true),
          (%twc("text-white3 border-solid border-b-[3px] border-b-grey8"), isSameTab),
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
    list{Attrs.class(%twc("flex w-full border-solid border-b-[3px] border-grey1"))},
    List.map(allTabs, ~f=tabTitle),
  )
}

let settingViewWrapper = (acc: T.t): Html.html<msg> => {
  let tabView = settingsTabToHtml(acc)
  Html.div(
    list{
      Attrs.class(
        %twc(
          "w-[calc(100%-30px)] flex flex-col h-full overflow-auto mt-2.5 mr-[5px] mb-2.5 ml-[35px] scrollbar-none"
        ),
      ),
    },
    list{Html.h1(list{}, list{Html.text("Settings")}), tabTitleView(acc.tab), ...tabView},
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
          "relative self-start w-[30px] cursor-pointer flex items-center justify-center mt-[5px] text-grey8 hover:text-grey2"
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
          "z-[100] fixed top-0 left-0 w-full h-full bg-[#484848cc] flex items-center justify-center"
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
              "h-[75%] w-[85%] max-w-[800px] bg-black2 rounded-[7px] text-white3 flex items-center justify-center"
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
