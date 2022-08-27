// open Tc

module Utils = SettingsUtils
module T = SettingsContributing

module Html = Tea.Html
module Events = Tea.Html.Events
module Attrs = Tea.Html.Attributes

module C = SettingsViewComponents

let viewTunnelSectionHeader = {
  let info = Html.p(
    list{},
    list{
      Html.text("To use your local client against the Dark server, use a tunnel provider such as "),
      Html.a(list{Attrs.href("https://localtunnel.me")}, list{Html.text("localtunnel")}),
      Html.text(" or "),
      Html.a(list{Attrs.href("https://ngrok.com")}, list{Html.text("ngrok")}),
      Html.text(". Starting the tunnel provides a hostname, which you should enter below"),
    },
  )
  C.sectionHeading("Tunnel", Some(info))
}

let viewTunnelHost = (th: T.TunnelHost.t): Html.html<AppTypes.msg> => {
  let value = th.value->Belt.Option.getWithDefault("")
  let loadingAttrs = switch th.loadStatus {
  | LoadStatus.Loading => list{Attrs.disabled(true)}
  | LoadStatus.Success(_)
  | LoadStatus.Error => list{Attrs.noProp}
  }
  let loadingSpinner = switch th.loadStatus {
  | LoadStatus.Loading => Html.i(list{Attrs.class("fa fa-spinner -ml-5 text-[#e8e8e8]")}, list{})
  | LoadStatus.Success(_)
  | LoadStatus.Error => Vdom.noNode
  }
  let field = Html.span(
    list{C.tw("px-2.5 py-2")},
    list{
      Html.span(list{C.tw("h-6 font-bold mr-1")}, list{Html.text("https://")}),
      Html.input'(
        List.append(
          loadingAttrs,
          list{
            // TODO: move colors into theme
            Attrs.class("px-2.5 h-9 bg-[#383838] text-[#d8d8d8] caret-[#b8b8b8]"),
            Attrs.placeholder("hostname"),
            Attrs.size(25),
            Attrs.spellcheck(false),
            Attrs.value(value),
            Events.onInput(str => AppTypes.Msg.SettingsMsg(
              Settings.ContributingMsg(T.TunnelHostMsg(T.TunnelHost.InputEdit(str))),
            )),
          },
        ),
        list{},
      ),
      loadingSpinner,
    },
  )
  let button = {
    C.button(
      ViewUtils.eventNoPropagation(~key="tunnel-button-set", "click", _ => SettingsMsg(
        Settings.ContributingMsg(T.TunnelHostMsg(T.TunnelHost.Submit)),
      )),
      th.saveStatus,
      list{Html.text("Set")},
    )
  }
  let row = C.settingRow(~info=None, ~error=None, "Tunnel url", list{field, button})

  Html.div(list{C.tw("align-baseline")}, list{row})
}

let viewTunnelToggle = (s: T.UseAssets.t): Html.html<AppTypes.msg> => {
  let toggle = {
    let enabled = s == UseTunnelAssets
    let attr = ViewUtils.eventNoPropagation(~key="toggle-settings", "click", _ => SettingsMsg(
      Settings.ContributingMsg(T.UseAssetsMsg(T.UseAssets.Toggle)),
    ))
    C.toggleButton(attr, enabled)
  }

  C.settingRow("Use tunneled assets", ~info=None, ~error=None, list{toggle})
}

let viewDebuggingOption = (ui: T.ContributorUI.t): Html.html<AppTypes.msg> => {
  let toggle = {
    let attr = ViewUtils.eventNoPropagation(~key="toggle-settings", "click", _ => SettingsMsg(
      Settings.ContributingMsg(
        SettingsContributing.ContributorUIMsg(
          T.ContributorUI.SetFluidDebugger(!ui.showFluidDebugger),
        ),
      ),
    ))
    C.toggleButton(attr, ui.showFluidDebugger)
  }
  let info = Some(
    "Show a menu in the (closed) sidebar with debugging options useful when working on the Darklang client",
  )
  C.settingRow("Show debugging options", ~info, ~error=None, list{toggle})
}

let viewIntroText = {
  Html.p(
    list{},
    list{
      Html.text("To contribute to Dark, check out the "),
      Html.a(list{Attrs.href("https://github.com/darklang/dark")}, list{Html.text("Dark repo")}),
      Html.text(" and read the "),
      Html.a(
        list{Attrs.href("https://docs.darklang.com/contributing/getting-started")},
        list{Html.text("contributor docs")},
      ),
    },
  )
}

let view = (s: T.t): list<Html.html<AppTypes.msg>> => {
  Belt.List.concatMany([
    list{viewIntroText},
    list{C.sectionHeading("Tools", None)},
    list{viewDebuggingOption(s.contributorUI)},
    list{viewTunnelSectionHeader},
    list{viewTunnelHost(s.tunnelHost)},
    list{viewTunnelToggle(s.useAssets)},
  ])
}
