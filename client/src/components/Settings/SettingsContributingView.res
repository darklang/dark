// open Tc

module Utils = SettingsUtils
module T = SettingsContributing

module Html = Tea.Html
module Events = Tea.Html.Events
module Attrs = Tea.Html.Attributes

module Msg = AppTypes.Msg

module C = SettingsViewComponents

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

let viewTools = (ui: T.Tools.t): Html.html<AppTypes.msg> => {
  let toggle = {
    let attr = EventListeners.eventNoPropagation(
      ~key=`toggle-settings-${string_of_bool(ui.showSidebarPanel)}`,
      "click",
      _ => Msg.SettingsMsg(
        Settings.ContributingMsg(
          SettingsContributing.ToolsMsg(T.Tools.SetSidebarPanel(!ui.showSidebarPanel)),
        ),
      ),
    )
    C.toggleButton(attr, ui.showSidebarPanel)
  }
  let info = Some(
    "Show a menu in the (closed) sidebar with debugging options. These are useful when working on the Darklang client (note they are not typically useful for writing Darklang code)",
  )
  C.settingRow("Show debugging options", ~info, ~error=None, list{toggle})
}

let viewTunnelSectionHeader = {
  list{
    C.sectionHeading("Tunnel", None),
    C.sectionIntroText(list{
      Html.text("When working on the Darklang client, you can use a tunnel service (such as "),
      Html.a(list{Attrs.href("https://localtunnel.me")}, list{Html.text("localtunnel")}),
      Html.text(" or "),
      Html.a(list{Attrs.href("https://ngrok.com")}, list{Html.text("ngrok")}),
      Html.text(") to use your local client against the Darklang production API"),
    }),
  }
}

let viewTunnelHost = (th: T.TunnelHost.t): Html.html<AppTypes.msg> => {
  let field = Html.span(
    list{},
    list{
      Html.span(list{C.tailwind("h-6 text-[#989898] mr-1")}, list{Html.text("https://")}),
      C.input(
        ~loadStatus=th.loadStatus,
        ~attrs=list{
          Attrs.placeholder("hostname"),
          Attrs.size(35),
          Attrs.spellcheck(false),
          Events.onInput(str => AppTypes.Msg.SettingsMsg(
            Settings.ContributingMsg(T.TunnelHostMsg(T.TunnelHost.InputEdit(str))),
          )),
        },
        th.value->Belt.Option.getWithDefault(""),
      ),
    },
  )
  let button = {
    C.button(
      ~tw="ml-2",
      EventListeners.eventNoPropagation(~key="tunnel-button-set", "click", _ => Msg.SettingsMsg(
        Settings.ContributingMsg(T.TunnelHostMsg(T.TunnelHost.Submit)),
      )),
      th.saveStatus,
      list{Html.text("Set")},
    )
  }
  let row = C.settingRow(
    ~info=Some(
      "Your tunnel provider (eg Ngrok) will provide this hostname once you start the tunnel",
    ),
    ~error=None,
    "Tunnel hostname",
    list{field, button},
  )

  Html.div(list{C.tailwind("align-baseline")}, list{row})
}

let viewTunnelToggle = (s: T.UseAssets.t): Html.html<AppTypes.msg> => {
  let toggle = {
    let enabled = s == UseTunnelAssets
    let attr = EventListeners.eventNoPropagation(
      ~key="toggle-settings",
      "click",
      _ => Msg.SettingsMsg(Settings.ContributingMsg(T.UseAssetsMsg(T.UseAssets.Toggle))),
    )
    C.toggleButton(attr, enabled)
  }

  C.settingRow("Use tunneled assets", ~info=None, ~error=None, list{toggle})
}

let view = (s: T.t): list<Html.html<AppTypes.msg>> => {
  Belt.List.concatMany([
    list{viewIntroText},
    list{C.sectionHeading("Tools", None)},
    list{viewTools(s.tools)},
    viewTunnelSectionHeader,
    list{viewTunnelHost(s.tunnelHost)},
    list{viewTunnelToggle(s.useAssets)},
  ])
}
