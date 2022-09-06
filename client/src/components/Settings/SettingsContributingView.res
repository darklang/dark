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

let viewSidebarDebuggerToggle = (showSidebarDebuggerPanel: bool): Html.html<AppTypes.msg> => {
  let toggle = {
    let attr = EventListeners.eventNoPropagation(
      ~key=`toggle-sidebar-debugger-${string_of_bool(showSidebarDebuggerPanel)}`,
      "click",
      _ => Msg.SettingsMsg(
        Settings.ContributingMsg(
          SettingsContributing.GeneralMsg(T.General.SetSidebarPanel(!showSidebarDebuggerPanel)),
        ),
      ),
    )
    C.toggleButton(attr, showSidebarDebuggerPanel)
  }
  let info = Some("Show a menu in the (closed) sidebar with debugging options. These are
    useful when working on the Darklang client (note they are not typically
    useful for writing Darklang code)")
  C.settingRow("Show debugging options", ~info, ~error=None, list{toggle})
}

let viewAllowTuples = (allowTuples: bool): Html.html<AppTypes.msg> => {
  let toggle = {
    let attr = EventListeners.eventNoPropagation(
      ~key=`toggle-allow-tuples-${string_of_bool(allowTuples)}`,
      "click",
      _ => Msg.SettingsMsg(
        Settings.ContributingMsg(
          SettingsContributing.InProgressFeaturesMsg(
            T.InProgressFeatures.SetTuplesAllowed(!allowTuples),
          ),
        ),
      ),
    )
    C.toggleButton(attr, allowTuples)
  }
  let info = Some("Tuples are currently being added to the Dark language - with
    this setting on, you'll be able to use tuples in your Dark code before the
    user experience around them is stable.")
  C.settingRow("Enable tuples", ~info, ~error=None, list{toggle})
}

let viewGeneral = (s: T.General.t): list<Html.html<AppTypes.msg>> => {
  list{C.sectionHeading("General", None), viewSidebarDebuggerToggle(s.showSidebarDebuggerPanel)}
}

let viewInProgressFeatures = (s: T.InProgressFeatures.t): list<Html.html<AppTypes.msg>> => {
  list{C.sectionHeading("In-Progress Features", None), viewAllowTuples(s.allowTuples)}
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
      Html.span(list{Attrs.class(%twc("h-6 text-grey6 mr-1"))}, list{Html.text("https://")}),
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
      ~style="ml-2",
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

  Html.div(list{Attrs.class(%twc("align-baseline"))}, list{row})
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

let viewTunnelSection = (s: T.t): list<Html.html<AppTypes.msg>> => {
  Belt.List.concatMany([
    viewTunnelSectionHeader,
    list{viewTunnelHost(s.tunnelHost)},
    list{viewTunnelToggle(s.useAssets)},
  ])
}

let view = (s: T.t): list<Html.html<AppTypes.msg>> => {
  Belt.List.concatMany([
    list{viewIntroText},
    viewGeneral(s.general),
    viewInProgressFeatures(s.inProgressFeatures),
    viewTunnelSection(s),
  ])
}
