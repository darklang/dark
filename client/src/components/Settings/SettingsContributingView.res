// open Tc

module Html = Tea.Html
module Events = Tea.Html.Events
module Attrs = Tea.Html.Attributes

module Utils = SettingsUtils
module T = SettingsContributing

let viewTunnel = (th: T.TunnelHost.t): list<Html.html<AppTypes.msg>> => {
  let introText = list{
    Html.h3(list{}, list{Html.text("Tunnel your local client")}),
    Html.p(
      list{},
      list{
        Html.text(
          "To use your local client against the Dark server, use a tunnel provider such as ",
        ),
        Html.a(list{Attrs.href("https://localtunnel.me")}, list{Html.text("localtunnel")}),
        Html.text(" or "),
        Html.a(list{Attrs.href("https://ngrok.com")}, list{Html.text("ngrok")}),
        Html.text(". Starting the tunnel provides a hostname, which you should enter below"),
      },
    ),
  }

  let form = {
    let value = th.value->Belt.Option.getWithDefault("")
    let length = value->String.length->max(35)
    let tunnelField = Html.div(
      list{Attrs.class'("px-1 space-x-1 py-2")},
      list{
        Html.span(list{Attrs.class'("h-6 font-bold")}, list{Html.text("https://")}),
        Html.input'(
          list{
            // TODO: move colors into theme
            Attrs.class("px-2.5 h-9 bg-[#383838] text-[#d8d8d8] caret-[#b8b8b8]"),
            Attrs.size(length),
            Attrs.spellcheck(false),
            Attrs.value(value),
            Events.onInput(str => AppTypes.Msg.SettingsMsg(
              Settings.ContributingMsg(T.TunnelHostMsg(T.TunnelHost.InputEdit(str))),
            )),
          },
          list{},
        ),
        Html.p(
          list{Attrs.class'("error-text")},
          list{Html.text(th.error->Belt.Option.getWithDefault(""))},
        ),
      },
    )

    let submitBtn = {
      let btn = list{
        Html.h3(
          list{
            ViewUtils.eventNoPropagation(~key="close-settings-modal", "click", _ => SettingsMsg(
              Settings.ContributingMsg(T.TunnelHostMsg(T.TunnelHost.Submit)),
            )),
          },
          list{Html.text("Set tunnel")},
        ),
      }

      Html.button(list{Attrs.class'("submit-btn")}, btn)
    }

    list{Html.div(list{Attrs.class'("tunnel-form")}, list{tunnelField, submitBtn})}
  }

  Belt.List.concat(introText, form)
}

let viewToggle = (_: int): list<Html.html<AppTypes.msg>> => {
  let _toggle = {
    Html.button(
      list{
        Attrs.class'(
          "bg-gray-200 relative inline-flex flex-shrink-0 h-6 w-11 border-2 border-transparent rounded-full cursor-pointer transition-colors ease-in-out duration-200 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500",
        ),
        ViewUtils.eventNoPropagation(~key="close-settings-modal", "click", _ => SettingsMsg(
          Settings.ContributingMsg(
            T.UseAssetsMsg(T.UseAssets.Set(T.UseAssets.UseProductionAssets)),
          ),
        )),
        Attrs.role("switch"),
        Attrs.ariaChecked(false),
      },
      list{
        Html.span(list{Attrs.class'("sr-only")}, list{Html.text("Use setting")}),
        Html.span(
          list{
            Attrs.class'(
              "translate-x-0 pointer-events-none inline-block h-5 w-5 rounded-full bg-white shadow transform ring-0 transition ease-in-out duration-200",
            ),
            Attrs.ariaHidden(true),
          },
          list{},
        ),
      },
    )
  }
  list{}
}

let view = (s: T.t): list<Html.html<AppTypes.msg>> => {
  let introText = list{
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
    ),
  }

  Belt.List.concatMany([introText, viewToggle(5), viewTunnel(s.tunnelHost)])
}
