// open Tc

module Html = Tea.Html
module Events = Tea.Html.Events
module Attrs = Tea.Html.Attributes

module Utils = SettingsUtils
module T = SettingsContributing

let tw = Attrs.class // tailwind

let viewTunnel = (th: T.TunnelHost.t): list<Html.html<AppTypes.msg>> => {
  let introText = list{
    Html.span(list{tw("font-bold text-xl mt-3")}, list{Html.text("Tunnel your local client")}),
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
    let tunnelRow = {
      let value = th.value->Belt.Option.getWithDefault("")
      let loadingAttrs = switch th.loadStatus {
      | Loading => list{Attrs.disabled(true)}
      | Loaded(_) => list{Attrs.noProp}
      }
      let loadingSpinner = switch th.loadStatus {
      | Loading => Html.i(list{Attrs.class("fa fa-spinner -ml-5 text-[#e8e8e8]")}, list{})
      | Loaded(_) => Vdom.noNode
      }
      let tunnelField = Html.span(
        list{tw("px-2.5 py-2")},
        list{
          Html.span(list{tw("h-6 font-bold mr-1")}, list{Html.text("https://")}),
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
      let tunnelButton = {
        let savingSpinner = switch th.saveStatus {
        | Saving => Html.i(list{Attrs.class("fa fa-spinner text-[#e8e8e8] px-1")}, list{})
        | Saved => Html.i(list{Attrs.class("fa fa-check text-[#a1b56c] px-1")}, list{})
        | NotSaving => Vdom.noNode
        }
        Html.button(
          list{
            tw(
              "rounded h-9 px-2.5 py-1 bg-[#585858] hover:bg-[#484848] text-[#d8d8d8] cursor-pointer text-xl font-bold align-top",
            ),
            ViewUtils.eventNoPropagation(~key="tunnel-button-set", "click", _ => SettingsMsg(
              Settings.ContributingMsg(T.TunnelHostMsg(T.TunnelHost.Submit)),
            )),
          },
          list{savingSpinner, Html.text("Set")},
        )
      }

      Html.div(list{tw("align-baseline")}, list{tunnelField, tunnelButton})
    }

    let tunnelError = Html.span(
      list{},
      list{
        Html.p(
          list{Attrs.class("error-text")},
          list{Html.text(th.error->Belt.Option.getWithDefault(""))},
        ),
      },
    )

    list{Html.div(list{Attrs.class("tunnel-form")}, list{tunnelRow, tunnelError})}
  }

  Belt.List.concat(introText, form)
}

let viewToggle = (_: int): list<Html.html<AppTypes.msg>> => {
  let toggle = {
    Html.button(
      list{
        tw(
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
        Html.span(list{tw("sr-only")}, list{Html.text("Use setting")}),
        Html.span(
          list{
            tw(
              "translate-x-0 pointer-events-none inline-block h-5 w-5 rounded-full bg-white shadow transform ring-0 transition ease-in-out duration-200",
            ),
            Attrs.ariaHidden(true),
          },
          list{},
        ),
      },
    )
  }
  list{toggle}
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
