// open Tc

module Utils = SettingsUtils
module T = SettingsContributing

open SettingsViewPrelude

let viewTunnel = (th: T.TunnelHost.t): list<Html.html<AppTypes.msg>> => {
  let introText = list{
    sectionHeading("Tunnel your local client"),
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

let viewToggle = (s: T.UseAssets.t): list<Html.html<AppTypes.msg>> => {
  let toggle = {
    let enabled = s == UseTunnelAssets
    let attr = ViewUtils.eventNoPropagation(~key="toggle-settings", "click", _ => SettingsMsg(
      Settings.ContributingMsg(T.UseAssetsMsg(T.UseAssets.Toggle)),
    ))
    toggleButton(attr, enabled)
  }
  list{
    Html.div(
      list{tw("mt-8 flex justify-center")},
      list{
        Html.div(
          list{tw("text-center bg-[#383838] py-4 px-16 rounded")},
          list{
            Html.span(
              list{tw("inline-block align-top text-xl pr-8")},
              list{Html.text("Use tunneled assets")},
            ),
            toggle,
          },
        ),
      },
    ),
  }
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

  Belt.List.concatMany([introText, viewTunnel(s.tunnelHost), viewToggle(s.useAssets)])
}
