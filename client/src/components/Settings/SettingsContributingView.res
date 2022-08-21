// open Tc

module Html = Tea.Html
module Events = Tea.Html.Events
module Attrs = Tea.Html.Attributes

module Utils = SettingsUtils
module T = SettingsContributing

let viewTunnel = (_svs: T.t): list<Html.html<AppTypes.msg>> => {
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
        Html.text(". After starting the tunnel, enter the url below"),
      },
    ),
  }

  let form = {
    let submitBtn = {
      let btn = list{
        Html.h3(
          list{
            ViewUtils.eventNoPropagation(~key="close-settings-modal", "click", _ => SettingsMsg(
              Settings.ContributingMsg(T.Submit),
            )),
          },
          list{Html.text("Reload with tunnel")},
        ),
      }

      Html.button(list{Attrs.class'("submit-btn")}, btn)
    }

    let _toggle = {
      Html.button(
        list{
          Attrs.class'(
            "bg-gray-200 relative inline-flex flex-shrink-0 h-6 w-11 border-2 border-transparent rounded-full cursor-pointer transition-colors ease-in-out duration-200 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500",
          ),
          ViewUtils.eventNoPropagation(~key="close-settings-modal", "click", _ => SettingsMsg(
            Settings.ContributingMsg(T.Submit),
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

    list{
      Html.div(
        list{Attrs.class'("tunnel-form")},
        list{
          Html.div(
            list{Attrs.class'("form-field")},
            list{
              Html.h3(list{}, list{Html.text("Tunnel URL:")}),
              Html.div(
                list{
                  Events.onInput(str => AppTypes.Msg.SettingsMsg(
                    Settings.ContributingMsg(T.InputEdit(str)),
                  )),
                },
                list{
                  Html.input'(list{Vdom.attribute("", "spellcheck", "false")}, list{}),
                  Html.p(list{Attrs.class'("error-text")}, list{Html.text(" ")}),
                },
              ),
            },
          ),
          submitBtn,
        },
      ),
    }
  }

  Belt.List.concat(introText, form)
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

  Belt.List.concat(introText, viewTunnel(s))
}
