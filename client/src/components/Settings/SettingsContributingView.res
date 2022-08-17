// open Tc

module Html = Tea_html_extended
module Events = Tea.Html2.Events
module Attrs = Tea.Html2.Attributes

module Utils = SettingsUtils
module T = SettingsContributing

let viewTunnel = (_svs: T.t): list<Html.html<AppTypes.msg>> => {
  let introText = list{
    Html.h3(list{}, list{Html.text("Tunnel your local client")}),
    Html.p(
      list{},
      list{
        Html.text(
          "If you're working on the Darklang client, you can load it against this canvas by entering your tunnel link",
          //  (the link provided by your tunneling proider, such as Ngrok or Localtunnel, such as |https://seven-wings-sniff-69-204-249-142.loca.lt)",
        ),
      },
    ),
  }

  let form = {
    let submitBtn = {
      let btn = list{
        Html.h3(
          list{
            ViewUtils.eventNoPropagation(~key="close-settings-modal", "click", _ => SettingsMsg(
              Settings.ContributingMsg(T.SubmitTunnelHostForm),
            )),
          },
          list{Html.text("Reload with tunnel")},
        ),
      }

      Html.button(list{Html.class'("submit-btn")}, btn)
    }

    list{
      Html.div(
        list{Html.class'("tunnel-form")},
        list{
          Html.div(
            list{Html.class'("form-field")},
            list{
              Html.h3(list{}, list{Html.text("Tunnel URL:")}),
              Html.div(
                list{
                  Events.onInput(str => AppTypes.Msg.SettingsMsg(
                    Settings.ContributingMsg(T.UpdateTunnelHostInput(str)),
                  )),
                },
                list{
                  Html.input'(list{Vdom.attribute("", "spellcheck", "false")}, list{}),
                  Html.p(list{Html.class'("error-text")}, list{Html.text(" ")}),
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
        Html.a(list{Attrs.href("https://github.com/darklang/dark")}, list{Html.text("dark repo")}),
        Html.text("and read the "),
        Html.a(
          list{Attrs.href("https://docs.darklang.com/contributing/getting-started")},
          list{Html.text("contributor docs")},
        ),
      },
    ),
  }

  Belt.List.concat(introText, viewTunnel(s))
}
