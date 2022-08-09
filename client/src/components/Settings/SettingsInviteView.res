open Tc

module K = FluidKeyboard
module Html = Tea_html_extended
module Events = Tea.Html2.Events
module Attributes = Tea.Html2.Attributes

module Utils = SettingsUtils

module T = SettingsInviteState

let view = (state: T.t): list<Html.html<AppTypes.msg>> => {
  let introText = list{
    Html.h2(list{}, list{Html.text("Share Dark with a friend or colleague")}),
    Html.p(
      list{},
      list{
        Html.text(
          "Share the love! Invite a friend, and we'll send them an email saying you invited them.",
        ),
      },
    ),
    Html.p(
      list{},
      list{
        Html.text(
          "Note: This will not add them to any of your existing organizations or canvases.",
        ),
      },
    ),
  }

  let inviteform = {
    let submitBtn = {
      let btn = if state.loading {
        list{ViewUtils.fontAwesome("spinner"), Html.h3(list{}, list{Html.text("Loading")})}
      } else {
        list{Html.h3(list{}, list{Html.text("Send invite")})}
      }

      Html.button(
        list{
          Html.class'("submit-btn"),
          Html.Attributes.disabled(state.loading),
          ViewUtils.eventNoPropagation(
            ~key="close-settings-modal",
            "click",
            _ => AppTypes.Msg.SettingsMsg(SettingsState.InviteMsg(T.Submit)),
          ),
        },
        btn,
      )
    }

    list{
      Html.div(
        list{Html.class'("invite-form")},
        list{
          Html.div(
            list{Html.class'("form-field")},
            list{
              Html.h3(list{}, list{Html.text("Email:")}),
              Html.div(
                list{},
                list{
                  Html.input'(
                    list{
                      Vdom.attribute("", "spellcheck", "false"),
                      Events.onInput(str => AppTypes.Msg.SettingsMsg(
                        SettingsState.InviteMsg(T.Update(str)),
                      )),
                      Attributes.value(state.email.value),
                    },
                    list{},
                  ),
                  Html.p(
                    list{Html.class'("error-text")},
                    list{Html.text(state.email.error |> Option.unwrap(~default=""))},
                  ),
                },
              ),
            },
          ),
          submitBtn,
        },
      ),
    }
  }

  Belt.List.concat(introText, inviteform)
}
