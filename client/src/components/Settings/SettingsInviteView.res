open Tc

module Html = Tea.Html
module Events = Tea.Html.Events
module Attrs = Tea.Html.Attributes

module K = FluidKeyboard
module Utils = SettingsUtils

module T = SettingsInvite

module C = SettingsViewComponents

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
        list{
          Icons.fontAwesome("spinner"),
          Html.h3(list{Attrs.class(%twc("m-0 pl-2.5"))}, list{Html.text("Loading")}),
        }
      } else {
        list{Html.h3(list{Attrs.class(%twc("m-0"))}, list{Html.text("Send invite")})}
      }

      C.submitBtn(
        ~style="ml-2",
        Html.Attributes.disabled(state.loading),
        EventListeners.eventNoPropagation(
          ~key="close-settings-modal",
          "click",
          _ => AppTypes.Msg.SettingsMsg(Settings.InviteMsg(T.Submit)),
        ),
        btn,
      )
    }
    list{
      Html.div(
        list{Attrs.class(%twc("flex items-center justify-center flex-col"))},
        list{
          Html.div(
            list{
              Attrs.class(
                %twc("flex items-baseline justify-around w-full max-w-lg mt-6 flex-wrap"),
              ),
            },
            list{
              Html.h3(list{Attrs.class(%twc("m-0"))}, list{Html.text("Email:")}),
              Html.div(
                list{},
                list{
                  C.emailInput(
                    ~style="",
                    ~attrs=list{
                      Attrs.spellcheck(false),
                      Events.onInput(str => AppTypes.Msg.SettingsMsg(
                        Settings.InviteMsg(T.Update(str)),
                      )),
                      Attrs.value(state.email.value),
                    },
                  ),
                  C.errorSpan(state.email.error |> Option.unwrap(~default="")),
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
