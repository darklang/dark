open Tc

module Html = Tea.Html
module Events = Tea.Html.Events
module Attrs = Tea.Html.Attributes

module K = FluidKeyboard
module Utils = SettingsUtils

module T = SettingsInvite

module C = SettingsViewComponents

let tw = Attrs.class

let view = (state: T.t): list<Html.html<AppTypes.msg>> => {
  let introText = list{
    C.sectionHeading("Share Dark with a friend or colleague", None),
    Html.p(
      list{tw(%twc("font-text text-lg"))},
      list{
        Html.text(
          "Share the love! Invite a friend, and we'll send them an email saying you invited them.",
        ),
      },
    ),
  }

  let note = list{
    C.sectionIntroText(list{
      Html.text("Note: This will not add them to any of your existing organizations or canvases."),
    }),
  }

  let inviteform = {
    let submitBtn = {
      let btn = if state.loading {
        list{
          Icons.fontAwesome("spinner"),
          Html.h4(list{tw(%twc("font-text tracking-wider m-0 pl-2"))}, list{Html.text("Sending")}),
        }
      } else {
        list{Html.h4(list{tw(%twc("font-text tracking-wider m-0"))}, list{Html.text("Invite")})}
      }

      C.submitBtn(
        ~style="",
        Html.Attributes.disabled(state.loading),
        EventListeners.eventNoPropagation(
          ~key="close-settings-modal",
          "click",
          _ => AppTypes.Msg.SettingsMsg(Settings.InviteMsg(T.Submit)),
        ),
        btn,
      )
    }

    let field = Html.div(
      list{},
      list{
        C.input(
          ~loadStatus=LoadStatus.Success(""),
          ~style="w-80",
          ~attrs=list{
            Attrs.spellcheck(false),
            Events.onInput(str => AppTypes.Msg.SettingsMsg(Settings.InviteMsg(T.Update(str)))),
            Attrs.value(state.email.value),
          },
          "",
        ),
        C.errorSpan(state.email.error |> Option.unwrap(~default="")),
      },
    )

    list{
      Html.div(
        list{tw(%twc("flex items-baseline justify-center my-2"))},
        list{
          Html.p(
            list{tw(%twc("font-text text-lg mr-4 font-semibold tracking-wider"))},
            list{Html.text("Email:")},
          ),
          field,
          submitBtn,
        },
      ),
    }
  }
  Belt.List.concatMany([introText, inviteform, note])
}
