// open Tc

module Html = Tea.Html
module Attrs = Tea.Attrs

module T = SettingsPrivacy

module Msg = AppTypes.Msg

module C = SettingsViewComponents

let explanation = "To help us understand how people learn Dark, is it okay if we track your session in a replayable format (using Fullstory)."

let viewFSConsent = (checked: bool): Html.html<AppTypes.msg> => {
  let toggle = {
    let attr = EventListeners.eventNoPropagation(
      ~key = `toggle-fs-consent-${string_of_bool(checked)}`,
      "click",
      _ => Msg.SettingsMsg(
        Settings.PrivacyMsg(SettingsPrivacy.SetRecordConsent(!checked))
      ),
    )
    C.toggleButton(attr, checked)
  }

  Html.div(list{Attrs.class(%twc("mt-10"))}, list{
    C.settingRow(
      ~info=None,
      ~error=None,
      "",
      list{
        Html.div(list{Attrs.class(%twc("flex"))}, list{
        Html.span(list{}, list{
          C.sectionHeading("Record me using Dark", None),
          C.sectionIntroText(list{Html.text(explanation)})
        }),
        Html.span(list{Attrs.class(%twc("mt-6"))},list{toggle})
        })
      }
    )}
  )
}

let view = (state: T.t): list<Html.html<AppTypes.msg>> => {
  list{viewFSConsent(state.recordConsent|> Belt.Option.getExn)}
}

