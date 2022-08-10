// open Tc

module Cmd = Tea.Cmd
module Attr = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module Html = Tea_html_extended

module FullstoryJs = {
  @val @scope(("window", "Dark", "fullstory")) external _setConsent: bool => unit = "setConsent"

  let setConsent = (allow: bool): AppTypes.cmd => Cmd.call(_ => _setConsent(allow))
}

let explanation = "To help us understand how people learn Dark, is it okay if we track your session in a replayable format (using Fullstory)."

let disableOmniOpen = ViewUtils.nothingMouseEvent("mousedown")

let radio = (~value: string, ~label: string, ~msg: SettingsState.msg, ~checked: bool): Html.html<
  AppTypes.msg,
> => {
  let key = "fs-consent-" ++ value
  Html.div(
    list{Html.class'("choice"), disableOmniOpen},
    list{
      Html.input'(
        list{
          Html.type'("radio"),
          Html.id(key),
          Html.name("fs-consent"),
          Html.value(value),
          Html.checked(checked),
          ViewUtils.eventNoPropagation(~key, "click", _ => SettingsMsg(msg)),
        },
        list{},
      ),
      Html.label(list{Html.for'(key)}, list{Html.text(label)}),
    },
  )
}

let consentRow = (recordConsent: option<bool>, ~longLabels: bool): Html.html<AppTypes.msg> => {
  let (yes, no) = if longLabels {
    ("Yes, please go ahead", "No, please don't")
  } else {
    ("Yes", "No")
  }

  Html.div(
    list{Html.class'("setting-row")},
    list{
      Html.div(
        list{Html.class'("setting-label")},
        list{
          Html.div(list{Html.class'("title")}, list{Html.text("Record me using Dark")}),
          Html.div(list{Html.class'("description")}, list{Html.text(explanation)}),
        },
      ),
      Html.div(
        list{Html.class'("setting-control")},
        list{
          radio(
            ~value="yes",
            ~label=yes,
            ~msg=SettingsState.PrivacyMsg(SettingsPrivacy.SetRecordConsent(true)),
            ~checked=recordConsent == Some(true),
          ),
          radio(
            ~value="no",
            ~label=no,
            ~msg=SettingsState.PrivacyMsg(SettingsPrivacy.SetRecordConsent(false)),
            ~checked=recordConsent == Some(false),
          ),
        },
      ),
    },
  )
}

let html = (m: AppTypes.model): Html.html<AppTypes.msg> => {
  let content = list{consentRow(m.settingsView.privacySettings.recordConsent, ~longLabels=true)}

  let cls = if m.settingsView.privacySettings.recordConsent == None {
    "ask"
  } else {
    "hide"
  }

  Html.div(
    list{
      Html.classList(list{
        (
          "modal-overlay",
          m.settingsView.privacySettings.recordConsent == None &&
            m.integrationTestState == NoIntegrationTest,
        ),
      }),
    },
    list{Html.div(list{Html.class'("fullstory-modal " ++ cls)}, content)},
  )
}
