// open Tc

module Html = Tea_html_extended

module T = SettingsPrivacy

let explanation = "To help us understand how people learn Dark, is it okay if we track your session in a replayable format (using Fullstory)."

let disableOmniOpen = ViewUtils.nothingMouseEvent("mousedown")

let radio = (~value: string, ~label: string, ~msg: Settings.msg, ~checked: bool): Html.html<
  AppTypes.msg,
> => {
  let key = "fs-consent-" ++ value
  Html.div(
    list{Attrs.class'("choice"), disableOmniOpen},
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
    list{Attrs.class'("setting-row")},
    list{
      Html.div(
        list{Attrs.class'("setting-label")},
        list{
          Html.div(list{Attrs.class'("title")}, list{Html.text("Record me using Dark")}),
          Html.div(list{Attrs.class'("description")}, list{Html.text(explanation)}),
        },
      ),
      Html.div(
        list{Attrs.class'("setting-control")},
        list{
          radio(
            ~value="yes",
            ~label=yes,
            ~msg=Settings.PrivacyMsg(SettingsPrivacy.SetRecordConsent(true)),
            ~checked=recordConsent == Some(true),
          ),
          radio(
            ~value="no",
            ~label=no,
            ~msg=Settings.PrivacyMsg(SettingsPrivacy.SetRecordConsent(false)),
            ~checked=recordConsent == Some(false),
          ),
        },
      ),
    },
  )
}

let view = (state: T.t): list<Html.html<AppTypes.msg>> => {
  list{consentRow(state.recordConsent, ~longLabels=false)}
}

let viewTopbar = (state: T.t): Html.html<AppTypes.msg> => {
  let content = list{consentRow(state.recordConsent, ~longLabels=true)}

  let cls = if state.recordConsent == None {
    "ask"
  } else {
    "hide"
  }

  Html.div(
    list{Html.classList(list{("modal-overlay", state.recordConsent == None)})},
    list{Html.div(list{Attrs.class'("fullstory-modal " ++ cls)}, content)},
  )
}
