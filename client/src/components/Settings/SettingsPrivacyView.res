// open Tc

module Html = Tea_html_extended

module Utils = SettingsUtils
module T = SettingsPrivacyState

let view = (state: T.t): list<Html.html<AppTypes.msg>> => {
  list{FullstoryView.consentRow(state.recordConsent, ~longLabels=false)}
}
