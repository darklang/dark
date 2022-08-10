// open Tc

module T = SettingsPrivacyState

let update = (_: T.t, msg: T.msg): (T.t, AppTypes.modification) => {
  switch msg {
  | SetRecordConsent(allow) => (
      {recordConsent: Some(allow)},
      MakeCmd(FullstoryView.FullstoryJs.setConsent(allow)),
    )
  }
}
