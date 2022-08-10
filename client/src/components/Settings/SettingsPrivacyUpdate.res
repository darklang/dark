// open Tc

module T = SettingsPrivacyState

let update = (_: T.t, msg: T.msg): (T.t, T.effect) => {
  switch msg {
  | SetRecordConsent(allow) => (
      {recordConsent: Some(allow)},
      RecordConsent(allow),
      // MakeCmd(FullstoryView.FullstoryJs.setConsent(allow)),
    )
  }
}
