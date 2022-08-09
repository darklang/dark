// open Tc

module T = SettingsPrivacyState

let update = (_: T.t, msg: T.msg): T.t => {
  switch msg {
  | SetRecordConsent(allow) => {recordConsent: Some(allow)}
  }
}
