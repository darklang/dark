// open Tc

module T = SettingsPrivacyState

let update = (_: T.t, msg: T.msg): T.t => {
  switch msg {
  | InitRecordConsent(recordConsent) => {recordConsent: recordConsent}
  | SetRecordConsent(allow) => {recordConsent: Some(allow)}
  }
}
