type t =
  { consent : bool option
  ; recording : bool }
[@@deriving show]

let defaultT = {consent = Some true; recording = false}

type msg =
  | InitConsent of bool option
  | SetConsent of bool
  | PauseRecording
  | RestartRecording
[@@deriving show]
