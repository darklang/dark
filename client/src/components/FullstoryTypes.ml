type t = {consent : bool option} [@@deriving show]

let defaultT = {consent = Some true}

type msg =
  | InitConsent of bool option
  | SetConsent of bool
[@@deriving show]
