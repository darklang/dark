type t = string option [@@deriving show]

let clear _ = None

let default = None

let set str _ = Some str

let asOption t = t
