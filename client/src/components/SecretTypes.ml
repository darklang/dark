type t =
  { secretName : string
  ; secretValue : string
  ; secretVersion : int }
  [@@deriving show]

type msg =
  | OpenCreateModal
  | CloseCreateModal
  | SaveNewSecret
  [@@deriving show]

type createModal = { newSecret: t ; visible: bool }
[@@deriving show]

let defaultCreateModal = { newSecret = {secretName = "" ; secretValue = "" ;secretVersion = 0}; visible = false}