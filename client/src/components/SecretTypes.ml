type t =
  { secretName : string
  ; secretValue : string
  ; secretVersion : int }
  [@@deriving show]

type msg =
  | OpenCreateModal
  | CloseCreateModal
  | OnUpdateName of string
  | OnUpdateValue of string
  | SaveNewSecret
  [@@deriving show]

type createModal =
  { secretName : string
  ; isNameValid : bool
  ; secretValue : string
  ; isValueValid : bool
  ; error : string option
  ; visible: bool }
[@@deriving show]

let defaultCreateModal =
  { secretName = ""
  ; isNameValid = true
  ; secretValue = ""
  ; isValueValid = true
  ; error = None
  ; visible = false}