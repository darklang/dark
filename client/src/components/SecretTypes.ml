type t =
  { secretName : string
  ; secretValue : string }
[@@deriving show]

type msg =
  | OpenCreateModal
  | CloseCreateModal
  | OnUpdateName of string
  | OnUpdateValue of string
  | SaveNewSecret
[@@deriving show]

type insertModal =
  { newSecretName : string
  ; isNameValid : bool
  ; newSecretValue : string
  ; isValueValid : bool
  ; error : string option
  ; visible : bool
  ; usedNames : string list }
[@@deriving show]

let defaultInsertModal =
  { newSecretName = ""
  ; isNameValid = true
  ; newSecretValue = ""
  ; isValueValid = true
  ; error = None
  ; visible = false
  ; usedNames = [] }
