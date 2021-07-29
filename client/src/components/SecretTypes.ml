type t =
  { secretName : string
  ; secretValue : string }
[@@ppx.deriving show]

type msg =
  | OpenCreateModal
  | CloseCreateModal
  | OnUpdateName of string
  | OnUpdateValue of string
  | SaveNewSecret
[@@ppx.deriving show]

type insertModal =
  { newSecretName : string
  ; isNameValid : bool
  ; newSecretValue : string
  ; isValueValid : bool
  ; error : string option
  ; visible : bool
  ; usedNames : string list }
[@@ppx.deriving show]

let defaultInsertModal =
  { newSecretName = ""
  ; isNameValid = true
  ; newSecretValue = ""
  ; isValueValid = true
  ; error = None
  ; visible = false
  ; usedNames = [] }


let getSecretValue (s : t) : string = s.secretValue
