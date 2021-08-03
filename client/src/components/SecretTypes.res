@ppx.deriving(show)
type t = {
  secretName: string,
  secretValue: string,
}

@ppx.deriving(show)
type msg =
  | OpenCreateModal
  | CloseCreateModal
  | OnUpdateName(string)
  | OnUpdateValue(string)
  | SaveNewSecret

@ppx.deriving(show)
type insertModal = {
  newSecretName: string,
  isNameValid: bool,
  newSecretValue: string,
  isValueValid: bool,
  error: option<string>,
  visible: bool,
  usedNames: list<string>,
}

let defaultInsertModal = {
  newSecretName: "",
  isNameValid: true,
  newSecretValue: "",
  isValueValid: true,
  error: None,
  visible: false,
  usedNames: list{},
}

let getSecretValue = (s: t): string => s.secretValue
