// CLEANUP: the naming scheme here doesn't match everything else
// hmmm: fix this^

@ppx.deriving(show)
type rec t = {
  // CLEANUP: change field names
  secretName: string,
  secretValue: string,
}

let decode = (j): t => {
  open Json_decode_extended
  {
    secretName: field("name", string, j),
    secretValue: field("value", string, j),
  }
}

let encode = (secret: t): Js.Json.t => {
  open Json_encode_extended
  object_(list{("name", string(secret.secretName)), ("value", string(secret.secretValue))})
}

@ppx.deriving(show)
type rec msg =
  | OpenCreateModal
  | CloseCreateModal
  | OnUpdateName(string)
  | OnUpdateValue(string)
  | SaveNewSecret

@ppx.deriving(show)
type rec insertModal = {
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
