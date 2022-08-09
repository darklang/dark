@ppx.deriving(show)
type rec formField = {
  value: string,
  error: option<string>,
}

let defaultFormField = {value: "", error: None}
