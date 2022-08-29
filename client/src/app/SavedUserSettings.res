open Prelude

let toModel = (m: AppTypes.model, e: AppTypes.SavedSettings.User.t): AppTypes.model => {
  ...m,
  firstVisitToDark: e.firstVisitToDark,
  settings: {...m.settings, privacySettings: {recordConsent: e.recordConsent}},
}

let model2editor = (m: AppTypes.model): AppTypes.SavedSettings.User.t => {
  firstVisitToDark: m.firstVisitToDark,
  recordConsent: m.settings.privacySettings.recordConsent,
}

let fromString = (json: option<string>): AppTypes.SavedSettings.User.t =>
  switch json {
  | None =>
    Debug.loG("no serialized editor", None)
    AppTypes.SavedSettings.User.default
  | Some(json) =>
    try json |> Json.parseOrRaise |> AppTypes.SavedSettings.User.decode catch {
    | e =>
      Debug.loG("error parsing serialized editor", e)
      AppTypes.SavedSettings.User.default
    }
  }

let toString = (se: AppTypes.SavedSettings.User.t): string =>
  se |> AppTypes.SavedSettings.User.encode |> Json.stringify

let save = (m: AppTypes.model): unit => {
  let state = m |> model2editor |> toString
  Dom.Storage.setItem("userState-" ++ m.username, state, Dom.Storage.localStorage)
}

let load = (username: string): AppTypes.SavedSettings.User.t =>
  Dom.Storage.localStorage |> Dom.Storage.getItem("userState-" ++ username) |> fromString
