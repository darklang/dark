open Prelude

let toModel (m : model) (e : savedUserSettings) : model =
  let userTutorial =
    if e.showUserWelcomeModal then UserTutorial.defaultStep else None
  in
  { m with
    userTutorial
  ; showUserWelcomeModal = e.showUserWelcomeModal
  ; settingsView =
      {m.settingsView with privacy = {recordConsent = e.recordConsent}} }


let model2editor (m : model) : savedUserSettings =
  (* showUserWelcomeModal indicates if it is the users first time visiting any dark canvas *)
  { showUserWelcomeModal = m.showUserWelcomeModal
  ; recordConsent = m.settingsView.privacy.recordConsent }


let fromString (json : string option) : savedUserSettings =
  match json with
  | None ->
      Debug.loG "no serialized editor" None ;
      Defaults.defaultUserSettings
  | Some json ->
    ( try json |> Json.parseOrRaise |> Decoders.savedUserSettings
      with e ->
        Debug.loG "error parsing serialized editor" e ;
        Defaults.defaultUserSettings )


let toString (se : savedUserSettings) : string =
  se |> Encoders.savedUserSettings |> Json.stringify


let save (m : model) : unit =
  let state = m |> model2editor |> toString in
  Dom.Storage.setItem ("userState-" ^ m.username) state Dom.Storage.localStorage


let load (username : string) : savedUserSettings =
  Dom.Storage.localStorage
  |> Dom.Storage.getItem ("userState-" ^ username)
  |> fromString
