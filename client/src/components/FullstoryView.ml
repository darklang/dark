(* open Tc *)

module Cmd = Tea.Cmd
module Attr = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module Html = Tea_html_extended

module FullstoryJs = struct
  external _setConsent : bool -> unit = "setConsent"
    [@@bs.val] [@@bs.scope "window", "Dark", "fullstory"]

  let setConsent (allow : bool) : Types.msg Cmd.t =
    Cmd.call (fun _ -> _setConsent allow)
end

module SetConsent = struct
  let decode =
    let open Tea.Json.Decoder in
    field "detail" (Decoders.wrapDecoder Decoders.optBool)


  let listen ~key tagger =
    BrowserListeners.registerGlobal "FullstoryConsent" key tagger decode
end

let explanation =
  "We track your session in a replayable format (using Fullstory) to help us understand how people learn Dark."


let disableOmniOpen = ViewUtils.nothingMouseEvent "mousedown"

let radio
    ~(value : string)
    ~(label : string)
    ~(msg : SettingsViewTypes.settingsMsg)
    ~(checked : bool) : Types.msg Html.html =
  let key = "fs-consent-" ^ value in
  Html.div
    [Html.class' "choice"; disableOmniOpen]
    [ Html.input'
        [ Html.type' "radio"
        ; Html.id key
        ; Html.name "fs-consent"
        ; Html.value value
        ; Html.checked checked
        ; ViewUtils.eventNoPropagation ~key "click" (fun _ ->
              SettingsViewMsg msg) ]
        []
    ; Html.label [Html.for' key] [Html.text label] ]


let html (m : Types.model) : Types.msg Html.html =
  let content =
    [ Html.p
        []
        [ Html.text
            "To help us understand how people learn Dark, do you mind if we track your session in a replayable format (using Fullstory)."
        ]
    ; Html.div
        [Html.class' "consent"]
        [ radio
            ~value:"yes"
            ~label:"Yes, please go ahead"
            ~msg:(SetRecordConsent true)
            ~checked:false
        ; radio
            ~value:"no"
            ~label:"No, please don't"
            ~msg:(SetRecordConsent false)
            ~checked:false ] ]
  in
  let cls =
    if m.settingsView.privacy.recordConsent = None then "ask" else "hide"
  in
  Html.div [Html.class' ("fullstory-modal " ^ cls)] content
