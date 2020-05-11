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

let explanation =
  "To help us understand how people learn Dark, is it okay if we track your session in a replayable format (using Fullstory)."


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


let consentRow (recordConsent : bool option) ~(longLabels : bool) :
    Types.msg Html.html =
  let yes, no =
    if longLabels
    then ("Yes, please go ahead", "No, please don't")
    else ("Yes", "No")
  in
  Html.div
    [Html.class' "setting-row"]
    [ Html.div
        [Html.class' "setting-label"]
        [ Html.div [Html.class' "title"] [Html.text "Record me using Dark"]
        ; Html.div [Html.class' "description"] [Html.text explanation] ]
    ; Html.div
        [Html.class' "setting-control"]
        [ radio
            ~value:"yes"
            ~label:yes
            ~msg:(SetRecordConsent true)
            ~checked:(recordConsent = Some true)
        ; radio
            ~value:"no"
            ~label:no
            ~msg:(SetRecordConsent false)
            ~checked:(recordConsent = Some false) ] ]


let html (m : Types.model) : Types.msg Html.html =
  let content =
    [consentRow m.settingsView.privacy.recordConsent ~longLabels:true]
  in
  let cls =
    if m.settingsView.privacy.recordConsent = None then "ask" else "hide"
  in
  let overlay =
    if m.settingsView.privacy.recordConsent = None
    then "modal-overlay"
    else "hide"
  in
  Html.div
    [Html.class' overlay]
    [Html.div [Html.class' ("fullstory-modal " ^ cls)] content]
