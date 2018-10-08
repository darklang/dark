open Belt
open Porting
module Attrs = Html.Attributes
module Events = Html.Events
open Prelude
open Types
open ViewUtils

let viewEntry m =
  match unwrapCursorState m.cursorState with
  | Entering (Creating pos) ->
      let html =
        Html.div [Attrs.class_ "omnibox"]
          [entryHtml StringEntryAllowed StringEntryNormalWidth "" m.complete]
      in
      [placeHtml m pos html]
  | _ -> []

let stringEntryHtml ac width =
  let maxWidthChars =
    match width with
    | StringEntryNormalWidth -> 120
    | StringEntryShortWidth -> 40
  in
  let value = Util.transformToStringEntry ac.value in
  let longestLineLength =
    value |> String.split "\n"
    |> List.map visualStringLength
    |> List.foldr max 1 |> min maxWidthChars |> ( + ) 1
  in
  let rowCount =
    value |> String.split "\n"
    |> List.map (fun line ->
           line |> visualStringLength |> toFloat
           |> ( * ) (1. / toFloat longestLineLength)
           |> ceiling |> max 1 )
    |> List.sum
  in
  let input =
    Html.textarea
      [ Attrs.id Defaults.entryID
      ; Events.onInput (EntryInputMsg << Util.transformFromStringEntry)
      ; Attrs.value value
      ; Attrs.spellcheck false
      ; nothingMouseEvent "mouseup"
      ; nothingMouseEvent "click"
      ; nothingMouseEvent "mousedown"
      ; Attrs.rows rowCount
      ; widthInCh longestLineLength
      ; Attrs.autocomplete false ]
      []
  in
  let sizeClass =
    if Autocomplete.isSmallStringEntry ac then "small-string"
    else "large-string"
  in
  Html.div
    [Attrs.class_ "string-entry"]
    [ Html.form
        [ Events.onSubmit EntrySubmitMsg
        ; Attrs.class_ ("string-container " ^ sizeClass) ]
        [input] ]

let normalEntryHtml placeholder ac =
  let autocompleteList =
    List.indexedMap
      (fun i item ->
        let highlighted = ac.index = i in
        let name = Autocomplete.asName item in
        let view item classes =
          match item with
          | ACFunction _ -> viewFnName name classes
          | _ -> Html.text name
        in
        Html.li
          [ Attrs.classList
              [("autocomplete-item", true); ("highlighted", highlighted)]
          ; nothingMouseEvent "mouseup"
          ; nothingMouseEvent "mousedown"
          ; eventNoPropagation "click" (fun _ -> AutocompleteClick name) ]
          [ view item []
          ; Html.span [Attrs.class_ "types"]
              [Html.text <| Autocomplete.asTypeString item] ] )
      (List.concat ac.completions)
  in
  let autocomplete =
    Html.ul [Attrs.id "autocomplete-holder"] autocompleteList
  in
  let _ = "comment" in
  let _ = "comment" in
  let _, suggestion, search =
    Autocomplete.compareSuggestionWithActual ac ac.value
  in
  let searchWidth =
    search |> String.length
    |> fun n -> if 0 = n then String.length placeholder else n
  in
  let searchInput =
    Html.input
      [ Attrs.id Defaults.entryID
      ; Events.onInput EntryInputMsg
      ; Attrs.value search
      ; Attrs.placeholder placeholder
      ; Attrs.spellcheck false
      ; Attrs.autocomplete false ]
      []
  in
  let _ = "comment" in
  let _ = "comment" in
  let _ = "comment" in
  let suggestionSpan = Html.span [Attrs.id "suggestionBox"] [Html.text ""] in
  let _ = "comment" in
  let fluidWidthSpan =
    Html.span
      [Attrs.id "fluidWidthSpan"; Attrs.attribute "contentEditable" ""]
      [Html.text search]
  in
  let input =
    Html.fieldset
      [Attrs.id "search-container"; widthInCh searchWidth]
      [searchInput; suggestionSpan; fluidWidthSpan]
  in
  let viewForm =
    Html.form [Events.onSubmit EntrySubmitMsg] [input; autocomplete]
  in
  let wrapper = Html.div [Attrs.class_ "entry"] [viewForm] in
  wrapper

let entryHtml permission width placeholder ac =
  match permission with
  | StringEntryAllowed ->
      if Autocomplete.isStringEntry ac then stringEntryHtml ac width
      else normalEntryHtml placeholder ac
  | StringEntryNotAllowed -> normalEntryHtml placeholder ac
