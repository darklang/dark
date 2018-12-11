open! Porting
open Prelude
open Types
open ViewUtils

(* Tea *)
module Attributes = Tea.Html2.Attributes
module Events = Tea.Html2.Events

let onSubmit ~key fn =
  Html.onWithOptions
    ~key
    "submit"
    {stopPropagation = true; preventDefault = true}
    (Decoders.wrapDecoder fn)


let stringEntryHtml (ac : autocomplete) (width : stringEntryWidth) :
    msg Html.html =
  let maxWidthChars =
    match width with
    (* max-width rules from CSS *)
    | StringEntryNormalWidth ->
        120
    | StringEntryShortWidth ->
        40
  in
  let value = Util.transformToStringEntry ac.value in
  let longestLineLength =
    value
    |> String.split "\n"
    |> List.map visualStringLength
    |> List.foldr max 1
    |> min maxWidthChars
  in
  let rowCount =
    value
    |> String.split "\n"
    |> List.map (fun line ->
           line
           |> visualStringLength
           |> float_of_int
           |> ( *. ) (1. /. float_of_int longestLineLength)
           |> ceil
           |> max 1. )
    |> List.floatSum
    |> int_of_float
  in
  let input =
    Html.textarea
      [ Attributes.id Defaults.entryID
      ; Events.onInput
          ((fun x -> EntryInputMsg x) << Util.transformFromStringEntry)
      ; Attributes.value value
      ; Attributes.spellcheck false (* Stop other events firing *)
      ; nothingMouseEvent "mouseup"
      ; nothingMouseEvent "click"
      ; nothingMouseEvent "mousedown"
      ; Attributes.rows rowCount
      ; widthInCh longestLineLength
      ; Attributes.autocomplete false ]
      []
  in
  let sizeClass =
    if Autocomplete.isSmallStringEntry ac
    then "small-string"
    else "large-string"
  in
  Html.div
    [Html.class' "string-entry"]
    [ Html.form
        [ onSubmit ~key:"esm" (fun _ -> EntrySubmitMsg)
        ; Html.class' ("string-container " ^ sizeClass) ]
        [input] ]


let normalEntryHtml (placeholder : string) (ac : autocomplete) : msg Html.html
    =
  let autocompleteList =
    List.indexedMap
      (fun i item ->
        let highlighted = ac.index = i in
        let name = Autocomplete.asName item in
        let view item classes =
          match item with
          | ACFunction _ ->
              viewFnName name classes
          | _ ->
              Html.text name
        in
        Html.li
          [ Attributes.classList
              [("autocomplete-item", true); ("highlighted", highlighted)]
          ; nothingMouseEvent "mouseup"
          ; nothingMouseEvent "mousedown"
          ; eventNoPropagation ~key:("ac-" ^ name) "click" (fun _ ->
                AutocompleteClick name ) ]
          [ view item []
          ; Html.span
              [Html.class' "types"]
              [Html.text <| Autocomplete.asTypeString item] ] )
      ac.completions
  in
  let autocomplete =
    Html.ul [Attributes.id "autocomplete-holder"] autocompleteList
  in
  (* two overlapping input boxes, one to provide suggestions, one *)
  (* to provide the search *)
  let _, _, search = Autocomplete.compareSuggestionWithActual ac ac.value in
  let searchWidth =
    search
    |> String.length
    |> fun n -> if 0 = n then String.length placeholder else n
  in
  let searchInput =
    Html.input'
      [ Attributes.id Defaults.entryID
      ; Events.onInput (fun x -> EntryInputMsg x)
      ; Attributes.value search
      ; Attributes.placeholder placeholder
      ; Attributes.spellcheck false
      ; Attributes.autocomplete false ]
      []
  in
  (* TODO(ian): deliberately using an empty string here *)
  (* and changing absolutely nothing else re: the layout/width *)
  (* here because I have no idea what the effects will be *)
  let suggestionSpan =
    Html.span [Attributes.id "suggestionBox"] [Html.text ""]
  in
  (* http://making.fiftythree.com/fluid-text-inputs/ *)
  let fluidWidthSpan =
    Html.span
      [Attributes.id "fluidWidthSpan"; Vdom.prop "contentEditable" "true"]
      [Html.text search]
  in
  let input =
    Html.fieldset
      [Attributes.id "search-container"; widthInCh searchWidth]
      [searchInput; suggestionSpan; fluidWidthSpan]
  in
  let viewForm =
    Html.form
      [onSubmit ~key:"esm2" (fun _ -> EntrySubmitMsg)]
      [input; autocomplete]
  in
  let wrapper = Html.div [Html.class' "entry"] [viewForm] in
  wrapper


let entryHtml
    (permission : stringEntryPermission)
    (width : stringEntryWidth)
    (placeholder : string)
    (ac : autocomplete) : msg Html.html =
  match permission with
  | StringEntryAllowed ->
      if Autocomplete.isStringEntry ac
      then stringEntryHtml ac width
      else normalEntryHtml placeholder ac
  | StringEntryNotAllowed ->
      normalEntryHtml placeholder ac


let viewEntry (m : model) : msg Html.html list =
  match unwrapCursorState m.cursorState with
  | Entering (Creating pos) ->
      let html =
        Html.div
          [Html.class' "omnibox"]
          [entryHtml StringEntryAllowed StringEntryNormalWidth "" m.complete]
      in
      [placeHtml pos html]
  | _ ->
      []
