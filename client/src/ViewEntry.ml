open Tc
open Prelude
open Types
open ViewUtils

(* Tea *)
module Attributes = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module RT = Runtime

let onSubmit ~key fn =
  Html.onWithOptions
    ~key
    "submit"
    {stopPropagation = true; preventDefault = true}
    (Decoders.wrapDecoder fn)


(* This is a paste handler that allows the default behaviour, ignores
 * the incoming Msg, and does _not_ propagate the event upwords.
 *
 * This is to prevent the paste handler on `document` from eating the event *)
let defaultPasteHandler =
  Html.onWithOptions
    ~key:"paste"
    "paste"
    {stopPropagation = true; preventDefault = false}
    (Decoders.wrapDecoder (fun _ -> IgnoreMsg))


let normalEntryHtml (placeholder : string) (ac : autocomplete) : msg Html.html
    =
  let toList acis class' index =
    List.indexedMap
      ~f:(fun i item ->
        let highlighted = index = i in
        let name = Autocomplete.asName item in
        let typeStr = Autocomplete.asTypeString item in
        let specialClass = Autocomplete.asTypeClass item in
        Html.li
          [ Attributes.classList
              [ ("autocomplete-item", true)
              ; ("highlighted", highlighted)
              ; (class', true)
              ; (specialClass, true) ]
          ; nothingMouseEvent "mouseup"
          ; defaultPasteHandler
          ; nothingMouseEvent "mousedown"
          ; eventNoPropagation ~key:("ac-" ^ name) "click" (fun _ ->
                AutocompleteClick i ) ]
          [ Html.span [Html.class' "name"] [Html.text name]
          ; Html.span [Html.class' "types"] [Html.text typeStr] ] )
      acis
  in
  let autocompleteList = toList ac.completions "valid" ac.index in
  let autocomplete =
    Html.ul [Attributes.id "autocomplete-holder"] autocompleteList
  in
  (* two overlapping input boxes, one to provide suggestions, one * to provide
   * the search. (Note: we used to use this, but took it out. Leaving in the
   * technical means to show "preview" values, even though it's currently
   * unused.) *)
  let search = ac.value in
  let searchWidth =
    search
    |> String.length
    |> fun n -> if 0 = n then String.length placeholder else n
  in
  let searchInput =
    Html.input'
      [ Attributes.id Defaults.entryID
      ; Events.onInput (fun x -> EntryInputMsg x)
      ; defaultPasteHandler
      ; Attributes.value search
      ; Attributes.placeholder placeholder
      ; Vdom.attribute "" "spellcheck" "false"
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
      (if ac.visible then [input; autocomplete] else [input])
  in
  let wrapper = Html.div [Html.class' "entry"] [viewForm] in
  wrapper


let viewEntry (m : model) : msg Html.html =
  match unwrapCursorState m.cursorState with
  | Entering (Creating pos) ->
      let styleProp =
        if VariantTesting.variantIsActive m GridLayout
        then Vdom.noProp
        else
          let offset = m.canvasProps.offset in
          let loc = Viewport.subPos pos offset in
          Html.styles
            [ ("left", string_of_int loc.x ^ "px")
            ; ("top", string_of_int loc.y ^ "px") ]
      in
      Html.div
        [Html.class' "omnibox"; styleProp]
        [normalEntryHtml "" m.complete]
  | _ ->
      Vdom.noNode
