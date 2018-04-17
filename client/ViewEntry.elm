-- The Entry is the form that we use to enter text.
module ViewEntry exposing (viewEntry, entryHtml)


-- builtin

-- lib
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import String.Extra as SE

-- dark
import Types exposing (..)
import Prelude exposing (..)
import Util
import Defaults
import Autocomplete
import ViewUtils exposing (..)

viewEntry : Model -> List (Html.Html Msg)
viewEntry m =
  case unwrapCursorState m.cursorState of
    Entering (Creating pos) ->
      let html =
            Html.div
              [Attrs.class "omnibox"]
              [entryHtml True "" m.complete]
      in [placeHtml m pos html]
    _ ->
      []

-- The view we see is different from the value representation in a few
-- ways:
-- - the start and end quotes are skipped
-- - all other quotes are escaped
transformToStringEntry : String -> String
transformToStringEntry s_ =
  -- the first time we won't have a closing quote so add it
  let s = if String.endsWith "\"" s_ then s_ else s_ ++ "\"" in
  s
  |> String.dropLeft 1
  |> String.dropRight 1
  |> Util.replace "\\\\\"" "\""

transformFromStringEntry : String -> String
transformFromStringEntry s =
  let s2 = s
           |> Util.replace "\"" "\\\""
  in
  "\"" ++ s2 ++ "\""

stringEntryHtml : Autocomplete -> Html.Html Msg
stringEntryHtml ac =
  let
      -- stick with the overlapping things for now, just ignore the back
      -- one
      value = transformToStringEntry ac.value
      -- NB: if the letter spacing isn't normal, need to adjust the length here for that
      length = value
               |> String.length
               |> max 1 -- need this to be at least 1 for the cursor to blink?


      smallInput =
        Html.input [ Attrs.id Defaults.entryID
                   , Events.onInput (EntryInputMsg << transformFromStringEntry)
                   , nothingMouseEvent "mouseup"
                   , nothingMouseEvent "mouseclick"
                   , nothingMouseEvent "mousedown"
                   , Attrs.value value
                   , widthInCh length
                   , Attrs.spellcheck False
                   , Attrs.autocomplete False
                   ] []
      fluidWidthSpan = Html.span [ Attrs.id "fluidWidthSpan"
                                 , Attrs.attribute "contentEditable" ""
                                 ] [Html.text value]


      largeInput =
        Html.textarea [ Attrs.id Defaults.entryID
                      , Events.onInput (EntryInputMsg << transformFromStringEntry)
                      , Attrs.value value
                      , Attrs.spellcheck False
                      -- Stop other events firing
                      , nothingMouseEvent "mouseup"
                      , nothingMouseEvent "mouseclick"
                      , nothingMouseEvent "mousedown"
                      , Attrs.cols 50
                      , Attrs.rows (5 + SE.countOccurrences "\n" value)
                      , Attrs.autocomplete False
                      ] []
    in
    if Autocomplete.isSmallStringEntry ac
    then
      Html.div
      [ Attrs.class "string-entry small-string-entry" ]
      [
        Html.form
        [ Events.onSubmit (EntrySubmitMsg)
        , Attrs.class "string-container"
        ]
        [ smallInput, fluidWidthSpan ]
      ]
    else
       Html.div
      [ Attrs.class "string-entry big-string-entry" ]
      [
        Html.form
        [ Events.onSubmit (EntrySubmitMsg)
        , Attrs.class "string-container"
        ]
        [ largeInput ]
      ]


normalEntryHtml : String -> Autocomplete -> Html.Html Msg
normalEntryHtml placeholder ac =
  let autocompleteList =
        (List.indexedMap
           (\i item ->
              let highlighted = ac.index == i
                  hlClass = if highlighted then " highlighted" else ""
                  name = Autocomplete.asName item
              in Html.li
                [ Attrs.class <| "autocomplete-item" ++ hlClass
                , eventNoPropagation "mouseup"
                    (\_ -> AutocompleteClick name)
                ]
                [ Html.text name
                , Html.span
                    [Attrs.class "types"]
                    [Html.text <| Autocomplete.asTypeString item ]
                ])
           (List.concat ac.completions))

      autocomplete = Html.ul
                     [ Attrs.id "autocomplete-holder" ]
                     autocompleteList


      -- two overlapping input boxes, one to provide suggestions, one
      -- to provide the search
      (indent, suggestion, search) =
        Autocomplete.compareSuggestionWithActual ac ac.value

      indentWidth = String.length indent
      searchWidth = search ++ indent
                    |> String.length
                    |> max (String.length suggestion)
      searchInput = Html.input [ Attrs.id Defaults.entryID
                               , Events.onInput EntryInputMsg
                               , Attrs.style [("text-indent", inCh indentWidth)]
                               , Attrs.value search
                               , Attrs.placeholder placeholder
                               , Attrs.spellcheck False
                               , Attrs.autocomplete False
                               ] []
      suggestionSpan = Html.span [ Attrs.id "suggestionBox"
                                 ] [Html.text suggestion]

      -- http://making.fiftythree.com/fluid-text-inputs/
      fluidWidthSpan = Html.span [ Attrs.id "fluidWidthSpan"
                                 , Attrs.attribute "contentEditable" ""
                                 , Attrs.style [("text-indent", inCh indentWidth)]
                                 ] [Html.text search]

      input = Html.fieldset
              [ Attrs.id "search-container"
              , widthInCh searchWidth
              ]
              [searchInput, suggestionSpan, fluidWidthSpan]

      viewForm = Html.form
                 [ Events.onSubmit (EntrySubmitMsg) ]
                 [ input, autocomplete ]

      wrapper = Html.div
                [ Attrs.class "entry" ]
                [ viewForm ]
  in wrapper

entryHtml : Bool -> String -> Autocomplete -> Html.Html Msg
entryHtml allowStringEntry placeholder ac =
  if allowStringEntry && Autocomplete.isStringEntry ac
  then stringEntryHtml ac
  else normalEntryHtml placeholder ac
