-- The Entry is the form that we use to enter text.
module ViewEntry exposing (viewEntry, entryHtml)


-- builtin

-- lib
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import List

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
              [entryHtml StringEntryAllowed StringEntryNormalWidth "" m.complete]
      in [placeHtml m pos html]
    _ ->
      []


stringEntryHtml : Autocomplete -> StringEntryWidth -> Html.Html Msg
stringEntryHtml ac width =
  let maxWidthChars =
        case width of -- max-width rules from CSS
          StringEntryNormalWidth -> 120
          StringEntryShortWidth -> 40
      value = Util.transformToStringEntry ac.value
      longestLineLength = value
                          |> String.split "\n"
                          |> List.map visualStringLength
                          |> List.foldr max 1
                          |> min maxWidthChars
                          |> (+) 1
      rowCount = value
                 |> String.split "\n"
                 |> List.map (\line ->
                                line
                                |> visualStringLength
                                |> toFloat
                                |> (*) (1.0 / toFloat longestLineLength)
                                |> ceiling
                                |> max 1
                    )
                 |> List.sum
      input =
        Html.textarea [ Attrs.id Defaults.entryID
                      , Events.onInput (EntryInputMsg << Util.transformFromStringEntry)
                      , Attrs.value value
                      , Attrs.spellcheck False
                      -- Stop other events firing
                      , nothingMouseEvent "mouseup"
                      , nothingMouseEvent "click"
                      , nothingMouseEvent "mousedown"
                      , Attrs.rows rowCount
                      , widthInCh longestLineLength
                      , Attrs.autocomplete False
                      ] []

      sizeClass = if Autocomplete.isSmallStringEntry ac then "small-string" else "large-string"
    in
    Html.div
    [ Attrs.class "string-entry" ]
    [
      Html.form
        [ Events.onSubmit (EntrySubmitMsg)
        , Attrs.class ("string-container " ++ sizeClass)
        ]
      [ input ]
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
                , nothingMouseEvent "mouseup"
                , nothingMouseEvent "mousedown"
                , eventNoPropagation "click"
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
                    |> \n ->
                      if 0 == n
                      then
                        String.length placeholder
                      else
                        n
                    |> max (String.length suggestion)
      searchInput = Html.input [ Attrs.id Defaults.entryID
                               , Events.onInput EntryInputMsg
                               , Attrs.style "text-indent" (inCh indentWidth)
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
                                 , Attrs.style "text-indent" (inCh indentWidth)
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

entryHtml : StringEntryPermission -> StringEntryWidth -> String -> Autocomplete -> Html.Html Msg
entryHtml permission width placeholder ac =
  case permission of
    StringEntryAllowed ->
      if Autocomplete.isStringEntry ac
      then stringEntryHtml ac width
      else normalEntryHtml placeholder ac
    StringEntryNotAllowed -> normalEntryHtml placeholder ac
