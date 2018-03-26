module View exposing (view)

-- builtin

-- lib
import Html
import Html.Attributes as Attrs
import Html.Events as Events

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import ViewEntry
import ViewUtils exposing (..)
import ViewScaffold
import ViewRoutingTable
import ViewBlankOr exposing (viewText, wc)
import ViewCode exposing (viewExpr, viewDarkType)


view : Model -> Html.Html Msg
view m =
  let (w, h) = Util.windowSize () in
  Html.div
   [ Attrs.id "grid"
   , Events.onWithOptions
       "mouseup"
       { stopPropagation = False, preventDefault = True}
       (decodeClickEvent GlobalClick)
   ]
   [ ViewScaffold.viewError m.error
   , (viewCanvas m)
   , ViewScaffold.viewButtons m
   ]

viewCanvas : Model -> Html.Html Msg
viewCanvas m =
    let entry = ViewEntry.viewEntry m
        asts = List.map (viewTL m) m.toplevels
        yaxis = axisLine m {x=0, y=1}
        xaxis = axisLine m {x=1, y=0}
        routing = ViewRoutingTable.viewRoutingTable m
        allDivs = xaxis :: yaxis :: routing :: (asts ++ entry)
    in Html.div [Attrs.id "canvas"] allDivs

viewTL : Model -> Toplevel -> Html.Html Msg
viewTL m tl =
  let body =
        case tl.data of
          TLHandler h ->
            viewHandler (createVS m tl) h
          TLDB db ->
            viewDB (createVS m tl) db
      events =
        [ eventNoPropagation "mousedown" (ToplevelMouseDown tl.id)
        , eventNoPropagation "mouseup" (ToplevelMouseUp tl.id)
        , eventNoPropagation "click" (ToplevelClick tl.id)
        ]

      selected = if Just tl.id == tlidOf m.state
                 then "selected"
                 else ""
      class = [selected, toString (deTLID tl.id), "toplevel", "cursor-" ++ (toString tl.cursor)]
              |> String.join " "

      html =
        Html.div
          [Attrs.class "sidebar-box"] -- see comment in css
          [ Html.div
              (Attrs.class class :: events)
              body
          ]

  in
      placeHtml m tl.pos html

viewDB : ViewState -> DB -> List (Html.Html Msg)
viewDB vs db =
  let namediv = Html.div
                 [ Attrs.class "dbname"]
                 [ Html.text db.name]
      coldivs =
        db.cols
        |> List.map (\(n, t) ->
             Html.div
               [ Attrs.class "col" ]
               [ viewText DBColName vs [wc "name"] n
               , viewText DBColType vs [wc "type"] t
               ])
  in
  [
    Html.div
      [ Attrs.class "db"]
      (namediv :: coldivs)
  ]

viewHandler : ViewState -> Handler -> List (Html.Html Msg)
viewHandler vs h =
  let ast = Html.div
              [ Attrs.class "ast"]
              [viewExpr 0 vs [] h.ast]

      externalLink =
        case (h.spec.modifier, h.spec.name) of
          (F _ "GET", F _ name)  ->
            [Html.a [ Attrs.class "external"
                    , Attrs.href name
                    , Attrs.target "_blank"
                    ]
                    [fontAwesome "external-link"]]
          _ -> []

      input =
        Html.div
          [Attrs.class "spec-type input-type"]
          [ Html.span [Attrs.class "header"] [Html.text "Input:"]
          , viewDarkType vs [] h.spec.types.input]
      output =
        Html.div
          [Attrs.class "spec-type output-type"]
          [ Html.span [Attrs.class "header"] [Html.text "Output:"]
          , viewDarkType vs [] h.spec.types.output]

      header =
        Html.div
          [Attrs.class "header"]
          [ viewText EventName vs [wc "name"] h.spec.name
          , (Html.div
            [ Attrs.class "modifier" ]
             externalLink)
          , viewText EventSpace vs [wc "module"] h.spec.module_
          , viewText EventModifier vs [wc "modifier"] h.spec.modifier]
  in [header, ast]
