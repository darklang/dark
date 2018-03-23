module View exposing (view)

-- builtin

-- lib
import Html
import Html.Attributes as Attrs
import Html.Events as Events

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import Pointer as P
import ViewEntry
import ViewUtils exposing (..)
import ViewScaffold
import ViewRoutingTable
import ViewBlankOr exposing (viewBlankOrText)
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
            viewHandler m tl h
          TLDB db ->
            viewDB m tl db
      events =
        [ eventNoPropagation "mousedown" (ToplevelClickDown tl)
        , eventNoPropagation "mouseup" (ToplevelClickUp tl.id Nothing)
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

viewDB : Model -> Toplevel -> DB -> List (Html.Html Msg)
viewDB m tl db =
  let namediv = Html.div
                 [ Attrs.class "dbname"]
                 [ Html.text db.name]
      coldivs =
        List.map (\(n, t) ->
                           Html.div
                             [ Attrs.class "col" ]
                             [ Html.span
                                 [ Attrs.class "name" ]
                                 [ viewBlankOrText DBColName m tl [] n ]
                             , Html.span
                                 [ Attrs.class "type" ]
                                 [ viewBlankOrText DBColType m tl [] t ]
                             ])
                         db.cols
  in
  [
    Html.div
      [ Attrs.class "db"]
      (namediv :: coldivs)
  ]

viewHandler : Model -> Toplevel -> Handler -> List (Html.Html Msg)
viewHandler m tl h =
  let (id, filling) =
        case unwrapState m.state of
          Selecting tlid (Just p) -> (P.toID p, False)
          Entering (Filling tlid p) -> (P.toID p, True)
          _ -> (ID 0, False)

      hovering =
        case m.hovering |> List.head of
          Just hid -> hid
          Nothing -> ID 0

      ast = Html.div
              [ Attrs.class "ast"]
              [ viewExpr m tl [] h.ast ]

      externalLink =
        case (h.spec.modifier, h.spec.name) of
          (F _ "GET", F _ name)  ->
            [Html.a [ Attrs.class "external"
                    , Attrs.href name
                    , Attrs.target "_blank"
                    ]
                    [Html.i [Attrs.class "fa fa-external-link"] []]]
          _ -> []

      input =
        Html.div
          [Attrs.class "spec-type input-type"]
          [ Html.span [Attrs.class "header"] [Html.text "Input:"]
          , viewDarkType m tl [] h.spec.types.input]
      output =
        Html.div
          [Attrs.class "spec-type output-type"]
          [ Html.span [Attrs.class "header"] [Html.text "Output:"]
          , viewDarkType m tl [] h.spec.types.output]

      header =
        Html.div
          [Attrs.class "header"]
          [ Html.div
            [ Attrs.class "name"]
            [ viewBlankOrText EventName m tl [] h.spec.name ]
          , Html.div
            [ Attrs.class "modifier" ]
            ( externalLink ++
              [ Html.div
                [ Attrs.class "module" ]
                [ viewBlankOrText EventSpace m tl [] h.spec.module_ ]
              , viewBlankOrText EventModifier m tl [] h.spec.modifier
              ]
            )
          ]
  in [header, ast]



