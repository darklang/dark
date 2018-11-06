module ViewScaffold exposing (..)

-- builtin

-- lib
import Html
import Html.Attributes as Attrs
import Json.Decode as JSD

-- dark
import Types exposing (..)
import ViewUtils exposing (..)
import JSONUtils
import Url



viewButtons : Model -> Html.Html Msg
viewButtons m =
  let integrationTestButton =
        case m.integrationTestState of
          IntegrationTestExpectation _ ->
            [ Html.a
            [ eventNoPropagation "mouseup" (\_ -> FinishIntegrationTest)
            , Attrs.src ""
            , Attrs.id "finishIntegrationTest"
            , Attrs.class "specialButton"]
            [ Html.text "Finish integration tests" ]]
          IntegrationTestFinished (Ok ()) ->
            [ Html.div [ Attrs.id "integrationTestSignal"
            , Attrs.class "specialButton success"]
            [ Html.text "success"]]
          IntegrationTestFinished (Err msg) ->
            [ Html.div [ Attrs.id "integrationTestSignal"
            , Attrs.class "specialButton failure" ]
            [ Html.text <| "failure: " ++ msg]]
          NoIntegrationTest -> []
      returnButton =
        case m.currentPage of
          Fn _ _ ->
            [Url.linkFor
              (Toplevels m.canvas.offset)
              "specialButton default-link"
              [ Html.text "Return to Canvas"]]
          _ -> []
      status =
        case m.error.message of
          Nothing ->
            Html.div [Attrs.class "status"] [Html.text "Dark"]
          Just msg ->
            Html.div
              [ Attrs.class "status error"]
              [ Html.text "Error: "
              , Html.a
                  [ Attrs.class "link"
                  , Attrs.href "#"
                  , eventNoPropagation "mouseup"
                      (\_ -> ShowErrorDetails (not m.error.showDetails)) ]
                  [ Html.text
                      (if m.error.showDetails then "hide details" else "see details") ]
              ]

  in
  Html.div [Attrs.id "buttons"]
    ([ Html.a
        [ eventNoPropagation "mouseup" (\_ -> SaveTestButton)
        , Attrs.src ""
        , Attrs.class "specialButton"]
        [ Html.text "SaveTest" ]
    , Html.a
        [ eventNoPropagation "mouseup" (\_ -> ToggleTimers)
        , Attrs.src ""
        , Attrs.class "specialButton"]
        [ Html.text
            (if m.timersEnabled then "DisableTimers" else "EnableTimers") ]
    , Html.span
        [ Attrs.class "specialButton"]
        [ Html.text (toString m.currentPage)]
    , Html.span
        [ Attrs.class "specialButton"]
        [ Html.text ("Tests: " ++ toString m.tests)]
    , Html.span
        [ Attrs.class ("specialButton environment " ++ m.environment)]
        [ Html.text (m.environment ++ "/" ++ "Elm")]
    ] ++ integrationTestButton ++ returnButton ++ [status])

viewError : DarkError -> Html.Html Msg
viewError err =
  let viewException exc =
    case exc.result of
      Nothing -> [ Html.text exc.short ]
      Just result -> [ Html.text result ]
  in
    Html.div
    [ Attrs.classList
      [ ("error-panel", True)
      , ("show", err.showDetails)]
      ]
    ( case err.message of
        Nothing -> []
        Just msg ->
          case JSD.decodeString JSONUtils.decodeException msg of
            Err _ -> [ Html.text msg ]
            Ok exc -> viewException exc
    )
