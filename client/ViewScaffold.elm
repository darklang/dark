module ViewScaffold exposing (..)

-- builtin

-- lib
import Html
import Html.Attributes as Attrs
import Json.Decode as JSD

-- dark
import Types exposing (..)
import ViewUtils exposing (..)
import JSON



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
          Fn _ ->
            [ Html.a
            [ eventNoPropagation "mouseup" (\_ -> ReturnToMainCanvas)
            , Attrs.src ""
            , Attrs.class "specialButton"]
            [ Html.text "Return to Canvas"]
            ]
          _ -> []

  in
  Html.div [Attrs.id "buttons"]
    ([ Html.a
      [ eventNoPropagation "mouseup" (\_ -> AddRandom)
      , Attrs.src ""
      , Attrs.class "specialButton"]
      [ Html.text "Random" ]
    , Html.a
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
      [Html.text (toString m.center)]
    , Html.span
      [ Attrs.class "specialButton"]
      [Html.text ("Active tests: " ++ toString m.tests)]
    ] ++ integrationTestButton ++ returnButton)

viewError : Maybe String -> Html.Html Msg
viewError mMsg =
  case mMsg of
    Nothing ->
      Html.div [Attrs.id "status"] [Html.text "Dark"]
    Just msg ->
      case JSD.decodeString JSON.decodeException msg of
        Err _ -> -- not json, just a regular string
          Html.div
            [Attrs.id "status", Attrs.class "error"]
            [Html.text ("Error: " ++ msg)]
        Ok exc ->
          Html.div
            [ Attrs.id "status"
            , Attrs.class "error" ]
            [ Html.span
                [ Attrs.class "message" ]
                [ Html.text ("Error: " ++ exc.short)]
            , Html.i
              [ Attrs.class "fa fa-info-circle"
              , Attrs.title msg
              ]
              []
            ]



