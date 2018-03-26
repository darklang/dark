module ViewScaffold exposing (..)

-- builtin

-- lib
import Html
import Html.Attributes as Attrs

-- dark
import Types exposing (..)
import ViewUtils exposing (..)



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

  in
  Html.div [Attrs.id "buttons"]
    ([ Html.a
      [ eventNoPropagation "mouseup" (\_ -> AddRandom)
      , Attrs.src ""
      , Attrs.class "specialButton"]
      [ Html.text "Random" ]
    , Html.a
      [ eventNoPropagation "mouseup" (\_ -> ClearGraph)
      , Attrs.src ""
      , Attrs.class "specialButton"]
      [ Html.text "Clear" ]
    , Html.a
      [ eventNoPropagation "mouseup" (\_ -> SaveTestButton)
      , Attrs.src ""
      , Attrs.class "specialButton"]
      [ Html.text "SaveTest" ]
    , Html.a
      [ eventNoPropagation "mouseup" (\_ -> ToggleSync)
      , Attrs.src ""
      , Attrs.class "specialButton"]
      [ Html.text
          (if m.syncEnabled then "DisableSync" else "EnableSync") ]
    , Html.span
      [ Attrs.class "specialButton"]
      [Html.text (toString m.center)]
    , Html.span
      [ Attrs.class "specialButton"]
      [Html.text ("Active tests: " ++ toString m.tests)]
    ] ++ integrationTestButton)

viewError : Maybe String -> Html.Html Msg
viewError mMsg = case mMsg of
  Just msg ->
    Html.div [Attrs.id "status", Attrs.class "error"] [Html.text msg]
  Nothing ->
    Html.div [Attrs.id "status"] [Html.text "Dark"]


