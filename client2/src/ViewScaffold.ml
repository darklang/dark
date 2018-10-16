open Tea
open! Porting
module Attrs = Html.Attributes
module JSD = Json.Decode
open Types
open ViewUtils

let viewButtons (m : model) : msg Html.html =
  let integrationTestButton =
    match m.integrationTestState with
    | IntegrationTestExpectation _ ->
        [ Html.a
            [ eventNoPropagation "mouseup" (fun _ -> FinishIntegrationTest)
            ; Attrs.src ""
            ; Attrs.id "finishIntegrationTest"
            ; Html.class' "specialButton" ]
            [Html.text "Finish integration tests"] ]
    | IntegrationTestFinished (Ok ()) ->
        [ Html.div
            [ Attrs.id "integrationTestSignal"
            ; Html.class' "specialButton success" ]
            [Html.text "success"] ]
    | IntegrationTestFinished (Error msg) ->
        [ Html.div
            [ Attrs.id "integrationTestSignal"
            ; Html.class' "specialButton failure" ]
            [Html.text <| "failure: " ^ msg] ]
    | NoIntegrationTest -> []
  in
  let returnButton =
    match m.currentPage with
    | Fn (_, _) ->
        [ Url.linkFor (Toplevels m.canvas.offset) "specialButton default-link"
            [Html.text "Return to Canvas"] ]
    | _ -> []
  in
  let status =
    match m.error.message with
    | None -> Html.div [Html.class' "status"] [Html.text "Dark"]
    | Some msg ->
        Html.div
          [Html.class' "status error"]
          [ Html.text "Error: "
          ; Html.a
              [ Html.class' "link"
              ; Html.href "#"
              ; eventNoPropagation "mouseup" (fun _ ->
                    ShowErrorDetails (not m.error.showDetails) ) ]
              [ Html.text
                  ( if m.error.showDetails then "hide details"
                  else "see details" ) ] ]
  in
  Html.div [Attrs.id "buttons"]
    ( [ Html.a
          [ eventNoPropagation "mouseup" (fun _ -> SaveTestButton)
          ; Attrs.src ""
          ; Html.class' "specialButton" ]
          [Html.text "SaveTest"]
      ; Html.a
          [ eventNoPropagation "mouseup" (fun _ -> ToggleTimers)
          ; Attrs.src ""
          ; Html.class' "specialButton" ]
          [ Html.text
              (if m.timersEnabled then "DisableTimers" else "EnableTimers") ]
      ; Html.span
          [Html.class' "specialButton"]
          [Html.text (toString m.currentPage)]
      ; Html.span
          [Html.class' "specialButton"]
          [Html.text ("Tests: " ^ toString m.tests)]
      ; Html.span
          [Html.class' ("specialButton environment " ^ m.environment)]
          [Html.text m.environment] ]
    ^ integrationTestButton ^ returnButton ^ [status] )

let viewError (err : darkError) : msg Html.html =
  let viewException exc =
    match exc.result with
    | None -> [Html.text exc.short]
    | Some result -> [Html.text result]
  in
  Html.div
    [Attrs.classList [("error-panel", true); ("show", err.showDetails)]]
    ( match err.message with
    | None -> []
    | Some msg -> (
      match JSD.decodeString JSONUtils.decodeException msg with
      | Error _ -> [Html.text msg]
      | Ok exc -> viewException exc ) )
