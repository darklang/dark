open Tea
open Types
open! Porting
module JSD = Json_decode_extended


let isDebugging () =
  match String.uncons (Tea_navigation.getLocation()).search with
  | Some ('?', rest) ->
    rest
    |> String.toLower
    |> String.split "&"
    |> (fun strs -> Belt.List.some strs ((=) "debug"))
  | _ -> false

let debuggerLinkLoc () =
let debug = not (isDebugging ()) in
let loc = Tea_navigation.getLocation() in
let newSearch =
  match (debug, String.uncons loc.search) with
  | true, Some ('?', rest) -> ("?" ^ rest ^ "&debug")
  | true, _ -> "?debug"
  | false, Some ('?', "debug") -> ""
  | false, Some ('?', rest) -> (rest
                                |> String.split "&"
                                |> List.filter ((!=) "debug")
                                |> String.join "&"
                                |> (++) "?")
  | false, _ -> ""
in
Printf.sprintf "%s//%s%s%s%s" loc.protocol loc.host loc.pathname newSearch loc.hash


let viewButtons (m : model) : msg Html.html =
  let integrationTestButton =
    match m.integrationTestState with
    | IntegrationTestExpectation _ ->
        [ Html.a
            [ ViewUtils.eventNoPropagation ~key:"fit" "mouseup" (fun _ -> FinishIntegrationTest)
            ; Html.src ""
            ; Html.id "finishIntegrationTest"
            ; Html.class' "specialButton" ]
            [Html.text "Finish integration tests"] ]
    | IntegrationTestFinished (Ok ()) ->
        [ Html.div
            [ Html.id "integrationTestSignal"
            ; Html.class' "specialButton success" ]
            [Html.text "success"] ]
    | IntegrationTestFinished (Error msg) ->
        [ Html.div
            [ Html.id "integrationTestSignal"
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
              ; ViewUtils.eventNoPropagation ~key:(string_of_bool m.error.showDetails)"mouseup" (fun _ ->
                    ShowErrorDetails (not m.error.showDetails) ) ]
              [ Html.text
                  ( if m.error.showDetails then "hide details"
                  else "see details" ) ] ]
  in
  let posToString pos :string =
    Printf.sprintf "{ x = %i, y = %i}" pos.x pos.y
  in
  let pageToString pg =
    match pg with
      Toplevels pos -> "Toplevels " ^ (posToString pos)
    | Fn (tlid, pos) ->
      (Printf.sprintf "Fn (TLID %i %s)" (Prelude.deTLID tlid) (posToString pos))
  in
  Html.div [Html.id "buttons"]
    ( [ Html.a
          [ ViewUtils.eventNoPropagation ~key:"stb" "mouseup" (fun _ -> SaveTestButton)
          ; Html.src ""
          ; Html.class' "specialButton" ]
          [Html.text "SaveTest"]
      ; Html.a
          [ ViewUtils.eventNoPropagation ~key:"tt" "mouseup" (fun _ -> ToggleTimers)
          ; Html.src ""
          ; Html.class' "specialButton" ]
          [ Html.text
              (if m.timersEnabled then "DisableTimers" else "EnableTimers") ]
      ; Html.span
          [Html.class' "specialButton"]
          [Html.text (pageToString m.currentPage)]
      ; Html.span
          [Html.class' "specialButton"]
          [Html.text ("Tests: [" ^ Js.Array.toString (Array.of_list (List.map
                                                                       show_variantTest
                                                                       m.tests))^ "]")]
      ; Html.span
          [Html.class' ("specialButton environment " ^ m.environment)]
          [Html.text (m.environment ^ "/" ^ "Bucklescript")]
      ; Html.a
          [Html.href (debuggerLinkLoc ())
          ; Html.src ""
          ; Html.class' "specialButton" ]
          [ Html.text
              (if isDebugging () then "DisableDebugger" else "EnableDebugger") ] ]
    @ integrationTestButton @ returnButton @ [status] )

let viewError (err : darkError) : msg Html.html =
  let viewException exc =
    match exc.result with
    | None -> [Html.text exc.short]
    | Some result -> [Html.text result]
  in
  Html.div
    [Html.classList [("error-panel", true); ("show", err.showDetails)]]
    ( match err.message with
    | None -> []
    | Some msg -> (
      match JSD.decodeString Decoders.exception_ msg with
      | Error _ -> [Html.text msg]
      | Ok exc -> viewException exc ) )
