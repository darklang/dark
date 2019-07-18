open Types
open Tc
open Prelude

let debuggerLinkLoc () =
  let loc = Tea_navigation.getLocation () in
  let newSearch =
    Url.queryParams
    |> List.filter ~f:(fun (k, _) -> k <> "debugger")
    |> (fun x -> if Url.isDebugging then x else ("debugger", true) :: x)
    |> List.map ~f:(fun (k, v) -> k ^ "=" ^ if v then "1" else "0")
    |> String.join ~sep:"&"
    |> fun x -> if x = "" then "" else "?" ^ x
  in
  Printf.sprintf
    "%s//%s%s%s%s"
    loc.protocol
    loc.host
    loc.pathname
    newSearch
    loc.hash


let viewIntegrationTestButton (testState : integrationTestState) :
    msg Html.html =
  let integrationTestButton =
    match testState with
    | IntegrationTestExpectation _ ->
        [ Html.a
            [ ViewUtils.eventNoPropagation ~key:"fit" "mouseup" (fun _ ->
                  FinishIntegrationTest )
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
    | NoIntegrationTest ->
        []
  in
  Html.div [Html.id "buttons"] integrationTestButton


let viewError (err : darkError) : msg Html.html =
  let viewException (exc : exception_) =
    match exc.result with
    | None ->
        [Html.text exc.short]
    | Some result ->
        [Html.text result]
  in
  Html.div
    [Html.classList [("error-panel", true); ("show", err.showDetails)]]
    ( match err.message with
    | None ->
        []
    | Some msg ->
      ( match Json_decode_extended.decodeString Decoders.exception_ msg with
      | Error _ ->
          [Html.text msg]
      | Ok exc ->
          viewException exc ) )


let readOnlyMessage (m : model) : msg Html.html =
  Html.div
    [ Html.classList
        [ ("message-panel", true)
          (* Only show this on confirmed Read-only so it doesn't pop up before initial_load. *)
        ; ("show", m.permission = Some Read) ] ]
    [ Html.strong [] [Html.text "Heads up:"]
    ; Html.text
        " this canvas is read-only; you'll be able to view and copy it but not change it."
    ]
