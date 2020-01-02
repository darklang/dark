open Prelude

let debuggerLinkLoc m =
  let loc = Tea_navigation.getLocation () in
  let newSearch =
    Url.queryParams ()
    |> List.filter ~f:(fun (k, _) -> k <> "debugger")
    |> (fun x -> if m.teaDebuggerEnabled then x else ("debugger", true) :: x)
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


let viewIntegrationTestButton (testState : integrationTestState) : msg Html.html
    =
  let integrationTestButton =
    match testState with
    | IntegrationTestExpectation _ ->
        [ Html.a
            [ ViewUtils.eventNoPropagation ~key:"fit" "mouseup" (fun _ ->
                  FinishIntegrationTest)
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


let viewError (message : string option) : msg Html.html =
  let viewErrorMsg =
    match message with
    | None ->
        [Vdom.noNode]
    | Some msg ->
      ( match Json_decode_extended.decodeString Decoders.exception_ msg with
      | Error _ ->
          [Html.p [] [Html.text msg]]
      | Ok {result = Some msg; _} ->
          [Html.p [] [Html.text msg]]
      | Ok exc ->
          [Html.p [] [Html.text exc.short]] )
  in
  let viewDismissBtn =
    [ Html.p
        [ Html.class' "dismissBtn"
        ; ViewUtils.eventNoPropagation "click" ~key:"dismiss-error" (fun _ ->
              DismissErrorBar) ]
        [Html.text "Dismiss"] ]
  in
  Html.div
    [Html.classList [("error-panel", true); ("show", message <> None)]]
    (viewErrorMsg @ viewDismissBtn)


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
