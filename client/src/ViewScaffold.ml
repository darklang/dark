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


let viewButtons (m : model) : msg Html.html =
  let integrationTestButton =
    match m.integrationTestState with
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
  let returnButton =
    match m.currentPage with
    | Architecture ->
        []
    | _ ->
        [ Html.a
            [ Html.class' "specialButton default-link return-to-canvas"
            ; Html.href "#" ]
            [Html.text "Return to Canvas"] ]
  in
  let status =
    match m.error.message with
    | None ->
        Html.div [Html.class' "status"] [Html.text "Dark"]
    | Some _ ->
        Html.div
          [Html.class' "status error"]
          [ Html.text "Error: "
          ; Html.a
              [ Html.class' "link"
              ; Html.href "#"
              ; ViewUtils.eventNoPropagation
                  ~key:(string_of_bool m.error.showDetails)
                  "mouseup"
                  (fun _ -> ShowErrorDetails (not m.error.showDetails) ) ]
              [ Html.text
                  ( if m.error.showDetails
                  then "hide details"
                  else "see details" ) ] ]
  in
  let pageToString pg =
    match pg with
    | Architecture ->
        "Architecture"
    | FocusedFn tlid ->
        Printf.sprintf "Fn (TLID %s)" (deTLID tlid)
    | FocusedHandler tlid ->
        Printf.sprintf "Handler (TLID %s)" (deTLID tlid)
    | FocusedDB tlid ->
        Printf.sprintf "DB (TLID %s)" (deTLID tlid)
    | FocusedType tlid ->
        Printf.sprintf "Type (TLID %s)" (deTLID tlid)
  in
  let saveTestButton =
    Html.a
      [ ViewUtils.eventNoPropagation ~key:"stb" "mouseup" (fun _ ->
            SaveTestButton )
      ; Html.src ""
      ; Html.class' "specialButton" ]
      [Html.text "SaveTest"]
  in
  let toggleTimersButton =
    Html.a
      [ ViewUtils.eventNoPropagation ~key:"tt" "mouseup" (fun _ -> ToggleTimers)
      ; Html.src ""
      ; Html.class' "specialButton" ]
      [Html.text (if m.timersEnabled then "DisableTimers" else "EnableTimers")]
  in
  let currentPage =
    Html.span
      [Html.class' "specialButton"]
      [Html.text (pageToString m.currentPage)]
  in
  let variants =
    Html.span
      [Html.class' "specialButton"]
      [ Html.text
          ( "Tests: ["
          ^ (m.tests |> List.map ~f:show_variantTest |> String.join ~sep:", ")
          ^ "]" ) ]
  in
  let environment =
    Html.span
      [Html.class' ("specialButton environment " ^ m.environment)]
      [Html.text m.environment]
  in
  let debugger =
    Html.a
      [Html.href (debuggerLinkLoc ()); Html.src ""; Html.class' "specialButton"]
      [ Html.text
          (if Url.isDebugging then "DisableDebugger" else "EnableDebugger") ]
  in
  Html.div
    [Html.id "buttons"]
    ( integrationTestButton
    @ [ saveTestButton
      ; toggleTimersButton
      ; currentPage
      ; variants
      ; environment
      ; debugger ]
    @ returnButton
    @ [status] )


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
