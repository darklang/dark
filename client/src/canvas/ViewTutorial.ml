open Prelude
open ViewUtils

let defaultStep : tutorialSteps option = Some Welcome

let getPrevStep (current : tutorialSteps option) : tutorialSteps option =
  match current with
  | Some Welcome ->
      None
  | Some VerbChange ->
      Some Welcome
  | Some ReturnValue ->
      Some VerbChange
  | Some OpenTab ->
      Some ReturnValue
  | Some GettingStarted ->
      Some OpenTab
  | None ->
      None


let getNextStep (current : tutorialSteps option) : tutorialSteps option =
  match current with
  | Some Welcome ->
      Some VerbChange
  | Some VerbChange ->
      Some ReturnValue
  | Some ReturnValue ->
      Some OpenTab
  | Some OpenTab ->
      Some GettingStarted
  | Some GettingStarted | None ->
      None


let update (m : model) (msg : tutorialMsg) : modification =
  let userTutorial, mods =
    match msg with
    | NextStep ->
        (getNextStep m.userTutorial, [])
    | PrevStep ->
        (getPrevStep m.userTutorial, [])
    | CloseTutorial ->
        (* Entry.sendSegmentMessage WelcomeModal ; *)
        (None, [])
    | ReopenTutorial ->
        (defaultStep, [])
  in
  if List.isEmpty mods
  then
    ReplaceAllModificationsWithThisOne
      (fun m -> ({m with userTutorial}, Tea.Cmd.none))
  else
    Many
      ( mods
      @ [ ReplaceAllModificationsWithThisOne
            (fun m -> ({m with userTutorial}, Tea.Cmd.none)) ] )


let tutorialStepsToText (step : tutorialSteps) : msg Html.html =
  match step with
  | Welcome ->
      Html.p
        []
        [ Html.text
            "Welcome to Dark! Let's get started by creating our first Hello World. Click anywhere on the canvas (the lighter grey area), type hello and choose \"New HTTP handler\""
        ]
  | VerbChange ->
      Html.p [] [Html.text "Select GET as the verb for your HTTP handler."]
  | ReturnValue ->
      Html.p
        []
        [ Html.text
            "In the return value - the light grey area - type Hello World." ]
  | OpenTab ->
      Html.p
        []
        [ Html.text
            "Click on the hamburger menu in the upper right and select Open in New Tab."
        ]
  | GettingStarted ->
      Html.p
        []
        [ Html.text
            "Congratulations, you've created your first Hello World in Dark!"
        ; Html.text
            "To help you continue to learn, we've created a Getting Started canvas. link should take them to username-gettingstarted"
        ]


let view (step : tutorialSteps) : msg Html.html =
  let closeTutorial =
    Html.div
      [ ViewUtils.nothingMouseEvent "mousedown"
      ; ViewUtils.nothingMouseEvent "mouseup"
      ; ViewUtils.eventNoPropagation ~key:"close-tutorial" "click" (fun _ ->
            TutorialMsg CloseTutorial) ]
      [Html.p [] [Html.text "End tutorial"]]
  in
  let prevBtn =
    match getPrevStep (Some step) with
    | Some _ ->
        Html.div
          [ ViewUtils.nothingMouseEvent "mousedown"
          ; ViewUtils.nothingMouseEvent "mouseup"
          ; ViewUtils.eventNoPropagation
              ~key:"close-welcome-modal"
              "click"
              (fun _ -> TutorialMsg PrevStep) ]
          [Html.p [] [Html.text "Previous"]]
    | None ->
        Vdom.noNode
  in
  let nextBtn =
    match getNextStep (Some step) with
    | Some _ ->
        Html.div
          [ ViewUtils.nothingMouseEvent "mousedown"
          ; ViewUtils.nothingMouseEvent "mouseup"
          ; ViewUtils.eventNoPropagation
              ~key:"close-welcome-modal"
              "click"
              (fun _ -> TutorialMsg NextStep) ]
          [Html.p [] [Html.text "Next"]]
    | None ->
        Vdom.noNode
  in
  Html.div
    [ Html.id "sidebar-right"
    ; nothingMouseEvent "mousedown"
    ; ViewUtils.eventNoPropagation ~key:"ept" "mouseover" (fun _ ->
          EnablePanning false)
    ; ViewUtils.eventNoPropagation ~key:"epf" "mouseout" (fun _ ->
          EnablePanning true) ]
    [tutorialStepsToText step; prevBtn; nextBtn; closeTutorial]
