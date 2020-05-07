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


let tutorialStepsToText (step : tutorialSteps) (username : string) :
    msg Html.html =
  match step with
  | Welcome ->
      Html.p
        [Html.class' "tutorial-txt"]
        [ Html.text
            "Welcome to Dark! Let's get started by creating our first Hello World. Click anywhere on the canvas (the lighter grey area), type hello and choose \"New HTTP handler\""
        ]
  | VerbChange ->
      Html.p
        [Html.class' "tutorial-txt"]
        [Html.text "Select GET as the verb for your HTTP handler."]
  | ReturnValue ->
      Html.p
        [Html.class' "tutorial-txt"]
        [ Html.text
            "In the return value - the light grey area - type Hello World." ]
  | OpenTab ->
      Html.p
        [Html.class' "tutorial-txt"]
        [ Html.text
            "Click on the hamburger menu in the upper right and select Open in New Tab."
        ]
  | GettingStarted ->
      let btn =
        let link = "https://darklang.com/a/" ^ username ^ "-gettingstarted" in
        Html.div
          [ ViewUtils.nothingMouseEvent "mousedown"
          ; ViewUtils.nothingMouseEvent "mouseup"
          ; Html.class' "getting-started" ]
          [ Html.a
              [Html.href link; Html.target "_blank"]
              [Html.text "Take me to the walkthrough"] ]
      in
      Html.div
        [Html.class' "tutorial-txt"]
        [ Html.p
            []
            [ Html.text
                "Congratulations, you've created your first Hello World in Dark!"
            ; Html.text
                "To help you continue to learn, we've created a Getting Started canvas."
            ]
        ; btn ]


let view (step : tutorialSteps) (username : string) : msg Html.html =
  let closeTutorial =
    Html.p
      [ Html.class' "close-btn"
      ; ViewUtils.nothingMouseEvent "mousedown"
      ; ViewUtils.nothingMouseEvent "mouseup"
      ; ViewUtils.eventNoPropagation ~key:"close-tutorial" "click" (fun _ ->
            TutorialMsg CloseTutorial) ]
      [Html.text "End tutorial"]
  in
  let prevBtn =
    let event =
      match getPrevStep (Some step) with
      | Some _ ->
          [ ViewUtils.eventNoPropagation
              ~key:"close-welcome-modal"
              "click"
              (fun _ -> TutorialMsg PrevStep) ]
      | None ->
          []
    in
    Html.div
      [ ViewUtils.nothingMouseEvent "mousedown"
      ; ViewUtils.nothingMouseEvent "mouseup"
      ; Html.classList [("btn", true); ("disabled", event = [])] ]
      [Html.p event [Html.text "Previous"]]
  in
  let nextBtn =
    let event =
      match getNextStep (Some step) with
      | Some _ ->
          [ ViewUtils.eventNoPropagation
              ~key:"close-welcome-modal"
              "click"
              (fun _ -> TutorialMsg NextStep) ]
      | None ->
          []
    in
    Html.div
      [ ViewUtils.nothingMouseEvent "mousedown"
      ; ViewUtils.nothingMouseEvent "mouseup"
      ; Html.classList [("btn", true); ("disabled", event = [])] ]
      [Html.p event [Html.text "Next"]]
  in
  let btnContainer =
    Html.div [Html.class' "btn-container"] [prevBtn; nextBtn]
  in
  Html.div
    [ Html.id "sidebar-right"
    ; nothingMouseEvent "mousedown"
    ; ViewUtils.eventNoPropagation ~key:"ept" "mouseover" (fun _ ->
          EnablePanning false)
    ; ViewUtils.eventNoPropagation ~key:"epf" "mouseout" (fun _ ->
          EnablePanning true) ]
    [tutorialStepsToText step username; btnContainer; closeTutorial]
