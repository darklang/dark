open Prelude
open ViewUtils

(* Spec: https://www.notion.so/darklang/Hello-World-Tutorial-9f0caa9890e74e47b5ac3e66ee826a4c *)
let defaultStep : tutorialSteps option = Some Welcome

let isGettingStartedCanvas (username : string) (canvasName : string) : bool =
  String.toLower canvasName = username ^ "-gettingstarted"


let numberOfSteps (current : tutorialSteps) : string * string =
  let currentStep =
    match current with
    | Welcome ->
        "1"
    | VerbChange ->
        "2"
    | ReturnValue ->
        "3"
    | OpenTab ->
        "4"
    | GettingStarted ->
        "5"
  in
  let totalSteps = "5" in
  (currentStep, totalSteps)


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
        if isGettingStartedCanvas m.username m.canvasName
        then Entry.sendSegmentMessage WelcomeModal ;
        ( None
        , [ ReplaceAllModificationsWithThisOne
              (fun m ->
                ( { m with
                    showUserWelcomeModal = false
                  ; firstVisitToCanvas = false }
                , Tea.Cmd.none )) ] )
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
  let stepTitle =
    let current, total = numberOfSteps step in
    Html.p [Html.class' "step-title"] [Html.text (current ^ "/" ^ total)]
  in
  match step with
  | Welcome ->
      Html.div
        [Html.class' "tutorial-txt"]
        [ stepTitle
        ; Html.p
            []
            [ Html.text
                "Welcome to Dark! Let's get started by creating our first Hello World."
            ]
        ; Html.p
            []
            [ Html.text
                "Click anywhere on the canvas (the lighter grey area), type hello and choose \"New HTTP handler\""
            ] ]
  | VerbChange ->
      Html.div
        [Html.class' "tutorial-txt"]
        [ stepTitle
        ; Html.p [] [Html.text "Select GET as the verb for your HTTP handler."]
        ]
  | ReturnValue ->
      Html.div
        [Html.class' "tutorial-txt"]
        [ stepTitle
        ; Html.p
            []
            [ Html.text
                "In the return value (the light grey box), type \"Hello World\"."
            ] ]
  | OpenTab ->
      Html.div
        [Html.class' "tutorial-txt"]
        [ stepTitle
        ; Html.p
            []
            [ Html.text
                "Click on the hamburger menu in the upper right and select Open in New Tab."
            ] ]
  | GettingStarted ->
      let btn =
        let link = "https://darklang.com/a/" ^ username ^ "-gettingstarted" in
        Html.div
          [ ViewUtils.nothingMouseEvent "mousedown"
          ; ViewUtils.nothingMouseEvent "mouseup"
          ; Html.class' "getting-started" ]
          [ Html.a
              [Html.href link; Html.target "_blank"]
              [Html.text "Open my getting started canvas"] ]
      in
      Html.div
        [Html.class' "tutorial-txt"]
        [ stepTitle
        ; Html.p
            []
            [ Html.text
                "Congratulations, you've created your first Hello World in Dark!"
            ]
        ; Html.p
            []
            [ Html.text
                "To help you continue to learn, we've created a Getting Started canvas."
            ]
        ; btn ]


let closeTutorial : msg Html.html =
  Html.p
    [ Html.class' "close-btn"
    ; ViewUtils.nothingMouseEvent "mousedown"
    ; ViewUtils.nothingMouseEvent "mouseup"
    ; ViewUtils.eventNoPropagation ~key:"close-tutorial" "click" (fun _ ->
          TutorialMsg CloseTutorial) ]
    [Html.text "End tutorial"]


let viewNavigationBtns (step : tutorialSteps) : msg Html.html =
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
  Html.div [Html.class' "btn-container"] [prevBtn; nextBtn]


let viewGettingStarted : msg Html.html =
  let btn =
    let link = "https://darklang.github.io/docs/introduction" in
    Html.div
      [ ViewUtils.nothingMouseEvent "mousedown"
      ; ViewUtils.nothingMouseEvent "mouseup"
      ; Html.class' "getting-started" ]
      [ Html.a
          [Html.href link; Html.target "_blank"]
          [Html.text "Take me to the walkthrough"] ]
  in
  Html.div
    []
    [ Html.div
        [Html.class' "tutorial-txt"]
        [ Html.p
            []
            [ Html.text
                "This is your Getting Started canvas, which showcases some basic functionality."
            ]
        ; Html.p
            []
            [ Html.text
                "If you'd like to go through the steps of building this canvas by hand, a full walkthrough is available in the documentation."
            ] ]
    ; btn ]


let view
    (step : tutorialSteps option)
    (username : string)
    (canvasName : string)
    (firstVisitToCanvas : bool) : msg Html.html =
  match step with
  | _ when firstVisitToCanvas && isGettingStartedCanvas username canvasName ->
      Html.div
        [ Html.id "sidebar-right"
        ; nothingMouseEvent "mousedown"
        ; ViewUtils.eventNoPropagation ~key:"ept" "mouseover" (fun _ ->
              EnablePanning false)
        ; ViewUtils.eventNoPropagation ~key:"epf" "mouseout" (fun _ ->
              EnablePanning true) ]
        [viewGettingStarted; closeTutorial]
  | Some step ->
      let btnContainer = viewNavigationBtns step in
      Html.div
        [ Html.id "sidebar-right"
        ; nothingMouseEvent "mousedown"
        ; ViewUtils.eventNoPropagation ~key:"ept" "mouseover" (fun _ ->
              EnablePanning false)
        ; ViewUtils.eventNoPropagation ~key:"epf" "mouseout" (fun _ ->
              EnablePanning true) ]
        [tutorialStepsToText step username; btnContainer; closeTutorial]
  | _ ->
      Vdom.noNode
