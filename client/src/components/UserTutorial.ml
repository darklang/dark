open Prelude
open ViewUtils

(* Spec: https://www.notion.so/darklang/Hello-World-Tutorial-9f0caa9890e74e47b5ac3e66ee826a4c *)
let defaultStep : tutorialStep option = Some Welcome

let isGettingStartedCanvas ~(username : string) ~(canvasname : string) : bool =
  canvasname = username ^ "-gettingstarted"


(** [currentStepFraction currentStep] returns a tuple of the form [(currentStepNumber, totalSteps)], given the [currentStep]. *)
let currentStepFraction (currentStep : tutorialStep) : int * int =
  let currentStepNumber =
    match currentStep with
    | Welcome ->
        1
    | VerbChange ->
        2
    | ReturnValue ->
        3
    | OpenTab ->
        4
    | GettingStarted ->
        5
  in
  let totalSteps = 5 in
  (currentStepNumber, totalSteps)


let getPrevStep (current : tutorialStep option) : tutorialStep option =
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


let getNextStep (current : tutorialStep option) : tutorialStep option =
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
        if isGettingStartedCanvas ~username:m.username ~canvasname:m.canvasName
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


let htmlForStep (step : tutorialStep) (username : string) : msg Html.html =
  let stepTitle =
    let current, total = currentStepFraction step in
    Html.p
      [Html.class' "step-title"]
      [Html.text (Printf.sprintf "%d/%d" current total)]
  in
  match step with
  | Welcome ->
      Html.div
        [Html.class' "tutorial-txt"]
        [ stepTitle
        ; Html.p
            []
            [ Html.text
                "Welcome to Dark! Let's get started by creating a \"Hello World\" endpoint."
            ]
        ; Html.p
            []
            [ Html.text
                "Click anywhere on the canvas (the large light gray region in the center of the screen), type "
            ; Html.span [Html.class' "grey-highlight"] [Html.text "\"hello\""]
            ; Html.text " and choose (by clicking or pressing enter) "
            ; Html.span
                [Html.class' "grey-highlight"]
                [Html.text "\"New HTTP handler named /hello\""]
            ; Html.text
                ". This will create a handler for the /hello endpoint of your app."
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
                "In the return value (the small light gray box inside your HTTP handler), type "
            ; Html.span
                [Html.class' "grey-highlight"]
                [Html.text "\"Hello World\""]
            ; Html.text ". Make sure to include the quotes!" ] ]
  | OpenTab ->
      Html.div
        [Html.class' "tutorial-txt"]
        [ stepTitle
        ; Html.p
            []
            [ Html.text
                "Now let's test out the /hello endpoint. Click on the hamburger menu in the upper right of your HTTP handler and select "
            ; Html.span
                [Html.class' "grey-highlight"]
                [Html.text "\"Open in new tab\""]
            ; Html.text "." ] ]
  | GettingStarted ->
      let btn =
        let link = "https://darklang.com/a/" ^ username ^ "-gettingstarted" in
        Html.div
          [ ViewUtils.nothingMouseEvent "mousedown"
          ; ViewUtils.nothingMouseEvent "mouseup"
          ; Html.class' "getting-started" ]
          [ Html.a
              [Html.href link; Html.target "_blank"]
              [Html.text "Open my Getting Started canvas"] ]
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


let viewNavigationBtns (step : tutorialStep) : msg Html.html =
  let prevBtn =
    let clickEvent =
      match getPrevStep (Some step) with
      | Some _ ->
          let stepNum, _ = currentStepFraction step in
          ViewUtils.eventNoPropagation
            ~key:("prev-step-" ^ string_of_int stepNum)
            "click"
            (fun _ -> TutorialMsg PrevStep)
      | None ->
          Vdom.noProp
    in
    Html.button
      [ Html.class' "page-btn"
      ; ViewUtils.nothingMouseEvent "mousedown"
      ; ViewUtils.nothingMouseEvent "mouseup"
      ; clickEvent
      ; Html.Attributes.disabled (clickEvent = Vdom.noProp) ]
      [Html.text "Previous"]
  in
  let nextBtn =
    let clickEvent =
      match getNextStep (Some step) with
      | Some _ ->
          let stepNum, _ = currentStepFraction step in
          ViewUtils.eventNoPropagation
            ~key:("next-step-" ^ string_of_int stepNum)
            "click"
            (fun _ -> TutorialMsg NextStep)
      | None ->
          Vdom.noProp
    in
    Html.button
      [ Html.class' "page-btn"
      ; ViewUtils.nothingMouseEvent "mousedown"
      ; ViewUtils.nothingMouseEvent "mouseup"
      ; clickEvent
      ; Html.Attributes.disabled (clickEvent = Vdom.noProp) ]
      [Html.text "Next"]
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
    [ Html.id "sidebar-right"
    ; nothingMouseEvent "mousedown"
    ; ViewUtils.eventNoPropagation
        ~key:"disable-panning-gettingstarted"
        "mouseover"
        (fun _ -> EnablePanning false)
    ; ViewUtils.eventNoPropagation
        ~key:"enable-panning-gettingstarted"
        "mouseout"
        (fun _ -> EnablePanning true) ]
    [ Html.div
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
        ; btn ] ]


let viewStep (step : tutorialStep option) (username : string) : msg Html.html =
  match step with
  | Some step ->
      let btnContainer = viewNavigationBtns step in
      Html.div
        [ Html.id "sidebar-right"
        ; nothingMouseEvent "mousedown"
        ; ViewUtils.eventNoPropagation
            ~key:"disable-panning-tutorial"
            "mouseover"
            (fun _ -> EnablePanning false)
        ; ViewUtils.eventNoPropagation
            ~key:"enableable-panning-tutorial"
            "mouseout"
            (fun _ -> EnablePanning true) ]
        [htmlForStep step username; btnContainer]
  | None ->
      Vdom.noNode


let view
    (step : tutorialStep option)
    (username : string)
    (canvasname : string)
    (firstVisitToCanvas : bool) : msg Html.html =
  if firstVisitToCanvas && isGettingStartedCanvas ~username ~canvasname
  then viewGettingStarted
  else viewStep step username
