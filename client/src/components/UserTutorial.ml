(* Spec for User Tutorial : https://www.notion.so/darklang/Hello-World-Tutorial-9f0caa9890e74e47b5ac3e66ee826a4c *)
(* We will be implimenting pretty tooltips soon that replace this file so Sydney put the html for the temporary tooltips here since both will need to be deleted once pretty tooltips is impliented *)

open Prelude
open ViewUtils

let defaultStep : tutorialStep option = Some Welcome

let isTutorialCanvas ~(username : string) ~(canvasname : string) : bool =
  canvasname = username ^ "-crud"


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


let stepNumToStep (currentStep : int) : tutorialStep option =
  match currentStep with
  | 1 ->
      Some Welcome
  | 2 ->
      Some VerbChange
  | 3 ->
      Some ReturnValue
  | 4 ->
      Some OpenTab
  | 5 ->
      Some GettingStarted
  | _ ->
      None


let getPrevStep (current : tutorialStep option) : tutorialStep option =
  match current with
  | Some step ->
      let currentStepNumber, _ = currentStepFraction step in
      stepNumToStep (currentStepNumber - 1)
  | None ->
      None


let getNextStep (current : tutorialStep option) : tutorialStep option =
  match current with
  | Some step ->
      let currentStepNumber, _ = currentStepFraction step in
      stepNumToStep (currentStepNumber + 1)
  | None ->
      None


let update (m : model) (msg : tutorialMsg) : modification =
  let userTutorial, mods =
    match msg with
    | NextStep ->
        (getNextStep m.userTutorial, [])
    | PrevStep ->
        (getPrevStep m.userTutorial, [])
    | CloseTutorial ->
        if m.showUserWelcomeModal then Entry.sendSegmentMessage WelcomeModal ;
        ( None
        , [ ReplaceAllModificationsWithThisOne
              (fun m ->
                ( { m with
                    showUserWelcomeModal = false
                  ; firstVisitToThisCanvas = false }
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


let htmlForTempTooltip (tooltip : tooltip) : msg Html.html =
  let learnMoreLink (link : string option) : msg Html.html =
    match link with
    | Some link ->
        Html.a
          [Html.class' "learn-more-link"; Html.href link; Html.target "_blank"]
          [Html.text "Learn More"]
    | None ->
        Vdom.noNode
  in
  let content, link =
    match tooltip with
    | Http ->
        let link = Some "https://darklang.github.io/docs/first-api-endpoint" in
        (["Click the plus sign to create a REST API endpoint."], link)
    | Worker ->
        let link = Some "https://darklang.github.io/docs/first-worker" in
        ( [ "Click the plus sign to create a worker to process asynchronous tasks."
          ]
        , link )
    | Cron ->
        let link = Some "https://darklang.github.io/docs/first-cron" in
        (["Click the plus sign to create a scheduled job."], link)
    | Repl ->
        let link = Some "https://darklang.github.io/docs/first-repl" in
        (["Click the plus sign to create a general purpose coding block."], link)
    | Datastore ->
        let link = Some "https://darklang.github.io/docs/first-datastore" in
        (["Click to create a key-value store."], link)
    | Function ->
        let link = Some "https://darklang.github.io/docs/first-function" in
        (["Click to create a reusable block of code."], link)
    | FourOhFour ->
        let link =
          Some "https://darklang.github.io/docs/trace-driven-development"
        in
        ( [ "Attempts to hit endpoints that do not yet have handlers appear here."
          ; "If you're looking for a 404 but not seeing it in this list, check the 'Deleted' section of the sidebar."
          ]
        , link )
    | Deleted ->
        let link = None in
        (["Deleted handlers appear here."], link)
    | PackageManager ->
        let link = None in
        ( [ "A list of built-in Dark functions. Click on the name of the function to preview it."
          ; "To use the function in your canvas, start typing its name in your handler and select it from autocomplete."
          ]
        , link )
    | StaticAssets ->
        let link = Some "https://darklang.github.io/docs/static-assets" in
        (["Learn more about hosting static assets here."], link)
  in
  Html.div
    [Html.class' "tutorial-txt"]
    ( (content |> List.map ~f:(fun p -> Html.p [] [Html.text p]))
    @ [learnMoreLink link] )


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
            ; Html.span [Html.class' "grey-highlight"] [Html.text "hello"]
            ; Html.text " and choose (by clicking or pressing enter) "
            ; Html.span
                [Html.class' "bold"]
                [Html.text "New HTTP handler named /hello"]
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
            ; Html.span [Html.class' "bold"] [Html.text "Open in new tab"]
            ; Html.text "." ] ]
  | GettingStarted ->
      let btn =
        let link = "https://darklang.com/a/" ^ username ^ "-crud" in
        Html.div
          [ ViewUtils.nothingMouseEvent "mousedown"
          ; ViewUtils.nothingMouseEvent "mouseup"
          ; Html.class' "getting-started" ]
          [ Html.a
              [Html.href link; Html.target "_blank"]
              [Html.text "Open CRUD app canvas"] ]
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
                "To help you continue to learn, we've created a sample CRUD app canvas."
            ]
        ; btn ]


let closeTutorial (text : string) (action : msg) : msg Html.html =
  Html.p
    [ Html.class' "close-btn"
    ; ViewUtils.nothingMouseEvent "mousedown"
    ; ViewUtils.nothingMouseEvent "mouseup"
    ; ViewUtils.eventNoPropagation ~key:"close-tutorial" "click" (fun _ ->
          action) ]
    [Html.text text]


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
    let link = "https://darklang.github.io/docs/your-first" in
    Html.div
      [ ViewUtils.nothingMouseEvent "mousedown"
      ; ViewUtils.nothingMouseEvent "mouseup"
      ; Html.class' "getting-started" ]
      [ Html.a
          [Html.href link; Html.target "_blank"]
          [Html.text "Getting Started tutorial"] ]
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
                    "This is an example of a fully working CRUD application. It enables you to create, delete or edit a blog post, store the content and metadata in a datastore and access the blog posts via API endpoint."
                ]
            ; Html.br []
            ; Html.br []
            ; Html.p
                []
                [ Html.text
                    "If you'd like to try building something complex, we've created a Getting Started tutorial in our documentation."
                ]
            ; btn ] ]
    ; closeTutorial "End tutorial" (TutorialMsg CloseTutorial) ]


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
        [ htmlForStep step username
        ; btnContainer
        ; closeTutorial "End tutorial" (TutorialMsg CloseTutorial) ]
  | None ->
      Vdom.noNode


let viewTempToolTip (tooltip : tooltip option) : msg Html.html =
  match tooltip with
  | Some tooltip ->
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
        [htmlForTempTooltip tooltip; closeTutorial "Close" (ToolTipMsg Close)]
  | None ->
      Vdom.noNode


let view
    (step : tutorialStep option)
    (username : string)
    (canvasname : string)
    (firstVisitToThisCanvas : bool)
    (tooltip : tooltip option) : msg Html.html =
  if Option.isSome tooltip
  then viewTempToolTip tooltip
  else if firstVisitToThisCanvas && isTutorialCanvas ~username ~canvasname
  then viewGettingStarted
  else viewStep step username
