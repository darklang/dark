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
        (getNextStep m.tooltipState.userTutorial, [])
    | PrevStep ->
        (getPrevStep m.tooltipState.userTutorial, [])
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
      (fun m -> ({m with tooltipState = {m.tooltipState with userTutorial}}, Tea.Cmd.none))
  else
    Many
      ( mods
      @ [ ReplaceAllModificationsWithThisOne
            (fun m -> ({m with tooltipState = {m.tooltipState with userTutorial}}, Tea.Cmd.none)) ] )

            
let closeTutorial (text : string) (action : msg) : msg Html.html =
  Html.p
    [ Html.class' "close-btn"
    ; ViewUtils.nothingMouseEvent "mousedown"
    ; ViewUtils.nothingMouseEvent "mouseup"
    ; ViewUtils.eventNoPropagation ~key:"close-tutorial" "click" (fun _ ->
          action) ]
    [Html.text text]

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

let view
    (username : string)
    (canvasname : string)
    (firstVisitToThisCanvas : bool) : msg Html.html =
  if firstVisitToThisCanvas && isTutorialCanvas ~username ~canvasname
  then viewGettingStarted
  else Vdom.noNode
