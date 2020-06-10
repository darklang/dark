(* Spec for User Tutorial : https://www.notion.so/darklang/Hello-World-Tutorial-9f0caa9890e74e47b5ac3e66ee826a4c *)
(* We will be implimenting pretty tooltips soon that replace this file so Sydney put the html for the temporary tooltips here since both will need to be deleted once pretty tooltips is impliented *)

open Prelude
open ViewUtils

let defaultStep : tutorialStep option = Some Welcome

let isTutorialCanvas ~(username : string) ~(canvasname : string) : bool =
  canvasname = username ^ "-crud"


let update (m : model) (msg : tutorialMsg) : modification =
  let userTutorial, mods =
    match msg with
    | NextStep ->
        (Tooltips.getNextStep m.tooltipState.userTutorial, [])
    | PrevStep ->
        (Tooltips.getPrevStep m.tooltipState.userTutorial, [])
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
      (fun m ->
        ( {m with tooltipState = {m.tooltipState with userTutorial}}
        , Tea.Cmd.none ))
  else
    Many
      ( mods
      @ [ ReplaceAllModificationsWithThisOne
            (fun m ->
              ( {m with tooltipState = {m.tooltipState with userTutorial}}
              , Tea.Cmd.none )) ] )


let generateTooltipContent (tutorialStep : tutorialStep) (username : string) :
    Tooltips.tooltipContent =
  match tutorialStep with
  | Welcome ->
      { title =
          "Welcome to Dark! Let's get started by creating a \"Hello World\" endpoint."
      ; details =
          Some
            "Click anywhere on the canvas (the large light gray region in the center of the screen), type \"hello\" and choose (by clicking or pressing enter) \"New HTTP handler named /hello\". This will create a handler for the /hello endpoint of your app."
      ; action = None
      ; align = Left
      ; tipAlignment = "align-left"
      ; tooltipStyle = Tutorial tutorialStep }
  | VerbChange ->
      { title = "Select GET as the verb for your HTTP handler."
      ; details = None
      ; action = None
      ; align = Top
      ; tipAlignment = "align-left"
      ; tooltipStyle = Tutorial tutorialStep }
  | ReturnValue ->
      { title =
          "In the return value (the small light gray box inside your HTTP handler), type \"Hello World\". Make sure to include the quotes!"
      ; details = None
      ; action = None
      ; align = Left
      ; tipAlignment = "align-left"
      ; tooltipStyle = Tutorial tutorialStep }
  | OpenTab ->
      { title =
          "Now let's test out the /hello endpoint. Click on the hamburger menu in the upper right of your HTTP handler and select \"Open in new tab\"."
      ; details = None
      ; action = None
      ; align = Right
      ; tipAlignment = "align-left"
      ; tooltipStyle = Tutorial tutorialStep }
  | GettingStarted ->
      { title = "Congratulations, you've created your first Hello World in Dark!"
      ; details =
          Some
            "To help you continue to learn, we've created a sample CRUD app canvas."
      ; action =
          Some
            ( "Open CRUD app canvas"
            , ToolTipMsg
                (OpenLink ("https://darklang.com/a/" ^ username ^ "-crud")) )
      ; align = Right
      ; tipAlignment = "align-left"
      ; tooltipStyle = Tutorial tutorialStep }


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
    (username : string) (canvasname : string) (firstVisitToThisCanvas : bool) :
    msg Html.html =
  if firstVisitToThisCanvas && isTutorialCanvas ~username ~canvasname
  then viewGettingStarted
  else Vdom.noNode
