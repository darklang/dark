open Prelude

(* Dark *)
module B = BlankOr

type viewState = ViewUtils.viewState

let fontAwesome = ViewUtils.fontAwesome

type fnExecutionStatus =
  | Unsafe
  | IncompleteArgs
  | Ready
  | Executing
  | Replayable
  | NoPermission

type state =
  { analysisStore : analysisStore
  ; ast : FluidAST.t
  ; executingFunctions : ID.t list
  ; permission : permission option
  ; tlid : TLID.t }

let stateFromViewState (s : ViewUtils.viewState) : state =
  { analysisStore = s.analysisStore
  ; ast = s.ast
  ; executingFunctions = s.executingFunctions
  ; permission = s.permission
  ; tlid = s.tlid }


let fnExecutionStatus
    (s : state) (fn : function_) (id : ID.t) (args : ID.t list) =
  let functionIsExecuting (fid : ID.t) : bool =
    List.member ~value:fid s.executingFunctions
  in
  let isComplete id =
    match Analysis.getLiveValue' s.analysisStore id with
    | None | Some (DError _) | Some (DIncomplete _) ->
        false
    | Some _ ->
        true
  in
  let paramsComplete = List.all ~f:isComplete args in
  let resultHasValue = isComplete id in
  let name = fn.fnName in
  if s.permission <> Some ReadWrite
  then NoPermission
  else if name = "Password::check" || name = "Password::hash"
  then Unsafe
  else if not paramsComplete
  then IncompleteArgs
  else if functionIsExecuting id
  then Executing
  else if resultHasValue
  then Replayable
  else Ready


let executionClass status =
  match status with
  | Unsafe ->
      "execution-button-unsafe"
  | IncompleteArgs ->
      "execution-button-unavailable"
  | Ready ->
      "execution-button-needed"
  | Executing ->
      "execution-button-needed is-executing"
  | Replayable ->
      "execution-button-repeat"
  | NoPermission ->
      "execution-button-unavailable"


let executionTitle status =
  match status with
  | Unsafe ->
      "Cannot run interactively for security reasons"
  | IncompleteArgs ->
      "Cannot run: some parameters are incomplete"
  | Ready ->
      "Click to execute function"
  | Executing ->
      "Function is executing"
  | Replayable ->
      "Click to execute function again"
  | NoPermission ->
      "You do not have permission to execute this function"


let executionError status =
  match status with
  | Unsafe ->
      "Cannot run interactively for security reasons"
  | IncompleteArgs ->
      "Cannot run: some parameters are incomplete"
  | Ready ->
      "Click Play to execute function"
  | Executing ->
      "Function is executing"
  | Replayable ->
      "Click to execute function again"
  | NoPermission ->
      "You do not have permission to execute this function"


let executionIcon status =
  match status with
  | Unsafe | NoPermission ->
      "times"
  | IncompleteArgs | Ready | Executing ->
      "play"
  | Replayable ->
      "redo"


let executionEvents status tlid id name =
  match status with
  | Unsafe | Executing | IncompleteArgs | NoPermission ->
      [Html.noProp; Html.noProp; Html.noProp; Html.noProp]
  | Ready | Replayable ->
      [ ViewUtils.eventNoPropagation
          ~key:("efb-" ^ TLID.toString tlid ^ "-" ^ ID.toString id ^ "-" ^ name)
          "click"
          (fun _ -> ExecuteFunctionButton (tlid, id, name))
      ; ViewUtils.nothingMouseEvent "mouseup"
      ; ViewUtils.nothingMouseEvent "mousedown"
      ; ViewUtils.nothingMouseEvent "dblclick" ]


let fnExecutionButton
    (s : state) (fn : function_) (id : ID.t) (args : ID.t list) =
  let name = fn.fnName in
  let status = fnExecutionStatus s fn id args in
  match fn.fnPreviewSafety with
  | Safe ->
      Vdom.noNode
  | Unsafe ->
      let class_ = executionClass status in
      let title = executionTitle status in
      let icon = executionIcon status in
      let events = executionEvents status s.tlid id name in
      Html.div
        ([Html.class' ("execution-button " ^ class_); Html.title title] @ events)
        [fontAwesome icon]
