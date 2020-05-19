open Prelude

(* Dark *)
module B = BlankOr

type viewProps = ViewUtils.viewProps

let fontAwesome = ViewUtils.fontAwesome

type fnExecutionStatus =
  | Unsafe
  | IncompleteArgs
  | Ready
  | Executing
  | Replayable
  | NoPermission

type props =
  { analysisStore : analysisStore
  ; ast : FluidAST.t
  ; executingFunctions : ID.t list
  ; permission : permission option
  ; tlid : TLID.t }

let propsFromViewProps (p : ViewUtils.viewProps) : props =
  { analysisStore = p.analysisStore
  ; ast = p.astInfo.ast
  ; executingFunctions = p.executingFunctions
  ; permission = p.permission
  ; tlid = p.tlid }


let fnExecutionStatus
    (p : props) (fn : function_) (id : ID.t) (args : ID.t list) =
  let functionIsExecuting (fid : ID.t) : bool =
    List.member ~value:fid p.executingFunctions
  in
  let isComplete id =
    match Analysis.getLiveValue' p.analysisStore id with
    | None | Some (DError _) | Some (DIncomplete _) ->
        false
    | Some _ ->
        true
  in
  let fnIsComplete id =
    match Analysis.getLiveValue' p.analysisStore id with
    | Some (DIncomplete (SourceId (srcTlid, srcId)))
    | Some (DError (SourceId (srcTlid, srcId), _))
    (* assume tlids are the same if the ids are *)
      when (srcTlid, srcId) = (p.tlid, id) ->
        (* this means the live value is an error/incomplete created by this
         * function, so the function is incomplete because it's unplayed. *)
        false
    | None | Some (DError _) | Some (DIncomplete _) ->
        (* this means the live value is an error/incomplete that was not
         * created by the current function (which means this function is not
         * responsible for it, hence this function is complete. Note that the
         * Stored_function_result DB drops the SourceId from DIncompletes and
         * DErrors, which is why this specific implementation was necessary. *)
        true
    | Some _ ->
        true
  in
  let paramsComplete = List.all ~f:isComplete args in
  let resultHasValue = fnIsComplete id in
  let name = fn.fnName in
  if p.permission <> Some ReadWrite
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
    (p : props) (fn : function_) (id : ID.t) (args : ID.t list) =
  let name = fn.fnName in
  let status = fnExecutionStatus p fn id args in
  match fn.fnPreviewSafety with
  (* UserFunctions always need play buttons to add the arguments to the trace *)
  | Safe when fn.fnOrigin <> UserFunction ->
      Vdom.noNode
  | Safe | Unsafe ->
      let class_ = executionClass status in
      let title = executionTitle status in
      let icon = executionIcon status in
      let events = executionEvents status p.tlid id name in
      Html.div
        ([Html.class' ("execution-button " ^ class_); Html.title title] @ events)
        [fontAwesome icon]
