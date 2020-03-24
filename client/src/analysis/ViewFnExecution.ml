open Prelude

(* Dark *)
module B = BlankOr

type viewState = ViewUtils.viewState

let fontAwesome = ViewUtils.fontAwesome

type fnExecutionStatus =
  | Unsafe
  | IncompleteArgs
  | Executing
  | Ready of traceID * dval list
  | Replayable of traceID * dval list
  | NoPermission

type state =
  { analysisStore : analysisStore
  ; ast : FluidAST.t
  ; executingFunctions : ID.t list
  ; permission : permission option
  ; tlid : TLID.t
  ; traceID : traceID option }

let stateFromViewState (s : ViewUtils.viewState) : state =
  { analysisStore = s.analysisStore
  ; ast = s.ast
  ; executingFunctions = s.executingFunctions
  ; permission = s.permission
  ; tlid = s.tlid
  ; traceID = s.selectedTraceID }


let fnExecutionStatus
    (s : state) (fn : function_) (callerID : ID.t) (argIDs : ID.t list) =
  match s.traceID with
  | None ->
      IncompleteArgs
  | Some traceID ->
      let functionIsExecuting (fid : ID.t) : bool =
        List.member ~value:fid s.executingFunctions
      in
      let args =
        List.map argIDs ~f:(fun id ->
            Analysis.getLiveValue' s.analysisStore id
            |> Option.withDefault ~default:(DIncomplete SourceNone))
      in
      let isComplete arg =
        match arg with DError _ | DIncomplete _ -> false | _ -> true
      in
      let paramsComplete = List.all ~f:isComplete args in
      let resultHasValue =
        Analysis.getLiveValue' s.analysisStore callerID
        |> Option.map ~f:isComplete
        |> Option.withDefault ~default:false
      in
      let name = fn.fnName in
      if s.permission <> Some ReadWrite
      then NoPermission
      else if name = "Password::check" || name = "Password::hash"
      then Unsafe
      else if not paramsComplete
      then IncompleteArgs
      else if functionIsExecuting callerID
      then Executing
      else if resultHasValue
      then Replayable (traceID, args)
      else Ready (traceID, args)


let executionClass status =
  match status with
  | Unsafe ->
      "execution-button-unsafe"
  | IncompleteArgs ->
      "execution-button-unavailable"
  | Ready _ ->
      "execution-button-needed"
  | Executing ->
      "execution-button-needed is-executing"
  | Replayable _ ->
      "execution-button-repeat"
  | NoPermission ->
      "execution-button-unavailable"


let executionTitle status =
  match status with
  | Unsafe ->
      "Cannot run interactively for security reasons"
  | IncompleteArgs ->
      "Cannot run: some parameters are incomplete"
  | Ready _ ->
      "Click to execute function"
  | Executing ->
      "Function is executing"
  | Replayable _ ->
      "Click to execute function again"
  | NoPermission ->
      "You do not have permission to execute this function"


let executionError status =
  match status with
  | Unsafe ->
      "Cannot run interactively for security reasons"
  | IncompleteArgs ->
      "Cannot run: some parameters are incomplete"
  | Ready _ ->
      "Click Play to execute function"
  | Executing ->
      "Function is executing"
  | Replayable _ ->
      "Click to execute function again"
  | NoPermission ->
      "You do not have permission to execute this function"


let executionIcon status =
  match status with
  | Unsafe | NoPermission ->
      "times"
  | IncompleteArgs | Ready _ | Executing ->
      "play"
  | Replayable _ ->
      "redo"


let executionEvents status tlid callerID fnName =
  match status with
  | Unsafe | Executing | IncompleteArgs | NoPermission ->
      [Html.noProp; Html.noProp; Html.noProp; Html.noProp]
  | Ready (traceID, args) | Replayable (traceID, args) ->
      (* TODO: add traceid/args to key *)
      [ ViewUtils.eventNoPropagation
          ~key:
            ( "efb-"
            ^ TLID.toString tlid
            ^ "-"
            ^ ID.toString callerID
            ^ "-"
            ^ fnName )
          "click"
          (fun _ ->
            FunctionExecutionMsg
              (FunctionExecutionExecuteFunction
                 { efpTLID = tlid
                 ; efpTraceID = traceID
                 ; efpCallerID = callerID
                 ; efpArgs = args
                 ; efpFnName = fnName }))
      ; ViewUtils.nothingMouseEvent "mouseup"
      ; ViewUtils.nothingMouseEvent "mousedown"
      ; ViewUtils.nothingMouseEvent "dblclick" ]


let fnExecutionButton
    (s : state) (fn : function_) (id : ID.t) (argIDs : ID.t list) =
  let name = fn.fnName in
  let status = fnExecutionStatus s fn id argIDs in
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
