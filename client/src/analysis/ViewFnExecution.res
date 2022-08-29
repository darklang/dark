open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs

// Dark
module Msg = AppTypes.Msg
module B = BlankOr

type viewProps = ViewUtils.viewProps

type fnExecutionStatus =
  | Unsafe
  | IncompleteArgs
  | Ready
  | Executing
  | Replayable
  | NoPermission

type props = {
  analysisStore: AnalysisTypes.analysisStore,
  ast: FluidAST.t,
  executingFunctions: list<id>,
  permission: option<AccountTypes.Permission.t>,
  tlid: TLID.t,
}

let propsFromViewProps = (p: ViewUtils.viewProps): props => {
  analysisStore: p.analysisStore,
  ast: p.astInfo.ast,
  executingFunctions: p.executingFunctions,
  permission: p.permission,
  tlid: p.tlid,
}

let fnExecutionStatus = (p: props, fn: Function.t, id: id, args: list<id>) => {
  let functionIsExecuting = (fid: id): bool => List.member(~value=fid, p.executingFunctions)

  let isComplete = id =>
    switch Analysis.getLiveValue'(p.analysisStore, id) {
    | None | Some(DError(_)) | Some(DIncomplete(_)) => false
    | Some(_) => true
    }

  let fnIsComplete = id =>
    switch Analysis.getLiveValue'(p.analysisStore, id) {
    | Some(DIncomplete(SourceID(srcTlid, srcId)))
    | Some(DError(SourceID(srcTlid, srcId), _))
      if // assume tlids are the same if the ids are
      (srcTlid, srcId) ==
        (p.tlid, id) => /* this means the live value is an error/incomplete created by this
       * function, so the function is incomplete because it's unplayed. */
      false
    | None
    | Some(DError(_))
    | Some(DIncomplete(_)) => /* this means the live value is an error/incomplete that was not
       * created by the current function (which means this function is not
       * responsible for it, hence this function is complete. Note that the
       * Stored_function_result DB drops the SourceID from DIncompletes and
       * DErrors, which is why this specific implementation was necessary. */
      true
    | Some(_) => true
    }

  let paramsComplete = List.all(~f=isComplete, args)
  let resultHasValue = fnIsComplete(id)
  let name = FQFnName.toString(fn.fnName)
  if p.permission != Some(ReadWrite) {
    NoPermission
  } else if name == "Password::check" || name == "Password::hash" {
    Unsafe
  } else if !paramsComplete {
    IncompleteArgs
  } else if functionIsExecuting(id) {
    Executing
  } else if resultHasValue {
    Replayable
  } else {
    Ready
  }
}

let executionClass = status =>
  switch status {
  | Unsafe => "execution-button-unsafe"
  | IncompleteArgs => "execution-button-unavailable"
  | Ready => "execution-button-needed"
  | Executing => "execution-button-needed is-executing"
  | Replayable => "execution-button-repeat"
  | NoPermission => "execution-button-unavailable"
  }

let executionTitle = status =>
  switch status {
  | Unsafe => "Cannot run interactively for security reasons"
  | IncompleteArgs => "Cannot run: some parameters are incomplete"
  | Ready => "Click to execute function"
  | Executing => "Function is executing"
  | Replayable => "Click to execute function again"
  | NoPermission => "You do not have permission to execute this function"
  }

let executionError = status =>
  switch status {
  | Unsafe => "Cannot run interactively for security reasons"
  | IncompleteArgs => "Cannot run: some parameters are incomplete"
  | Ready => "Click Play to execute function"
  | Executing => "Function is executing"
  | Replayable => "Click to execute function again"
  | NoPermission => "You do not have permission to execute this function"
  }

let executionIcon = status =>
  switch status {
  | Unsafe | NoPermission => "times"
  | IncompleteArgs | Ready | Executing => "play"
  | Replayable => "redo"
  }

let executionEvents = (status, tlid, id, name) =>
  switch status {
  | Unsafe | Executing | IncompleteArgs | NoPermission => list{
      Attrs.noProp,
      Attrs.noProp,
      Attrs.noProp,
      Attrs.noProp,
    }
  | Ready | Replayable => list{
      EventListeners.eventNoPropagation(
        ~key="efb-" ++ (TLID.toString(tlid) ++ ("-" ++ (ID.toString(id) ++ ("-" ++ name)))),
        "click",
        _ => Msg.ExecuteFunctionButton(tlid, id, name),
      ),
      EventListeners.nothingMouseEvent("mouseup"),
      EventListeners.nothingMouseEvent("mousedown"),
      EventListeners.nothingMouseEvent("dblclick"),
    }
  }

let fnExecutionButton = (p: props, fn: Function.t, id: id, args: list<id>) => {
  let name = FQFnName.toString(fn.fnName)
  let status = fnExecutionStatus(p, fn, id, args)
  switch fn.fnPreviewSafety {
  // UserFunctions always need play buttons to add the arguments to the trace
  | Safe if fn.fnOrigin != UserFunction => Vdom.noNode
  | Safe | Unsafe =>
    let class_ = executionClass(status)
    let title = executionTitle(status)
    let icon = executionIcon(status)
    let events = executionEvents(status, p.tlid, id, name)
    Html.div(
      list{Attrs.class'("execution-button " ++ class_), Attrs.title(title), ...events},
      list{Icons.fontAwesome(icon)},
    )
  }
}
