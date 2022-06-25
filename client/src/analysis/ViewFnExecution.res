open Prelude

// Dark
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

type props = {
  analysisStore: analysisStore,
  ast: FluidAST.t,
  executingFunctions: list<id>,
  permission: option<permission>,
  tlid: TLID.t,
}

let propsFromViewProps = (p: ViewUtils.viewProps): props => {
  analysisStore: p.analysisStore,
  ast: p.astInfo.ast,
  executingFunctions: p.executingFunctions,
  permission: p.permission,
  tlid: p.tlid,
}

let fnExecutionStatus = (p: props, fn: function_, id: id, args: list<id>) => {
  let functionIsExecuting = (fid: id): bool => List.member(~value=fid, p.executingFunctions)

  let isComplete = id =>
    switch Analysis.getLiveValue'(p.analysisStore, id) {
    | None | Some(DError(_)) | Some(DIncomplete(_)) => false
    | Some(_) => true
    }

  let fnIsComplete = id =>
    switch Analysis.getLiveValue'(p.analysisStore, id) {
    | Some(DIncomplete(SourceId(srcTlid, srcId)))
    | Some(DError(SourceId(srcTlid, srcId), _))
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
       * Stored_function_result DB drops the SourceId from DIncompletes and
       * DErrors, which is why this specific implementation was necessary. */
      true
    | Some(_) => true
    }

  let paramsComplete = List.all(~f=isComplete, args)
  let resultHasValue = fnIsComplete(id)
  let name = fn.fnName
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
      Html.noProp,
      Html.noProp,
      Html.noProp,
      Html.noProp,
    }
  | Ready | Replayable => list{
      ViewUtils.eventNoPropagation(
        ~key="efb-" ++ (TLID.toString(tlid) ++ ("-" ++ (ID.toString(id) ++ ("-" ++ name)))),
        "click",
        _ => ExecuteFunctionButton(tlid, id, name),
      ),
      ViewUtils.nothingMouseEvent("mouseup"),
      ViewUtils.nothingMouseEvent("mousedown"),
      ViewUtils.nothingMouseEvent("dblclick"),
    }
  }

let fnExecutionButton = (p: props, fn: function_, id: id, args: list<id>) => {
  let name = fn.fnName
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
      list{Html.class'("execution-button " ++ class_), Html.title(title), ...events},
      list{fontAwesome(icon)},
    )
  }
}
