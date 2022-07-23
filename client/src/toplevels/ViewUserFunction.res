open Prelude

// Dark
module B = BlankOr

type msg = AppTypes.msg
type viewProps = ViewUtils.viewProps

let fontAwesome = ViewUtils.fontAwesome

let onEvent = ViewUtils.onEvent

type exeFunction =
  | CanExecute(traceID, list<RT.Dval.t>)
  | CannotExecute(string)
  | IsExecuting

let viewUserFnName = (~classes: list<string>, vp: viewProps, v: blankOr<string>): Html.html<msg> =>
  ViewBlankOr.viewText(~classes, ~enterable=true, FnName, vp, v)

let viewExecuteBtn = (vp: viewProps, fn: PT.UserFunction.t): Html.html<msg> => {
  let exeStatus = if vp.isExecuting {
    IsExecuting
  } else {
    // Attempts to get trace inputValues for this function
    switch Analysis.selectedTrace(vp.tlTraceIDs, vp.traces, vp.tlid) {
    | Some(traceID, Ok(td)) =>
      let args = UserFunctions.inputToArgs(fn, td.input)
      // If any of the args is Incomplete/Error then we don't want to bother allowing this function to be executed
      if List.any(~f=dv =>
        switch dv {
        | DIncomplete(_) => true
        | _ => false
        }
      , args) {
        CannotExecute("Cannot run function with incomplete arguments")
      } else if List.any(~f=dv =>
        switch dv {
        | DError(_) => true
        | _ => false
        }
      , args) {
        CannotExecute("Cannot run function with arguments that has an error")
      } else {
        CanExecute(traceID, args)
      }
    | _ => CannotExecute("Cannot run function with no trace data")
    }
  }

  let events = // If function is ready for re-execution, attach onClick listener
  switch (fn.metadata.name, exeStatus) {
  | (F(_, fnName), CanExecute(traceID, args)) =>
    ViewUtils.eventNoPropagation(
      ~key="run-fun" ++ ("-" ++ (TLID.toString(fn.tlid) ++ ("-" ++ traceID))),
      "click",
      _ => ExecuteFunctionFromWithin({
        efpTLID: fn.tlid,
        efpCallerID: FluidAST.toID(fn.ast),
        efpTraceID: traceID,
        efpFnName: fnName,
        efpArgs: args,
      }),
    )
  | _ => Vdom.noProp
  }

  let title = switch exeStatus {
  | CannotExecute(msg) => msg
  | CanExecute(_) => "Click to execute function"
  | IsExecuting => "Function is executing"
  }

  Html.div(
    list{
      Html.classList(list{
        ("execution-button", true),
        (
          "is-ready",
          vp.permission == Some(ReadWrite) &&
            switch exeStatus {
            | CanExecute(_) => true
            | _ => false
            },
        ),
        ("is-executing", exeStatus == IsExecuting),
      }),
      events,
      Html.title(title),
    },
    list{fontAwesome("redo")},
  )
}

let viewMetadata = (vp: viewProps, fn: functionTypes, showFnTooltips: bool): Html.html<msg> => {
  let addParamBtn = switch fn {
  | UserFunction(fn) =>
    switch vp.permission {
    | Some(ReadWrite) =>
      let strTLID = TLID.toString(fn.tlid)
      Html.div(
        ~unique="add-param-col-" ++ strTLID,
        list{
          Html.class'("col new-parameter"),
          ViewUtils.eventNoPropagation(
            ~key="aufp-" ++ strTLID,
            "click",
            _ => AddUserFunctionParameter(fn.tlid),
          ),
        },
        list{
          Html.div(
            list{Html.class'("parameter-btn allowed add")},
            list{fontAwesome("plus-circle")},
          ),
          Html.span(list{Html.class'("btn-label")}, list{Html.text("add new parameter")}),
        },
      )
    | Some(Read) | None => Vdom.noNode
    }
  | PackageFn(_) => Vdom.noNode
  }

  let titleRow = {
    let titleText = switch fn {
    | UserFunction(fn) => fn.metadata.name
    | PackageFn(fn) => BlankOr.newF(PT.FQFnName.PackageFnName.toString(fn.name))
    }

    let executeBtn = switch fn {
    | UserFunction(fn) =>
      let menuView = {
        let uploadPackageFnAction: TLMenu.menuItem = {
          title: "Upload Function",
          key: "upload-ufn-",
          icon: Some("upload"),
          action: _ => UploadFn(fn.tlid),
          disableMsg: None,
        }

        let delAct: TLMenu.menuItem = {
          let disableMsg = if !UserFunctions.canDelete(vp.usedInRefs, fn.tlid) {
            Some(
              "Cannot delete this function as it is used in your code base. Use the references on the right to find and change this function's callers, after which you'll be able to delete it.",
            )
          } else {
            None
          }

          {
            title: "Delete",
            key: "del-ufn-",
            icon: Some("times"),
            action: _ => DeleteUserFunction(fn.tlid),
            disableMsg: disableMsg,
          }
        }

        let menuItems = if vp.isAdmin {
          list{delAct, uploadPackageFnAction}
        } else {
          list{delAct}
        }

        Html.div(list{Html.class'("menu")}, list{TLMenu.viewMenu(vp.menuState, vp.tlid, menuItems)})
      }

      Html.div(list{Html.class'("fn-actions")}, list{viewExecuteBtn(vp, fn), menuView})
    | PackageFn(_) => Html.span(list{Html.class'("fn-readonly")}, list{Html.text("Read Only")})
    }

    Html.div(
      list{Html.class'("spec-header")},
      list{
        ViewUtils.darkIcon("fn"),
        viewUserFnName(vp, ~classes=list{"fn-name-content"}, titleText),
        executeBtn,
      },
    )
  }

  let paramRows = {
    let fnParamTooltip =
      Tooltips.generateContent(FnParam) |> Tooltips.viewToolTip(
        ~shouldShow=showFnTooltips,
        ~tlid=None,
      )

    Html.div(
      list{Html.id("fnparams"), Html.class'("params")},
      Belt.List.concat(FnParams.view(fn, vp), list{addParamBtn, fnParamTooltip}),
    )
  }

  let returnRow = {
    let returnType = switch fn {
    | UserFunction(fn) => fn.metadata.returnType
    | PackageFn(fn) => BlankOr.newF(fn.returnType)
    }

    Html.div(
      list{Html.id("fnreturn"), Html.class'("col param")},
      list{
        fontAwesome("level-down-alt"),
        ViewBlankOr.viewTipe(~classes=list{"type"}, ~enterable=true, FnReturnTipe, vp, returnType),
      },
    )
  }

  Html.div(list{Html.class'("fn-header")}, list{titleRow, paramRows, returnRow})
}

let view = (vp: viewProps, fn: functionTypes, showFnTooltips: bool): Html.html<msg> =>
  Html.div(
    list{
      Html.class'(
        switch fn {
        | UserFunction(_) => "user-fn-toplevel"
        | PackageFn(_) => "pkg-fn-toplevel"
        },
      ),
    },
    list{
      Html.div(list{Html.class'("metadata")}, list{viewMetadata(vp, fn, showFnTooltips)}),
      Html.div(list{Html.class'("function-body expand")}, FluidView.view(vp, list{})),
    },
  )
