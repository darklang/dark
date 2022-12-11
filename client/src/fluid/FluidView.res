open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs
module Events = Tea.Events

// Fluid
module K = FluidKeyboard
module AC = FluidAutocomplete
module T = FluidToken
module E = FluidExpression
module MP = FluidMatchPattern
module Printer = FluidPrinter
module RT = RuntimeTypes

open ProgramTypes.Expr

module Msg = AppTypes.Msg

type viewProps = ViewUtils.viewProps

type token = T.t
type msg = AppTypes.msg

let viewCopyButton = (tlid, value): Html.html<msg> =>
  Html.div(
    list{
      Attrs.class("copy-value"),
      Attrs.title("Copy this expression's value to the clipboard"),
      EventListeners.eventNoPropagation(
        "click",
        ~key="copylivevalue-" ++ (value ++ TLID.toString(tlid)),
        m => Msg.ClipboardCopyLivevalue(value, m.mePos),
      ),
    },
    list{Icons.fontAwesome("copy")},
  )

let viewArrow = (curID: id, srcID: id): Html.html<msg> => {
  let curSelector = ".id-" ++ ID.toString(curID)
  let srcSelector = ".id-" ++ ID.toString(srcID)
  switch (
    Webapi.Dom.document->Webapi.Dom.Document.querySelector(curSelector),
    Webapi.Dom.document->Webapi.Dom.Document.querySelector(srcSelector),
  ) {
  | (Some(curElem), Some(srcElem)) =>
    let curRect = Native.Ext.getBoundingClient(curElem, curSelector)
    let srcRect = Native.Ext.getBoundingClient(srcElem, srcSelector)
    let height = curRect.bottom - srcRect.top
    Html.div(
      list{
        Attrs.class("src-arrow"),
        Attrs.styles(list{("height", "calc(" ++ (string_of_int(height) ++ "px - 2.5em)"))}),
      },
      list{},
    )
  | _ => Vdom.noNode
  }
}

let viewDval = (dval, tlid, secrets, ~canCopy: bool) => {
  let text = dval->Runtime.toRepr->Util.hideSecrets(secrets)
  list{
    Html.text(text),
    if canCopy {
      viewCopyButton(tlid, text)
    } else {
      Vdom.noNode
    },
  }
}

@ppx.deriving(show({with_path: false}))
type rec lvResult =
  | WithMessage(string)
  | WithDval({value: RT.Dval.t, canCopy: bool})
  | WithMessageAndDval({msg: string, value: RT.Dval.t, canCopy: bool})
  | WithSource({tlid: TLID.t, srcID: id, propValue: RT.Dval.t, srcResult: lvResult})
  | Loading

let rec lvResultToDebugString = (result: lvResult) => {
  switch result {
  | WithMessage(msg) => "WithMessage(" ++ msg ++ ")"
  | WithDval({value, canCopy}) =>
    "WithDval(" ++ value->RT.Dval.toDebugString ++ ", " ++ string_of_bool(canCopy) ++ ")"
  | WithMessageAndDval({msg, value, canCopy}) =>
    "WithMessageAndDval(" ++
    msg ++
    ", " ++
    value->RT.Dval.toDebugString ++
    ", " ++
    string_of_bool(canCopy) ++ ")"
  | WithSource({tlid, srcID, propValue, srcResult}) =>
    "WithSource(" ++
    TLID.toString(tlid) ++
    ", " ++
    ID.toString(srcID) ++
    ", " ++
    propValue->RT.Dval.toDebugString ++
    ", " ++
    srcResult->lvResultToDebugString ++ ")"
  | Loading => "Loading"
  }
}

let rec lvResultForId = (~recurred=false, vp: viewProps, id: id): lvResult => {
  let fnLoading = {
    // If fn needs to be manually executed, check status
    let ast = vp.astInfo.ast
    FluidAST.findExpr(id, ast)
    |> Option.andThen(~f=expr =>
      switch expr {
      | EFnCall(_, name, _, _) => Functions.find(name, vp.functions)
      | EInfix(_, InfixFnCall(name, _), _, _) =>
        Functions.find(Stdlib(PT.InfixStdlibFnName.toStdlib(name)), vp.functions)
      | EInfix(_, BinOp(op), _, _) =>
        Functions.find(
          Stdlib({version: 0, module_: "", function: PT.Expr.BinaryOperation.toString(op)}),
          vp.functions,
        )
      | _ => None
      }
    )
    |> Option.andThen(~f=(fn: Function.t) =>
      switch fn.previewable {
      | Pure => None
      | Impure | ImpurePreviewable =>
        let args = ast |> AST.getArguments(id) |> List.map(~f=E.toID)
        let s = ViewFnExecution.propsFromViewProps(vp)
        ViewFnExecution.fnExecutionStatus(s, fn, id, args)
        |> ViewFnExecution.executionError
        |> Option.some
      }
    )
  }

  switch Analysis.getLiveValueLoadable(vp.analysisStore, id) {
  | Loadable.Success(ExecutedResult(DIncomplete(_))) if Option.isSome(fnLoading) =>
    fnLoading |> Option.map(~f=msg => WithMessage(msg)) |> Option.unwrap(~default=Loading)
  | Loadable.Success(ExecutedResult(DIncomplete(SourceID(srcTlid, srcID)) as propValue))
  | Loadable.Success(ExecutedResult(DError(SourceID(srcTlid, srcID), _) as propValue))
    if srcID != id || srcTlid != vp.tlid =>
    if recurred {
      WithDval({value: propValue, canCopy: false})
    } else {
      WithSource({
        tlid: srcTlid,
        srcID: srcID,
        propValue: propValue,
        srcResult: lvResultForId(~recurred=true, vp, srcID),
      })
    }
  | Loadable.Success(ExecutedResult(DError(_) as dval))
  | Loadable.Success(ExecutedResult(DIncomplete(_) as dval)) =>
    WithDval({value: dval, canCopy: false})
  | Loadable.Success(ExecutedResult(dval)) => WithDval({value: dval, canCopy: true})
  | Loadable.NotInitialized | Loadable.Loading(_) => Loading
  | Loadable.Success(NonExecutedResult(DError(_) as dval))
  | Loadable.Success(NonExecutedResult(DIncomplete(_) as dval)) =>
    WithMessageAndDval({
      msg: "This code was not executed in this trace",
      value: dval,
      canCopy: false,
    })
  | Loadable.Success(NonExecutedResult(dval)) =>
    WithMessageAndDval({
      msg: "This code was not executed in this trace",
      value: dval,
      canCopy: true,
    })
  | Loadable.Error(err) => WithMessage("Error loading live value: " ++ err)
  }
}

let viewLiveValue = (vp: viewProps): Html.html<msg> => {
  // isLoaded will be set to false later if we are in the middle of loading
  // results. All other states are considered loaded. This is used to apply
  // a class ".loaded" purely for integration tests being able to know when
  // the live value content is ready and can be asserted on
  let isLoaded = ref(true)
  // Renders dval
  let renderDval = val => viewDval(val, vp.tlid, vp.secretValues)
  // Renders live value for token
  let renderTokenLv = id =>
    switch lvResultForId(vp, id) {
    | WithMessage(msg) => list{Html.text(msg)}
    | WithDval({value, canCopy}) => renderDval(value, ~canCopy)
    | WithMessageAndDval({msg, value, canCopy}) => list{
        Html.text(msg),
        Html.br(list{}),
        Html.br(list{}),
        ...renderDval(value, ~canCopy),
      }
    | WithSource({tlid, srcID, propValue, srcResult}) =>
      let msg = switch srcResult {
      | WithMessage(msg) => list{Html.text(msg)}
      | WithDval({value, canCopy}) => renderDval(value, ~canCopy)
      | WithMessageAndDval({msg, value, canCopy}) => list{
          Html.span(list{}, list{Html.text(msg), Html.br(list{}), ...renderDval(value, ~canCopy)}),
        }
      | _ => renderDval(propValue, ~canCopy=true)
      }

      list{
        viewArrow(id, srcID),
        Html.div(
          list{
            EventListeners.eventNoPropagation(
              ~key="lv-src-" ++ (ID.toString(srcID) ++ TLID.toString(tlid)),
              "click",
              _ => Msg.FluidMsg(FluidFocusOnToken(tlid, srcID)),
            ),
            Attrs.class("jump-src"),
            Attrs.title("Click here to go to the source of problem"),
          },
          Belt.List.concat(msg, list{Icons.fontAwesome("arrow-alt-circle-up")}),
        ),
      }
    | Loading =>
      isLoaded := false
      list{Icons.fontAwesome("spinner")}
    }

  FluidTokenizer.ASTInfo.getToken(vp.astInfo)
  |> Option.andThen(~f=(ti: T.tokenInfo) => {
    let content = switch AC.highlighted(vp.fluidState.ac) {
    | Some(FACVariable(_, Some(dv))) =>
      // If autocomplete is open and a variable is highlighted,
      // then show its dval
      Some(renderDval(dv, ~canCopy=true))
    | Some(FACSecret(_, dv)) => Some(renderDval(dv, ~canCopy=true))
    | Some(FACDatastore(_)) => None
    | Some(FACLiteral(LBool(true))) => Some(renderDval(DBool(true), ~canCopy=true))
    | Some(FACLiteral(LBool(false))) => Some(renderDval(DBool(false), ~canCopy=true))
    | Some(FACLiteral(LNull)) => Some(renderDval(DNull, ~canCopy=true))
    | Some(FACKeyword(_)) => None
    | _ =>
      // Else show live value of current token
      let token = ti.token
      let id = T.analysisID(token)
      if T.validID(id) {
        Some(renderTokenLv(id))
      } else {
        None
      }
    }

    Option.pair(content, Some(ti.startRow))
  }) // Render live value to the side
  |> Option.map(~f=((content, row)) => {
    let offset = Int.toFloat(row)
    Html.div(
      list{
        Attrs.classList(list{("live-value", true), ("loaded", isLoaded.contents)}),
        Attrs.styles(list{("top", Js.Float.toString(offset) ++ "rem")}),
        Attrs.autofocus(false),
        Vdom.attribute("", "spellcheck", "false"),
      },
      content,
    )
  })
  |> // If there's a failure at any point, we don't render the live-value wrapper
  Option.unwrap(~default=Vdom.noNode)
}

let viewReturnValue = (vp: ViewUtils.viewProps, dragEvents: ViewUtils.domEventList): Html.html<
  msg,
> =>
  if CursorState.tlidOf(vp.cursorState) == Some(vp.tlid) {
    let id = FluidAST.toID(vp.astInfo.ast)
    switch Analysis.getLiveValueLoadable(vp.analysisStore, id) {
    | Loadable.Success(ExecutedResult(dval)) =>
      let isRefreshed = switch vp.handlerProp {
      | Some({execution: Complete, _}) => true
      | _ => false
      }

      let warningHtml = {
        let onDefaultTrace = tlid =>
          switch vp.traces {
          | list{(tid, _)} if tid == Analysis.defaultTraceIDForTL(~tlid) => true
          | _ => false
          }

        let warningAttr = Attrs.class("warning-message")
        let text = contents =>
          Html.div(list{warningAttr}, list{Html.text(contents), Html.br(list{})})

        /* Since HTTP and userFunctions are the case where Incomplete return
         * is likely to case and error, we only want to highlight those
         * cases. */
        switch (dval, vp.tl) {
        | (DIncomplete(_), TLHandler(h)) if SpecHeaders.spaceOf(h.spec) == HSHTTP =>
          text("Your code needs to return a value in the last expression")
        | (DIncomplete(_), TLFunc(f)) if onDefaultTrace(f.tlid) =>
          text(
            "This function has not yet been called, so there are no values assigned to the parameters. Call this function in another handler.",
          )
        | (DIncomplete(_), TLFunc(_)) =>
          text("Your code needs to return a value in the last expression")
        | (_, TLFunc(f)) =>
          let actualType = dval |> RT.Dval.toType
          let declaredType = f.returnType

          if Runtime.isCompatible(actualType, declaredType) {
            Vdom.noNode
          } else {
            let actualTypeString = DType.type2str(actualType)
            let declaredTypeString = DType.type2str(declaredType)
            Html.div(
              list{warningAttr},
              list{
                Html.span(list{Attrs.class("err")}, list{Html.text("Type error: ")}),
                Html.text(
                  "This function should return " ++
                  (Util.indefiniteArticleFor(declaredTypeString) ++
                  " "),
                ),
                Html.span(list{Attrs.class("type")}, list{Html.text(declaredTypeString)}),
                Html.text(
                  ", but this trace returns " ++
                  (Util.indefiniteArticleFor(actualTypeString) ++
                  " "),
                ),
                Html.span(list{Attrs.class("type")}, list{Html.text(actualTypeString)}),
              },
            )
          }
        | (_, TLPmFunc(_)) | (_, TLHandler(_)) | (_, TLDB(_)) | (_, TLType(_)) => Vdom.noNode
        }
      }

      let returnHtml = {
        let dvalString = Runtime.toRepr(dval)
        let newLine = if String.includes(~substring="\n", dvalString) {
          Html.br(list{})
        } else {
          Vdom.noNode
        }

        Html.div(
          list{Attrs.class("value")},
          list{
            Html.text("This trace returns: "),
            newLine,
            ...viewDval(dval, vp.tlid, vp.secretValues, ~canCopy=true),
          },
        )
      }

      Html.div(
        list{
          Attrs.classList(list{
            ("return-value", true),
            ("refreshed", isRefreshed),
            ("draggable", dragEvents != list{}),
          }),
          ...dragEvents,
        },
        list{warningHtml, returnHtml},
      )
    | _ => Vdom.noNode
    }
  } else {
    Vdom.noNode
  }

let viewAST = (vp: ViewUtils.viewProps, dragEvents: ViewUtils.domEventList): list<
  Html.html<msg>,
> => {
  let liveValue = if vp.cursorState == FluidEntering(vp.tlid) {
    viewLiveValue(vp)
  } else {
    Vdom.noNode
  }

  let editorState = {
    FluidEditorView.analysisStore: vp.analysisStore,
    ast: vp.astInfo.ast,
    functions: vp.functions,
    executingFunctions: vp.executingFunctions,
    editor: MainEditor(vp.tlid),
    hoveringRefs: vp.hoveringRefs,
    fluidState: vp.fluidState,
    permission: vp.permission,
    tlid: vp.tlid,
    tokens: vp.astInfo.mainTokenInfos,
  }

  let mainEditor = FluidEditorView.view(editorState)
  let returnValue = viewReturnValue(vp, dragEvents)
  let debugAST = if vp.showHandlerASTs {
    Html.div(
      list{Attrs.class("debug-ast")},
      list{Html.text(E.toHumanReadable(FluidAST.toExpr(vp.astInfo.ast), true))},
    )
  } else {
    Vdom.noNode
  }

  let secondaryEditors = {
    let findRowOffestOfMainTokenWithId = (flagID: id): option<int> =>
      /* FIXME(ds) this is a giant hack to find the row offset of the corresponding
       * token in the main view for each secondary editor. This works by getting
       * the id of the split (ie, the id of the first token in the split)
       * and then looking through the main tokens [O(N)] to find one with a
       * corresponding id. This is brittle and will likely break at some point. We
       * should do something better. */
      FluidAST.findExpr(flagID, vp.astInfo.ast)
      |> Option.andThen(~f=x =>
        switch x {
        | EFeatureFlag(_, _, _, oldCode, _) => Some(E.toID(oldCode))
        | _ => None
        }
      )
      |> Option.andThen(~f=oldCodeID =>
        List.find(vp.astInfo.mainTokenInfos, ~f=ti => oldCodeID == T.tid(ti.token))
      )
      |> Option.map(~f=(ti: T.tokenInfo) => ti.startRow)

    Fluid.buildFeatureFlagEditors(vp.tlid, vp.astInfo.ast) |> List.map(~f=e =>
      switch e {
      | FluidTypes.Editor.NoEditor =>
        recover("got NoEditor when building feature flag editors", Html.div(list{}, list{}))
      | MainEditor(_) =>
        recover("got MainEditor when building feature flag editors", Html.div(list{}, list{}))
      | FeatureFlagEditor(_, flagID) =>
        let flagIcon = Html.div(
          list{Attrs.class("ff-icon"), Attrs.title("feature flag")},
          list{Icons.fontAwesome("flag")},
        )

        let rowOffset = flagID |> findRowOffestOfMainTokenWithId |> Option.unwrap(~default=0)

        let tokens =
          FluidTokenizer.ASTInfo.ffTokenInfosFor(flagID, vp.astInfo) |> recoverOpt(
            "can't find tokens for real flag",
            ~default=list{},
          )

        let editorProps = {
          FluidEditorView.analysisStore: vp.analysisStore,
          ast: vp.astInfo.ast,
          functions: vp.functions,
          executingFunctions: vp.executingFunctions,
          editor: e,
          hoveringRefs: vp.hoveringRefs,
          fluidState: vp.fluidState,
          permission: vp.permission,
          tlid: vp.tlid,
          tokens: tokens,
        }

        Html.div(
          list{
            Attrs.class("fluid-secondary-editor"),
            Attrs.styles(list{("top", string_of_int(rowOffset) ++ ".5rem")}),
          },
          list{flagIcon, FluidEditorView.view(editorProps)},
        )
      }
    )
  }

  list{mainEditor, liveValue, returnValue, debugAST, ...secondaryEditors}
}

let view = (vp: ViewUtils.viewProps, dragEvents: ViewUtils.domEventList) => list{
  Html.div(list{Attrs.class("fluid-ast")}, viewAST(vp, dragEvents)),
}
