open Prelude

// Tea
module Attrs = Tea.Html2.Attributes
module Events = Tea.Html2.Events

// Fluid
module K = FluidKeyboard
module AC = FluidAutocomplete
module T = FluidToken
module E = FluidExpression
module P = FluidPattern
module Printer = FluidPrinter

open ProgramTypes.Expr

type viewProps = ViewUtils.viewProps

type token = T.t

let viewCopyButton = (tlid, value): Html.html<msg> =>
  Html.div(
    list{
      Html.class'("copy-value"),
      Html.title("Copy this expression's value to the clipboard"),
      ViewUtils.eventNoPropagation(
        "click",
        ~key="copylivevalue-" ++ (value ++ TLID.toString(tlid)),
        m => ClipboardCopyLivevalue(value, m.mePos),
      ),
    },
    list{ViewUtils.fontAwesome("copy")},
  )

let viewArrow = (curID: id, srcID: id): Html.html<Types.msg> => {
  let curSelector = ".id-" ++ ID.toString(curID)
  let srcSelector = ".id-" ++ ID.toString(srcID)
  switch (Native.Ext.querySelector(curSelector), Native.Ext.querySelector(srcSelector)) {
  | (Some(curElem), Some(srcElem)) =>
    let curRect = Native.Ext.getBoundingClient(curElem, curSelector)
    let srcRect = Native.Ext.getBoundingClient(srcElem, srcSelector)
    let height = curRect.bottom - srcRect.top
    Html.div(
      list{
        Html.class'("src-arrow"),
        Html.styles(list{("height", "calc(" ++ (string_of_int(height) ++ "px - 2.5em)"))}),
      },
      list{},
    )
  | _ => Vdom.noNode
  }
}

let viewDval = (tlid, secrets, dval, ~canCopy: bool) => {
  let text = Runtime.toRepr(dval) |> Util.hideSecrets(secrets)
  list{
    Html.text(text),
    if canCopy {
      viewCopyButton(tlid, text)
    } else {
      Vdom.noNode
    },
  }
}

type rec lvResult =
  | WithMessage(string)
  | WithDval({value: dval, canCopy: bool})
  | WithMessageAndDval({msg: string, value: dval, canCopy: bool})
  | WithSource({tlid: TLID.t, srcID: id, propValue: dval, srcResult: lvResult})
  | Loading

let rec lvResultForId = (~recurred=false, vp: viewProps, id: id): lvResult => {
  let fnLoading = {
    // If fn needs to be manually executed, check status
    let ast = vp.astInfo.ast
    FluidAST.find(id, ast)
    |> Option.andThen(~f=expr =>
      switch expr {
      | EFnCall(_, name, _, _) | EBinOp(_, name, _, _, _) => Functions.find(name, vp.functions)
      | _ => None
      }
    )
    |> Option.andThen(~f=fn =>
      switch fn.fnPreviewSafety {
      | Safe => None
      | Unsafe =>
        let args = ast |> AST.getArguments(id) |> List.map(~f=E.toID)
        let s = ViewFnExecution.propsFromViewProps(vp)
        ViewFnExecution.fnExecutionStatus(s, fn, id, args)
        |> ViewFnExecution.executionError
        |> Option.some
      }
    )
  }

  switch Analysis.getLiveValueLoadable(vp.analysisStore, id) {
  | LoadableSuccess(ExecutedResult(DIncomplete(_))) if Option.isSome(fnLoading) =>
    fnLoading |> Option.map(~f=msg => WithMessage(msg)) |> Option.unwrap(~default=Loading)
  | LoadableSuccess(ExecutedResult(DIncomplete(SourceId(srcTlid, srcID)) as propValue))
  | LoadableSuccess(ExecutedResult(DError(SourceId(srcTlid, srcID), _) as propValue))
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
  | LoadableSuccess(ExecutedResult(DError(_) as dval))
  | LoadableSuccess(ExecutedResult(DIncomplete(_) as dval)) =>
    WithDval({value: dval, canCopy: false})
  | LoadableSuccess(ExecutedResult(dval)) => WithDval({value: dval, canCopy: true})
  | LoadableNotInitialized | LoadableLoading(_) => Loading
  | LoadableSuccess(NonExecutedResult(DError(_) as dval))
  | LoadableSuccess(NonExecutedResult(DIncomplete(_) as dval)) =>
    WithMessageAndDval({
      msg: "This code was not executed in this trace",
      value: dval,
      canCopy: false,
    })
  | LoadableSuccess(NonExecutedResult(dval)) =>
    WithMessageAndDval({
      msg: "This code was not executed in this trace",
      value: dval,
      canCopy: true,
    })
  | LoadableError(err) => WithMessage("Error loading live value: " ++ err)
  }
}

let viewLiveValue = (vp: viewProps): Html.html<Types.msg> => {
  /* isLoaded will be set to false later if we are in the middle of loading
   * results. All other states are considered loaded. This is used to apply
   * a class ".loaded" purely for integration tests being able to know when
   * the live value content is ready and can be asserted on */
  let isLoaded = ref(true)
  // Renders dval
  let renderDval = viewDval(vp.tlid, vp.secretValues)
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
      | WithMessage(msg) => msg
      | WithDval({value, _}) => Runtime.toRepr(value)
      | WithMessageAndDval({msg, value, _}) => msg ++ ("\n\n" ++ Runtime.toRepr(value))
      | _ => Runtime.toRepr(propValue)
      }

      list{
        viewArrow(id, srcID),
        Html.div(
          list{
            ViewUtils.eventNoPropagation(
              ~key="lv-src-" ++ (ID.toString(srcID) ++ TLID.toString(tlid)),
              "click",
              _ => FluidMsg(FluidFocusOnToken(tlid, srcID)),
            ),
            Html.class'("jump-src"),
            Html.title("Click here to go to the source of problem"),
          },
          list{Html.text(msg), ViewUtils.fontAwesome("arrow-alt-circle-up")},
        ),
      }
    | Loading =>
      isLoaded := false
      list{ViewUtils.fontAwesome("spinner")}
    }

  FluidTokenizer.ASTInfo.getToken(vp.astInfo)
  |> Option.andThen(~f=ti => {
    let row = ti.startRow
    let content = switch AC.highlighted(vp.fluidState.ac) {
    | Some(FACVariable(_, Some(dv))) =>
      /* If autocomplete is open and a variable is highlighted,
       * then show its dval */
      Some(renderDval(dv, ~canCopy=true))
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

    Option.pair(content, Some(row))
  })
  |> // Render live value to the side
  Option.map(~f=((content, row)) => {
    let offset = float_of_int(row)
    Html.div(
      list{
        Html.classList(list{("live-value", true), ("loaded", isLoaded.contents)}),
        Html.styles(list{("top", Js.Float.toString(offset) ++ "rem")}),
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
  Types.msg,
> =>
  if CursorState.tlidOf(vp.cursorState) == Some(vp.tlid) {
    let id = FluidAST.toID(vp.astInfo.ast)
    switch Analysis.getLiveValueLoadable(vp.analysisStore, id) {
    | LoadableSuccess(ExecutedResult(dval)) =>
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

        let warningAttr = Html.class'("warning-message")
        let text = contents =>
          Html.div(list{warningAttr}, list{Html.text(contents), Html.br(list{})})

        /* Since HTTP and userFunctions are the case where Incomplete return
         * is likely to case and error, we only want to highlight those
         * cases. */
        switch (dval, vp.tl) {
        | (DIncomplete(_), TLHandler(h)) if SpecHeaders.spaceOf(h.spec) == HSHTTP =>
          text("Your code needs to return a value in the last expression")
        | (DIncomplete(_), TLFunc(f)) if onDefaultTrace(f.ufTLID) =>
          text(
            "This function has not yet been called, so there are no values assigned to the parameters. Call this function in another handler.",
          )
        | (DIncomplete(_), TLFunc(_)) =>
          text("Your code needs to return a value in the last expression")
        | (_, TLFunc(f)) =>
          let actualType = dval |> Runtime.typeOf
          let declaredType = BlankOr.valueWithDefault(DType.TAny, f.ufMetadata.ufmReturnTipe)

          if Runtime.isCompatible(actualType, declaredType) {
            Vdom.noNode
          } else {
            let actualTypeString = Runtime.tipe2str(actualType)
            let declaredTypeString = Runtime.tipe2str(declaredType)
            Html.div(
              list{warningAttr},
              list{
                Html.span(list{Html.class'("err")}, list{Html.text("Type error: ")}),
                Html.text(
                  "This function should return " ++
                  (Util.indefiniteArticleFor(declaredTypeString) ++
                  " "),
                ),
                Html.span(list{Html.class'("type")}, list{Html.text(declaredTypeString)}),
                Html.text(
                  ", but this trace returns " ++
                  (Util.indefiniteArticleFor(actualTypeString) ++
                  " "),
                ),
                Html.span(list{Html.class'("type")}, list{Html.text(actualTypeString)}),
              },
            )
          }
        | (_, TLPmFunc(_)) | (_, TLHandler(_)) | (_, TLDB(_)) | (_, TLTipe(_)) => Vdom.noNode
        }
      }

      let dvalString = Runtime.toRepr(dval)
      let returnHtml = {
        let newLine = if String.includes(~substring="\n", dvalString) {
          Html.br(list{})
        } else {
          Vdom.noNode
        }

        Html.div(
          list{Html.class'("value")},
          list{
            Html.text("This trace returns: "),
            newLine,
            ...viewDval(vp.tlid, vp.secretValues, dval, ~canCopy=true),
          },
        )
      }

      Html.div(
        list{
          Html.classList(list{
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
  Html.html<Types.msg>,
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
      list{Html.class'("debug-ast")},
      list{Html.text(E.toHumanReadable(FluidAST.toExpr(vp.astInfo.ast)))},
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
      FluidAST.find(flagID, vp.astInfo.ast)
      |> Option.andThen(~f=x =>
        switch x {
        | EFeatureFlag(_, _, _, oldCode, _) => Some(E.toID(oldCode))
        | _ => None
        }
      )
      |> Option.andThen(~f=oldCodeID =>
        List.find(vp.astInfo.mainTokenInfos, ~f=ti => oldCodeID == T.tid(ti.token))
      )
      |> Option.map(~f=ti => ti.startRow)

    Fluid.buildFeatureFlagEditors(vp.tlid, vp.astInfo.ast) |> List.map(~f=e =>
      switch e {
      | NoEditor =>
        recover("got NoEditor when building feature flag editors", Html.div(list{}, list{}))
      | MainEditor(_) =>
        recover("got MainEditor when building feature flag editors", Html.div(list{}, list{}))
      | FeatureFlagEditor(_, flagID) =>
        let flagIcon = Html.div(
          list{Html.class'("ff-icon"), Html.title("feature flag")},
          list{ViewUtils.fontAwesome("flag")},
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
            Html.class'("fluid-secondary-editor"),
            Html.styles(list{("top", string_of_int(rowOffset) ++ ".5rem")}),
          },
          list{flagIcon, FluidEditorView.view(editorProps)},
        )
      }
    )
  }

  list{mainEditor, liveValue, returnValue, debugAST, ...secondaryEditors}
}

let view = (vp: ViewUtils.viewProps, dragEvents: ViewUtils.domEventList) => list{
  Html.div(list{Html.class'("fluid-ast")}, viewAST(vp, dragEvents)),
}
