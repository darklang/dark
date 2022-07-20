open Prelude

open ProgramTypes.Expr

type props = {
  analysisStore: analysisStore,
  ast: FluidAST.t,
  functions: functionsType,
  executingFunctions: list<id>,
  editor: fluidEditor,
  hoveringRefs: list<id>,
  fluidState: fluidState,
  permission: option<AppTypes.Permission.t>,
  tlid: TLID.t,
  tokens: list<FluidToken.tokenInfo>,
}

type executionFlow =
  | CodeExecuted
  | CodeNotExecuted
  | UnknownExecution

let isActiveEditor = (p: props) => p.fluidState.activeEditor == p.editor

let propsToFnExecutionProps = (p: props): ViewFnExecution.props => {
  analysisStore: p.analysisStore,
  ast: p.ast,
  executingFunctions: p.executingFunctions,
  permission: p.permission,
  tlid: p.tlid,
}

let viewPlayIcon = (p: props, ti: FluidToken.tokenInfo): Html.html<AppTypes.msg> =>
  switch ViewUtils.fnForToken(p.functions, ti.token) {
  | Some({fnOrigin: UserFunction, _} as fn)
  | /* HACK: UserFunctions need to be executable so that the user can get a value
   * into the trace. Otherwise, when they edit the function they won't have any
   * live values. */
  Some({fnPreviewSafety: Unsafe, _} as fn) =>
    /* Looking these up can be slow, so the fnPreviewSafety check
     * above is very important.
     *
     * Note that fnPreviewSafety is calculated dynamically by
     * FluidAutocomplete. */
    let allExprs = AST.getArguments(FluidToken.tid(ti.token), p.ast)
    let argIDs = List.map(~f=FluidExpression.toID, allExprs)
    switch ti.token {
    | TFnVersion(id, _, _, _) =>
      ViewFnExecution.fnExecutionButton(propsToFnExecutionProps(p), fn, id, argIDs)
    | TFnName(id, _, displayName, fnName, _)
      if // If fn is unversioned or is v0
      displayName == fnName || displayName ++ "_v0" == fnName =>
      ViewFnExecution.fnExecutionButton(propsToFnExecutionProps(p), fn, id, argIDs)
    | _ => Vdom.noNode
    }
  | Some({fnPreviewSafety: Safe, _}) | None => Vdom.noNode
  }

let toHtml = (p: props, duplicatedRecordFields: list<(id, Set.String.t)>): list<
  Html.html<AppTypes.msg>,
> => {
  let exeFlow = ti => {
    let id = FluidToken.analysisID(ti.token)
    switch Analysis.getLiveValueLoadable(p.analysisStore, id) {
    | LoadableSuccess(ExecutedResult(_)) => CodeExecuted
    | LoadableSuccess(NonExecutedResult(_)) => CodeNotExecuted
    | _ => UnknownExecution
    }
  }

  let caretToken = FluidTokenizer.getTokenNotWhitespace(p.tokens, p.fluidState)
  let caretParentBlockID = caretToken |> Option.andThen(~f=ti =>
    // We only care to check further if caret is in a faded region.
    if exeFlow(ti) == CodeNotExecuted {
      FluidToken.parentBlockID(ti.token)
    } else {
      None
    }
  )

  // Returns true if token is in the same block as the caret or on the same row
  let isNearCaret = ti =>
    switch caretParentBlockID {
    | Some(pid) => FluidToken.parentBlockID(ti.token) == Some(pid) && exeFlow(ti) == CodeNotExecuted
    | None =>
      let isNotInBlock =
        // So we don't unfade tokens in the same row, but belong to another block
        FluidToken.parentBlockID(ti.token) == None

      let caretRow = Option.map(~f=ti => ti.startRow, caretToken)
      Some(ti.startRow) == caretRow && (isNotInBlock && exeFlow(ti) == CodeNotExecuted)
    }

  // Returns true if token is part of the expr the opened command palette will act on
  let isInCPExpr = ti =>
    switch p.fluidState.cp.location {
    | Some(_, id) if id == FluidToken.tid(ti.token) => true
    | _ => false
    }

  // Gets the source of a DIncomplete given an expr id
  let sourceOfExprValue = id =>
    if FluidToken.validID(id) {
      // Only highlight incompletes and errors on executed paths
      switch Analysis.getLiveValueLoadable(p.analysisStore, id) {
      | LoadableSuccess(ExecutedResult(DIncomplete(SourceId(tlid, id)))) => (
          Some(tlid, id),
          "dark-incomplete",
        )
      | LoadableSuccess(ExecutedResult(DError(SourceId(tlid, id), _))) => (
          Some(tlid, id),
          "dark-error",
        )
      | _ => (None, "")
      }
    } else {
      (None, "")
    }

  let currentTokenInfo = FluidTokenizer.getToken'(p.tokens, p.fluidState)
  let sourceOfCurrentToken = onTi =>
    currentTokenInfo |> Option.andThen(~f=ti =>
      if FluidToken.isBlank(ti.token) || onTi.startRow == ti.startRow {
        None
      } else {
        let (someId, _) = FluidToken.analysisID(ti.token) |> sourceOfExprValue

        someId
      }
    )

  let nesting = ref(0)
  let cmdToken = switch p.fluidState.cp.location {
  | Some(ltlid, id) if p.tlid == ltlid =>
    // Reversing list will get us the last token visually rendered with matching expression ID, so we don't have to keep track of max pos
    p.tokens |> List.reverse |> List.getBy(~f=ti => FluidToken.tid(ti.token) == id)
  | _ => None
  }

  let dropdown = ti =>
    switch cmdToken {
    | Some(onTi) if onTi == ti => FluidCommands.viewCommandPalette(p.fluidState.cp)
    | _ =>
      if Fluid.isAutocompleting(ti, p.fluidState) && isActiveEditor(p) {
        FluidAutocompleteView.view(p.fluidState.ac)
      } else {
        Vdom.noNode
      }
    }

  let isSelected = (tokenStart, tokenEnd) => {
    let (selStart, selEnd) = FluidUtil.getSelectionRange(p.fluidState)
    isActiveEditor(p) && (selStart <= tokenStart && tokenEnd <= selEnd)
  }

  let idsInAFlag = /* If we're in the main editor, find all the FF expressions, then build a
   * set of all the IDs of them and their children. This is used below to
   * apply a CSS class to highlight tokens contained in a FF. */
  switch p.editor {
  | NoEditor => ID.Set.empty
  | FeatureFlagEditor(_) => ID.Set.empty
  | MainEditor(_) =>
    FluidAST.filter(p.ast, ~f=x =>
      switch x {
      | EFeatureFlag(_) => true
      | _ => false
      }
    ) |> List.fold(~initial=ID.Set.empty, ~f=(acc, e) =>
      switch e {
      | EFeatureFlag(_, _, _, oldCode, _) =>
        Set.addMany(acc, ~values=FluidExpression.decendants(oldCode))
      | _ => acc
      }
    )
  }

  /* We want to highlight all tokens that are in the old-code of a feature
   * flag. to do this, we highlight any token with an ID in idsInAFlag, which
   * has the ID of every expression within that old code. But we also need to
   * highlight the indents, which don't have an ID.
   *
   * So, we toggle this flag on the first time we see a token within the flag
   * IDs above, then toggle it off as soon as we see a non-whitespace token
   * that's not contained in the set. */
  let withinFlag = ref(false)
  List.map(p.tokens, ~f=ti => {
    let element = nested => {
      let tokenId = FluidToken.tid(ti.token)
      let idStr = ID.toString(tokenId)
      let content = FluidToken.toText(ti.token)
      let analysisId = FluidToken.analysisID(ti.token)
      /* Toggle withinFlag if we've crossed a flag boundary.
       * See above comment where withinFlag is defined */
      if !withinFlag.contents && Set.member(idsInAFlag, ~value=tokenId) {
        withinFlag := true
      } else if (
        FluidToken.validID(tokenId) &&
        (withinFlag.contents &&
        !Set.member(idsInAFlag, ~value=tokenId))
      ) {
        withinFlag := false
      }
      // Apply CSS classes to token
      let tokenClasses = FluidToken.toCssClasses(ti.token)
      let (backingNestingClass, innerNestingClass) = {
        let (tokenBackingPrecedence, tokenInnerPrecedence) = {
          let currNesting = nesting.contents
          switch ti.token {
          | TParenOpen(_) =>
            nesting := nesting.contents + 1
            (currNesting, Some(nesting.contents))
          | TParenClose(_) =>
            nesting := nesting.contents - 1
            (nesting.contents, Some(currNesting))
          | _ => (currNesting, None)
          }
        }

        /* We want 0 precedence to only show up at the AST root and not in
         * any wraparounds, so this goes 0123412341234... */
        let wraparoundPrecedenceClass = (~ext, n) => {
          let wraparoundPrecedence = if n > 0 {
            mod(n - 1, 4) + 1
          } else {
            n
          }

          list{"precedence-" ++ (wraparoundPrecedence |> string_of_int), ...ext}
        }

        (
          tokenBackingPrecedence |> wraparoundPrecedenceClass(~ext=list{}),
          tokenInnerPrecedence |> Option.map(
            ~f=wraparoundPrecedenceClass(~ext=list{"fluid-inner"}),
          ),
        )
      }

      let cls =
        list{
          "fluid-entry",
          "id-" ++ idStr,
          ...Belt.List.concat(backingNestingClass, tokenClasses),
        } |> List.map(~f=s => (s, true))

      let isInvalidToken = ti =>
        switch ti.token {
        | TRecordFieldname({fieldName, recordID, _}) =>
          duplicatedRecordFields
          |> List.find(~f=((rID, fns)) => rID == recordID && Set.member(~value=fieldName, fns))
          |> Option.isSome
        | _ => false
        }

      let conditionalClasses = {
        let (sourceId, errorType) = sourceOfExprValue(analysisId)
        let isError =
          // Only apply to text tokens (not TSep, TNewlines, etc.)
          FluidToken.isErrorDisplayable(ti.token) &&
          (/* This expression is the source of its own incompleteness. We
            only draw underlines under sources of incompletes, not all
            propagated occurrences. */
          sourceId == Some(p.tlid, analysisId) || isInvalidToken(ti))

        let isNotExecuted = exeFlow(ti) == CodeNotExecuted
        /* Unfade non-executed code if the caret is in it,
         * so auto-complete and command-palette will render at full opacity. */
        let isInFocus = isNotExecuted && (isNearCaret(ti) || isInCPExpr(ti))
        list{
          ("related-change", List.member(~value=tokenId, p.hoveringRefs)),
          ("cursor-on", currentTokenInfo == Some(ti)),
          ("in-flag", withinFlag.contents),
          ("fluid-error", isError),
          ("fluid-not-executed", isNotExecuted),
          ("fluid-code-focus", isInFocus),
          (errorType, errorType != ""),
          /* This expression is the source of an incomplete propogated
           * into another, where the cursor is currently on */
          ("is-origin", sourceOfCurrentToken(ti) == Some(p.tlid, analysisId)),
          (
            "jumped-to",
            switch p.fluidState.errorDvSrc {
            | SourceNone => false
            | SourceId(tlid, id) => id == tokenId && p.tlid == tlid
            },
          ),
          ("selected", isSelected(ti.startPos, ti.endPos)),
        }
      }

      let innerNode = switch innerNestingClass {
      | Some(cls) => list{
          Html.span(list{Html.class'(cls |> String.join(~sep=" "))}, list{Html.text(content)}),
        }
      | None => list{Html.text(content)}
      }

      Html.span(
        list{Html.classList(Belt.List.concat(cls, conditionalClasses))},
        Belt.List.concat(innerNode, nested),
      )
    }

    if p.permission == Some(ReadWrite) {
      list{element(list{dropdown(ti), viewPlayIcon(p, ti)})}
    } else {
      list{element(list{})}
    }
  }) |> List.flatten
}

let tokensView = (p: props): Html.html<AppTypes.msg> => {
  let tlidStr = TLID.toString(p.tlid)
  let textInputListeners = /* the command palette is inside div.fluid-editor but has it's own input
   * handling, so don't do normal fluid input stuff if it's open */
  if FluidCommands.isOpened(p.fluidState.cp) {
    (Html.noProp, Html.noProp, Html.noProp)
  } else {
    (
      Html.onCB(
        "keydown",
        "keydown" ++ tlidStr,
        FluidKeyboard.onKeydown(x => FluidMsg(FluidInputEvent(Keypress(x)))),
      ),
      Html.onCB("beforeinput", "beforeinput" ++ tlidStr, FluidTextInput.fromInputEvent),
      Html.onCB(
        "compositionend",
        "compositionend" ++ tlidStr,
        FluidTextInput.fromCompositionEndEvent,
      ),
    )
  }

  let clickHandlers = list{
    ViewUtils.eventNeither(~key="fluid-selection-dbl-click" ++ tlidStr, "dblclick", ({altKey, _}) =>
      switch Entry.getFluidSelectionRange() {
      | Some(startPos, endPos) =>
        let selection = if altKey {
          SelectExpressionAt(startPos)
        } else {
          SelectTokenAt(startPos, endPos)
        }

        FluidMsg(FluidMouseDoubleClick({tlid: p.tlid, editor: p.editor, selection: selection}))
      | None => Msg.IgnoreMsg("fluid-dblclick-noselection")
      }
    ),
    ViewUtils.eventNoPropagation(
      ~key="fluid-selection-mousedown" ++ tlidStr,
      "mousedown",
      _ => FluidMsg(FluidMouseDown(p.tlid)),
    ),
    ViewUtils.eventNoPropagation(~key="fluid-selection-mouseup" ++ tlidStr, "mouseup", _ =>
      switch Entry.getFluidSelectionRange() {
      | Some(startPos, endPos) =>
        let selection = if startPos == endPos {
          ClickAt(startPos)
        } else {
          SelectText(startPos, endPos)
        }

        FluidMsg(FluidMouseUp({tlid: p.tlid, editor: p.editor, selection: selection}))
      | None =>
        // Select the handler, if not selected
        FluidMsg(FluidMouseUp({tlid: p.tlid, editor: p.editor, selection: ClickAt(0)}))
      }
    ),
    ViewUtils.onAnimationEnd(~key="anim-end" ++ tlidStr, ~listener=msg =>
      if msg == "flashError" || msg == "flashIncomplete" {
        FluidMsg(FluidClearErrorDvSrc)
      } else {
        Msg.IgnoreMsg("fluid-animation-end")
      }
    ),
  }

  let idAttr = if p.fluidState.activeEditor == p.editor {
    Html.id("active-editor")
  } else {
    Html.noProp
  }

  let duplicatedRecordFields = {
    let exprAst = FluidAST.toExpr(p.ast)
    exprAst |> FluidExpression.filterMap(~f=e =>
      switch e {
      | ERecord(id, fields) =>
        let (_, duplicates) =
          fields
          |> List.map(~f=((name, _)) => name)
          |> List.fold(~initial=(Set.String.empty, Set.String.empty), ~f=(
            (fns, duplicates),
            name,
          ) =>
            if Set.member(~value=name, fns) {
              (fns, Set.add(~value=name, duplicates))
            } else {
              (Set.add(~value=name, fns), duplicates)
            }
          )

        if duplicates |> Set.isEmpty {
          None
        } else {
          Some(id, duplicates)
        }
      | _ => None
      }
    )
  }

  Html.div(
    // disable grammarly crashes

    Belt.List.concatMany([
      list{
        idAttr,
        Html.class'("fluid-tokens"),
        Vdom.prop("contentEditable", "true"),
        Html.autofocus(true),
        Vdom.attribute("", "spellcheck", "false"),
        Vdom.attribute("", "data-gramm", "false"),
      },
      clickHandlers,
      Tuple3.toList(textInputListeners),
    ]),
    toHtml(p, duplicatedRecordFields),
  )
}

let viewErrorIndicator = (p: props, ti: FluidToken.tokenInfo): Html.html<AppTypes.msg> => {
  let returnTipe = (name: string) =>
    Functions.findByStr(name, p.functions)
    |> Option.map(~f=fn => fn.fnReturnTipe)
    |> Option.unwrap(~default=DType.TAny)

  let liveValue = (id: id) => Analysis.getLiveValue'(p.analysisStore, id)
  let isEvalSuccess = dv =>
    switch dv {
    | Some(DIncomplete(_)) | Some(DError(_)) => false
    | Some(_) => true
    | _ => false
    }

  switch ti.token {
  | TFnName(id, _, _, fnName, Rail) =>
    let offset = string_of_int(ti.startRow) ++ "rem"
    let icon = switch (returnTipe(fnName), liveValue(id)) {
    | (TResult, Some(DErrorRail(DResult(ResError(_))))) => ViewUtils.darkIcon("result-error")
    | (TResult, v) if isEvalSuccess(v) => ViewUtils.darkIcon("result-ok")
    | (TOption, Some(DErrorRail(DOption(OptNothing)))) => ViewUtils.darkIcon("option-nothing")
    | (TOption, v) if isEvalSuccess(v) => ViewUtils.darkIcon("option-just")
    | _ => Vdom.noNode
    }

    let event = Vdom.noProp
    /* TEMPORARY DISABLE
          ViewUtils.eventNoPropagation
            ~key:("er-" ^ show_id id)
            "click"
            (fun _ -> TakeOffErrorRail (tlid, id)) */

    Html.div(
      list{Html.class'("error-indicator"), Html.styles(list{("top", offset)}), event},
      list{icon},
    )
  | _ => Vdom.noNode
  }
}

let errorRailView = (p: props): Html.html<AppTypes.msg> => {
  let indicators = List.map(p.tokens, ~f=viewErrorIndicator(p))
  let hasMaybeErrors = List.any(~f=e => e != Vdom.noNode, indicators)
  Html.div(
    list{Html.classList(list{("fluid-error-rail", true), ("show", hasMaybeErrors)})},
    indicators,
  )
}

@ocaml.doc(" [view] builds a fluid editor ")
let view = (p: props): Html.html<AppTypes.msg> =>
  Html.div(list{Html.class'("fluid-editor")}, list{tokensView(p), errorRailView(p)})
