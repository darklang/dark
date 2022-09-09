module Editor = {
  @ppx.deriving(show({with_path: false}))
  type rec t =
    | NoEditor
    | MainEditor(TLID.t)
    | FeatureFlagEditor(TLID.t, ID.t)
}

module Command = {
  @ppx.deriving(show({with_path: false}))
  type rec t<'model, 'modification> = {
    commandName: string,
    action: ('model, Types.toplevel, ID.t) => 'modification,
    doc: string,
    shouldShow: ('model, Types.toplevel, ProgramTypes.Expr.t) => bool,
  }
  @ppx.deriving(show({with_path: false}))
  type rec state<'model, 'modification> = {
    index: int,
    commands: list<t<'model, 'modification>>,
    location: option<(TLID.t, ID.t)>,
    filter: option<string>,
  }
}

module Token = {
  @ppx.deriving(show) type rec analysisID = ID.t

  @ppx.deriving(show) type rec parentBlockID = ID.t

  module Placeholder = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      name: string,
      tipe: string,
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t =
    | TInteger(ID.t, int64, option<parentBlockID>)
    | TString(ID.t, string, option<parentBlockID>)
    | TStringOpenQuote(ID.t, string)
    | TStringCloseQuote(ID.t, string)
    // multi-line strings: id, segment, start offset, full-string
    | TStringML(ID.t, string, int, string)
    | TBlank(ID.t, option<parentBlockID>)
    | TPlaceholder({
        blankID: ID.t,
        fnID: ID.t,
        parentBlockID: option<parentBlockID>,
        placeholder: Placeholder.t,
      })
    | TTrue(ID.t, option<parentBlockID>)
    | TFalse(ID.t, option<parentBlockID>)
    | TNullToken(ID.t, option<parentBlockID>)
    | TFloatWhole(ID.t, string, option<parentBlockID>)
    | TFloatPoint(ID.t, option<parentBlockID>)
    | TFloatFractional(ID.t, string, option<parentBlockID>)
    /* If you're filling in an expr, but havent finished it. Not used for
     * non-expr names. */
    | TPartial(ID.t, string, option<parentBlockID>)
    // A partial that extends out to the right. Used to create binops.
    // A partial that preceeds an existing expression, used to wrap things in other things
    | TLeftPartial(ID.t, string, option<parentBlockID>)
    | TRightPartial(ID.t, string, option<parentBlockID>)
    /* When a partial used to be another thing, we want to show the name of the
     * old thing in a non-interactable way */
    | TPartialGhost(ID.t, string, option<parentBlockID>)
    // the id *here disambiguates with other separators for reflow
    | TSep(ID.t, option<parentBlockID>)
    /* The first id is the id of the expression directly associated with the
     * newline. The second id is the id of that expression's parent. In an
     * expression with potentially many newlines (ie, a pipeline), the int holds
     * the relative line number (index) of this newline. */
    | TNewline(option<(ID.t, ID.t, option<int>)>)
    | TIndent(int)
    | TLetKeyword(ID.t, analysisID, option<parentBlockID>)
    // Let-expr id * rhs id * varname
    | TLetVarName(ID.t, analysisID, string, option<parentBlockID>)
    | TLetAssignment(ID.t, analysisID, option<parentBlockID>)
    | TIfKeyword(ID.t, option<parentBlockID>)
    | TIfThenKeyword(ID.t, option<parentBlockID>)
    | TIfElseKeyword(ID.t, option<parentBlockID>)
    | TBinOp(ID.t, string, option<parentBlockID>)
    | TFieldOp(/* fieldAccess */ ID.t, /* lhs */ ID.t, option<parentBlockID>)
    | TFieldName(ID.t /* fieldAccess */, ID.t /* lhs */, string, option<parentBlockID>)
    | TFieldPartial(
        /* Partial ID, fieldAccess ID, analysisID (lhs), name */ ID.t,
        ID.t,
        ID.t,
        string,
        option<parentBlockID>,
      )
    | TVariable(ID.t, string, option<parentBlockID>)
    // id, Partial name (The TFnName display name + TFnVersion display name ex:'DB::getAllv3'), Display name (the name that should be displayed ex:'DB::getAll'), fnName (Name for backend, Includes the underscore ex:'DB::getAll_v3'), sendToRail
    | TFnName(ID.t, string, string, string, ProgramTypes.Expr.SendToRail.t)
    // id, Partial name (The TFnName display name + TFnVersion display name ex:'DB::getAllv3'), Display name (the name that should be displayed ex:'v3'), fnName (Name for backend, Includes the underscore ex:'DB::getAll_v3')
    | TFnVersion(ID.t, string, string, string)
    | TLambdaComma(ID.t, int, option<parentBlockID>)
    | TLambdaArrow(ID.t, option<parentBlockID>)
    | TLambdaSymbol(ID.t, option<parentBlockID>)
    | TLambdaVar(ID.t, analysisID, int, string, option<parentBlockID>)
    | TListOpen(ID.t, option<parentBlockID>)
    | TListClose(ID.t, option<parentBlockID>)
    | TListComma(ID.t, int)
    | TTupleOpen(ID.t)
    | TTupleClose(ID.t)
    | TTupleComma(ID.t, int)
    // 2nd int is the number of pipe segments there are
    | TPipe(ID.t, int, int, option<parentBlockID>)
    | TRecordOpen(ID.t, option<parentBlockID>)
    | TRecordFieldname({
        recordID: ID.t,
        exprID: ID.t,
        parentBlockID: option<parentBlockID>,
        index: int,
        fieldName: string,
      })
    | TRecordSep(ID.t, int, analysisID)
    | TRecordClose(ID.t, option<parentBlockID>)
    | TMatchKeyword(ID.t)
    | TMatchBranchArrow({matchID: ID.t, patternID: ID.t, index: int})
    /* for all these TPattern* variants:
     * - the first id *is the match id *
     * - the second id *is the pattern id *
     * - the final int is the index of the (pattern -> expr) */
    | TPatternVariable(ID.t, ID.t, string, int)
    | TPatternConstructorName(ID.t, ID.t, string, int)

    | TPatternInteger(ID.t, ID.t, int64, int)
    | TPatternString({matchID: ID.t, patternID: ID.t, str: string, branchIdx: int})

    | TPatternTrue(ID.t, ID.t, int)
    | TPatternFalse(ID.t, ID.t, int)

    | TPatternNullToken(ID.t, ID.t, int)

    | TPatternFloatWhole(ID.t, ID.t, string, int)
    | TPatternFloatPoint(ID.t, ID.t, int)
    | TPatternFloatFractional(ID.t, ID.t, string, int)

    | TPatternBlank(ID.t, ID.t, int)

    | TPatternTupleOpen(ID.t, ID.t)
    | TPatternTupleComma(ID.t, ID.t, int)
    | TPatternTupleClose(ID.t, ID.t)

    | TConstructorName(ID.t, string)

    | TParenOpen(ID.t)
    | TParenClose(ID.t)

    | TFlagWhenKeyword(ID.t)
    | TFlagEnabledKeyword(ID.t)
}

module TokenInfo = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    startRow: int,
    startCol: int,
    startPos: int,
    endPos: int,
    length: int,
    token: Token.t,
  }
}

module AutoComplete = {
  @ppx.deriving(show({with_path: false}))
  type rec patternItem =
    | FPAVariable(ID.t, string)
    | FPAConstructor(ID.t, string, list<ProgramTypes.Pattern.t>)
    | FPANull(ID.t)
    | FPABool(ID.t, bool)

  @ppx.deriving(show({with_path: false}))
  type rec keyword =
    | KLet
    | KIf
    | KLambda
    | KMatch
    | KPipe

  @ppx.deriving(show({with_path: false}))
  type rec literalItem =
    | LNull
    | LBool(bool)

  @ppx.deriving(show({with_path: false}))
  type rec item =
    | FACFunction(Function.t)
    | FACConstructorName(string, int)
    | FACField(string)
    | FACVariable(string, option<RuntimeTypes.Dval.t>)
    | FACLiteral(literalItem)
    | FACKeyword(keyword)
    | FACPattern(patternItem)
    | FACCreateFunction(string, TLID.t, ID.t)

  @ppx.deriving(show({with_path: false}))
  type rec validity =
    | FACItemValid
    | FACItemInvalidReturnType(Types.TypeInformation.t)
    | FACItemInvalidPipedArg(DType.t)

  @ppx.deriving(show({with_path: false}))
  type rec data = {
    item: item,
    validity: validity,
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = {
    // -------------------------------
    // state
    // -------------------------------
    index: option<int>,
    query: // We need to refer back to the previous one
    option<(TLID.t, TokenInfo.t)>,
    // -------------------------------
    // Cached results
    // -------------------------------
    completions: list<data>,
  }
  let default = {index: None, query: None, completions: list{}}
}

module Msg = {
  // https://rawgit.com/w3c/input-events/v1/index.html#interface-InputEvent-Attributes
  @ppx.deriving(show({with_path: false}))
  type rec mouseUpClickType =
    | SelectText(int, int) // selection read from the DOM
    | ClickAt(int)

  @ppx.deriving(show({with_path: false}))
  type rec mouseUp = {
    tlid: TLID.t,
    // fluidEditor is either MainEditor or a FeatureFlagEditor
    editor: Editor.t,
    selection: mouseUpClickType,
  }

  @ppx.deriving(show({with_path: false}))
  type rec mouseDoubleClickType =
    | SelectExpressionAt(int)
    /* [selectTokenAt start end]: the two ints represent the selection when the
     * double click happened, which might represent a token or a part of the
     * token (in the case of strings, a word sometimes) */
    | SelectTokenAt(int, int)

  @ppx.deriving(show({with_path: false}))
  type rec mouseDoubleClick = {
    tlid: TLID.t,
    // fluidEditor is either MainEditor or a FeatureFlagEditor
    editor: Editor.t,
    selection: mouseDoubleClickType,
  }

  @ppx.deriving(show({with_path: false}))
  type rec inputEvent =
    | Keypress(FluidKeyboard.keyEvent) // Backwards compatibility. Not an InputEvent inputType.
    | InsertText(string)
    | DeleteContentBackward
    | DeleteContentForward
    | DeleteWordBackward
    | DeleteWordForward
    | DeleteSoftLineBackward
    | DeleteSoftLineForward
    | ReplaceText(string)

  @ppx.deriving(show({with_path: false}))
  type rec t<'model, 'modification> =
    | FluidAutocompleteClick(AutoComplete.item)
    | FluidInputEvent(inputEvent)
    | FluidCut
    | FluidPaste(Types.clipboardContents)
    | FluidMouseDown(TLID.t)
    | FluidMouseUp(mouseUp)
    | /* A mouse click has happened elsewhere that might have started in fluid, so
     * let fluid know */
    FluidMouseUpExternal
    | FluidMouseDoubleClick(mouseDoubleClick)
    | FluidCommandsFilter(string)
    | FluidCommandsClick(Command.t<'model, 'modification>)
    | FluidFocusOnToken(TLID.t, ID.t)
    | FluidClearErrorDvSrc
    | FluidUpdateAutocomplete
    // Index of the dropdown(autocomplete or command palette) item
    | FluidUpdateDropdownIndex(int)
    | FluidCloseCmdPalette
}

module State = {
  @ppx.deriving(show({with_path: false}))
  type rec t<'model, 'modification> = {
    error: option<string>,
    actions: list<string>,
    oldPos: int,
    newPos: int,
    upDownCol: /* When moving up or down and going through whitespace,
     * track the column so we can go back to it */
    option<int>,
    lastInput: Msg.inputEvent,
    ac: AutoComplete.t,
    cp: Command.state<'model, 'modification>,
    selectionStart: option<int>, // The selection ends at newPos
    // If we get a renderCallback between a mousedown and a mouseUp, we
    // lose the information we're trying to get from the click.
    midClick: bool,
    // The source id of an error-dval of where the cursor is on and we might
    // have recently jumped to
    errorDvSrc: RuntimeTypes.Dval.DvalSource.t,
    activeEditor: Editor.t,
  }

  let default = {
    actions: list{},
    error: None,
    oldPos: 0,
    newPos: 0,
    upDownCol: None,
    lastInput: Keypress({
      key: FluidKeyboard.Escape,
      shiftKey: false,
      altKey: false,
      metaKey: false,
      ctrlKey: false,
    }),
    ac: AutoComplete.default,
    cp: {index: 0, commands: list{}, location: None, filter: None},
    selectionStart: None,
    errorDvSrc: SourceNone,
    midClick: false,
    activeEditor: NoEditor,
  }
}

module FluidSettings = {
  type t = {allowTuples: bool}
}

// TODO: Consider renaming to 'Environment'
module Props = {
  type rec t = {
    functions: Functions.t,
    settings: FluidSettings.t,
  }
}
