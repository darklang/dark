/* Specs:
 * Pressing enter
 * https://trello.com/c/nyf3SyA4/1513-handle-pressing-enter-at-the-end-of-a-line
 * https://trello.com/c/27jn8uzF/1507-handle-pressing-enter-at-the-start-of-a-line
 * renaming functions:
 * https://trello.com/c/49TTRcad/973-renaming-functions
 * movement shortcuts:
 * https://trello.com/c/IK9fQZoW/1072-support-ctrl-a-ctrl-e-ctrl-d-ctrl-k
 */

open Prelude
module K = FluidKeyboard
module Mouse = Tea.Mouse
module TL = Toplevel
module Regex = Util.Regex
module DUtil = Util
module AC = FluidAutocomplete
module Commands = FluidCommands
module T = FluidToken
module E = FluidExpression
module P = FluidPattern
module Printer = FluidPrinter
module Tokenizer = FluidTokenizer
module Util = FluidUtil
module Clipboard = FluidClipboard
module CT = CaretTarget
module ASTInfo = Tokenizer.ASTInfo
module Sign = ProgramTypes.Sign

open ProgramTypes.Expr
open ProgramTypes.Pattern

// TUPLETODO re-enable
let allowUserToCreateTuple = false

// --------------------
// Utils
// --------------------
type token = T.t

type tokenInfos = list<T.tokenInfo>

type state = Types.fluidState

type props = Types.fluidProps

let deselectFluidEditor = (s: fluidState): fluidState => {
  ...s,
  oldPos: 0,
  newPos: 0,
  upDownCol: None,
  activeEditor: NoEditor,
}

// --------------------
// Nearby tokens
// --------------------

let astInfoFromModelAndTLID = (~removeTransientState=true, m: model, tlid: TLID.t): option<
  ASTInfo.t,
> => {
  /* TODO: codify removeHandlerTransientState as an external function,
   * make `fromExpr` accept only the info it needs, and differentiate between
   * handler-specific and global fluid state. */
  let removeHandlerTransientState = m =>
    if removeTransientState {
      {...m, fluidState: {...m.fluidState, ac: AC.init}}
    } else {
      m
    }

  TL.get(m, tlid)
  |> Option.andThen(~f=TL.getAST)
  |> Option.map(~f=ast => {
    let state = // We need to discard transient state if the selected handler has changed
    if Some(tlid) == CursorState.tlidOf(m.cursorState) {
      m.fluidState
    } else {
      let newM = removeHandlerTransientState(m)
      newM.fluidState
    }

    ASTInfo.make(FluidUtil.propsFromModel(m), ast, state)
  })
}

let astInfoFromModel = (m: model): option<ASTInfo.t> =>
  CursorState.tlidOf(m.cursorState) |> Option.andThen(~f=astInfoFromModelAndTLID(m))

let getStringIndexMaybe = (ti: T.tokenInfo, pos: int): option<int> =>
  switch ti.token {
  | TString(_, _, _) => Some(pos - ti.startPos - 1)
  | TStringMLStart(_, _, offset, _) => Some(pos - ti.startPos + offset - 1)
  | TStringMLMiddle(_, _, offset, _) | TStringMLEnd(_, _, offset, _) =>
    Some(pos - ti.startPos + offset)
  | _ => None
  }

let getStringIndex = (ti, pos): int =>
  switch getStringIndexMaybe(ti, pos) {
  | Some(i) => i
  | None => recover("getting index of non-string", ~debug=(ti.token, pos), 0)
  }

// --------------------
// Update fluid state
// --------------------
let recordAction' = (~pos=-1000, action: string, s: state): state => {
  let action = if pos == -1000 {
    action
  } else {
    action ++ (" " ++ string_of_int(pos))
  }

  {...s, actions: Belt.List.concat(s.actions, list{action})}
}

/* Returns a new state with the arbitrary string "action" recorded for debugging.
 * If a ~pos or ~ti (token info) is passed, it will be added to the action. */
let recordAction = (~pos=-1000, action: string, astInfo: ASTInfo.t): ASTInfo.t => {
  let action = if pos == -1000 {
    action
  } else {
    action ++ (" " ++ string_of_int(pos))
  }

  ASTInfo.modifyState(~f=recordAction'(~pos, action), astInfo)
}

let setPosition = (~resetUD=false, pos: int, astInfo: ASTInfo.t): ASTInfo.t =>
  astInfo
  |> recordAction(~pos, "setPosition")
  |> ASTInfo.modifyState(~f=s => {
    ...s,
    newPos: pos,
    selectionStart: None,
    upDownCol: if resetUD {
      None
    } else {
      s.upDownCol
    },
  })

let report = (e: string, astInfo: ASTInfo.t) =>
  astInfo |> recordAction("report") |> ASTInfo.modifyState(~f=s => {...s, error: Some(e)})

// --------------------
// Update
// --------------------

let length = (tokens: list<token>): int =>
  (tokens |> List.map(~f=T.toText) |> List.map(~f=String.length))->List.sum(module(Int))

// Returns the token to the left and to the right. Ignores indent tokens

let getLeftTokenAt = (newPos: int, tis: tokenInfos): option<T.tokenInfo> =>
  List.find(~f=ti => newPos <= ti.endPos && newPos >= ti.startPos, tis)

type gridPos = {
  row: int,
  col: int,
}

// Result will definitely be a valid position.
let gridFor = (~pos: int, tokens: tokenInfos): gridPos => {
  let ti = List.find(tokens, ~f=ti => ti.startPos <= pos && ti.endPos >= pos)

  switch ti {
  | Some(ti) =>
    if T.isNewline(ti.token) {
      {row: ti.startRow + 1, col: 0}
    } else {
      {row: ti.startRow, col: ti.startCol + (pos - ti.startPos)}
    }
  | None => {row: 0, col: 0}
  }
}

// Result will definitely be a valid position.
let posFor = (~row: int, ~col: int, tokens: tokenInfos): int =>
  if row < 0 || col < 0 {
    0
  } else {
    let ti = List.find(tokens, ~f=ti =>
      ti.startRow == row && (ti.startCol <= col && ti.startCol + ti.length >= col)
    )

    switch ti {
    | Some(ti) => ti.startPos + (col - ti.startCol)
    | None =>
      switch List.last(tokens) {
      | None => 0
      | Some(last) => last.endPos
      }
    }
  }

/* Ensures that the position places will be valid within the code. When placed
 * in an indent, will be moved to the next char. Empty lines get moved to the
 * start. When placed past the end of a line, will go on the end of the last
 * item in it. */
let adjustedPosFor = (~row: int, ~col: int, tokens: tokenInfos): int =>
  if row < 0 || col < 0 {
    0
  } else {
    let thisRow = List.filter(tokens, ~f=ti => ti.startRow == row)
    let ti = List.find(thisRow, ~f=ti => ti.startCol <= col && ti.startCol + ti.length > col)

    switch ti {
    | Some(ti) => ti.startPos + (col - ti.startCol)
    | None =>
      switch (List.head(thisRow), List.last(thisRow)) {
      | (Some(h), _) if col < h.startCol => h.startPos
      | (_, Some(l)) if col >= l.startCol =>
        switch l.token {
        | TNewline(_) => l.startPos
        | _ => l.startPos + l.length
        }
      | (None, None) => posFor(~row, ~col=0, tokens)
      | (_, _) => recover("unexpected adjustedPosFor", ~debug=(row, col), 0)
      }
    }
  }

// ----------------
// Movement
// ----------------

let moveToNextNonWhitespaceToken = (pos: int, astInfo: ASTInfo.t): ASTInfo.t => {
  let rec getNextWS = (tokenInfos: tokenInfos) =>
    switch tokenInfos {
    | list{} => pos
    | list{ti, ...rest} =>
      if ti.token |> T.isWhitespace {
        getNextWS(rest)
      } else if pos > ti.startPos {
        getNextWS(rest)
      } else {
        ti.startPos
      }
    }

  let newPos = getNextWS(ASTInfo.activeTokenInfos(astInfo))
  astInfo
  |> recordAction(~pos, "moveToNextNonWhitespaceToken")
  |> setPosition(~resetUD=true, newPos)
}

/* getStartOfLineCaretPos returns the first desirable (excluding indents, pipes, and newline tokens)
 caret pos at the start of the line containing the given T.tokenInfo */
let getStartOfLineCaretPos = (ti: T.tokenInfo, astInfo: ASTInfo.t): int => {
  let token =
    astInfo
    |> ASTInfo.activeTokenInfos
    |> List.find(~f=info =>
      if info.startRow == ti.startRow {
        switch info.token {
        // To prevent the result pos from being set inside TPipe or TIndent tokens
        | TPipe(_) | TIndent(_) => false
        | _ => true
        }
      } else {
        false
      }
    )
    |> Option.unwrap(~default=ti)

  token.startPos
}

/* getBegOfWordInStrCaretPos returns the closest whitespace position before the
 * current caret position in a string */
let getBegOfWordInStrCaretPos = (~pos: int, ti: T.tokenInfo): int => {
  let posInString = pos - ti.startPos
  let nextPos: ref<int> = ref(ti.length)
  let _ =
    T.toText(ti.token)
    |> String.split(~on="")
    |> List.reverse
    |> List.find(~f=a =>
      if (a === " " || (a == "\"" || (a == "\n" || a == "\t"))) && nextPos.contents < posInString {
        true
      } else {
        nextPos := nextPos.contents - 1
        false
      }
    )

  ti.startPos + nextPos.contents
}

/* getEndOfWordInStrCaretPos returns the closest whitespace position after the
 * current caret position in a string */
let getEndOfWordInStrCaretPos = (~pos: int, ti: T.tokenInfo): int => {
  let posInString = pos - ti.startPos
  let nextPos: ref<int> = ref(0)
  let _ =
    T.toText(ti.token)
    |> String.split(~on="")
    |> List.find(~f=a =>
      if (
        (a === " " || ((a == "\"" && nextPos.contents > 0) || (a == "\n" || a == "\t"))) &&
          nextPos.contents > posInString
      ) {
        true
      } else {
        nextPos := nextPos.contents + 1
        false
      }
    )

  ti.startPos + nextPos.contents
}

/* getEndOfLineCaretPos returns the last desirable (excluding indents and newline tokens)
 caret pos at the end of the line containing the given tokenInfo */
let getEndOfLineCaretPos = (ti: T.tokenInfo, astInfo: ASTInfo.t): int => {
  let token =
    astInfo
    |> ASTInfo.activeTokenInfos
    |> List.reverse
    |> List.find(~f=info => info.startRow == ti.startRow)
    |> Option.unwrap(~default=ti)

  let pos = switch token.token {
  // To prevent the result pos from being set to the end of an indent or to a new line
  | TNewline(_) | TIndent(_) => token.startPos
  | _ => token.endPos
  }

  pos
}

/* moveToStartOfLine moves the caret to the first desirable (excluding indents, pipes, and newline tokens)
 caret pos at the start of the line containing the given tokenInfo */
let moveToStartOfLine = (ti: T.tokenInfo, astInfo: ASTInfo.t): ASTInfo.t =>
  astInfo |> recordAction("moveToStartOfLine") |> setPosition(getStartOfLineCaretPos(ti, astInfo))

/* moveToEndOfLine moves the caret to the last desirable (excluding indents and newline tokens)
 caret pos at the end of the line containing the given tokenInfo */
let moveToEndOfLine = (ti: T.tokenInfo, astInfo: ASTInfo.t): ASTInfo.t =>
  astInfo |> recordAction("moveToEndOfLine") |> setPosition(getEndOfLineCaretPos(ti, astInfo))

/* We want to find the closest editable token that is before the current cursor position
 * so the cursor always lands in a position where a user is able to type */
let getStartOfWordPos = (pos: int, ti: T.tokenInfo, astInfo: ASTInfo.t): int => {
  let previousToken =
    astInfo
    |> ASTInfo.activeTokenInfos
    |> List.reverse
    |> List.find(~f=(t: T.tokenInfo) => T.isTextToken(t.token) && pos > t.startPos)

  let tokenInfo = previousToken |> Option.unwrap(~default=ti)
  if T.isStringToken(tokenInfo.token) && pos !== tokenInfo.startPos {
    getBegOfWordInStrCaretPos(~pos, tokenInfo)
  } else {
    tokenInfo.startPos
  }
}

let goToStartOfWord = (pos: int, ti: T.tokenInfo, astInfo: ASTInfo.t): ASTInfo.t =>
  astInfo |> recordAction("goToStartOfWord") |> setPosition(getStartOfWordPos(pos, ti, astInfo))

/* We want to find the closest editable token that is after the current cursor
 * position so the cursor always lands in a position where a user is able to
 * type */
let getEndOfWordPos = (pos: int, ti: T.tokenInfo, astInfo: ASTInfo.t): int => {
  let tokenInfo =
    astInfo
    |> ASTInfo.activeTokenInfos
    |> List.find(~f=(t: T.tokenInfo) => T.isTextToken(t.token) && pos < t.endPos)
    |> Option.unwrap(~default=ti)

  if T.isStringToken(tokenInfo.token) && pos !== tokenInfo.endPos {
    getEndOfWordInStrCaretPos(~pos, tokenInfo)
  } else {
    tokenInfo.endPos
  }
}

let goToEndOfWord = (pos: int, ti: T.tokenInfo, astInfo: ASTInfo.t): ASTInfo.t =>
  astInfo |> recordAction("goToEndOfWord") |> setPosition(getEndOfWordPos(pos, ti, astInfo))

let moveToEnd = (ti: T.tokenInfo, astInfo: ASTInfo.t): ASTInfo.t =>
  astInfo |> recordAction(~pos=ti.endPos, "moveToEnd") |> setPosition(~resetUD=true, ti.endPos - 1)

let moveToStart = (ti: T.tokenInfo, astInfo: ASTInfo.t): ASTInfo.t =>
  astInfo
  |> recordAction(~pos=ti.startPos, "moveToStart")
  |> setPosition(~resetUD=true, ti.startPos)

let moveToAfter = (ti: T.tokenInfo, astInfo: ASTInfo.t): ASTInfo.t =>
  astInfo |> recordAction(~pos=ti.endPos, "moveToAfter") |> setPosition(~resetUD=true, ti.endPos)

let moveOneLeft = (pos: int, astInfo: ASTInfo.t): ASTInfo.t =>
  astInfo |> recordAction(~pos, "moveOneLeft") |> setPosition(~resetUD=true, max(0, pos - 1))

let moveOneRight = (pos: int, astInfo: ASTInfo.t): ASTInfo.t =>
  astInfo |> recordAction(~pos, "moveOneRight") |> setPosition(~resetUD=true, pos + 1)

let moveTo = (newPos: int, astInfo: ASTInfo.t): ASTInfo.t =>
  astInfo |> recordAction(~pos=newPos, "moveTo") |> setPosition(newPos)

/* Find first `target` expression (starting at the back), and return a state
 * with its location. If blank, will go to the start of the blank */
let moveToEndOfNonWhitespaceTarget = (target: id, astInfo: ASTInfo.t): ASTInfo.t => {
  let astInfo = recordAction("moveToEndOfNonWhitespaceTarget", astInfo)
  switch astInfo
  |> ASTInfo.activeTokenInfos
  |> List.reverse
  |> List.find(~f=ti => T.tid(ti.token) == target && !T.isWhitespace(ti.token)) {
  | None => recover("cannot find token to moveToEndOfNonWhitespaceTarget", ~debug=target, astInfo)
  | Some(lastToken) =>
    let newPos = if T.isBlank(lastToken.token) {
      lastToken.startPos
    } else {
      lastToken.endPos
    }

    moveTo(newPos, astInfo)
  }
}

let rec getNextBlank = (pos: int, astInfo: ASTInfo.t): option<T.tokenInfo> =>
  astInfo
  |> ASTInfo.activeTokenInfos
  |> List.find(~f=ti => T.isBlank(ti.token) && ti.startPos > pos)
  |> Option.orElseLazy(() =>
    if pos == 0 {
      None
    } else {
      getNextBlank(0, astInfo)
    }
  )

let getNextBlankPos = (pos: int, astInfo: ASTInfo.t): int =>
  astInfo |> getNextBlank(pos) |> Option.map(~f=ti => ti.startPos) |> Option.unwrap(~default=pos)

let moveToNextBlank = (pos: int, astInfo: ASTInfo.t): ASTInfo.t => {
  let pos = getNextBlankPos(pos, astInfo)
  astInfo |> recordAction(~pos, "moveToNextBlank") |> setPosition(~resetUD=true, pos)
}

@ocaml.doc(" [getNextEditable pos tokens] returns the first editable token after [pos] in
 * [tokens], wrapped in an option. If no editable token exists, wrap around, returning the first editable token in
 * [tokens], or None if no editable token exists. ")
let rec getNextEditable = (pos: int, astInfo: ASTInfo.t): option<T.tokenInfo> =>
  astInfo
  |> ASTInfo.activeTokenInfos
  |> List.find(~f=ti => {
    let isEditable = // Skip the editable tokens that are part of a combination of tokens to place the caret in the right place in the token combination
    switch ti.token {
    | TStringMLEnd(_) | TStringMLMiddle(_) | TFnVersion(_) => false
    | _ => T.isTextToken(ti.token)
    }

    isEditable && ti.startPos > pos
  })
  |> Option.orElseLazy(() =>
    if pos == 0 {
      None
    } else {
      getNextEditable(-1, astInfo)
    }
  )

let getNextEditablePos = (pos: int, astInfo: ASTInfo.t): int =>
  astInfo |> getNextEditable(pos) |> Option.map(~f=ti => ti.startPos) |> Option.unwrap(~default=pos)

let moveToNextEditable = (pos: int, astInfo: ASTInfo.t): ASTInfo.t => {
  let pos = getNextEditablePos(pos, astInfo)
  astInfo |> recordAction(~pos, "moveToNextEditable") |> setPosition(~resetUD=true, pos)
}

@ocaml.doc(" [getPrevEditable pos tokens] returns the closest editable token before [pos] in
* [tokens], wrapped in an option. If no such token exists, wrap around, returning the last editable token in
* [tokens], or None if no editable exists. ")
let getPrevEditable = (pos: int, astInfo: ASTInfo.t): option<T.tokenInfo> => {
  let revTokens = astInfo |> ASTInfo.activeTokenInfos |> List.reverse
  let rec findEditable = (pos: int): option<T.tokenInfo> =>
    revTokens
    |> List.find(~f=ti => {
      let isEditable = // Skip the editable tokens that are part of a combination of tokens to place the caret in the right place in the token combination
      switch ti.token {
      | TStringMLStart(_) | TStringMLMiddle(_) | TFnName(_) => false
      | _ => T.isTextToken(ti.token)
      }

      isEditable && ti.endPos < pos
    })
    |> Option.orElseLazy(() => {
      let lastPos = switch List.head(revTokens) {
      | Some(tok) => tok.endPos
      | None => 0
      }

      if pos == lastPos {
        None
      } else {
        findEditable(lastPos + 1)
      }
    })

  findEditable(pos)
}

let getPrevEditablePos = (pos: int, astInfo: ASTInfo.t): int =>
  astInfo
  |> getPrevEditable(pos)
  |> Option.map(~f=ti =>
    if T.isBlank(ti.token) {
      ti.startPos
    } else {
      ti.endPos
    }
  )
  |> Option.unwrap(~default=pos)

let moveToPrevEditable = (pos: int, astInfo: ASTInfo.t): ASTInfo.t => {
  let pos = getPrevEditablePos(pos, astInfo)
  astInfo |> recordAction(~pos, "moveToPrevEditable") |> setPosition(~resetUD=true, pos)
}

let rec getPrevBlank = (pos: int, astInfo: ASTInfo.t): option<T.tokenInfo> =>
  astInfo
  |> ASTInfo.activeTokenInfos
  |> List.filter(~f=ti => T.isBlank(ti.token) && ti.endPos <= pos)
  |> List.last
  |> Option.orElseLazy(() => {
    let lastPos =
      astInfo
      |> ASTInfo.activeTokenInfos
      |> List.last
      |> Option.map(~f=ti => ti.endPos)
      |> Option.unwrap(~default=0)

    if pos == lastPos {
      None
    } else {
      getPrevBlank(lastPos, astInfo)
    }
  })

let getPrevBlankPos = (pos: int, astInfo: ASTInfo.t): int =>
  astInfo |> getPrevBlank(pos) |> Option.map(~f=ti => ti.startPos) |> Option.unwrap(~default=pos)

let moveToPrevBlank = (pos: int, astInfo: ASTInfo.t): ASTInfo.t => {
  let pos = getPrevBlankPos(pos, astInfo)
  astInfo |> recordAction(~pos, "moveToPrevBlank") |> setPosition(~resetUD=true, pos)
}

let doLeft = (~pos: int, ti: T.tokenInfo, astInfo: ASTInfo.t): ASTInfo.t => {
  let astInfo = recordAction(~pos, "doLeft", astInfo)
  if T.isAtom(ti.token) {
    moveToStart(ti, astInfo)
  } else {
    moveOneLeft(min(pos, ti.endPos), astInfo)
  }
}

let selectAll = (~pos: int, astInfo: ASTInfo.t): ASTInfo.t => {
  let lastPos =
    astInfo
    |> ASTInfo.activeTokenInfos
    |> List.last
    |> (
      x =>
        switch x {
        | Some(l) => l.endPos
        | None => 0
        }
    )

  astInfo |> ASTInfo.modifyState(~f=s => {
    ...s,
    newPos: lastPos,
    oldPos: pos,
    selectionStart: Some(0),
  })
}

let doRight = (
  ~pos: int,
  ~next: option<T.tokenInfo>,
  ti: T.tokenInfo,
  astInfo: ASTInfo.t,
): ASTInfo.t => {
  let astInfo = recordAction(~pos, "doRight", astInfo)
  if T.isAtom(ti.token) {
    switch next {
    | None => moveToAfter(ti, astInfo)
    | Some(nInfo) => moveToStart(nInfo, astInfo)
    }
  } else {
    switch next {
    | Some(n) if pos + 1 >= ti.endPos => moveToStart(n, astInfo)
    | _ =>
      /* When we're in whitespace, current is the next non-whitespace. So we
       * don't want to use pos, we want to use the startPos of current. */
      let startingPos = max(pos, ti.startPos - 1)
      moveOneRight(startingPos, astInfo)
    }
  }
}

let doUp = (~pos: int, astInfo: ASTInfo.t): ASTInfo.t => {
  let astInfo = recordAction(~pos, "doUp", astInfo)
  let tokenInfos = ASTInfo.activeTokenInfos(astInfo)
  let {row, col} = gridFor(~pos, tokenInfos)
  let col = switch astInfo.state.upDownCol {
  | None => col
  | Some(savedCol) => savedCol
  }

  if row == 0 {
    moveTo(0, astInfo)
  } else {
    let pos = adjustedPosFor(~row=row - 1, ~col, tokenInfos)
    astInfo |> ASTInfo.modifyState(~f=s => {...s, upDownCol: Some(col)}) |> moveTo(pos)
  }
}

let doDown = (~pos: int, astInfo: ASTInfo.t): ASTInfo.t => {
  let astInfo = recordAction(~pos, "doDown", astInfo)
  let tokenInfos = ASTInfo.activeTokenInfos(astInfo)
  let {row, col} = gridFor(~pos, tokenInfos)
  let col = switch astInfo.state.upDownCol {
  | None => col
  | Some(savedCol) => savedCol
  }

  let pos = adjustedPosFor(~row=row + 1, ~col, tokenInfos)
  astInfo |> ASTInfo.modifyState(~f=s => {...s, upDownCol: Some(col)}) |> moveTo(pos)
}

// ****************************
// Movement with CaretTarget
// ****************************

/* posFromCaretTarget returns the position in the token stream corresponding to
   the passed caretTarget within the passed ast. We expect to succeed in finding
   the target. If we cannot, we `recover` and return the current caret pos
   as a fallback.

   This is useful for determining the precise position to which the caret should
   jump after a transformation. */
let posFromCaretTarget = (ct: caretTarget, astInfo: ASTInfo.t): int => {
  /* Essentially we're using List.findMap to map a function that
   * matches across astref,token combinations (exhaustively matching astref but not token)
   * to determine the corresponding caretPos.
   *
   * This is purposefully verbose, as we want to ensure we have an exhaustive
   * match. Please do not use '_' in any of the astRef match conditions or
   * refactor in any way that removes the exhaustive matching of the astRefs.
   *
   * NB: These were somewhat hastily added and are very possibly incorrect.
   * Please fix.
   */

  /* This is the most common way of going from a single token info to a caretPos.
    It only really makes sense for situations where there is a 1:1 correspondence
    between an ASTRef and a token. It does not make sense for ASTRefs that scan across
    multiple tokens (eg Multiline Strings) */
  let posForTi = (ti): option<int> => Some(ti.startPos + min(ct.offset, ti.length))
  let clampedPosForTi = (ti, pos): option<int> => Some(ti.startPos + max(0, min(pos, ti.length)))

  /* takes a caretTarget and tokenInfo and produces the corresponding
   * token-stream-global caretPos within the token stream, or None if the
   * passed token isn't one we care about. The function will be used below
     as part of a List.findMap */
  let targetAndTokenInfoToMaybeCaretPos = ((ct, ti): (caretTarget, T.tokenInfo)): option<int> =>
    switch (ct.astRef, ti.token) {
    | (ARBinOp(id), TBinOp(id', _, _))
    | (ARBlank(id), TBlank(id', _) | TPlaceholder({blankID: id', _}))
    | (ARBool(id), TTrue(id', _) | TFalse(id', _))
    | (ARConstructor(id), TConstructorName(id', _))
    | (ARFieldAccess(id, FAPFieldname), TFieldName(id', _, _, _))
    | (ARFieldAccess(id, FAPFieldOp), TFieldOp(id', _, _))
    | (ARIf(id, IPIfKeyword), TIfKeyword(id', _))
    | (ARIf(id, IPThenKeyword), TIfThenKeyword(id', _))
    | (ARIf(id, IPElseKeyword), TIfElseKeyword(id', _))
    | (ARInteger(id), TInteger(id', _, _))
    | (ARLet(id, LPKeyword), TLetKeyword(id', _, _))
    | (ARLet(id, LPVarName), TLetVarName(id', _, _, _))
    | (ARLet(id, LPAssignment), TLetAssignment(id', _, _))
    | (ARList(id, LPOpen), TListOpen(id', _))
    | (ARList(id, LPClose), TListClose(id', _))
    | (ARTuple(id, TPOpen), TTupleOpen(id'))
    | (ARTuple(id, TPClose), TTupleClose(id'))
    | (ARMatch(id, MPKeyword), TMatchKeyword(id'))
    | (ARNull(id), TNullToken(id', _))
    | (ARPartial(id), TPartial(id', _, _))
    | (ARPartial(id), TFieldPartial(id', _, _, _, _))
    | (ARRightPartial(id), TRightPartial(id', _, _))
    | (ARLeftPartial(id), TLeftPartial(id', _, _))
    | (ARRecord(id, RPOpen), TRecordOpen(id', _))
    | (ARRecord(id, RPClose), TRecordClose(id', _))
    | (ARVariable(id), TVariable(id', _, _))
    | (ARLambda(id, LBPSymbol), TLambdaSymbol(id', _))
    | (ARLambda(id, LBPArrow), TLambdaArrow(id', _))
    | (ARPattern(id, PPVariable), TPatternVariable(_, id', _, _))
    | (ARPattern(id, PPConstructor), TPatternConstructorName(_, id', _, _))
    | (ARPattern(id, PPInteger), TPatternInteger(_, id', _, _))
    | (ARPattern(id, PPBool), TPatternTrue(_, id', _) | TPatternFalse(_, id', _))
    | (ARPattern(id, PPBlank), TPatternBlank(_, id', _))
    | (ARPattern(id, PPNull), TPatternNullToken(_, id', _))
    | (ARFlag(id, FPWhenKeyword), TFlagWhenKeyword(id'))
    | (ARFlag(id, FPEnabledKeyword), TFlagEnabledKeyword(id')) if id == id' =>
      posForTi(ti)
    | (ARList(id, LPComma(idx)), TListComma(id', idx'))
    | (ARTuple(id, TPComma(idx)), TTupleComma(id', idx'))
    | (ARMatch(id, MPBranchArrow(idx)), TMatchBranchArrow({matchID: id', index: idx', _}))
    | (ARPipe(id, idx), TPipe(id', idx', _, _))
    | (ARRecord(id, RPFieldname(idx)), TRecordFieldname({recordID: id', index: idx', _}))
    | (ARRecord(id, RPFieldSep(idx)), TRecordSep(id', idx', _))
    | (ARLambda(id, LBPVarName(idx)), TLambdaVar(id', _, idx', _, _))
    | (ARLambda(id, LBPComma(idx)), TLambdaComma(id', idx', _)) if id == id' && idx == idx' =>
      posForTi(ti)
    /*
     * Floats
     */
    | (ARPattern(id, PPFloat(FPPoint)), TPatternFloatPoint(_, id', _))
    | (ARPattern(id, PPFloat(FPWhole)), TPatternFloatWhole(_, id', _, _))
    | (ARFloat(id, FPPoint), TFloatPoint(id', _))
    | (ARFloat(id, FPWhole), TFloatWhole(id', _, _)) if id == id' =>
      posForTi(ti)
    | (ARPattern(id, PPFloat(FPWhole)), TPatternFloatPoint(_, id', _))
    | (ARFloat(id, FPWhole), TFloatPoint(id', _)) if id == id' =>
      /* This accounts for situations like `|.45`, where the float doesn't have a whole part but
           we're still targeting it (perhaps due to deletion).
           Because the 'findMap' below scans from left to right and we try to match the whole first,
           we can still find positions like `1|2.54` */
      Some(ti.startPos)
    | (ARPattern(id, PPFloat(FPFractional)), TPatternFloatPoint(_, id', _))
    | (ARFloat(id, FPFractional), TFloatPoint(id', _)) if id == id' && ct.offset == 0 =>
      /* This accounts for situations like `12.|`, where the float doesn't have a decimal part but
       we're still targeting it (perhaps due to deletion). */
      Some(ti.endPos)
    | (ARPattern(id, PPFloat(FPFractional)), TPatternFloatFractional(_, id', _, _))
    | (ARFloat(id, FPFractional), TFloatFractional(id', _, _)) if id == id' =>
      posForTi(ti)
    /*
     * Function calls
     */
    | (ARFnCall(id), TFnName(id', partialName, displayName, _, _)) if id == id' =>
      let dispLen = String.length(displayName)
      if ct.offset > dispLen && String.length(partialName) > dispLen {
        // A version token exists and we must be there instead
        None
      } else {
        // Within current token
        clampedPosForTi(ti, ct.offset)
      }
    | (ARFnCall(id), TFnVersion(id', _, _, backendFnName)) if id == id' =>
      let nameWithoutVersion = FluidUtil.fnDisplayName(backendFnName)
      clampedPosForTi(ti, ct.offset - String.length(nameWithoutVersion))
    /*
     * Single-line Strings
     */
    | (ARString(id, SPOpenQuote), TString(id', _, _))
    | (ARPattern(id, PPString(SPOpenQuote)), TPatternString({patternID: id', _})) if id == id' =>
      clampedPosForTi(ti, ct.offset)
    /*
     * Multi-line Strings
     */
    | (ARString(id, SPOpenQuote), tok) =>
      switch tok {
      | TStringMLStart(id', str, _, _) if id == id' =>
        let len = String.length(str) + 1 // to account for open quote
        if ct.offset > len {
          // Must be in a later token
          None
        } else {
          // Within current token
          posForTi(ti)
        }
      | TStringMLMiddle(id', str, startOffsetIntoString, _) if id == id' =>
        let len = String.length(str)
        let offsetInStr = ct.offset - 1
        // to account for open quote in the start

        let endOffset = startOffsetIntoString + len
        if offsetInStr > endOffset {
          // Must be in later token
          None
        } else {
          // Within current token
          clampedPosForTi(ti, offsetInStr - startOffsetIntoString)
        }
      | TStringMLEnd(id', _, startOffsetIntoString, _) if id == id' =>
        // Must be in this token because it's the last token in the string
        let offsetInStr = ct.offset - 1
        // to account for open quote in the start

        clampedPosForTi(ti, offsetInStr - startOffsetIntoString)
      | _ => None
      }
    // Exhaustiveness satisfaction for astRefs
    | (ARBinOp(_), _)
    | (ARBlank(_), _)
    | (ARBool(_), _)
    | (ARConstructor(_), _)
    | (ARFieldAccess(_, FAPFieldname), _)
    | (ARFieldAccess(_, FAPFieldOp), _)
    | (ARFloat(_, FPWhole), _)
    | (ARFloat(_, FPPoint), _)
    | (ARFloat(_, FPFractional), _)
    | (ARFnCall(_), _)
    | (ARIf(_, IPIfKeyword), _)
    | (ARIf(_, IPThenKeyword), _)
    | (ARIf(_, IPElseKeyword), _)
    | (ARInteger(_), _)
    | (ARLet(_, LPKeyword), _)
    | (ARLet(_, LPVarName), _)
    | (ARLet(_, LPAssignment), _)
    | (ARList(_, LPOpen), _)
    | (ARList(_, LPClose), _)
    | (ARList(_, LPComma(_)), _)
    | (ARTuple(_, TPOpen), _)
    | (ARTuple(_, TPClose), _)
    | (ARTuple(_, TPComma(_)), _)
    | (ARMatch(_, MPKeyword), _)
    | (ARMatch(_, MPBranchArrow(_)), _)
    | (ARNull(_), _)
    | (ARPartial(_), _)
    | (ARRightPartial(_), _)
    | (ARLeftPartial(_), _)
    | (ARPipe(_, _), _)
    | (ARRecord(_, RPOpen), _)
    | (ARRecord(_, RPClose), _)
    | (ARRecord(_, RPFieldname(_)), _)
    | (ARRecord(_, RPFieldSep(_)), _)
    | (ARVariable(_), _)
    | (ARLambda(_, LBPSymbol), _)
    | (ARLambda(_, LBPArrow), _)
    | (ARLambda(_, LBPVarName(_)), _)
    | (ARLambda(_, LBPComma(_)), _)
    | (ARPattern(_, PPVariable), _)
    | (ARPattern(_, PPConstructor), _)
    | (ARPattern(_, PPInteger), _)
    | (ARPattern(_, PPBool), _)
    | (ARPattern(_, PPFloat(FPPoint)), _)
    | (ARPattern(_, PPFloat(FPWhole)), _)
    | (ARPattern(_, PPFloat(FPFractional)), _)
    | (ARPattern(_, PPBlank), _)
    | (ARPattern(_, PPNull), _)
    | (ARPattern(_, PPString(SPOpenQuote)), _)
    | (ARFlag(_, FPWhenKeyword), _)
    | (ARFlag(_, FPEnabledKeyword), _) =>
      None
    // Invalid
    | (ARInvalid, _) => None
    }

  switch astInfo
  |> ASTInfo.activeTokenInfos
  |> List.findMap(~f=ti => targetAndTokenInfoToMaybeCaretPos((ct, ti))) {
  | Some(newPos) => newPos
  | None =>
    // NOTE: This is very useful for fixing issues in dev, but much too large for Rollbar:
    // Debug.loG(((show_caretTarget ct)^(show_fluidExpr ast)^(Printer.eToStructure ~incluID.toStrings:true ast)), ())
    recover(
      "We expected to find the given caretTarget in the token stream but couldn't.",
      ~debug=show_caretTarget(ct),
      astInfo.state.newPos,
    )
  }
}

@ocaml.doc(" moveToCaretTarget returns a modified fluidState with newPos set to reflect
    the caretTarget. ")
let moveToCaretTarget = (ct: caretTarget, astInfo: ASTInfo.t) => {
  ...astInfo,
  state: {...astInfo.state, newPos: posFromCaretTarget(ct, astInfo)},
}

@ocaml.doc(" caretTargetFromTokenInfo returns Some caretTarget corresponding to
  * the given top-level-global caret `pos`, with the precondition that
  * the pos is within the passed tokenInfo `ti`.
  * There are a few tokens that have no corresponding caretTarget.
  * In such cases, we return None instead.

  * We attempt to ensure that a single caret target uniquely
  * identifies each pos.  ")
let caretTargetFromTokenInfo = (pos: int, ti: T.tokenInfo): option<caretTarget> => {
  let offset = pos - ti.startPos
  switch ti.token {
  | TString(id, _, _) | TStringMLStart(id, _, _, _) => Some(CT.forARStringOpenQuote(id, offset))
  | TStringMLMiddle(id, _, startOffset, _)
  | TStringMLEnd(id, _, startOffset, _) =>
    Some(CT.forARStringText(id, startOffset + pos - ti.startPos))
  | TInteger(id, _, _) => Some({astRef: ARInteger(id), offset: offset})
  | TBlank(id, _) | TPlaceholder({blankID: id, _}) => Some({astRef: ARBlank(id), offset: offset})
  | TTrue(id, _) | TFalse(id, _) => Some({astRef: ARBool(id), offset: offset})
  | TNullToken(id, _) => Some({astRef: ARNull(id), offset: offset})
  | TFloatWhole(id, _, _) => Some({astRef: ARFloat(id, FPWhole), offset: offset})
  | TFloatPoint(id, _) => Some({astRef: ARFloat(id, FPPoint), offset: offset})
  | TFloatFractional(id, _, _) => Some({astRef: ARFloat(id, FPFractional), offset: offset})
  | TPartial(id, _, _) => Some({astRef: ARPartial(id), offset: offset})
  | TFieldPartial(id, _, _, _, _) => Some({astRef: ARPartial(id), offset: offset})
  | TRightPartial(id, _, _) => Some({astRef: ARRightPartial(id), offset: offset})
  | TLeftPartial(id, _, _) => Some({astRef: ARLeftPartial(id), offset: offset})
  | TLetKeyword(id, _, _) => Some({astRef: ARLet(id, LPKeyword), offset: offset})
  | TLetVarName(id, _, _, _) => Some({astRef: ARLet(id, LPVarName), offset: offset})
  | TLetAssignment(id, _, _) => Some({astRef: ARLet(id, LPAssignment), offset: offset})
  | TIfKeyword(id, _) => Some({astRef: ARIf(id, IPIfKeyword), offset: offset})
  | TIfThenKeyword(id, _) => Some({astRef: ARIf(id, IPThenKeyword), offset: offset})
  | TIfElseKeyword(id, _) => Some({astRef: ARIf(id, IPElseKeyword), offset: offset})
  | TBinOp(id, _, _) => Some({astRef: ARBinOp(id), offset: offset})
  | TFieldName(id, _, _, _) => Some({astRef: ARFieldAccess(id, FAPFieldname), offset: offset})
  | TFieldOp(id, _, _) => Some({astRef: ARFieldAccess(id, FAPFieldOp), offset: offset})
  | TVariable(id, _, _) => Some({astRef: ARVariable(id), offset: offset})
  | TFnName(id, _, _, _, _) => Some({astRef: ARFnCall(id), offset: offset})
  | TFnVersion(id, _, versionName, backendFnName) =>
    /* TODO: This is very brittle and should probably be moved into a function responsible
     for grabbing the appropriate bits of functions */
    Some({
      astRef: ARFnCall(id),
      offset: offset + String.length(backendFnName) - String.length(versionName) - 1,
    })
  | TLambdaComma(id, idx, _) => Some({astRef: ARLambda(id, LBPComma(idx)), offset: offset})
  | TLambdaArrow(id, _) => Some({astRef: ARLambda(id, LBPArrow), offset: offset})
  | TLambdaSymbol(id, _) => Some({astRef: ARLambda(id, LBPSymbol), offset: offset})
  | TLambdaVar(id, _, idx, _, _) => Some({astRef: ARLambda(id, LBPVarName(idx)), offset: offset})
  | TListOpen(id, _) => Some({astRef: ARList(id, LPOpen), offset: offset})
  | TListClose(id, _) => Some({astRef: ARList(id, LPClose), offset: offset})
  | TListComma(id, idx) => Some({astRef: ARList(id, LPComma(idx)), offset: offset})
  | TTupleOpen(id) => Some({astRef: ARTuple(id, TPOpen), offset: offset})
  | TTupleClose(id) => Some({astRef: ARTuple(id, TPClose), offset: offset})
  | TTupleComma(id, idx) => Some({astRef: ARTuple(id, TPComma(idx)), offset: offset})
  | TPipe(id, idx, _, _) => Some({astRef: ARPipe(id, idx), offset: offset})
  | TRecordOpen(id, _) => Some({astRef: ARRecord(id, RPOpen), offset: offset})
  | TRecordFieldname({recordID: id, index: idx, _}) =>
    Some({astRef: ARRecord(id, RPFieldname(idx)), offset: offset})
  | TRecordSep(id, idx, _) => Some({astRef: ARRecord(id, RPFieldSep(idx)), offset: offset})
  | TRecordClose(id, _) => Some({astRef: ARRecord(id, RPClose), offset: offset})
  | TMatchKeyword(id) => Some({astRef: ARMatch(id, MPKeyword), offset: offset})
  | TMatchBranchArrow({matchID: id, index: idx, _}) =>
    Some({astRef: ARMatch(id, MPBranchArrow(idx)), offset: offset})
  | TPatternVariable(_, id, _, _) => Some({astRef: ARPattern(id, PPVariable), offset: offset})
  | TPatternConstructorName(_, id, _, _) =>
    Some({astRef: ARPattern(id, PPConstructor), offset: offset})
  | TPatternInteger(_, id, _, _) => Some({astRef: ARPattern(id, PPInteger), offset: offset})
  | TPatternString({patternID: id, _}) => Some(CT.forPPStringOpenQuote(id, offset))
  | TPatternTrue(_, id, _) | TPatternFalse(_, id, _) =>
    Some({astRef: ARPattern(id, PPBool), offset: offset})
  | TPatternNullToken(_, id, _) => Some({astRef: ARPattern(id, PPNull), offset: offset})
  | TPatternFloatWhole(_, id, _, _) =>
    Some({astRef: ARPattern(id, PPFloat(FPWhole)), offset: offset})
  | TPatternFloatPoint(_, id, _) => Some({astRef: ARPattern(id, PPFloat(FPPoint)), offset: offset})
  | TPatternFloatFractional(_, id, _, _) =>
    Some({astRef: ARPattern(id, PPFloat(FPFractional)), offset: offset})
  | TPatternBlank(_, id, _) => Some({astRef: ARPattern(id, PPBlank), offset: offset})
  | TConstructorName(id, _) => Some({astRef: ARConstructor(id), offset: offset})
  | TFlagWhenKeyword(id) => Some({astRef: ARFlag(id, FPWhenKeyword), offset: offset})
  | TFlagEnabledKeyword(id) => Some({astRef: ARFlag(id, FPEnabledKeyword), offset: offset})
  /*
    These have no valid caretTarget because they are not
    strictly part of the AST.
 */
  | TPartialGhost(_)
  | TNewline(_)
  | TSep(_)
  | TIndent(_)
  | TParenOpen(_)
  | TParenClose(_) =>
    None
  }
}

let caretTargetForNextNonWhitespaceToken = (~pos, tokens: tokenInfos): option<caretTarget> => {
  let rec getNextWS = tokens =>
    switch tokens {
    | list{} => None
    | list{ti, ...rest} =>
      if T.isWhitespace(ti.token) || pos > ti.startPos {
        getNextWS(rest)
      } else {
        caretTargetFromTokenInfo(ti.startPos, ti)
      }
    }

  getNextWS(tokens)
}

@ocaml.doc(" moveToAstRef returns a modified fluidState with newPos set to reflect
    the targeted astRef.

    If given, offset is the offset of the caretTarget, in characters. Defaults
    to 0, or the beginning of the targeted expression. ")
let moveToAstRef = (~offset=0, astRef: astRef, astInfo: ASTInfo.t): ASTInfo.t =>
  moveToCaretTarget({astRef: astRef, offset: offset}, astInfo)

@ocaml.doc(" [caretTargetForEndOfExpr' expr] produces a caretTarget corresponding
 * to the very end of the expr. The concept of \"very end\" is related to an
 * understanding of the tokenization of the expr, even though this function
 * doesn't explicitly depend on any tokenization functions. ")
let rec caretTargetForEndOfExpr': fluidExpr => caretTarget = x =>
  switch x {
  | EVariable(id, str) => {astRef: ARVariable(id), offset: String.length(str)}
  | EFieldAccess(id, _, fieldName) => {
      astRef: ARFieldAccess(id, FAPFieldname),
      offset: String.length(fieldName),
    }
  | EInteger(id, value) => {astRef: ARInteger(id), offset: String.length(Int64.to_string(value))}
  | EBool(id, true) => {astRef: ARBool(id), offset: String.length("true")}
  | EBool(id, false) => {astRef: ARBool(id), offset: String.length("false")}
  | EString(id, str) => CT.forARStringCloseQuote(id, 1, str)
  | EFloat(id, _, _, decimalStr) => {
      astRef: ARFloat(id, FPFractional),
      offset: String.length(decimalStr),
    }
  | ENull(id) => {astRef: ARNull(id), offset: String.length("null")}
  | EBlank(id) => {astRef: ARBlank(id), offset: 0}
  | ELet(_, _, _, bodyExpr) => caretTargetForEndOfExpr'(bodyExpr)
  | EIf(_, _, _, elseExpr) => caretTargetForEndOfExpr'(elseExpr)
  | EBinOp(_, _, _, rhsExpr, _) => caretTargetForEndOfExpr'(rhsExpr)
  | ELambda(_, _, bodyExpr) => caretTargetForEndOfExpr'(bodyExpr)
  | EFnCall(id, fnName, argExprs, _) =>
    /* Caret targets don't make sense for EPipeTargets, so we
     * return a caret target for the end of the last fn argument
     * that isn't an EPipeTarget, or the end of the extended
     * function name, if there are no non-EPipeTargets. */
    argExprs
    |> List.reverse
    |> List.find(~f=e =>
      switch e {
      | EPipeTarget(_) => false
      | _ => true
      }
    )
    |> Option.map(~f=lastNonPipeTarget => caretTargetForEndOfExpr'(lastNonPipeTarget))
    |> Option.unwrap(
      ~default={
        astRef: ARFnCall(id),
        offset: fnName |> FluidUtil.fnDisplayNameWithVersion |> String.length,
      },
    )
  | EPartial(_, _, EBinOp(_, _, _, rhsExpr, _)) =>
    /* We need this so that (for example) when we backspace a binop containing a binop within a partial,
     * we can keep hitting backspace to delete the whole thing. This isn't (currently) needed for
     * other types of partials because deleting non-binop partials deletes their args,
     * whereas deleting binop partials merges and hoists the args. */
    caretTargetForEndOfExpr'(rhsExpr)
  | EPartial(id, str, _) => // Intentionally using the thing that was typed; not the existing expr
    {astRef: ARPartial(id), offset: String.length(str)}
  | ERightPartial(
      id,
      str,
      _,
    ) => // Intentionally using the thing that was typed; not the existing expr
    {astRef: ARRightPartial(id), offset: String.length(str)}
  | ELeftPartial(
      id,
      str,
      _,
    ) => // Intentionally using the thing that was typed; not the existing expr
    {astRef: ARLeftPartial(id), offset: String.length(str)}
  | EList(id, _) => {astRef: ARList(id, LPClose), offset: 1 /* End of the close ] */}
  | ETuple(id, _, _, _) => {astRef: ARTuple(id, TPClose), offset: 1 /* End of the close ) */}
  | ERecord(id, _) => {astRef: ARRecord(id, RPClose), offset: 1 /* End of the close } */}
  | EPipe(id, p1, p2, pipeExprs) =>
    switch List.last(list{p1, p2, ...pipeExprs}) {
    | Some(lastExpr) => caretTargetForEndOfExpr'(lastExpr)
    | None => {astRef: ARPipe(id, 0), offset: String.length("|>")}
    }
  | EMatch(_, matchedExpr, matchItems) =>
    switch List.last(matchItems) {
    | Some(_, branchBody) => caretTargetForEndOfExpr'(branchBody)
    | None => caretTargetForEndOfExpr'(matchedExpr)
    }
  | EConstructor(id, name, containedExprs) =>
    switch List.last(containedExprs) {
    | Some(lastExpr) => caretTargetForEndOfExpr'(lastExpr)
    | None => {astRef: ARConstructor(id), offset: String.length(name)}
    }
  | (EFeatureFlag(_, _, _, _, _) | EPipeTarget(_)) as expr =>
    recover(
      "we don't yet support caretTargetForEndOfExpr' for this",
      ~debug=show_fluidExpr(expr),
      {astRef: ARInvalid, offset: 0},
    )
  }

/* [caretTargetForEndOfExpr id ast] produces a caretTarget corresponding
 * to the "very end" of the expr identified by id within the [ast].
 * The concept of "very end" depends on caretTargetForEndOfExpr'.
 */
let caretTargetForEndOfExpr = (astPartId: id, ast: FluidAST.t): caretTarget =>
  switch FluidAST.find(astPartId, ast) {
  | Some(expr) => caretTargetForEndOfExpr'(expr)
  | None =>
    recover(
      "caretTargetForEndOfExpr got an id outside of the AST",
      ~debug=astPartId,
      {astRef: ARInvalid, offset: 0},
    )
  }

/* [caretTargetForStartOfExpr' expr] produces a caretTarget corresponding
 * to the very beginning of the [expr]. The concept of "very beginning" is related to an
 * understanding of the tokenization of the expr, even though this function
 * doesn't explicitly depend on any tokenization functions. */
let rec caretTargetForStartOfExpr': fluidExpr => caretTarget = x =>
  switch x {
  | EInteger(id, _) => {astRef: ARInteger(id), offset: 0}
  | EBool(id, _) => {astRef: ARBool(id), offset: 0}
  | EString(id, _) => CT.forARStringOpenQuote(id, 0)
  | EFloat(id, _, _, _) => {astRef: ARFloat(id, FPWhole), offset: 0}
  | ENull(id) => {astRef: ARNull(id), offset: 0}
  | EBlank(id) => {astRef: ARBlank(id), offset: 0}
  | ELet(id, _, _, _) => {astRef: ARLet(id, LPKeyword), offset: 0}
  | EIf(id, _, _, _) => {astRef: ARIf(id, IPIfKeyword), offset: 0}
  | EMatch(id, _, _) => {astRef: ARMatch(id, MPKeyword), offset: 0}
  | EBinOp(_, _, lhsExpr, _, _) => caretTargetForStartOfExpr'(lhsExpr)
  | EFnCall(id, _, _, _) => {astRef: ARFnCall(id), offset: 0}
  | ELambda(id, _, _) => {astRef: ARLambda(id, LBPSymbol), offset: 0}
  | EFieldAccess(_, expr, _) => caretTargetForStartOfExpr'(expr)
  | EVariable(id, _) => {astRef: ARVariable(id), offset: 0}
  | EPartial(id, _, _) => {astRef: ARPartial(id), offset: 0}
  | ERightPartial(id, _, _) => {astRef: ARRightPartial(id), offset: 0}
  | ELeftPartial(id, _, _) => {astRef: ARLeftPartial(id), offset: 0}
  | EList(id, _) => {astRef: ARList(id, LPOpen), offset: 0}
  | ETuple(id, _, _, _) => {astRef: ARTuple(id, TPOpen), offset: 0}
  | ERecord(id, _) => {astRef: ARRecord(id, RPOpen), offset: 0}
  | EPipe(_, e1, _, _) => caretTargetForStartOfExpr'(e1)
  | EConstructor(id, _, _) => {astRef: ARConstructor(id), offset: 0}
  | (EFeatureFlag(_) | EPipeTarget(_)) as expr =>
    recover(
      "unhandled expr in caretTargetForStartOfExpr'",
      ~debug=show_fluidExpr(expr),
      {astRef: ARInvalid, offset: 0},
    )
  }

/* [caretTargetForStartOfExpr id ast] produces a caretTarget corresponding
 * to the "very beginning" of the expr identified by [id] within the [ast].
 * The concept of "very beginning" depends on caretTargetForStartOfExpr'.
 */
let caretTargetForStartOfExpr = (astPartId: id, ast: FluidAST.t): caretTarget =>
  switch FluidAST.find(astPartId, ast) {
  | Some(expr) => caretTargetForStartOfExpr'(expr)
  | None =>
    recover(
      "caretTargetForStartOfExpr got an id outside of the AST",
      ~debug=astPartId,
      {astRef: ARInvalid, offset: 0},
    )
  }

/* caretTargetForStartOfPattern returns a caretTarget representing caret
 placement at the very start of the expression in `pattern` */
let caretTargetForStartOfPattern = (pattern: fluidPattern): caretTarget =>
  switch pattern {
  | PVariable(id, _) => {astRef: ARPattern(id, PPVariable), offset: 0}
  | PConstructor(id, _, _) => {astRef: ARPattern(id, PPConstructor), offset: 0}
  | PInteger(id, _) => {astRef: ARPattern(id, PPInteger), offset: 0}
  | PBool(id, _) => {astRef: ARPattern(id, PPBool), offset: 0}
  | PString(id, _) => CT.forPPStringOpenQuote(id, 0)
  | PFloat(id, _, _, _) => {astRef: ARPattern(id, PPFloat(FPWhole)), offset: 0}
  | PNull(id) => {astRef: ARPattern(id, PPNull), offset: 0}
  | PBlank(id) => {astRef: ARPattern(id, PPBlank), offset: 0}
  }

/* caretTargetForEndOfPattern returns a caretTarget representing caret
 * placement at the very end of the expression in `pattern`.
 *
 * The concept of "very end" is related to an understanding of the
 * tokenization of the ast, even though this function doesn't explicitly depend
 * on any tokenization functions. */
let rec caretTargetForEndOfPattern = (pattern: fluidPattern): caretTarget =>
  switch pattern {
  | PVariable(id, varName) => {
      astRef: ARPattern(id, PPVariable),
      offset: String.length(varName),
    }
  | PConstructor(id, name, containedPatterns) =>
    switch List.last(containedPatterns) {
    | Some(lastPattern) => caretTargetForEndOfPattern(lastPattern)
    | None => {astRef: ARPattern(id, PPConstructor), offset: String.length(name)}
    }
  | PInteger(id, value) => {
      astRef: ARPattern(id, PPInteger),
      offset: String.length(Int64.to_string(value)),
    }
  | PBool(id, true) => {astRef: ARPattern(id, PPBool), offset: String.length("true")}
  | PBool(id, false) => {astRef: ARPattern(id, PPBool), offset: String.length("false")}
  | PString(id, str) => CT.forPPStringCloseQuote(id, 1, str) // end of close quote
  | PFloat(id, _, _, frac) => {
      astRef: ARPattern(id, PPFloat(FPFractional)),
      offset: String.length(frac),
    }
  | PNull(id) => {astRef: ARPattern(id, PPNull), offset: String.length("null")}
  | PBlank(id) => // Consider changing this from 3 to 0 if we don't want blanks to have two spots
    {astRef: ARPattern(id, PPBlank), offset: 3}
  }

/* caretTargetForBeginningOfMatchBranch returns a caretTarget representing caret
 * placement at the very start of the match branch identified by `matchID` and `index`
 * within the `ast`.
 * It is an error to pass an id of a non-match or an index outside the match.
 *
 * "very start" is based on the definition of caretTargetForStartOfPattern
 */
let caretTargetForBeginningOfMatchBranch = (
  matchID: id,
  index: int,
  ast: FluidAST.t,
): caretTarget => {
  let maybeTarget = switch FluidAST.find(matchID, ast) {
  | Some(EMatch(_, _, branches)) =>
    branches
    |> List.getAt(~index)
    |> Option.map(~f=((pattern, _)) => caretTargetForStartOfPattern(pattern))
  | _ => None
  }

  maybeTarget |> recoverOpt(
    "caretTargetForBeginningOfMatchBranch got an invalid id/idx",
    ~debug=(matchID, index),
    ~default={astRef: ARInvalid, offset: 0},
  )
}

/* caretTargetForEndOfMatchPattern returns a caretTarget representing caret
 * placement at the end of the match pattern in the branch identified by `matchID` and `index`
 * within the `ast`.
 * It is an error to pass an id of a non-match or an index outside the match.
 *
 * "end" is based on the definition of caretTargetForEndOfPattern
 */
let caretTargetForEndOfMatchPattern = (ast: FluidAST.t, matchID: id, index: int): caretTarget => {
  let maybeTarget = switch FluidAST.find(matchID, ast) {
  | Some(EMatch(_, _, branches)) =>
    branches
    |> List.getAt(~index)
    |> Option.map(~f=((pattern, _)) => caretTargetForEndOfPattern(pattern))
  | _ => None
  }

  maybeTarget |> recoverOpt(
    "caretTargetForEndOfMatchPattern got an invalid id/index",
    ~debug=(matchID, index),
    ~default={astRef: ARInvalid, offset: 0},
  )
}

// ----------------
// Patterns
// ----------------

let recursePattern = (~f: fluidPattern => fluidPattern, pat: fluidPattern): fluidPattern =>
  switch pat {
  | PInteger(_)
  | PBlank(_)
  | PString(_)
  | PVariable(_)
  | PBool(_)
  | PNull(_)
  | PFloat(_) => pat
  | PConstructor(id, name, pats) => PConstructor(id, name, List.map(~f, pats))
  }

let updatePattern = (
  ~f: fluidPattern => fluidPattern,
  matchID: id,
  patID: id,
  ast: FluidAST.t,
): FluidAST.t =>
  FluidAST.update(matchID, ast, ~f=m =>
    switch m {
    | EMatch(matchID, expr, pairs) =>
      let rec run = p =>
        if patID == P.toID(p) {
          f(p)
        } else {
          recursePattern(~f=run, p)
        }

      let newPairs = List.map(pairs, ~f=((pat, expr)) => (run(pat), expr))

      EMatch(matchID, expr, newPairs)
    | _ => m
    }
  )

let replacePattern = (~newPat: fluidPattern, matchID: id, patID: id, ast: FluidAST.t): FluidAST.t =>
  updatePattern(matchID, patID, ast, ~f=_ => newPat)

@ocaml.doc(" addMatchPatternAt adds a new match row (PBlank, EBlank) into the EMatch
    with `matchId` at `idx`.

    Returns a new ast and fluidState with the action recorded. ")
let addMatchPatternAt = (matchId: id, idx: int, astInfo: ASTInfo.t): ASTInfo.t => {
  let action = Printf.sprintf("addMatchPatternAt(id=%s idx=%d)", ID.toString(matchId), idx)

  let astInfo = recordAction(action, astInfo)
  let ast = FluidAST.update(matchId, astInfo.ast, ~f=x =>
    switch x {
    | EMatch(_, cond, rows) =>
      let newVal = (PBlank(gid()), E.newB())
      let newRows = List.insertAt(rows, ~index=idx, ~value=newVal)
      EMatch(matchId, cond, newRows)
    | e => recover("expected to find EMatch to update", ~debug=e, e)
    }
  )

  astInfo |> ASTInfo.setAST(ast)
}

// ----------------
// Blanks
// ----------------

/* [insBlankOrPlaceholderHelper' ins]
 * shouldn't be called directly, only via
 * maybeInsertInBlankExpr or insertInPlaceholderExpr.
 *
 * It encodes the shared behavior of inserting text to
 * blanks or placeholders, which are identical
 * except for when creating lambdas.
 */
let insBlankOrPlaceholderHelper' = (ins: string): option<(E.t, caretTarget)> =>
  if ins == " " || ins == "," {
    None
  } else if ins == "\\" {
    recover(
      "insBlankOrPlaceholderHelper' - call insertInBlankExpr or insertInPlaceholderExpr instead",
      None,
    )
  } else {
    Some({
      let newID = gid()
      if ins == "\"" {
        (EString(newID, ""), CT.forARStringOpenQuote(newID, 1))
      } else if ins == "[" {
        (EList(newID, list{}), {astRef: ARList(newID, LPOpen), offset: 1})
      // TODO: prior to PR merge, disable below (until tuples functionality fuller)
      } else if ins == "(" && allowUserToCreateTuple {
        (ETuple(newID, EBlank(gid()), EBlank(gid()), list{}), {astRef: ARTuple(newID, TPOpen), offset: 1})
      } else if ins == "{" {
        (ERecord(newID, list{}), {astRef: ARRecord(newID, RPOpen), offset: 1})
      } else if Util.isNumber(ins) {
        let int = ins->Util.coerceStringTo64BitInt
        (
          EInteger(newID, int),
          {astRef: ARInteger(newID), offset: int->Int64.to_string->String.length},
        )
      } else {
        (
          EPartial(newID, ins, EBlank(gid())),
          {astRef: ARPartial(newID), offset: String.length(ins)},
        )
      }
    })
  }

/* [maybeInsertInBlankExpr ins]
 * produces Some (newExpr, newCaretTarget) tuple that should be
 * used to replace a blank when inserting the text [ins], or
 * None if the text shouldn't replace the blank.
 */
let maybeInsertInBlankExpr = (ins: string): option<(E.t, caretTarget)> =>
  if ins == "\\" {
    let newID = gid()
    Some(
      ELambda(newID, list{(gid(), "")}, EBlank(gid())),
      {astRef: ARLambda(newID, LBPVarName(0)), offset: 0},
    )
  } else {
    insBlankOrPlaceholderHelper'(ins)
  }

/* [insertInBlankExpr ins]
 * produces the (newExpr, newCaretTarget) tuple that should be
 * used to replace a blank when inserting the text [ins].
 */
let insertInBlankExpr = (ins: string): (E.t, caretTarget) =>
  maybeInsertInBlankExpr(ins) |> Option.unwrap(
    ~default={
      let newID = gid()
      (EBlank(newID), {astRef: ARBlank(newID), offset: 0})
    },
  )

/* [insertInPlaceholderExpr ~fnID ~placeholder ~ins ast functions]
 * produces the (newExpr, newCaretTarget) tuple that should be
 * used to replace a placeholder when inserting the text [ins],
 * given a list of [functions] that might contain data about the
 * containing function with [fnID] in [ast].
 *
 * Placeholders are almost the same as blanks but have special behavior
 * in conjunction with lambdas.
 */
let insertInPlaceholderExpr = (
  ~fnID: id,
  ~placeholder: placeholder,
  ~ins: string,
  ast: FluidAST.t,
  props: props,
): (E.t, caretTarget) => {
  let newID = gid()
  let lambdaArgs = () => {
    let fnname = switch FluidAST.find(fnID, ast) {
    | Some(EFnCall(_, name, _, _)) => Some(name)
    | _ => None
    }

    fnname
    |> Option.andThen(~f=name => Functions.find(name, props.functions))
    |> Option.andThen(~f=fn =>
      List.find(~f=({paramName, _}) => paramName == placeholder.name, fn.fnParameters)
    )
    |> Option.map(~f=p => p.paramBlock_args)
    |> Option.unwrap(~default=list{""})
    |> List.map(~f=str => (gid(), str))
  }

  let (newExpr, newTarget) = if ins == "\\" {
    (
      ELambda(newID, lambdaArgs(), EBlank(gid())),
      // TODO: if lambdaArgs is a populated list, place caret at the end
      {astRef: ARLambda(newID, LBPSymbol), offset: 1},
    )
  } else {
    insBlankOrPlaceholderHelper'(
      ins,
    ) |> // Just replace with a new blank -- we were creating eg a new list item
    Option.unwrap(~default=(EBlank(newID), {astRef: ARBlank(newID), offset: 0}))
  }

  (newExpr, newTarget)
}

// --------------------
// Strings
// --------------------
let maybeCommitStringPartial = (pos: int, ti: T.tokenInfo, astInfo: ASTInfo.t): ASTInfo.t => {
  let id = T.tid(ti.token)
  // Handle moving from a partial back to an EString
  let valid_escape_chars_alist = /* We use this alist for two things: knowing what chars are permitted
   * after a \, and knowing what to replace them with as we go from a
   * display string to the real thing */
  list{("n", "\n"), ("t", "\t"), ("\\", "\\")}

  let valid_escape_chars = valid_escape_chars_alist |> List.map(~f=fst)
  let invalid_escapes_in_string = (str: string): list<string> => {
    let captures = {
      /* Capture any single character following a '\'. Escaping is
       * terrible here. */
      /* takes a unit arg so we create a new re value every time we run -
       * the re value contains state */
      /* Non-capturing group of even number (inc'l 0) of slashes handles
       * '\\\o' - recognizes that the first pair is a valid sequence
       * ('\\'), and the second is not ('\o') */
      let re = () => %re("/(?:\\\\\\\\)*\\\\(.)/g")
      let rec matches = (acc: list<Js.Re.result>, re: Js.Re.t): list<Js.Re.result> =>
        switch Regex.matches(~re, str) {
        | None => acc
        | Some(r) => matches(list{r, ...acc}, re)
        }

      matches(list{}, re())
      |> List.map(~f=Js.Re.captures)
      |> List.filterMap(~f=x =>
        switch x {
        | [_whole_match, capture] => Some(capture)
        | _ => None
        }
      )
      |> List.filterMap(~f=Js.Nullable.toOption)
    }

    captures |> List.filter(~f=c => !List.member(~value=c, valid_escape_chars))
  }

  let origExpr = FluidAST.find(id, astInfo.ast)
  let processStr = (str: string): string =>
    valid_escape_chars_alist |> List.fold(~initial=str, ~f=(acc, (from, repl)) => {
      // workaround for how "\\" gets escaped
      let from = if from === "\\" {
        "\\\\"
      } else {
        from
      }
      Regex.replace(~re=Regex.regex("\\\\" ++ from), ~repl, acc)
    })

  let newAST = FluidAST.update(
    ~failIfMissing=false,
    ~f=/* no-op */ x =>
      switch x {
      | EPartial(_, str, EString(_)) as origExpr =>
        let processedStr = processStr(str)
        let invalid_escapes = invalid_escapes_in_string(str)
        if !List.isEmpty(invalid_escapes) {
          origExpr // no-op
        } else {
          EString(id, processedStr)
        }
      | origExpr => origExpr
      },
    id,
    astInfo.ast,
  )

  switch origExpr {
  | Some(EPartial(_, str, EString(_))) =>
    let invalid_escapes = invalid_escapes_in_string(str)
    if invalid_escapes != list{} {
      None // no-op
    } else {
      Some(str)
    }
  | _ => None // no-op
  }
  |> Option.map(~f=oldStr => {
    let oldOffset = pos - ti.startPos
    /* We might have shortened the string when we processed its
     * escapes - but only the ones to the left of the cursor would
     * move the cursor */
    let (oldlhs, _) = String.splitAt(~index=oldOffset, oldStr)
    let newlhs = processStr(oldlhs)
    let newOffset = oldOffset + (String.length(oldlhs) - String.length(newlhs))

    let astRef = switch FluidAST.find(id, newAST) {
    | Some(EString(_)) => ARString(id, SPOpenQuote)
    | Some(EPartial(_)) => ARPartial(id)
    | Some(expr) => recover("need an ASTRef match for ", ~debug=show_fluidExpr(expr), ARPartial(id))
    | _ => recover("no expr found for ID", ~debug=id, ARPartial(id))
    }

    let astInfo = ASTInfo.setAST(newAST, astInfo)
    moveToAstRef(~offset=newOffset + 1, astRef, astInfo)
  })
  |> /* If origExpr wasn't an EPartial (_, _, EString _), then we didn't
   * change the AST in the updateExpr call, so leave the newState as it
   * was */
  Option.unwrap(~default=astInfo)
}

let startEscapingString = (pos: int, ti: T.tokenInfo, astInfo: ASTInfo.t): ASTInfo.t => {
  /* I think this is correct but depends on how we 'render' strings - that
   * is, it is correct because we display '"', which bumps pos by 1. */
  let offset = getStringIndex(ti, pos)
  let id = T.tid(ti.token)
  astInfo.ast
  |> FluidAST.update(id, ~f=x =>
    switch x {
    | EString(_, str) as old_expr =>
      let new_str =
        String.splitAt(~index=offset, str)
        |> (((lhs, rhs)) => (lhs, rhs))
        |> (((lhs, rhs)) => lhs ++ ("\\" ++ rhs))

      EPartial(id, new_str, old_expr)
    | e => e
    }
  )
  |> (ast => ASTInfo.setAST(ast, astInfo) |> moveToAstRef(~offset=offset + 1, ARPartial(id)))
}

// ----------------
// Fields
// ----------------

/* [exprToFieldAccess id ~partialID ~fieldID] wraps the expression with `id` in
 * the `ast` with a partial-wrapped field access where the partial has partialID
 * and the field access has fieldID. It produces a (newASt, caretTarget) where
 * the caretTarget represents the end of the partial. */
let exprToFieldAccess = (id: id, ~partialID: id, ~fieldID: id, ast: FluidAST.t): (
  FluidAST.t,
  caretTarget,
) => {
  let newAST = FluidAST.update(id, ast, ~f=e => EPartial(
    partialID,
    "",
    EFieldAccess(fieldID, e, ""),
  ))

  (newAST, {astRef: ARPartial(partialID), offset: 0})
}

// ----------------
// Lambdas
// ----------------

let insertLambdaVar = (~index: int, ~name: string, id: id, ast: FluidAST.t): FluidAST.t =>
  FluidAST.update(id, ast, ~f=e =>
    switch e {
    | ELambda(id, vars, expr) =>
      let value = (gid(), name)
      ELambda(id, List.insertAt(~index, ~value, vars), expr)
    | _ => recover("not a list in insertLambdaVar", ~debug=e, e)
    }
  )

// ----------------
// Lets
// ----------------

@ocaml.doc(" makeIntoLetBody takes the `id` of an expression, which will be made into the
    body of a new ELet.

    Returns a new ast, fluidState, and the id of the newly inserted ELet, which
    may be useful for doing caret placement. ")
let makeIntoLetBody = (id: id, astInfo: ASTInfo.t): (ASTInfo.t, id) => {
  let astInfo = recordAction(Printf.sprintf("makeIntoLetBody(%s)", ID.toString(id)), astInfo)

  let lid = gid()
  let ast = FluidAST.update(id, astInfo.ast, ~f=expr => ELet(lid, "", E.newB(), expr))

  (astInfo |> ASTInfo.setAST(ast), lid)
}

// ----------------
// Records
// ----------------

// Add a row to the record
let addRecordRowAt = (~letter="", index: int, id: id, ast: FluidAST.t): FluidAST.t =>
  FluidAST.update(id, ast, ~f=e =>
    switch e {
    | ERecord(id, fields) => ERecord(id, List.insertAt(~index, ~value=(letter, E.newB()), fields))
    | _ => recover("Not a record in addRecordRowAt", ~debug=e, e)
    }
  )

let addRecordRowToBack = (id: id, ast: FluidAST.t): FluidAST.t =>
  FluidAST.update(id, ast, ~f=e =>
    switch e {
    | ERecord(id, fields) => ERecord(id, Belt.List.concat(fields, list{("", E.newB())}))
    | _ => recover("Not a record in addRecordRowToTheBack", ~debug=e, e)
    }
  )

let recordFields = (recordID: id, ast: FluidExpression.t): option<list<(string, fluidExpr)>> =>
  E.find(recordID, ast) |> Option.andThen(~f=expr =>
    switch expr {
    | ERecord(_, fields) => Some(fields)
    | _ => None
    }
  )

/* recordFieldAtIndex gets the field for the record in the ast with recordID at index,
 or None if the record has no field with that index */
let recordFieldAtIndex = (recordID: id, index: int, ast: FluidExpression.t): option<(
  string,
  fluidExpr,
)> => recordFields(recordID, ast) |> Option.andThen(~f=fields => List.getAt(~index, fields))

/* recordExprIdAtIndex gets the id of the field value for the record in the ast
 with recordID at index, or None if the record has no field with that index */
let recordExprIdAtIndex = (recordID: id, index: int, ast: FluidExpression.t): option<id> =>
  switch recordFieldAtIndex(recordID, index, ast) {
  | Some(_, fluidExpr) => Some(E.toID(fluidExpr))
  | _ => None
  }

/* [mergeExprs e1 e2] "merges" the given exprs [e1] and [e2] into a single expr
 * and returns that merged expr along with a caret target corresponding to the
 * "middle" of the merge.
 *
 * An example of where this is useful is when deleting a binary operator.
 *   Given 12 + 34, deleting the + results in the EInteger 1234.
 */
let rec mergeExprs = (e1: fluidExpr, e2: fluidExpr): (fluidExpr, caretTarget) =>
  switch (e1, e2) {
  | (EPipeTarget(_), e2) => (e2, caretTargetForStartOfExpr'(e2))
  | (_, EBinOp(id, op, lhs, rhs, rail)) =>
    // Example: 1 , (2+3) -> (1|2+3)
    let (merged, target) = mergeExprs(e1, lhs)
    (EBinOp(id, op, merged, rhs, rail), target)
  | (e1, EBlank(_)) => (e1, caretTargetForEndOfExpr'(e1))
  | (EBlank(_), e2) => (e2, caretTargetForStartOfExpr'(e2))
  | (EInteger(id, i1), EInteger(_, i2)) => (
      EInteger(id, Util.coerceStringTo64BitInt(Int64.to_string(i1) ++ Int64.to_string(i2))),
      {astRef: ARInteger(id), offset: i1->Int64.to_string->String.length},
    )
  | (EString(id, s1), EString(_, s2)) => (
      EString(id, s1 ++ s2),
      CT.forARStringText(id, String.length(s1)),
    )
  | (
      e1,
      _e2,
    ) => // TODO(JULIAN): consider preserving e2 as well, by (for example) creating a partial.
    // recover "mergeExprs can't handle this" ~debug:(show_fluidExpr e1, show_fluidExpr e2) (e1, caretTargetForEndOfExpr' e1)
    (e1, caretTargetForEndOfExpr'(e1))
  }

type compareParamsResult =
  | CPAligned
  | CPTypeAndPositionMatched
  | CPNothingMatched

let replacePartialWithArguments = (props: props, ~newExpr: E.t, id: id, ast: FluidAST.t): (
  FluidAST.t,
  caretTarget,
) => {
  let getFunctionParams = (fnname, count, varExprs): list<option<(string, string, E.t, int)>> =>
    List.map(List.range(0, count), ~f=index =>
      props.functions
      |> Functions.find(fnname)
      |> Option.andThen(~f=fn => List.getAt(~index, fn.fnParameters))
      |> Option.map(~f=p => (
        p.paramName,
        Runtime.tipe2str(p.paramTipe),
        List.getAt(~index, varExprs) |> Option.unwrap(~default=EBlank(gid())),
        index,
      ))
    )

  let rec wrapWithLets = (~expr, vars) =>
    switch vars {
    // don't wrap parameter who's value is a blank i.e. not set
    | list{} | list{(_, _, EBlank(_), _), ..._} => expr
    // don't wrap parameter that's set to a variable
    | list{(_, _, EVariable(_), _), ..._} => expr
    | list{(name, _, rhs, _), ...rest} => ELet(gid(), name, rhs, wrapWithLets(~expr, rest))
    }

  let getArgs = expr =>
    switch expr {
    | EFnCall(_, _, exprs, _) | EConstructor(_, _, exprs) => exprs
    | EBinOp(_, _, lhs, rhs, _) => list{lhs, rhs}
    | _ => recover("impossible", ~debug=expr, list{})
    }

  let chooseSter = (~oldName: string, ~oldExpr: E.t, newAllowed: sendToRail) => {
    /* decides whether the new function is on the rails. Note that are checking
     * if we should prefer the old setting. */
    let oldSter = switch oldExpr {
    | EFnCall(_, _, _, ster) | EBinOp(_, _, _, _, ster) => ster
    | _ => NoRail
    }

    let oldAllowed =
      props.functions
      |> Functions.find(oldName)
      |> Option.map(~f=fn =>
        if List.member(~value=fn.fnReturnTipe, Runtime.errorRailTypes) {
          Rail
        } else {
          NoRail
        }
      )
      |> Option.unwrap(~default=NoRail)

    /* The new function should be on the error rail if it was on the error rail
     * and the new function allows it, or if it wasn't on the error rail, but
     * the old function didn't allow it and the new one does */
    if newAllowed == Rail && (oldSter == Rail || oldAllowed == NoRail) {
      Rail
    } else {
      NoRail
    }
  }

  /* Compare two parameters, params are aligned if both name
   * and type are the same, or if both type and position are the same */
  let compareParams = (
    p1: option<(string, string, E.t, int)>,
    p2: option<(string, string, E.t, int)>,
  ): compareParamsResult =>
    switch (p1, p2) {
    | (Some(name, tipe, _, index), Some(name', tipe', _, index')) =>
      if name == name' && tipe == tipe' {
        CPAligned
      } else if tipe == tipe' && index == index' {
        CPTypeAndPositionMatched
      } else {
        CPNothingMatched
      }
    | (_, _) => CPNothingMatched
    }

  let ctForExpr = (expr: fluidExpr): caretTarget =>
    switch expr {
    | EBinOp(_id, _opName, lhs, _, _) => caretTargetForStartOfExpr'(lhs)
    | EFnCall(id, fnName, argExprs, _ster) =>
      argExprs
      |> List.find(~f=x =>
        switch x {
        | EPipeTarget(_) => false
        | _ => true
        }
      )
      |> Option.map(~f=arg => caretTargetForStartOfExpr'(arg))
      |> Option.unwrap(
        ~default={
          astRef: ARFnCall(id),
          offset: fnName |> FluidUtil.ghostPartialName |> String.length,
        },
      )
    | EConstructor(id, cName, argExprs) =>
      argExprs
      |> List.find(~f=x =>
        switch x {
        | EPipeTarget(_) => false
        | _ => true
        }
      )
      |> Option.map(~f=arg => caretTargetForStartOfExpr'(arg))
      |> Option.unwrap(~default={astRef: ARConstructor(id), offset: String.length(cName)})
    | ELet(id, _, _, _) => {astRef: ARLet(id, LPVarName), offset: 0}
    | EIf(_, condExpr, _, _) => caretTargetForStartOfExpr'(condExpr)
    | EMatch(_, mExpr, _) => caretTargetForStartOfExpr'(mExpr)
    | expr => caretTargetForEndOfExpr'(expr)
    }

  let mkExprAndTarget = (expr: fluidExpr): (fluidExpr, caretTarget) => (expr, ctForExpr(expr))

  FluidAST.find(id, ast)
  |> Option.map(~f=x =>
    // preserve partials with arguments
    switch x {
    | EPartial(_, _, EFnCall(_, oldName, _, _) as oldExpr)
    | EPartial(_, _, EBinOp(_, oldName, _, _, _) as oldExpr)
    | EPartial(_, _, EConstructor(_, oldName, _) as oldExpr) =>
      let existingExprs = getArgs(oldExpr)
      let fetchParams = (newName, placeholderExprs) => {
        let count = max(List.length(existingExprs), List.length(placeholderExprs))

        let newParams = getFunctionParams(newName, count, placeholderExprs)

        let oldParams = getFunctionParams(oldName, count, existingExprs)
        // Divide old existing params to matched and mismatched lists
        let (matchedParams, mismatchedParams) = List.partitionMap(oldParams, ~f=p => {
          // Split out the "aligned" parameters (parameters from the old fnCall that match the new fnCall)
          let comparedResults = List.map(newParams, ~f=compareParams(p))

          let alignedIndex =
            List.findIndex(comparedResults, ~f=(_, r) => r == CPAligned) |> Option.map(
              ~f=Tuple2.first,
            )

          let typeAndPositionMatchedIndex =
            List.findIndex(comparedResults, ~f=(_, r) =>
              r == CPTypeAndPositionMatched
            ) |> Option.map(~f=Tuple2.first)

          switch (alignedIndex, typeAndPositionMatchedIndex) {
          | (Some(index), _) | (None, Some(index)) =>
            let np = List.getAt(~index, newParams)
            switch (np, p) {
            | (Some(Some(name, tipe, _, index)), Some(_, _, expr, _)) =>
              // Assign new param position index with old param info
              Left(Some(name, tipe, expr, index))
            | _ => Right(p)
            }
          | (None, None) => Right(p)
          }
        }) |> Tuple2.mapAll(~f=Option.values)

        // Update new params with old matched params with their new position index
        let newParams = List.fold(matchedParams, ~initial=placeholderExprs, ~f=(
          exprs,
          (_, _, expr, index),
        ) => List.updateAt(~index, ~f=_ => expr, exprs))

        (newParams, mismatchedParams)
      }

      switch newExpr {
      | EBinOp(id, newName, lhs, rhs, newSter) =>
        let ster = chooseSter(~oldName, ~oldExpr, newSter)
        let (newParams, mismatchedParams) = fetchParams(newName, list{lhs, rhs})

        let newExpr = switch newParams {
        | list{newLHS, newRHS} => EBinOp(id, newName, newLHS, newRHS, ster)
        | _ =>
          recover(
            "wrong number of arguments",
            ~debug=newParams,
            EBinOp(id, newName, E.newB(), E.newB(), ster),
          )
        }

        (wrapWithLets(~expr=newExpr, mismatchedParams), ctForExpr(newExpr))
      | EFnCall(id, newName, newExprs, newSter) =>
        let ster = chooseSter(~oldName, ~oldExpr, newSter)
        let (newParams, mismatchedParams) = fetchParams(newName, newExprs)

        let newExpr = EFnCall(id, newName, newParams, ster)
        (wrapWithLets(~expr=newExpr, mismatchedParams), ctForExpr(newExpr))
      | EConstructor(_) =>
        let oldParams = existingExprs |> List.mapWithIndex(~f=(i, p) => {
          // create ugly automatic variable name
          let name = "var_" ++ string_of_int(DUtil.random())
          (name, Runtime.tipe2str(TAny), p, i)
        })

        (wrapWithLets(~expr=newExpr, oldParams), ctForExpr(newExpr))
      | newExpr => mkExprAndTarget(newExpr)
      }
    | _ => mkExprAndTarget(newExpr)
    }
  )
  |> Option.map(~f=((newExpr, target)) => (FluidAST.replace(id, ~replacement=newExpr, ast), target))
  |> recoverOpt("replacePartialWithArguments", ~default=(ast, {astRef: ARInvalid, offset: 0}))
}

// ----------------
// Partials
// ----------------

let rec extractSubexprFromPartial = (expr: E.t): E.t =>
  switch expr {
  | EPartial(_, _, subExpr) | ERightPartial(_, _, subExpr) => extractSubexprFromPartial(subExpr)
  | _ => expr
  }

// ----------------
// Pipes
// ----------------

/* Used for piping and wrapping line in let. For both we are often at the last
 * argument of a function call. We want to perform the operation on the entire
 * expression on the last line, not just the expression present in the token at
 * the end of the line. This function helps us find the whole expression that
 * we would want to perform it on.
 */
let rec findAppropriateParentToWrap = (oldExpr: FluidExpression.t, ast: FluidAST.t): option<
  FluidExpression.t,
> => {
  let child = oldExpr
  let parent = FluidAST.findParent(E.toID(oldExpr), ast)
  switch parent {
  | Some(parent) =>
    switch parent {
    | EInteger(_)
    | EBlank(_)
    | EString(_)
    | EVariable(_)
    | EBool(_)
    | ENull(_)
    | EPipeTarget(_)
    | EFloat(_) =>
      recover("these cant be parents", ~debug=parent, None)
    // If the parent is some sort of "resetting", then we probably meant the child
    | ELet(_)
    | EIf(_)
    | EMatch(_)
    | ERecord(_)
    | ELambda(_)
    | // Not sure what to do here, probably nothing fancy
    EFeatureFlag(_) =>
      Some(child)
    | EPipe(_) => Some(parent)
    // These are the expressions we're trying to skip. They are "sub-line" expressions.
    | EBinOp(_) | EFnCall(_) | EList(_) | ETuple(_) | EConstructor(_) | EFieldAccess(_) =>
      findAppropriateParentToWrap(parent, ast)
    // These are wrappers of the current expr.
    | EPartial(_) | ERightPartial(_) | ELeftPartial(_) => findAppropriateParentToWrap(parent, ast)
    }
  | None =>
    // If we get to the root
    Some(child)
  }
}

@ocaml.doc(" createPipe makes the expression with `id` the target of a new EPipe.

    If the ~findParent flag is passed, an \"appropriate\" parent of the
    expression is found as the pipe target. See findAppropriatePipingParent for
    details. If the flag is not passed, the pipe target is the expression as
    given.

    Returns a new ast, fluidState, and the id of the EBlank created as the
    first pipe expression (if the pipe was successfully added).
    ")
let createPipe = (~findParent: bool, id: id, astInfo: ASTInfo.t): (ASTInfo.t, option<id>) => {
  let action = Printf.sprintf("createPipe(id=%s findParent=%B)", ID.toString(id), findParent)

  let astInfo = recordAction(action, astInfo)
  let exprToReplace =
    FluidAST.find(id, astInfo.ast)
    |> Option.andThen(~f=e =>
      if findParent {
        findAppropriateParentToWrap(e, astInfo.ast)
      } else {
        Some(e)
      }
    )
    |> Option.map(~f=extractSubexprFromPartial)

  switch exprToReplace {
  | None => (astInfo, None)
  | Some(expr) =>
    let blankId = gid()
    let replacement = EPipe(gid(), expr, EBlank(blankId), list{})
    let ast = FluidAST.replace(E.toID(expr), astInfo.ast, ~replacement)
    (astInfo |> ASTInfo.setAST(ast), Some(blankId))
  }
}

@ocaml.doc(" addPipeExprAt adds a new EBlank into the EPipe with `pipeId` at `idx`.

    Returns a new ast, fluidState with the action recorded, and the id of the
    newly inserted EBlank, which may be useful for doing caret placement. ")
let addPipeExprAt = (pipeId: id, idx: int, astInfo: ASTInfo.t): (ASTInfo.t, id) => {
  let action = Printf.sprintf("addPipeExprAt(id=%s idx=%d)", ID.toString(pipeId), idx)

  let astInfo = recordAction(action, astInfo)
  let bid = gid()
  let ast = FluidAST.update(pipeId, astInfo.ast, ~f=x => {
    let newExpr = EBlank(bid)
    switch (x, idx) {
    | (EPipe(_, e1, e2, exprs), 0) => EPipe(pipeId, newExpr, e1, list{e2, ...exprs})
    | (EPipe(_, e1, e2, exprs), 1) => EPipe(pipeId, e1, newExpr, list{e2, ...exprs})
    | (EPipe(_, e1, e2, exprs), _) =>
      let exprs = List.insertAt(exprs, ~index=idx - 2, ~value=newExpr)
      EPipe(pipeId, e1, e2, exprs)
    | (e, _) => recover("expected to find EPipe to update", ~debug=e, e)
    }
  })

  (ASTInfo.setAST(ast, astInfo), bid)
}

// ----------------
// Lists
// ----------------

let insertInList = (~index: int, ~newExpr: E.t, id: id, ast: FluidAST.t): FluidAST.t =>
  FluidAST.update(id, ast, ~f=e =>
    switch e {
    | EList(id, exprs) => EList(id, List.insertAt(~index, ~value=newExpr, exprs))
    | _ => recover("not a list in insertInList", ~debug=e, e)
    }
  )

let insertAtListEnd = (~newExpr: E.t, id: id, ast: FluidAST.t): FluidAST.t =>
  FluidAST.update(id, ast, ~f=e =>
    switch e {
    | EList(id, exprs) => EList(id, Belt.List.concat(exprs, list{newExpr}))
    | _ => recover("not a list in insertInList", ~debug=e, e)
    }
  )

// ----------------
// Tuples
// ----------------

let insertInTuple = (~index: int, ~newExpr: E.t, id: id, ast: FluidAST.t): FluidAST.t =>
  FluidAST.update(id, ast, ~f=e =>
    switch e {
    | ETuple(id, first, second, theRest) =>
      switch index {
        | 0 => ETuple(id, newExpr, first, List.insertAt(~index=0, ~value=second, theRest))
        | 1 => ETuple(id, first, newExpr, List.insertAt(~index=0, ~value=second, theRest))
        | i => ETuple(id, first, second, List.insertAt(~index=i-2, ~value=newExpr, theRest))
      }

    | _ => recover("not a tuple in insertInTuple", ~debug=e, e)
    }
  )

let insertAtTupleEnd = (~newExpr: E.t, id: id, ast: FluidAST.t): FluidAST.t =>
  FluidAST.update(id, ast, ~f=e =>
    switch e {
    | ETuple(id, first, second, theRest) =>
      ETuple(id, first, second, Belt.List.concat(theRest, list{newExpr}))
    | _ => recover("not a tuple in insertAtTupleEnd", ~debug=e, e)
    }
  )


// --------------------
// Autocomplete
// --------------------

let acToExpr = (entry: Types.fluidAutocompleteItem): option<(E.t, caretTarget)> => {
  let mkBlank = (): (E.t, caretTarget) => {
    let bID = gid()
    (EBlank(bID), {astRef: ARBlank(bID), offset: 0})
  }

  switch entry {
  | FACFunction(fn) =>
    let count = List.length(fn.fnParameters)
    let r = if List.member(~value=fn.fnReturnTipe, Runtime.errorRailTypes) {
      Rail
    } else {
      NoRail
    }

    let args = List.initialize(count, ~f=_ => EBlank(gid()))
    if fn.fnInfix {
      switch args {
      | list{lhs, rhs} =>
        Some(EBinOp(gid(), fn.fnName, lhs, rhs, r), caretTargetForStartOfExpr'(rhs))
      | _ => recover("BinOp doesn't have 2 args", ~debug=args, None)
      }
    } else {
      /* functions with arguments should place the caret into the first argument
       * while functions without should place it just after the function name
       * List::head |_list_ [vs] List::empty| */
      let fID = gid()
      let target =
        args
        |> List.find(~f=x =>
          switch x {
          | EPipeTarget(_) => false
          | _ => true
          }
        )
        |> Option.map(~f=arg => caretTargetForStartOfExpr'(arg))
        |> Option.unwrap(
          ~default={
            astRef: ARFnCall(fID),
            offset: fn.fnName |> FluidUtil.partialName |> String.length,
          },
        )

      Some(EFnCall(fID, fn.fnName, args, r), target)
    }
  | FACKeyword(KLet) =>
    let (b, target) = mkBlank()
    Some(ELet(gid(), "", b, E.newB()), target)
  | FACKeyword(KIf) =>
    let (b, target) = mkBlank()
    Some(EIf(gid(), b, E.newB(), E.newB()), target)
  | FACKeyword(KLambda) =>
    let lID = gid()
    Some(
      ELambda(lID, list{(gid(), "")}, E.newB()),
      {astRef: ARLambda(lID, LBPVarName(0)), offset: 0},
    )
  | FACKeyword(KMatch) =>
    let matchID = gid()
    let (b, target) = mkBlank()
    Some(EMatch(matchID, b, list{(PBlank(gid()), E.newB())}), target)
  | FACKeyword(KPipe) =>
    let (b, target) = mkBlank()
    Some(EPipe(gid(), b, E.newB(), list{}), target)
  | FACVariable(name, _) =>
    let vID = gid()
    Some(EVariable(vID, name), {astRef: ARVariable(vID), offset: String.length(name)})
  | FACLiteral("true") =>
    let bID = gid()
    Some(EBool(bID, true), {astRef: ARBool(bID), offset: String.length("true")})
  | FACLiteral("false") =>
    let bID = gid()
    Some(EBool(bID, false), {astRef: ARBool(bID), offset: String.length("false")})
  | FACLiteral("null") =>
    let nID = gid()
    Some(ENull(nID), {astRef: ARNull(nID), offset: String.length("null")})
  | FACConstructorName(name, argCount) =>
    let args = List.initialize(argCount, ~f=_ => EBlank(gid()))
    let expr = EConstructor(gid(), name, args)
    Some(expr, caretTargetForEndOfExpr'(expr))
  | FACField(fieldname) =>
    let fID = gid()
    Some(
      EFieldAccess(fID, E.newB(), fieldname),
      {
        astRef: ARFieldAccess(fID, FAPFieldname),
        offset: String.length(fieldname),
      },
    )
  | FACLiteral(_) => recover("invalid literal in autocomplete", ~debug=entry, None)
  | FACPattern(_) =>
    // This only works for exprs
    None
  | FACCreateFunction(_) =>
    // This should be handled elsewhere
    recover("invalid call to FACCreateFunction", None)
  }
}

let acToPattern = (entry: Types.fluidAutocompleteItem): option<(id, fluidPattern, caretTarget)> => {
  let selectedPat: option<(id, P.t)> = switch entry {
  | FACPattern(p) =>
    switch p {
    | FPAConstructor(mID, patID, var, pats) => Some(mID, PConstructor(patID, var, pats))
    | FPAVariable(mID, patID, var) => Some(mID, PVariable(patID, var))
    | FPABool(mID, patID, var) => Some(mID, PBool(patID, var))
    | FPANull(mID, patID) => Some(mID, PNull(patID))
    }
  | _ =>
    // This only works for patterns
    None
  }

  selectedPat |> Option.map(~f=((mid, p)) => (mid, p, caretTargetForEndOfPattern(p)))
}

let acToPatternOrExpr = (entry: Types.fluidAutocompleteItem): (E.fluidPatOrExpr, caretTarget) =>
  acToPattern(entry)
  |> Option.map(~f=((mid, pat, target)) => (E.Pat(mid, pat), target))
  |> Option.orElseLazy(() =>
    acToExpr(entry) |> Option.map(~f=((expr, target)) => (E.Expr(expr), target))
  )
  |> recoverOpt(
    "acToPatternOrExpr",
    ~debug=entry,
    ~default=(E.Expr(E.newB()), {astRef: ARInvalid, offset: 0}),
  )

let initAC = (s: state): state => {...s, ac: AC.init}

let isAutocompleting = (ti: T.tokenInfo, s: state): bool =>
  T.isAutocompletable(ti.token) &&
  // upDownCol should be None to prevent the autocomplete from opening when moving the cursor up and down
  (s.upDownCol == None &&
  (s.ac.index != None && (s.newPos <= ti.endPos && s.newPos >= ti.startPos)))

let acSetIndex' = (i: int, s: state): state => {
  let s = recordAction'("acSetIndex", s)
  {...s, ac: {...s.ac, index: Some(i)}, upDownCol: None}
}

let acSetIndex = (i: int, astInfo: ASTInfo.t): ASTInfo.t =>
  ASTInfo.modifyState(~f=acSetIndex'(i), astInfo)

let acClear = (astInfo: ASTInfo.t): ASTInfo.t =>
  astInfo
  |> recordAction("acClear")
  |> ASTInfo.modifyState(~f=s => {...s, ac: {...s.ac, index: None}})

let acMaybeShow = (ti: T.tokenInfo, astInfo: ASTInfo.t): ASTInfo.t =>
  astInfo
  |> recordAction("acShow")
  |> ASTInfo.modifyState(~f=s =>
    if T.isAutocompletable(ti.token) && s.ac.index == None {
      {...s, ac: {...s.ac, index: Some(0)}, upDownCol: None}
    } else {
      {...s, ac: {...s.ac, query: None}}
    }
  )

let acMoveUp = (astInfo: ASTInfo.t): ASTInfo.t => {
  let index = switch astInfo.state.ac.index {
  | None => 0
  | Some(current) => max(0, current - 1)
  }

  astInfo |> recordAction("acMoveUp") |> acSetIndex(index)
}

let acMoveDown = (astInfo: ASTInfo.t): ASTInfo.t => {
  let index = switch astInfo.state.ac.index {
  | None => 0
  | Some(current) => min(current + 1, AC.numCompletions(astInfo.state.ac) - 1)
  }

  astInfo |> recordAction("acMoveDown") |> acSetIndex(index)
}

// Check to see if we should open autocomplete at new position
let updatePosAndAC = (newPos: int, astInfo: ASTInfo.t): ASTInfo.t => {
  // Update newPos and reset upDownCol and reset AC
  let astInfo = setPosition(~resetUD=true, newPos, astInfo) |> acClear
  astInfo
  |> ASTInfo.getToken
  |> Option.map(~f=ti => acMaybeShow(ti, astInfo))
  |> Option.unwrap(~default=astInfo)
}

/* acMoveBasedOnKey produces a new state with the caret placed in a position that
   makes sense for the specific key that was pressed to confirm the autocomplete.

   It accepts:
   - the pressed key,
   - the caret position at which the autocompleted token begins,
   - an "offset" that corresponds to how many additional steps the caret should take to get
      from there to where the caret would end up with no special handling,
   - the state after the completion has been added to the ast
   - the ast after the completion has been added */
let acMoveBasedOnKey = (
  key: K.key,
  currCaretTarget: caretTarget,
  astInfo: ASTInfo.t,
): ASTInfo.t => {
  let caretTarget: caretTarget = switch key {
  | K.Tab =>
    getNextBlank(astInfo.state.newPos, astInfo)
    |> Option.andThen(~f=nextBlankTi => caretTargetFromTokenInfo(nextBlankTi.startPos, nextBlankTi))
    |> Option.unwrap(~default=currCaretTarget)
  | K.ShiftTab =>
    getPrevBlank(astInfo.state.newPos, astInfo)
    |> Option.andThen(~f=prevBlankTi => caretTargetFromTokenInfo(prevBlankTi.startPos, prevBlankTi))
    |> Option.unwrap(~default=currCaretTarget)
  | K.Enter => currCaretTarget
  | K.Space =>
    /* TODO: consider skipping over non-whitespace separators
          as well, such as the commas in a list:
          we currently do [aced|,___]
          but could do    [aced,|___]
 */
    let startPos = posFromCaretTarget(currCaretTarget, astInfo)
    switch FluidTokenizer.getNeighbours(~pos=startPos, ASTInfo.activeTokenInfos(astInfo)) {
    | (_, R(TNewline(_), _), _) => // If we're on a newline, don't move forward
      currCaretTarget
    | _ =>
      caretTargetForNextNonWhitespaceToken(
        ~pos=startPos,
        ASTInfo.activeTokenInfos(astInfo),
      ) |> Option.unwrap(~default=currCaretTarget)
    }
  | _ => currCaretTarget
  }

  astInfo |> acClear |> moveToCaretTarget(caretTarget)
}

let updateFromACItem = (
  entry: fluidAutocompleteItem,
  ti: T.tokenInfo,
  key: K.key,
  astInfo: ASTInfo.t,
): ASTInfo.t => {
  open FluidExpression
  let id = T.tid(ti.token)
  let (newPatOrExpr, newTarget) = acToPatternOrExpr(entry)
  let ast = astInfo.ast
  let props = astInfo.props
  let oldExpr = FluidAST.find(id, ast)
  let parent = FluidAST.findParent(id, ast)
  let (newAST, target) = switch (ti.token, oldExpr, parent, newPatOrExpr) {
  /* since patterns have no partial but commit as variables
   * automatically, allow intermediate variables to
   * be autocompletable to other expressions */
  | (TPatternBlank(mID, pID, _) | TPatternVariable(mID, pID, _, _), _, _, Pat(_, newPat)) =>
    let newAST = replacePattern(~newPat, mID, pID, ast)
    (newAST, newTarget)
  | (
      TPartial(_) | TRightPartial(_),
      Some((ERightPartial(_, _, subExpr) | EPartial(_, _, subExpr)) as oldExpr),
      _,
      _,
    ) if entry == FACKeyword(KPipe) =>
    /* The pipe operator is intended to be roughly "line-based", which
     * means tht instead of tying this to the smallest expression (which is
     * within the partial) we go back and figure out the "line", which is
     * to say the largest expression that doesn't break a line. */
    let exprToReplace =
      findAppropriateParentToWrap(oldExpr, ast) |> Option.map(~f=extractSubexprFromPartial)

    switch exprToReplace {
    | None =>
      let blank = E.newB()
      let replacement = EPipe(gid(), subExpr, blank, list{})
      (FluidAST.replace(E.toID(oldExpr), ast, ~replacement), caretTargetForEndOfExpr'(blank))
    | Some(expr) if expr == subExpr =>
      let blank = E.newB()
      let replacement = EPipe(gid(), subExpr, blank, list{})
      (FluidAST.replace(E.toID(oldExpr), ast, ~replacement), caretTargetForEndOfExpr'(blank))
    | Some(expr) =>
      let blank = E.newB()
      let expr = E.replace(E.toID(oldExpr), expr, ~replacement=subExpr)
      let replacement = EPipe(gid(), expr, blank, list{})
      (FluidAST.replace(E.toID(expr), ast, ~replacement), caretTargetForEndOfExpr'(blank))
    }
  | (TLeftPartial(_), Some(ELeftPartial(pID, _, expr)), _, Expr(EIf(ifID, _, _, _))) =>
    // when committing `if` in front of another expression, put the expr into the if condition
    let replacement = EIf(ifID, expr, E.newB(), E.newB())
    let newAST = FluidAST.replace(~replacement, pID, ast)
    (newAST, caretTargetForStartOfExpr'(expr))
  | (TLeftPartial(_), Some(ELeftPartial(pID, _, expr)), _, Expr(EMatch(mID, _, pats))) =>
    // when committing `match` in front of another expression, put the expr into the match condition
    let replacement = EMatch(mID, expr, pats)
    let newAST = FluidAST.replace(~replacement, pID, ast)
    (newAST, caretTargetForStartOfExpr'(expr))
  | (TLeftPartial(_), Some(ELeftPartial(pID, _, expr)), _, Expr(ELet(letID, _, _, _))) =>
    // when committing `let` in front of another expression, put the expr into the RHS
    let blank = E.newB()
    let replacement = ELet(letID, "", expr, E.newB())
    let newAST = FluidAST.replace(~replacement, pID, ast)
    (newAST, caretTargetForStartOfExpr'(blank))
  | (TPartial(_), _, Some(EPipe(_)), Expr(EBinOp(bID, name, _, rhs, str))) =>
    let replacement = EBinOp(bID, name, EPipeTarget(gid()), rhs, str)
    let newAST = FluidAST.replace(~replacement, id, ast)
    (newAST, caretTargetForEndOfExpr'(replacement))
  | (TPartial(_), Some(oldExpr), Some(EPipe(_, firstExpr, _, _)), Expr(newExpr))
    if oldExpr == firstExpr =>
    // special case of the generic TPartial in EPipe case just below this:
    // when we are the first thing in the pipe, no pipe target required
    replacePartialWithArguments(props, ~newExpr, id, ast)
  | (TPartial(_), Some(_), Some(EPipe(_)), Expr(EFnCall(fnID, name, list{_, ...args}, str))) =>
    let newExpr = EFnCall(fnID, name, list{EPipeTarget(gid()), ...args}, str)
    // We can't use the newTarget because it might point to eg a blank
    // replaced with an argument
    replacePartialWithArguments(props, ~newExpr, id, ast)
  | (
      TPartial(_),
      Some(EPartial(_, _, EBinOp(_, _, lhs, rhs, _))),
      _,
      Expr(EBinOp(bID, name, _, _, str)),
    ) =>
    let replacement = EBinOp(bID, name, lhs, rhs, str)
    let newAST = FluidAST.replace(~replacement, id, ast)
    (newAST, caretTargetForStartOfExpr'(rhs))
  | (TPartial(_), _, _, Expr(newExpr)) =>
    // We can't use the newTarget because it might point to eg a blank
    // replaced with an argument
    replacePartialWithArguments(props, ~newExpr, id, ast)
  | (
      TRightPartial(_),
      Some(ERightPartial(_, _, oldExpr)),
      _,
      Expr(EBinOp(bID, name, _, rhs, str)),
    ) =>
    let replacement = EBinOp(bID, name, oldExpr, rhs, str)
    let newAST = FluidAST.replace(~replacement, id, ast)
    (newAST, caretTargetForStartOfExpr'(rhs))
  | (
      TFieldName(_) | TFieldPartial(_) | TBlank(_),
      Some(
        EFieldAccess(faID, expr, _)
        | EPartial(_, _, EFieldAccess(faID, expr, _)),
      ),
      _,
      Expr(EFieldAccess(_, _, newname)),
    ) =>
    let replacement = EFieldAccess(faID, expr, newname)
    let newAST = FluidAST.replace(~replacement, id, ast)
    (newAST, caretTargetForEndOfExpr'(replacement))
  | (_, _, _, Expr(newExpr)) =>
    let newAST = FluidAST.replace(~replacement=newExpr, id, ast)
    (newAST, newTarget)
  | (_, _, _, Pat(_)) =>
    recover(
      "updateFromACItem - unhandled pattern",
      ~debug=entry,
      (ast, {astRef: ARInvalid, offset: 0}),
    )
  }

  astInfo
  |> ASTInfo.modifyState(~f=s => {...s, ac: {...s.ac, query: None}})
  |> ASTInfo.setAST(newAST)
  |> acMoveBasedOnKey(key, target)
}

let acEnter = (ti: T.tokenInfo, key: K.key, astInfo: ASTInfo.t): ASTInfo.t => {
  let astInfo = recordAction("acEnter", astInfo)
  switch AC.highlighted(astInfo.state.ac) {
  | None =>
    switch ti.token {
    | TPatternVariable(_) => moveToNextBlank(astInfo.state.newPos, astInfo)
    | TFieldPartial(partialID, _fieldAccessID, anaID, fieldname, _) =>
      // Accept fieldname, even if it's not in the autocomplete
      FluidAST.find(anaID, astInfo.ast)
      |> Option.map(~f=expr => {
        let replacement = EFieldAccess(gid(), expr, fieldname)
        FluidAST.replace(~replacement, partialID, astInfo.ast)
      })
      |> Option.map(~f=ast => ASTInfo.setAST(ast, astInfo))
      |> Option.unwrap(~default=astInfo)
    | _ => astInfo
    }
  | Some(FACCreateFunction(_)) =>
    recover("FACNewfunction should be dealt with outside fluid.ml", astInfo)
  | Some(entry) => updateFromACItem(entry, ti, key, astInfo)
  }
}

let acClick = (entry: fluidAutocompleteItem, ti: T.tokenInfo, astInfo: ASTInfo.t): ASTInfo.t =>
  updateFromACItem(entry, ti, K.Enter, astInfo)

/* If `newPos` is outside `ti`, and `ti` matches the current autocomplete entry
 * perfectly, then select and commit that autocomplete entry */
let commitIfValid = (newPos: int, ti: T.tokenInfo, astInfo: ASTInfo.t): ASTInfo.t => {
  let highlightedText = astInfo.state.ac |> AC.highlighted |> Option.map(~f=AC.asName)

  let isInside = newPos >= ti.startPos && newPos <= ti.endPos
  /* TODO: if we can't move off because it's the start/end etc of the ast, we
   * may want to commit anyway. */
  if !isInside && (Some(T.toText(ti.token)) == highlightedText || T.isFieldPartial(ti.token)) {
    acEnter(ti, K.Enter, astInfo)
  } else {
    astInfo
  }
}

let acMaybeCommit = (newPos: int, astInfo: ASTInfo.t): ASTInfo.t =>
  switch astInfo.state.ac.query {
  | Some(_, ti) => commitIfValid(newPos, ti, astInfo)
  | None => astInfo
  }

/* Convert the expr ti into a FieldAccess, using the currently
 * selected Autocomplete value */
let acStartField = (ti: T.tokenInfo, astInfo: ASTInfo.t): ASTInfo.t => {
  let astInfo = recordAction("acStartField", astInfo)
  switch (AC.highlighted(astInfo.state.ac), ti.token) {
  | (Some(FACField(_) as entry), TFieldName(faID, _, _, _))
  | (Some(FACField(_) as entry), TFieldPartial(_, faID, _, _, _)) =>
    let astInfo = updateFromACItem(entry, ti, K.Enter, astInfo)
    let (ast, target) = exprToFieldAccess(faID, ~partialID=gid(), ~fieldID=gid(), astInfo.ast)

    astInfo |> ASTInfo.setAST(ast) |> moveToCaretTarget(target) |> acClear
  | (Some(entry), _) =>
    let replacement = switch acToPatternOrExpr(entry) {
    | (Expr(newExpr), _ignoredTarget) => EPartial(gid(), "", EFieldAccess(gid(), newExpr, ""))
    | (Pat(_), _) => recover("acStartField", E.newB())
    }

    astInfo
    |> ASTInfo.setAST(FluidAST.replace(~replacement, T.tid(ti.token), astInfo.ast))
    |> moveToCaretTarget(caretTargetForEndOfExpr'(replacement))
    |> acClear
  | _ => astInfo
  }
}

// --------------------
// Code entering/interaction
// --------------------

type newPosition =
  | SamePlace
  | Exactly(int)
  /* The hope is that we can migrate everything to
   AtTarget and then remove this entirely */
  | AtTarget(caretTarget)

let adjustPosForReflow = (
  oldTI: T.tokenInfo,
  oldPos: int,
  adjustment: newPosition,
  astInfo: ASTInfo.t,
): int => {
  /* Reflow refers to adjusting layout for to prevent overly long lines. Any
   * character change can cause that line to be too long (and so it will
   * reflow) or no longer too long (and so it will un-reflow).
   *
   * We need to find where the cursor should be in the new AST, given the old
   * position, the old token it was in, and the new AST. We do this by finding
   * the old token in the new token stream, and then doing the appropriate
   * adjustment. There are definitely places this won't work, but I haven't
   * found them yet. */
  let newTI =
    astInfo |> ASTInfo.activeTokenInfos |> List.find(~f=x => T.matches(oldTI.token, x.token))

  let diff = switch newTI {
  | Some(newTI) => newTI.startPos - oldTI.startPos
  | None => 0
  }

  let newPos = oldPos + diff
  switch (adjustment, newTI) {
  | (SamePlace, _) => newPos
  | (Exactly(pos), _) => pos
  | (AtTarget(target), _) => posFromCaretTarget(target, astInfo)
  }
}

let idOfASTRef = (astRef: astRef): option<id> =>
  switch astRef {
  | ARInteger(id)
  | ARBool(id)
  | ARString(id, _)
  | ARFloat(id, _)
  | ARNull(id)
  | ARBlank(id)
  | ARLet(id, _)
  | ARIf(id, _)
  | ARBinOp(id)
  | ARFieldAccess(id, _)
  | ARVariable(id)
  | ARFnCall(id)
  | ARPartial(id)
  | ARRightPartial(id)
  | ARLeftPartial(id)
  | ARList(id, _)
  | ARTuple(id, _)
  | ARRecord(id, _)
  | ARPipe(id, _)
  | ARConstructor(id)
  | ARMatch(id, _)
  | ARLambda(id, _)
  | ARPattern(id, _)
  | ARFlag(id, _) =>
    Some(id)
  | ARInvalid => None
  }

@ocaml.doc(" [itemsAtCurrAndNextIndex lst idx] produces Some tuple of the
 * item at the given [idx] and at [idx + 1]. If either of the
 * indices is not present in the list, it returns None.
 ")
let rec itemsAtCurrAndNextIndex = (lst: list<'a>, idx: int): option<('a, 'a)> =>
  switch lst {
  | list{} | list{_} => None
  | list{a, ...list{b, ..._} as rest} =>
    if idx > 0 {
      (@tailcall itemsAtCurrAndNextIndex)(rest, idx - 1)
    } else if idx == 0 {
      Some(a, b)
    } else {
      None
    }
  }

/* [doExplicitBackspace [currCaretTarget] [ast]] produces the
 * (newAST, newPosition) tuple resulting from performing
 * a backspace-style deletion at [currCaretTarget] in the
 * [ast]. Note that newPosition will be either
 * AtTarget or SamePlace -- either the caret stays in the
 * same place, or it ends up at a specific location.
 *
 * WARNING: in some cases, we may produce caret targets with
 * offsets that are outside of the targetable range.
 * In such cases, we currently rely on the behavior of
 * posFromCaretTarget to clamp the offset.
 *
 * Note also that doExplicitBackspace expects to receive
 * only "real" caret targets (via caretTargetFromTokenInfo);
 * we don't handle certain 0-offset [currCaretTarget]s because
 * we expect to receive tokens to the left of the caret instead
 * of the 0th offset of the caretTarget of the token to the right.
 *
 * A hacky exception to this is Blanks -- there are a few circumstances
 * where we obtain a zero offset for blanks even though the blank is to the
 * right of the caret instead of the left. This is to account for
 * deleting rows in eg a match (see doBackspace in updateKey).
 * Ideally we wouldn't need these hacks.
 */
let doExplicitBackspace = (currCaretTarget: caretTarget, ast: FluidAST.t): (
  FluidAST.t,
  newPosition,
) => {
  open FluidExpression
  let {astRef: currAstRef, offset: currOffset} = currCaretTarget
  let currCTMinusOne = {astRef: currAstRef, offset: currOffset - 1}
  let mutation: string => string = str => Util.removeCharAt(str, currOffset - 1)

  let mutationAt = (str: string, ~index: int): string => Util.removeCharAt(str, index)

  let doExprBackspace = (currAstRef: astRef, currExpr: E.t): option<(
    E.fluidPatOrExpr,
    caretTarget,
  )> => {
    let mkEBlank: unit => option<(E.fluidPatOrExpr, caretTarget)> = () => {
      let bID = gid()
      Some(Expr(EBlank(bID)), {astRef: ARBlank(bID), offset: 0})
    }

    let mkPartialOrBlank = (str: string, e: E.t): option<(E.fluidPatOrExpr, caretTarget)> =>
      if str == "" {
        mkEBlank()
      } else {
        let parID = gid()
        Some(Expr(EPartial(parID, str, e)), {astRef: ARPartial(parID), offset: currOffset - 1})
      }

    switch (currAstRef, currExpr) {
    | (ARInteger(_), EInteger(id, int)) =>
      let str = int->Int64.to_string->mutation
      if str == "" {
        mkEBlank()
      } else {
        let coerced = Util.coerceStringTo64BitInt(str)
        if coerced == int {
          None
        } else {
          Some(Expr(EInteger(id, coerced)), currCTMinusOne)
        }
      }
    | (ARString(_, kind), EString(id, str)) =>
      let strRelOffset = switch kind {
      | SPOpenQuote => currOffset - 1
      }
      if strRelOffset == 0 && str == "" {
        mkEBlank()
      } else {
        let newStr = str |> mutationAt(~index=strRelOffset - 1)
        Some(Expr(EString(id, newStr)), CT.forARStringOpenQuote(id, strRelOffset))
      }
    | (ARFloat(_, FPWhole), EFloat(id, sign, whole, frac)) =>
      let word = Sign.combine(sign, whole)
      let (sign, whole) = Sign.split(mutation(word))
      Some(Expr(EFloat(id, sign, whole, frac)), currCTMinusOne)
    | (ARFloat(_, FPPoint), EFloat(_, sign, whole, frac)) =>
      // TODO: If the float only consists of a . and has no whole or frac,
      // it should become a blank. Instead, it currently becomes a 0, which is weird.
      let i = Util.coerceStringTo64BitInt(ProgramTypes.Sign.combine(sign, whole) ++ frac)
      let iID = gid()
      Some(Expr(EInteger(iID, i)), {astRef: ARInteger(iID), offset: String.length(whole)})
    | (ARFloat(_, FPFractional), EFloat(id, sign, whole, frac)) =>
      Some(Expr(EFloat(id, sign, whole, mutation(frac))), currCTMinusOne)
    | (ARLet(_, LPVarName), ELet(id, oldName, value, body)) =>
      let newName = mutation(oldName)
      let newExpr = ELet(id, newName, value, E.renameVariableUses(~oldName, ~newName, body))

      if newName == "" {
        Some(Expr(newExpr), {astRef: currAstRef, offset: 0})
      } else {
        Some(Expr(newExpr), currCTMinusOne)
      }
    | (ARLambda(_, LBPComma(varAndSepIdx)), ELambda(id, oldVars, oldExpr)) =>
      itemsAtCurrAndNextIndex(oldVars, varAndSepIdx)
      |> Option.map(~f=(((_, keepVarName), (_, deleteVarName))) => {
        // remove expression in front of sep, not behind it, hence + 1
        let newVars = List.removeAt(~index=varAndSepIdx + 1, oldVars)
        let newExpr = E.removeVariableUse(deleteVarName, oldExpr)
        Some(
          Expr(ELambda(id, newVars, newExpr)),
          {
            astRef: ARLambda(id, LBPVarName(varAndSepIdx)),
            offset: String.length(keepVarName),
          },
        )
      })
      |> recoverOpt("doExplicitBackspace - LPComma", ~debug=(varAndSepIdx, oldVars), ~default=None)


    // Lists
    | (ARList(_, LPComma(elemAndSepIdx)), EList(id, exprs)) =>
      let (newExpr, target) =
        itemsAtCurrAndNextIndex(exprs, elemAndSepIdx)
        |> Option.map(~f=((beforeComma, afterComma)) => mergeExprs(beforeComma, afterComma))
        |> Option.unwrap(~default=(E.newB(), {astRef: ARList(id, LPOpen), offset: 1}))

      let newExprs =
        /* Considering a is the item at elemAndSepIdx and b is at elemAndSepIdx + 1,
         * we merge a and b in [...a,b...] by replacing a with ab and removing b */
        exprs
        |> List.updateAt(~index=elemAndSepIdx, ~f=_ => newExpr)
        |> List.removeAt(~index=elemAndSepIdx + 1)

      Some(Expr(EList(id, newExprs)), target)
    // Remove list wrapping from singleton
    | (ARList(_, LPOpen), EList(_, list{item})) =>
      Some(Expr(item), caretTargetForStartOfExpr'(item))


    // Tuples
    | (ARTuple(_, TPOpen), ETuple(id, first, second, theRest)) =>
      // When we're trying to delete the ( in a tuple,
      // - normally, don't do anything, and leave cursor at left of (
      // - if there are only blanks in the tuple, replace with blank
      // - if there's only 1 non-blank item, replace with that item
      let nonBlanks =
        list{first, second, ...theRest}
        |> List.filter(~f= expr => !isBlank(expr))

      switch nonBlanks {
        | list{} => Some(Expr(EBlank(id)), {astRef: ARBlank(id), offset: 0})
        | list{single} => Some(Expr(single), caretTargetForStartOfExpr'(single))
        | _ =>
          let target = {astRef: ARTuple(id, TPOpen), offset: 0}
          Some(Expr(ETuple(id, first, second, theRest)), target)
      }

    | (ARTuple(_, TPComma(elemAndSepIdx)), ETuple(id, first, second, theRest)) =>
      // When we're trying a comma (,) within in a tuple,
      // - normally, remove the element just after the comma
      // - if that leaves only one element, replace with that item
      let withoutDeleted =
        list{first, second, ...theRest}
        |> List.removeAt(~index=elemAndSepIdx + 1)

      switch withoutDeleted {
        | list{} => recover("Deletion unexpectedly resulted in tuple with 0 elements", ~debug=show_astRef(currAstRef), None)
        | list{single} =>
          let newTarget = caretTargetForEndOfExpr(toID(single), ast)
          Some(Expr(single), newTarget)
        | list{first, second, ...theRest} =>

          let newExpr = Expr(ETuple(id, first, second, theRest))

          // set target to RHS of the item to left of ,
          // something is broken about this. We're currently ending up on the LHS of the thing before the deleted item
          switch Belt.List.get(withoutDeleted, elemAndSepIdx) {
            | None => recover("Deletion unexpectedly resulted in tuple with 0 elements", ~debug=show_astRef(currAstRef), None)
            | Some(elementLeftOfDeletion) =>
              let newTarget = caretTargetForEndOfExpr(toID(elementLeftOfDeletion), ast)

              Some(newExpr, newTarget)
          }
      }

    | (ARTuple(_, TPClose), ETuple(id, first, second, theRest)) =>
      let target = {astRef: ARTuple(id, TPClose), offset: 0}
      Some(Expr(ETuple(id, first, second, theRest)), target)


    // Records
    | (ARRecord(_, RPOpen), expr) | (ARList(_, LPOpen), expr) | (ARTuple(_, TPOpen), expr) if E.isEmpty(expr) => mkEBlank()
    | (ARRecord(_, RPFieldname(index)), ERecord(id, nameValPairs)) =>
      List.getAt(~index, nameValPairs) |> Option.map(~f=x =>
        switch x {
        | ("", _) =>
          let target = switch recordExprIdAtIndex(id, index - 1, FluidAST.toExpr(ast)) {
          | None => {
              astRef: ARRecord(id, RPOpen),
              offset: 1 // right after the {
            }
          | Some(exprId) => caretTargetForEndOfExpr(exprId, ast)
          }

          (Expr(ERecord(id, List.removeAt(~index, nameValPairs))), target)
        | (name, _) =>
          let nameValPairs = List.updateAt(nameValPairs, ~index, ~f=((_, expr)) => (
            mutation(name),
            expr,
          ))

          (Expr(ERecord(id, nameValPairs)), currCTMinusOne)
        }
      )

    // Field access
    | (ARFieldAccess(_, FAPFieldOp), EFieldAccess(_, faExpr, _))
    | (ARFieldAccess(_, FAPFieldOp), EPartial(_, _, EFieldAccess(_, faExpr, _))) =>
      Some(Expr(faExpr), caretTargetForEndOfExpr'(faExpr))

    // Constructors
    | (ARConstructor(_), EConstructor(_, str, _)) =>
      mkPartialOrBlank(str |> mutation |> String.trim, currExpr)

    // Function call
    | (ARFnCall(_), EFnCall(_, fnName, _, _)) =>
      mkPartialOrBlank(fnName |> FluidUtil.partialName |> mutation |> String.trim, currExpr)

    // Bools
    | (ARBool(_), EBool(_, bool)) =>
      let str = if bool {
        "true"
      } else {
        "false"
      }
      mkPartialOrBlank(mutation(str), currExpr)

    | (ARNull(_), ENull(_)) => mkPartialOrBlank(mutation("null"), currExpr)
    | (ARVariable(_), EVariable(_, varName)) => mkPartialOrBlank(mutation(varName), currExpr)
    | (ARBinOp(_), EBinOp(_, op, lhsExpr, rhsExpr, _)) =>
      let str = op |> FluidUtil.ghostPartialName |> mutation |> String.trim
      if str == "" {
        // Delete the binop
        let (expr, target) = mergeExprs(lhsExpr, rhsExpr)
        Some(Expr(expr), target)
      } else {
        let newID = gid()
        Some(
          Expr(EPartial(newID, str, currExpr)),
          {astRef: ARPartial(newID), offset: currOffset - 1},
        )
      }
    | (ARFieldAccess(_, FAPFieldname), EFieldAccess(_, _, fieldName)) =>
      // Note that str is allowed to be empty in partials
      let str = mutation(fieldName)
      let newID = gid()
      Some(Expr(EPartial(newID, str, currExpr)), {astRef: ARPartial(newID), offset: currOffset - 1})
    | (ARPartial(_), EPartial(id, oldStr, oldExpr)) =>
      let str = oldStr |> mutation |> String.trim
      if str == "" {
        // inlined version of deletePartial, with appropriate exceptions added
        switch oldExpr {
        | EFieldAccess(_) =>
          // This is allowed to be the empty string.
          Some(Expr(EPartial(id, str, oldExpr)), currCTMinusOne)
        | EBinOp(_, _, lhsExpr, rhsExpr, _) =>
          let (expr, target) = mergeExprs(lhsExpr, rhsExpr)
          Some(Expr(expr), target)
        | _ => mkEBlank()
        }
      } else if String.startsWith(~prefix="\"", str) && String.endsWith(~suffix="\"", str) {
        let newID = gid()
        Some(
          Expr(EString(newID, String.slice(~from=1, ~to_=-1, str))),
          CT.forARStringText(newID, currOffset - 1),
        )
      } else {
        Some(Expr(EPartial(id, str, oldExpr)), currCTMinusOne)
      }
    | (ARLeftPartial(_), ELeftPartial(id, oldStr, oldValue)) =>
      let str = oldStr |> mutation |> String.trim
      /* Left partials are rendered in front of the expression they wrap,
       * so place the caret at the start of the old value when the partial is removed */
      if str == "" {
        Some(Expr(oldValue), caretTargetForStartOfExpr'(oldValue))
      } else {
        Some(Expr(ELeftPartial(id, str, oldValue)), currCTMinusOne)
      }
    | (ARRightPartial(_), ERightPartial(id, oldStr, oldValue)) =>
      let str = oldStr |> mutation |> String.trim
      /* Right partials are rendered in front of the expression they wrap,
       * so place the caret at the end of the old value when the partial is removed */
      if str == "" {
        Some(Expr(oldValue), caretTargetForEndOfExpr'(oldValue))
      } else {
        Some(Expr(ERightPartial(id, str, oldValue)), currCTMinusOne)
      }
    | (ARLambda(_, LBPVarName(index)), ELambda(id, vars, expr)) =>
      vars
      |> List.getAt(~index)
      |> Option.map(~f=((_, oldName)) => {
        let newName = oldName |> mutation
        /* Note that newName is intentionally
         allowed to be "" with no special handling */
        let vars = List.updateAt(vars, ~index, ~f=((varId, _)) => (varId, newName))

        (Expr(ELambda(id, vars, E.renameVariableUses(~oldName, ~newName, expr))), currCTMinusOne)
      })
    // We're deleting the pipe token itself, which removes the expression to the
    // RIGHT of the token, not the left
    | (ARPipe(_, _), EPipe(_, e1, _, list{})) => Some(Expr(e1), caretTargetForEndOfExpr'(e1))
    | (ARPipe(_, 0), EPipe(id, e1, _, list{e3, ...rest})) =>
      let newExpr = EPipe(id, e1, e3, rest)
      Some(Expr(newExpr), caretTargetForEndOfExpr'(e1))
    | (ARPipe(_, 1), EPipe(id, e1, e2, list{_, ...rest})) =>
      let newExpr = EPipe(id, e1, e2, rest)
      Some(Expr(newExpr), caretTargetForEndOfExpr'(e2))
    | (ARPipe(_, idx), EPipe(id, e1, e2, rest)) =>
      rest
      // This expr before the one we're removing, which the caret points at
      |> List.getAt(~index=idx - 2)
      |> Option.map(~f=expr => Some(
        Expr(EPipe(id, e1, e2, List.removeAt(~index=idx - 1, rest))),
        caretTargetForEndOfExpr'(expr),
      ))
      |> recoverOpt("doExplicitBackspace ARPipe", ~default=None)

    // Delete leading keywords of empty expressions
    | (ARLet(_, LPKeyword), ELet(_, varName, expr, EBlank(_)))
    | (ARLet(_, LPKeyword), ELet(_, varName, EBlank(_), expr)) if varName == "" || varName == "_" =>
      Some(Expr(expr), caretTargetForStartOfExpr'(expr))
    // Removing a let wrapping another let
    | (ARLet(_, LPKeyword), ELet(_, varName, ELet(id, nestedVarName, rhs, EBlank(_)), body))
      if varName == "" || varName == "_" =>
      let expr = ELet(id, nestedVarName, rhs, body)
      Some(Expr(expr), caretTargetForStartOfExpr'(expr))
    | (ARIf(_, IPIfKeyword), EIf(_, EBlank(_), EBlank(_), EBlank(_)))
    | (ARLambda(_, LBPSymbol), ELambda(_, _, EBlank(_))) =>
      // If the expr is empty and thus can be removed
      mkEBlank()
    | (ARMatch(_, MPKeyword), EMatch(_, EBlank(_), pairs))
      if List.all(pairs, ~f=((p, e)) =>
        switch (p, e) {
        | (PBlank(_), EBlank(_)) => true
        | _ => false
        }
      ) =>
      // the match has no content and can safely be deleted
      mkEBlank()
    | (ARMatch(_, MPKeyword), EMatch(_))
    | (ARLet(_, LPKeyword), ELet(_))
    | (ARIf(_, IPIfKeyword), EIf(_))
    | (ARLambda(_, LBPSymbol), ELambda(_)) =>
      // keywords of "non-empty" exprs shouldn't be deletable at all
      None

    /*
     * Immutable; just jump to the start
     */
    | (ARMatch(id, MPBranchArrow(idx)), expr) =>
      Some(Expr(expr), caretTargetForEndOfMatchPattern(ast, id, idx))
    | (ARIf(_, IPThenKeyword), expr)
    | (ARIf(_, IPElseKeyword), expr)
    | (ARLambda(_, LBPArrow), expr)
    | (ARBlank(_), expr)
    | (ARLet(_, LPAssignment), expr)
    | (ARRecord(_, RPOpen), expr)
    | (ARRecord(_, RPClose), expr)
    | (ARRecord(_, RPFieldSep(_)), expr)
    | (ARList(_, LPOpen), expr)
    | (ARList(_, LPClose), expr)
    | (ARTuple(_, TPClose), expr)
    | (ARTuple(_, TPOpen), expr)
    | (ARFlag(_, FPWhenKeyword), expr)
    | (ARFlag(_, FPEnabledKeyword), expr) =>
      /* We could alternatively move by a single character instead,
           which is what the old version did with minor exceptions;
           that isn't particularly useful as typing within these
           is meaningless and we want this to bring you to a location
           where you can meaningfully type. */
      Some(Expr(expr), {astRef: currAstRef, offset: 0})

    // Anything else is unexpected; this satisfies the exhaustiveness check
    | (ARBinOp(_), _)
    | (ARBool(_), _)
    | (ARConstructor(_), _)
    | (ARFieldAccess(_, FAPFieldname), _)
    | (ARFieldAccess(_, FAPFieldOp), _)
    | (ARFloat(_, FPFractional), _)
    | (ARFloat(_, FPPoint), _)
    | (ARFloat(_, FPWhole), _)
    | (ARFnCall(_), _)
    | (ARIf(_, IPIfKeyword), _)
    | (ARInteger(_), _)
    | (ARLambda(_, LBPSymbol), _)
    | (ARLambda(_, LBPComma(_)), _)
    | (ARLambda(_, LBPVarName(_)), _)
    | (ARLet(_, LPKeyword), _)
    | (ARLet(_, LPVarName), _)
    | (ARList(_, LPComma(_)), _)
    | (ARTuple(_, TPComma(_)), _)
    | (ARMatch(_, MPKeyword), _)
    | (ARNull(_), _)
    | (ARPartial(_), _)
    | (ARPipe(_, _), _)
    | (ARRecord(_, RPFieldname(_)), _)
    | (ARRightPartial(_), _)
    | (ARLeftPartial(_), _)
    | (ARString(_, SPOpenQuote), _)
    | (ARVariable(_), _)
    | /*
     * Non-exprs
     */
    (ARPattern(_), _)
    | (ARInvalid, _) =>
      recover("doExplicitBackspace - unexpected expr", ~debug=show_astRef(currAstRef), None)
    }
  }

  let doPatternBackspace = (
    patContainerRef: ref<option<id>>,
    currAstRef: astRef,
    mID: id,
    pat: fluidPattern,
  ): option<(E.fluidPatOrExpr, caretTarget)> => {
    let mkPBlank = (): option<(E.fluidPatOrExpr, caretTarget)> => {
      let bID = gid()
      Some(Pat(mID, PBlank(bID)), {astRef: ARPattern(bID, PPBlank), offset: 0})
    }

    switch (currAstRef, pat) {
    | (ARPattern(_, PPBlank), PBlank(pID)) =>
      if currOffset == 0 {
        switch FluidAST.find(mID, ast) {
        | Some(EMatch(_, cond, patterns)) =>
          patContainerRef := Some(mID)
          patterns
          |> /* FIXME: This is super broken because the pattern id could be anywhere
           but we only check at the pattern root */
          List.findIndex(~f=(_, (p, _)) => P.toID(p) == pID)
          |> Option.map(~f=((remIdx, _)) => {
            let newPatterns = if List.length(patterns) == 1 {
              patterns
            } else {
              List.removeAt(patterns, ~index=remIdx)
            }

            let targetExpr =
              patterns
              |> List.getAt(~index=remIdx - 1)
              |> Option.map(~f=((_, e)) => e)
              |> Option.unwrap(~default=cond)

            let target = caretTargetForEndOfExpr'(targetExpr)
            (E.Expr(EMatch(mID, cond, newPatterns)), target)
          })
        | _ => recover("doExplicitBackspace PPBlank", None)
        }
      } else {
        Some(Pat(mID, PBlank(pID)), {astRef: currAstRef, offset: 0})
      }
    | (ARPattern(_, PPVariable), PVariable(pID, oldName)) =>
      patContainerRef := Some(mID)
      let newName = mutation(oldName)
      let (newPat, target) = if newName == "" {
        (PBlank(pID), {astRef: ARPattern(pID, PPBlank), offset: 0})
      } else {
        (PVariable(pID, newName), currCTMinusOne)
      }

      switch FluidAST.find(mID, ast) {
      | Some(EMatch(_, cond, cases)) =>
        let rec run = p =>
          if pID == P.toID(p) {
            newPat
          } else {
            recursePattern(~f=run, p)
          }

        let newCases = List.map(cases, ~f=((pat, body)) =>
          if P.findPattern(pID, pat) != None {
            (run(pat), E.renameVariableUses(~oldName, ~newName, body))
          } else {
            (pat, body)
          }
        )

        Some(Expr(EMatch(mID, cond, newCases)), target)
      | _ => recover("doExplicitBackspace PVariable", None)
      }
    | (ARPattern(_, PPNull), PNull(_)) =>
      let str = mutation("null")
      let newID = gid()
      Some(
        Pat(mID, PVariable(newID, str)),
        {astRef: ARPattern(newID, PPVariable), offset: currOffset - 1},
      )
    | (ARPattern(_, PPBool), PBool(_, bool)) =>
      let str = if bool {
        "true"
      } else {
        "false"
      }
      let newStr = mutation(str)
      let newID = gid()
      Some(
        Pat(mID, PVariable(newID, newStr)),
        {astRef: ARPattern(newID, PPVariable), offset: currOffset - 1},
      )
    | (ARPattern(_, PPInteger), PInteger(pID, int)) =>
      let str = int->Int64.to_string->mutation
      if str == "" {
        mkPBlank()
      } else {
        let coerced = Util.coerceStringTo64BitInt(str)
        if coerced == int {
          None
        } else {
          Some(Pat(mID, PInteger(pID, coerced)), currCTMinusOne)
        }
      }
    | (ARPattern(_, PPConstructor), PConstructor(_, str, _patterns)) =>
      let str = str |> mutation |> String.trim
      let newID = gid()
      if str == "" {
        Some(Pat(mID, PBlank(newID)), {astRef: ARPattern(newID, PPBlank), offset: 0})
      } else {
        Some(
          Pat(mID, PVariable(newID, str)),
          {astRef: ARPattern(newID, PPVariable), offset: currOffset - 1},
        )
      }
    //
    // Floats
    //
    | (ARPattern(_, PPFloat(FPWhole)), PFloat(pID, sign, whole, frac)) =>
      let (sign, whole) = Sign.combine(sign, whole)->mutation->Sign.split
      Some(Pat(mID, PFloat(pID, sign, whole, frac)), currCTMinusOne)
    | (ARPattern(_, PPFloat(FPPoint)), PFloat(_, sign, whole, frac)) =>
      // TODO: If the float only consists of a . and has no whole or frac,
      // it should become a blank. Instead, it currently becomes a 0, which is weird.
      let i = Util.coerceStringTo64BitInt(Sign.toString(sign) ++ whole ++ frac)
      let iID = gid()
      Some(
        Pat(mID, PInteger(iID, i)),
        {astRef: ARPattern(iID, PPInteger), offset: String.length(whole)},
      )
    | (ARPattern(_, PPFloat(FPFractional)), PFloat(pID, sign, whole, frac)) =>
      Some(Pat(mID, PFloat(pID, sign, whole, mutation(frac))), currCTMinusOne)
    //
    // Strings
    //
    | (ARPattern(_, PPString(kind)), PString(id, str)) =>
      let strRelOffset = switch kind {
      | SPOpenQuote => currOffset - 1
      }
      if strRelOffset == 0 && str == "" {
        mkPBlank()
      } else {
        let str = str |> mutationAt(~index=strRelOffset - 1)
        Some(Pat(mID, PString(id, str)), CT.forPPStringOpenQuote(id, strRelOffset))
      }
    /* ****************
     * Exhaustiveness
     */
    | (ARPattern(_, PPBlank), _)
    | (ARPattern(_, PPBool), _)
    | (ARPattern(_, PPConstructor), _)
    | (ARPattern(_, PPFloat(FPFractional)), _)
    | (ARPattern(_, PPFloat(FPPoint)), _)
    | (ARPattern(_, PPFloat(FPWhole)), _)
    | (ARPattern(_, PPInteger), _)
    | (ARPattern(_, PPNull), _)
    | (ARPattern(_, PPString(SPOpenQuote)), _)
    | (ARPattern(_, PPVariable), _)
    | /*
     * non-patterns
     */
    (ARBinOp(_), _)
    | (ARBlank(_), _)
    | (ARBool(_), _)
    | (ARConstructor(_), _)
    | (ARFieldAccess(_), _)
    | (ARFloat(_), _)
    | (ARFnCall(_), _)
    | (ARIf(_), _)
    | (ARInteger(_), _)
    | (ARLambda(_), _)
    | (ARLet(_), _)
    | (ARList(_), _)
    | (ARTuple(_), _)
    | (ARMatch(_), _)
    | (ARNull(_), _)
    | (ARPartial(_), _)
    | (ARPipe(_), _)
    | (ARRecord(_), _)
    | (ARRightPartial(_), _)
    | (ARLeftPartial(_), _)
    | (ARString(_), _)
    | (ARVariable(_), _)
    | (ARFlag(_), _)
    | (ARInvalid, _) =>
      recover("doExplicitBackspace - unexpected pat", ~debug=show_astRef(currAstRef), None)
    }
  }

  /* FIXME: This is an ugly hack so we can modify match branches when editing a pattern.
     There's probably a nice way to do this without a ref, but that's a bigger change.
 */
  let patContainerRef: ref<option<id>> = ref(None)
  idOfASTRef(currAstRef)
  |> Option.andThen(~f=patOrExprID =>
    switch E.findExprOrPat(patOrExprID, Expr(FluidAST.toExpr(ast))) {
    | Some(patOrExpr) => Some(patOrExprID, patOrExpr)
    | None => None
    }
  )
  |> Option.andThen(~f=((patOrExprID, patOrExpr)) => {
    let maybeTransformedExprAndCaretTarget = switch patOrExpr {
    | E.Pat(mID, pat) => doPatternBackspace(patContainerRef, currAstRef, mID, pat)
    | E.Expr(expr) => doExprBackspace(currAstRef, expr)
    }

    switch maybeTransformedExprAndCaretTarget {
    | Some(Expr(newExpr), target) =>
      let patOrExprID = switch patContainerRef.contents {
      | None => patOrExprID
      | Some(mID) => mID
      }

      Some(FluidAST.replace(patOrExprID, ~replacement=newExpr, ast), AtTarget(target))
    | Some(Pat(mID, newPat), target) =>
      let newAST = replacePattern(mID, patOrExprID, ~newPat, ast)
      Some(newAST, AtTarget(target))
    | None => None
    }
  })
  |> Option.unwrap(~default=(ast, SamePlace))
}

let doBackspace = (~pos: int, ti: T.tokenInfo, astInfo: ASTInfo.t): ASTInfo.t => {
  let astInfo = recordAction(~pos, "doBackspace", astInfo)
  let (newAST, newPosition) = switch caretTargetFromTokenInfo(pos, ti) {
  | Some(ct) => doExplicitBackspace(ct, astInfo.ast)
  | None => (astInfo.ast, Exactly(ti.startPos))
  }

  let astInfo = ASTInfo.setAST(newAST, astInfo)
  let newPos = adjustPosForReflow(ti, pos, newPosition, astInfo)
  astInfo |> updatePosAndAC(newPos)
}

let doDelete = (~pos: int, ti: T.tokenInfo, astInfo: ASTInfo.t): ASTInfo.t => {
  /* Delete is approximately the same as backspace 1 place ahead,
   * as long as we process the token after the caret.
   * The only real difference is with multi-syllable extended grapheme clusters
   * like गा, which we currently fail to support, much as we currently fail to support
   * multi-codepoint emoji (the expected behavior of multi-syllable clusters differs from emoji).
   *
   * Note that we do not handle caret affinity properly, but caret affinity should behave the
   * same for backspace and delete.
   *
   * See https://www.notion.so/darklang/Keyboard-and-Input-Handling-44eeedc4953846159e96af1e979004ad.
   */
  let astInfo = recordAction(~pos, "doDelete", astInfo)
  let (newAST, newPosition) = switch caretTargetFromTokenInfo(pos, ti) {
  | Some(ct) => doExplicitBackspace({...ct, offset: ct.offset + 1}, astInfo.ast)
  | None => (astInfo.ast, SamePlace)
  }

  // Delete should only move the caret if the AST changed; that is a difference from backspace
  let (astInfo, newPosition) = if newAST == astInfo.ast {
    (astInfo, SamePlace)
  } else {
    (ASTInfo.setAST(newAST, astInfo), newPosition)
  }

  let newPos = adjustPosForReflow(ti, pos, newPosition, astInfo)
  {...astInfo, state: {...astInfo.state, newPos: newPos}}
}

/* [doExplicitInsert [extendedGraphemeCluster] [currCaretTarget] [ast]]
 * produces the (newAST, newPosition) tuple resulting from performing
 * a text insertion at [currCaretTarget] in the [ast].
 * Note that newPosition will be either AtTarget or SamePlace --
 * either the caret stays in the same place, or it ends up at a specific location.
 *
 * Note that there are some special-case inserts that aren't handled by doExplicitInsert.
 * See doInsert and updateKey for these exceptional cases.
 */
let doExplicitInsert = (
  props: props,
  extendedGraphemeCluster: string,
  currCaretTarget: caretTarget,
  ast: FluidAST.t,
): (FluidAST.t, newPosition) => {
  let {astRef: currAstRef, offset: currOffset} = currCaretTarget
  let caretDelta = extendedGraphemeCluster |> String.length
  let currCTPlusLen = {astRef: currAstRef, offset: currOffset + caretDelta}
  let mutation: string => string = str =>
    str |> String.insertAt(~index=currOffset, ~value=extendedGraphemeCluster)

  let mutationAt = (str: string, ~index: int): string =>
    str |> String.insertAt(~index, ~value=extendedGraphemeCluster)

  let doExprInsert = (currAstRef: astRef, currExpr: FluidExpression.t): option<(
    FluidExpression.t,
    caretTarget,
  )> => {
    let mkPartial = (str: string, oldExpr: FluidExpression.t): option<(
      FluidExpression.t,
      caretTarget,
    )> => {
      let newID = gid()
      Some(
        EPartial(newID, str, oldExpr),
        {astRef: ARPartial(newID), offset: currOffset + caretDelta},
      )
    }

    let mkLeftPartial: option<(E.t, caretTarget)> = {
      let id = gid()
      Some(
        ELeftPartial(id, extendedGraphemeCluster, currExpr),
        {astRef: ARLeftPartial(id), offset: currOffset + caretDelta},
      )
    }

    let maybeIntoLeftPartial: option<(
      E.t,
      caretTarget,
    )> = /* maybeIntoLeftPartial wraps the expression in a left partial only if it
     * is either 1) the top-level expression in the AST or 2) the first
     * expression directly inside a let. This means it's on the "left edge"
     * of the editor. */
    if VariantTesting.variantIsActive'(props.variants, LeftPartialVariant) {
      switch FluidAST.findParent(E.toID(currExpr), ast) {
      | None => mkLeftPartial
      | Some(ELet(_, _, _, body)) if currExpr == body => mkLeftPartial
      | _ => Some(currExpr, currCaretTarget)
      }
    } else {
      Some(currExpr, currCaretTarget)
    }

    switch (currAstRef, currExpr) {
    /* inserting at the beginning of these expressions wraps the expr in a left
     * partial under certain conditions (see maybeIntoLeftPartial)
     * https://www.notion.so/darklang/Allow-Lefthand-Partial-bc10233514cf4f93a564ee4f4fb83ee0 */
    | (ARVariable(_), EVariable(_))
    | (ARNull(_), ENull(_))
    | (ARBool(_), EBool(_))
    | (ARInteger(_), EInteger(_))
    | (ARFloat(_), EFloat(_))
    | (ARFnCall(_), EFnCall(_))
    | (ARString(_, SPOpenQuote), EString(_))
    | (ARList(_, LPOpen), EList(_))
    | (ARRecord(_, RPOpen), ERecord(_))
      if currCaretTarget.offset == 0 &&
        FluidUtil.isUnicodeLetter(
          extendedGraphemeCluster,
        ) => // only allow unicode letters, as we don't want symbols or numbers to create left partials
      maybeIntoLeftPartial
    | (ARString(_, kind), EString(id, str)) =>
      let len = String.length(str)
      let strRelOffset = switch kind {
      | SPOpenQuote => currOffset - 1
      }
      if strRelOffset < 0 || strRelOffset > len {
        // out of string bounds means you can't insert into the string
        None
      } else {
        let newStr = str |> mutationAt(~index=strRelOffset)
        Some(EString(id, newStr), CT.forARStringText(id, strRelOffset + caretDelta))
      }
    | (ARFloat(_, kind), EFloat(id, sign, whole, frac)) =>
      if FluidUtil.isNumber(extendedGraphemeCluster) {
        let (isWhole, index) = switch kind {
        | FPWhole => (true, currOffset)
        | FPFractional => (false, currOffset)
        | FPPoint =>
          if currCaretTarget.offset == 0 {
            (true, String.length(whole))
          } else {
            (false, 0)
          }
        }

        if isWhole {
          let newWhole = mutationAt(whole, ~index)
          // This enables |.67 -> 0|.67 but prevents |1.0 -> 0|1.0
          if String.slice(~from=0, ~to_=1, newWhole) == "0" && String.length(newWhole) > 1 {
            None
          } else {
            Some(
              EFloat(id, sign, newWhole, frac),
              {astRef: ARFloat(id, FPWhole), offset: index + caretDelta},
            )
          }
        } else {
          let newFrac = mutationAt(frac, ~index)
          Some(
            EFloat(id, sign, whole, newFrac),
            {
              astRef: ARFloat(id, FPFractional),
              offset: index + caretDelta,
            },
          )
        }
      } else {
        None
      }
    | (ARLambda(_, LBPVarName(index)), ELambda(id, vars, expr)) =>
      vars
      |> List.getAt(~index)
      |> Option.andThen(~f=((_, oldName)) => {
        let newName = oldName |> mutation
        if FluidUtil.isValidIdentifier(newName) {
          let vars = List.updateAt(vars, ~index, ~f=((varId, _)) => (varId, newName))

          Some(ELambda(id, vars, E.renameVariableUses(~oldName, ~newName, expr)), currCTPlusLen)
        } else {
          None
        }
      })
    | (ARPartial(_), EPartial(id, oldStr, oldExpr)) =>
      let str = oldStr |> mutation |> String.trim
      if String.startsWith(~prefix="\"", str) && String.endsWith(~suffix="\"", str) {
        let newID = gid()
        Some(
          EString(newID, String.slice(~from=1, ~to_=-1, str)),
          CT.forARStringOpenQuote(newID, currOffset + caretDelta),
        )
      } else {
        Some(EPartial(id, str, oldExpr), currCTPlusLen)
      }
    | (ARRightPartial(_), ERightPartial(id, oldStr, oldValue)) =>
      let str = oldStr |> mutation |> String.trim
      Some(ERightPartial(id, str, oldValue), currCTPlusLen)
    | (ARLeftPartial(_), ELeftPartial(id, str, expr)) =>
      // appending to a left partial appends to the string part
      let str = str |> mutation |> String.trim
      Some(ELeftPartial(id, str, expr), currCTPlusLen)
    | (ARBinOp(_), EBinOp(_, op, _, _, _) as oldExpr) =>
      let str = op |> FluidUtil.partialName |> mutation |> String.trim
      mkPartial(str, oldExpr)
    | (ARInteger(_), EInteger(id, int)) =>
      if currCaretTarget.offset == 0 && extendedGraphemeCluster == "0" {
        /* This prevents inserting leading 0s at the beginning of the int.
         * Note that Util.coerceStringTo64BitInt currently coerces strings with
         * leading "0"s to "0"; this prevents coerceStringTo64BitInt getting
         * a leading 0 in the first place. If Util.coerceStringTo64BitInt could
         * deal with leading 0s, we would still need this special case to deal with
         * caret placement, unless Util.coerceStringTo64BitInt preserved leading 0s.
         */
        None
      } else if Util.isNumber(extendedGraphemeCluster) {
        let str = int->Int64.to_string->mutation
        let coerced = Util.coerceStringTo64BitInt(str)
        if coerced == int {
          None
        } else {
          Some(EInteger(id, coerced), currCTPlusLen)
        }
      } else if extendedGraphemeCluster == "." {
        let newID = gid()
        let (whole, frac) = String.splitAt(~index=currOffset, Int64.to_string(int))
        Some(EFloat(newID, Positive, whole, frac), {astRef: ARFloat(newID, FPPoint), offset: 1})
      } else {
        None
      }
    | (ARRecord(_, RPFieldname(index)), ERecord(id, nameValPairs)) =>
      List.getAt(~index, nameValPairs) |> Option.andThen(~f=((name, _)) => {
        let newName = mutation(name)
        if FluidUtil.isValidRecordLiteralFieldName(newName) {
          let nameValPairs = List.updateAt(nameValPairs, ~index, ~f=((_, expr)) => (newName, expr))

          Some(ERecord(id, nameValPairs), currCTPlusLen)
        } else {
          None
        }
      })
    | (ARFieldAccess(_, FAPFieldname), EFieldAccess(_, _, fieldName) as oldExpr) =>
      let newName = mutation(fieldName)
      if FluidUtil.isValidIdentifier(newName) {
        mkPartial(newName, oldExpr)
      } else {
        None
      }
    | (ARFieldAccess(_, FAPFieldOp), old) =>
      recover(
        "doExplicitInsert - ARFieldAccess-FAPFieldOp is unhandled and doesn't seem to happen in practice",
        ~debug=old,
        None,
      )
    | (ARVariable(_), EVariable(_, varName)) =>
      // inserting in the middle or at the end of a variable turns it into a partial
      mkPartial(mutation(varName), currExpr)
    | (ARNull(_), ENull(_)) =>
      // inserting in the middle or at the end of null turns it into a partial
      mkPartial(mutation("null"), currExpr)
    | (ARBool(_), EBool(_, bool)) =>
      // inserting in the middle or at the end of a bool turns it into a partial
      let str = if bool {
        "true"
      } else {
        "false"
      }
      mkPartial(mutation(str), currExpr)
    | (ARLet(_, LPVarName), ELet(id, oldName, value, body)) =>
      let newName = mutation(oldName)
      if FluidUtil.isValidIdentifier(newName) {
        Some(
          ELet(id, newName, value, E.renameVariableUses(~oldName, ~newName, body)),
          currCTPlusLen,
        )
      } else {
        None
      }
    | (ARBlank(_), _) => maybeInsertInBlankExpr(extendedGraphemeCluster)
    | (ARFnCall(_), EFnCall(_, fnName, _, _)) =>
      // inserting in the middle or at the end of a fn call creates a partial
      mkPartial(mutation(fnName), currExpr)
    /*
     * Things you can't edit but probably should be able to edit
     */
    | (ARConstructor(_), EConstructor(_)) => None
    /*
     * Immutable keywords and symbols
     */
    | (ARLambda(_, LBPComma(_)), _)
    | (ARLambda(_, LBPSymbol), _)
    | (ARLambda(_, LBPArrow), _)
    | (ARRecord(_, RPOpen), _)
    | (ARRecord(_, RPFieldSep(_)), _)
    | (ARRecord(_, RPClose), _)
    | (ARList(_, LPOpen), _)
    | (ARList(_, LPComma(_)), _)
    | (ARList(_, LPClose), _)
    | (ARTuple(_, TPOpen), _)
    | (ARTuple(_, TPComma(_)), _)
    | (ARTuple(_, TPClose), _)
    | (ARLet(_, LPKeyword), _)
    | (ARLet(_, LPAssignment), _)
    | (ARIf(_, IPIfKeyword), _)
    | (ARIf(_, IPThenKeyword), _)
    | (ARIf(_, IPElseKeyword), _)
    | (ARPipe(_, _), _)
    | (ARMatch(_, MPKeyword), _)
    | (ARMatch(_, MPBranchArrow(_)), _)
    | (ARFlag(_, FPWhenKeyword), _)
    | (ARFlag(_, FPEnabledKeyword), _) =>
      None
    /* ****************
     * Exhaustiveness
     */
    | (ARBinOp(_), _)
    | (ARBool(_), _)
    | (ARConstructor(_), _)
    | (ARFieldAccess(_, FAPFieldname), _)
    | (ARFloat(_, FPFractional), _)
    | (ARFloat(_, FPPoint), _)
    | (ARFloat(_, FPWhole), _)
    | (ARFnCall(_), _)
    | (ARInteger(_), _)
    | (ARLambda(_, LBPVarName(_)), _)
    | (ARLet(_, LPVarName), _)
    | (ARNull(_), _)
    | (ARPartial(_), _)
    | (ARRecord(_, RPFieldname(_)), _)
    | (ARRightPartial(_), _)
    | (ARLeftPartial(_), _)
    | (ARString(_, SPOpenQuote), _)
    | (ARVariable(_), _)
    | /*
     * Non-exprs
     */
    (ARPattern(_), _)
    | (ARInvalid, _) =>
      recover("doExplicitInsert - unexpected expr", ~debug=show_astRef(currAstRef), None)
    }
  }

  let doPatInsert = (
    patContainerRef: ref<option<id>>,
    currAstRef: astRef,
    mID: id,
    pat: fluidPattern,
  ): option<(E.fluidPatOrExpr, caretTarget)> =>
    switch (currAstRef, pat) {
    | (ARPattern(_, PPFloat(kind)), PFloat(pID, sign, whole, frac)) =>
      if FluidUtil.isNumber(extendedGraphemeCluster) {
        let (isWhole, index) = switch kind {
        | FPWhole => (true, currOffset)
        | FPFractional => (false, currOffset)
        | FPPoint =>
          if currCaretTarget.offset == 0 {
            (true, String.length(whole))
          } else {
            (false, 0)
          }
        }

        if isWhole {
          let newWhole = mutationAt(whole, ~index)
          // This enables |.67 -> 0|.67 but prevents |1.0 -> 0|1.0
          if String.slice(~from=0, ~to_=1, newWhole) == "0" && String.length(newWhole) > 1 {
            None
          } else {
            Some(
              Pat(mID, PFloat(pID, sign, newWhole, frac)),
              {
                astRef: ARPattern(pID, PPFloat(FPWhole)),
                offset: index + caretDelta,
              },
            )
          }
        } else {
          let newFrac = mutationAt(frac, ~index)
          Some(
            Pat(mID, PFloat(pID, sign, whole, newFrac)),
            {
              astRef: ARPattern(pID, PPFloat(FPFractional)),
              offset: index + caretDelta,
            },
          )
        }
      } else {
        None
      }
    | (ARPattern(_, PPNull), PNull(_)) =>
      let str = mutation("null")
      let newID = gid()
      if FluidUtil.isValidIdentifier(str) {
        Some(
          Pat(mID, PVariable(newID, str)),
          {
            astRef: ARPattern(newID, PPVariable),
            offset: currOffset + caretDelta,
          },
        )
      } else {
        None
      }
    | (ARPattern(_, PPBool), PBool(_, bool)) =>
      let str = if bool {
        "true"
      } else {
        "false"
      }
      let newStr = mutation(str)
      let newID = gid()
      if FluidUtil.isValidIdentifier(str) {
        Some(
          Pat(mID, PVariable(newID, newStr)),
          {
            astRef: ARPattern(newID, PPVariable),
            offset: currOffset + caretDelta,
          },
        )
      } else {
        None
      }
    | (ARPattern(_, PPInteger), PInteger(pID, int)) =>
      if currCaretTarget.offset == 0 && extendedGraphemeCluster == "0" {
        /* This prevents inserting leading 0s at the beginning of the int.
         * Note that Util.coerceStringTo64BitInt currently coerces strings with
         * leading "0"s to "0"; this prevents coerceStringTo64BitInt getting
         * a leading 0 in the first place. If Util.coerceStringTo64BitInt could
         * deal with leading 0s, we would still need this special case to deal with
         * caret placement, unless Util.coerceStringTo64BitInt preserved leading 0s.
         */
        None
      } else if Util.isNumber(extendedGraphemeCluster) {
        let str = int->Int64.to_string->mutation
        let coerced = Util.coerceStringTo64BitInt(str)
        if coerced == int {
          None
        } else {
          Some(Pat(mID, PInteger(pID, coerced)), currCTPlusLen)
        }
      } else if extendedGraphemeCluster == "." {
        let newID = gid()
        let (whole, frac) = String.splitAt(~index=currOffset, Int64.to_string(int))
        let (sign, whole) = Sign.split(whole)
        Some(
          Pat(mID, PFloat(newID, sign, whole, frac)),
          {astRef: ARPattern(newID, PPFloat(FPPoint)), offset: 1},
        )
      } else {
        None
      }
    | (ARPattern(_, PPString(kind)), PString(id, str)) =>
      let len = String.length(str)
      let strRelOffset = switch kind {
      | SPOpenQuote => currOffset - 1
      }
      if strRelOffset < 0 || strRelOffset > len {
        // out of string bounds means you can't insert into the string
        None
      } else {
        let str = str |> mutationAt(~index=strRelOffset)
        Some(Pat(mID, PString(id, str)), CT.forPPStringText(id, strRelOffset + caretDelta))
      }
    | (ARPattern(_, PPBlank), PBlank(_)) =>
      let newID = gid()
      if extendedGraphemeCluster == "\"" {
        Some(Pat(mID, PString(newID, "")), CT.forPPStringText(newID, 0))
      } else if Util.isNumber(extendedGraphemeCluster) {
        Some(
          Pat(mID, PInteger(newID, extendedGraphemeCluster |> Util.coerceStringTo64BitInt)),
          {astRef: ARPattern(newID, PPInteger), offset: caretDelta},
        )
      } else if FluidUtil.isIdentifierChar(extendedGraphemeCluster) {
        Some(
          Pat(mID, PVariable(newID, extendedGraphemeCluster)),
          {astRef: ARPattern(newID, PPVariable), offset: caretDelta},
        )
      } else {
        None
      }
    | (ARPattern(_, PPVariable), PVariable(pID, oldName)) =>
      let newName = mutation(oldName)
      if FluidUtil.isValidIdentifier(newName) {
        patContainerRef := Some(mID)
        let (newPat, target) = (PVariable(pID, newName), currCTPlusLen)

        switch FluidAST.find(mID, ast) {
        | Some(EMatch(_, cond, cases)) =>
          let rec run = p =>
            if pID == P.toID(p) {
              newPat
            } else {
              recursePattern(~f=run, p)
            }

          let newCases = List.map(cases, ~f=((pat, body)) =>
            if P.findPattern(pID, pat) != None {
              (run(pat), E.renameVariableUses(~oldName, ~newName, body))
            } else {
              (pat, body)
            }
          )

          Some(Expr(EMatch(mID, cond, newCases)), target)
        | _ => recover("doExplicitInsert PVariable", None)
        }
      } else {
        None
      }
    /*
     * Things you can't edit but probably should be able to edit
     */
    | (ARPattern(_, PPConstructor), _) => None
    /* ****************
     * Exhaustiveness
     */
    | (ARPattern(_, PPBlank), _)
    | (ARPattern(_, PPBool), _)
    | (ARPattern(_, PPFloat(FPFractional)), _)
    | (ARPattern(_, PPFloat(FPPoint)), _)
    | (ARPattern(_, PPFloat(FPWhole)), _)
    | (ARPattern(_, PPInteger), _)
    | (ARPattern(_, PPNull), _)
    | (ARPattern(_, PPString(SPOpenQuote)), _)
    | (ARPattern(_, PPVariable), _)
    | /*
     * non-patterns
     */
    (ARBinOp(_), _)
    | (ARBlank(_), _)
    | (ARBool(_), _)
    | (ARConstructor(_), _)
    | (ARFieldAccess(_), _)
    | (ARFloat(_), _)
    | (ARFnCall(_), _)
    | (ARIf(_), _)
    | (ARInteger(_), _)
    | (ARLambda(_), _)
    | (ARLet(_), _)
    | (ARList(_), _)
    | (ARTuple(_), _)
    | (ARMatch(_), _)
    | (ARNull(_), _)
    | (ARPartial(_), _)
    | (ARPipe(_), _)
    | (ARRecord(_), _)
    | (ARRightPartial(_), _)
    | (ARLeftPartial(_), _)
    | (ARString(_), _)
    | (ARVariable(_), _)
    | (ARFlag(_), _)
    | (ARInvalid, _) =>
      recover("doExplicitInsert - unexpected pat", ~debug=show_astRef(currAstRef), None)
    }

  /* FIXME: This is an ugly hack so we can modify match branches when editing a pattern.
     There's probably a nice way to do this without a ref, but that's a bigger change.
 */
  let patContainerRef: ref<option<id>> = ref(None)
  idOfASTRef(currAstRef)
  |> Option.andThen(~f=patOrExprID =>
    switch E.findExprOrPat(patOrExprID, Expr(FluidAST.toExpr(ast))) {
    | Some(patOrExpr) => Some(patOrExprID, patOrExpr)
    | None => None
    }
  )
  |> Option.andThen(~f=((patOrExprID, patOrExpr)) => {
    let maybeTransformedExprAndCaretTarget = switch patOrExpr {
    | E.Pat(mID, pat) => doPatInsert(patContainerRef, currAstRef, mID, pat)
    | E.Expr(expr) =>
      switch doExprInsert(currAstRef, expr) {
      | None => None
      | Some(expr, ct) => Some(Expr(expr), ct)
      }
    }

    switch maybeTransformedExprAndCaretTarget {
    | Some(Expr(newExpr), target) =>
      let patOrExprID = switch patContainerRef.contents {
      | None => patOrExprID
      | Some(mID) => mID
      }

      Some(FluidAST.replace(patOrExprID, ~replacement=newExpr, ast), AtTarget(target))
    | Some(Pat(mID, newPat), target) =>
      let newAST = replacePattern(mID, patOrExprID, ~newPat, ast)
      Some(newAST, AtTarget(target))
    | None => None
    }
  })
  |> Option.unwrap(~default=(ast, SamePlace))
}

let doInsert = (~pos: int, letter: string, ti: T.tokenInfo, astInfo: ASTInfo.t): ASTInfo.t => {
  let astInfo =
    astInfo
    |> recordAction(~pos, "doInsert")
    |> ASTInfo.modifyState(~f=s => {...s, upDownCol: None})

  let (newAST, newPosition) = switch caretTargetFromTokenInfo(pos, ti) {
  | Some(ct) => doExplicitInsert(astInfo.props, letter, ct, astInfo.ast)
  | None => (astInfo.ast, SamePlace)
  }

  let astInfo = ASTInfo.setAST(newAST, astInfo)
  let newPos = adjustPosForReflow(ti, pos, newPosition, astInfo)
  {...astInfo, state: {...astInfo.state, newPos: newPos}}
}

@ocaml.doc(" [doInfixInsert ~pos infixTxt ti ast s] produces the
 * (newAST, newState) tuple resulting from performing
 * the insertion of the string [infixTxt], which we know
 * conforms to FluidTextInput.isInfixSymbol, within the
 * token with token info [ti], knowing that the [ast]-relative
 * caret position is [pos] and that the current state is [s].
 *
 * In most cases, we wrap the expr indicated by the [ti]
 * in a partial, particularly if we are at the end of an expr.
 * Otherwise (especially if we are in the middle of an expr), we
 * defer to the behavior of doInsert.
 ")
let doInfixInsert = (~pos, infixTxt: string, ti: T.tokenInfo, astInfo: ASTInfo.t): ASTInfo.t => {
  let astInfo =
    astInfo
    |> recordAction(~pos, "doInfixInsert")
    |> ASTInfo.modifyState(~f=s => {...s, upDownCol: None})

  caretTargetFromTokenInfo(pos, ti)
  |> Option.andThen(~f=ct =>
    idOfASTRef(ct.astRef) |> Option.andThen(~f=id =>
      switch FluidAST.find(id, astInfo.ast) {
      | Some(expr) => Some(id, ct, expr)
      | None => None
      }
    )
  )
  |> Option.andThen(~f=((id, ct, expr)) =>
    switch (ct.astRef, expr) {
    | (ARInteger(_), expr)
    | (ARBool(_), expr)
    | (ARFieldAccess(_, FAPFieldname), expr)
    | (ARString(_, SPOpenQuote), expr)
    | (ARFloat(_), expr)
    | (ARNull(_), expr)
    | (ARVariable(_), expr)
    | (ARList(_), expr)
    | (ARTuple(_), expr)
    | (ARRecord(_), expr)
    | /* This works for function calls because
     * the caretTargetForEndOfExpr' of a function
     * call will only line up with the
     * (caretTargetFromTokenInfo pos ti) if all its
     * arguments have been filled (because it has none or
     * because they're filled with pipe targets)
     */
    (ARFnCall(_), expr) =>
      if caretTargetForEndOfExpr'(expr) == ct {
        let newID = gid()
        Some(
          id,
          ERightPartial(newID, infixTxt, expr),
          {
            astRef: ARRightPartial(newID),
            offset: String.length(infixTxt),
          },
        )
      } else {
        None
      }
    | (ARBlank(_), expr) =>
      let newID = gid()
      Some(
        id,
        EPartial(newID, infixTxt, expr),
        {astRef: ARPartial(newID), offset: String.length(infixTxt)},
      )
    | (ARPipe(_, index), EPipe(id, e1, e2, rest)) =>
      let parID = gid()
      switch index {
      | 0 =>
        let newExpr = EPartial(parID, infixTxt, e2)
        Some(
          id,
          EPipe(id, e1, newExpr, rest),
          {astRef: ARPartial(parID), offset: String.length(infixTxt)},
        )
      | _ =>
        switch rest |> List.getAt(~index=index - 1) {
        | Some(pipedInto) =>
          /* Note that this essentially destroys the pipedInto
           * expression, because it becomes overwritten by the
           * partial. Handling this properly would involve
           * introducing a new construct -- perhaps a left partial. */
          let newExpr = EPartial(parID, infixTxt, pipedInto)
          let newPipeExprs = rest |> List.updateAt(~index=index - 1, ~f=_ => newExpr)

          Some(
            id,
            EPipe(id, e1, e2, newPipeExprs),
            {astRef: ARPartial(parID), offset: String.length(infixTxt)},
          )
        | None => None
        }
      }
    // Exhaustiveness
    | (ARPipe(_), _) => None
    // Don't insert
    | (ARFieldAccess(_, FAPFieldOp), _)
    | (ARLet(_), _)
    | (ARIf(_), _)
    | (ARBinOp(_), _)
    | (ARPartial(_), _)
    | (ARRightPartial(_), _)
    | (ARLeftPartial(_), _)
    | (ARConstructor(_), _)
    | (ARMatch(_), _)
    | (ARLambda(_), _)
    | (ARPattern(_), _)
    | (ARFlag(_), _) =>
      None
    | (ARInvalid, _) => None
    }
  )
  |> Option.map(~f=((replaceID, newExpr, newCaretTarget)) =>
    astInfo
    |> ASTInfo.setAST(FluidAST.replace(replaceID, ~replacement=newExpr, astInfo.ast))
    |> moveToCaretTarget(newCaretTarget)
  )
  |> Option.orElseLazy(() => Some(doInsert(~pos, infixTxt, ti, astInfo)))
  |> recoverOpt("doInfixInsert - can't return None due to lazy Some", ~default=astInfo)
}

let wrapInLet = (ti: T.tokenInfo, astInfo: ASTInfo.t): ASTInfo.t => {
  let astInfo = recordAction("wrapInLet", astInfo)
  let id = T.tid(ti.token)
  switch FluidAST.find(id, astInfo.ast) {
  | Some(expr) =>
    let bodyId = gid()
    let exprToWrap = switch findAppropriateParentToWrap(expr, astInfo.ast) {
    | Some(e) => e
    | None => expr
    }

    let replacement = ELet(gid(), "_", exprToWrap, EBlank(bodyId))
    astInfo
    |> ASTInfo.setAST(FluidAST.replace(~replacement, E.toID(exprToWrap), astInfo.ast))
    |> moveToCaretTarget({astRef: ARBlank(bodyId), offset: 0})
  | None => astInfo
  }
}

let orderRangeFromSmallToBig = ((rangeBegin, rangeEnd): (int, int)): (int, int) =>
  if rangeBegin > rangeEnd {
    (rangeEnd, rangeBegin)
  } else {
    (rangeBegin, rangeEnd)
  }

let updateSelectionRange = (newPos: int, astInfo: ASTInfo.t): ASTInfo.t =>
  ASTInfo.modifyState(astInfo, ~f=s => {
    ...s,
    newPos: newPos,
    selectionStart: Some(s.selectionStart |> Option.unwrap(~default=s.newPos)),
  })

let getOptionalSelectionRange = (s: fluidState): option<(int, int)> => {
  let endIdx = s.newPos
  switch s.selectionStart {
  | Some(beginIdx) => Some(beginIdx, endIdx)
  | None => None
  }
}

let tokensInRange = (selStartPos: int, selEndPos: int, astInfo: ASTInfo.t): tokenInfos =>
  astInfo
  |> ASTInfo.activeTokenInfos
  |> // this condition is a little flaky, sometimes selects wrong tokens
  List.filter(~f=t =>
    // selectionStart within token
    (t.startPos <= selStartPos && selStartPos < t.endPos) ||
      // selectionEnd within token
      ((t.startPos < selEndPos && selEndPos <= t.endPos) ||
      (// tokenStart within selection
      (selStartPos <= t.startPos && t.startPos < selEndPos) ||
        (// tokenEnd within selection
        selStartPos < t.endPos && t.endPos <= selEndPos)))
  )

let getTopmostSelectionID = (startPos: int, endPos: int, astInfo: ASTInfo.t): option<id> => {
  let (startPos, endPos) = orderRangeFromSmallToBig((startPos, endPos))
  // TODO: if there's multiple topmost IDs, return parent of those IDs
  tokensInRange(startPos, endPos, astInfo)
  |> List.filter(~f=ti => !T.isNewline(ti.token))
  |> List.fold(~initial=(None, 0), ~f=((topmostID, topmostDepth), ti) => {
    let curID = T.parentExprID(ti.token)
    let curDepth = FluidAST.ancestors(curID, astInfo.ast) |> List.length
    if (
      /* check if current token is higher in the AST than the last token,
       * or if there's no topmost ID yet */
      (curDepth < topmostDepth || topmostID == None) &&
        /* account for tokens that don't have ancestors (depth = 0)
         * but are not the topmost expression in the AST */
        !(curDepth == 0 && FluidAST.find(curID, astInfo.ast) !== Some(FluidAST.toExpr(astInfo.ast)))
    ) {
      (Some(curID), curDepth)
    } else {
      (topmostID, topmostDepth)
    }
  })
  |> Tuple2.first
}

let getSelectedExprID = (astInfo: ASTInfo.t): option<id> =>
  getOptionalSelectionRange(astInfo.state) |> Option.andThen(~f=((startPos, endPos)) =>
    getTopmostSelectionID(startPos, endPos, astInfo)
  )

let maybeOpenCmd = (m: Types.model): Types.modification => {
  let getExprIDOnCaret = (astInfo: ASTInfo.t) =>
    switch ASTInfo.getTokenNotWhitespace(astInfo) {
    | Some(ti) =>
      let id = T.tid(ti.token)
      if T.validID(id) {
        Some(id)
      } else {
        None
      }
    | None => None
    }

  let mod' = switch CursorState.tlidOf(m.cursorState) {
  | Some(tlid) =>
    astInfoFromModelAndTLID(m, tlid)
    |> Option.andThen(~f=astInfo =>
      getSelectedExprID(astInfo) |> Option.orElseLazy(_ => getExprIDOnCaret(astInfo))
    )
    |> Option.map(~f=id => FluidCommandsShow(tlid, id))
  | None => None
  }

  Option.unwrap(mod', ~default=NoChange)
}

let rec updateKey = (~recursing=false, inputEvent: fluidInputEvent, astInfo: ASTInfo.t) => {
  // These might be the same token
  let origAstInfo = astInfo
  let pos = astInfo.state.newPos
  let (toTheLeft, toTheRight, mNext) =
    astInfo |> ASTInfo.activeTokenInfos |> FluidTokenizer.getNeighbours(~pos)

  let onEdge = switch (toTheLeft, toTheRight) {
  | (L(lt, lti), R(rt, rti)) => (lt, lti) != (rt, rti)
  | _ => true
  }

  let keyIsInfix = switch inputEvent {
  | InsertText(txt) if FluidTextInput.isInfixSymbol(txt) => true
  | _ => false
  }

  /* TODO: When changing TVariable and TFieldName and probably TFnName we
   * should convert them to a partial which retains the old object */
  // Checks to see if the token is within an if-condition statement
  let isInIfCondition = token => {
    let rec recurseUp = (maybeExpr, prevId) =>
      switch maybeExpr {
      | Some(EIf(_, cond, _, _)) if E.toID(cond) == prevId => true
      | Some(e) =>
        let id = E.toID(e)
        recurseUp(FluidAST.findParent(id, astInfo.ast), id)
      | None => false
      }

    let tid = T.tid(token)
    recurseUp(FluidAST.findParent(tid, astInfo.ast), tid)
  }

  let astInfo = /* This match drives a big chunk of the change operations, but is
   * inconsistent about whether it looks left/right and also about what
   * conditions it applies to each of the tokens.
   *
   * The largest inconsistency is whether or not the case expresses "in this
   * exact case, do this exact thing" or "in this very general case, do this
   * thing". The mixing and matching of these two means the cases are very
   * sensitive to ordering. If you're adding a case that's sensitive to
   * ordering ADD A TEST, even if it's otherwise redundant from a product
   * POV. */
  switch (inputEvent, toTheLeft, toTheRight) {
  // **************
  // AUTOCOMPLETE
  // **************
  /* Note that these are spelt out explicitly on purpose, else they'll
   * trigger on the wrong element sometimes. */
  | (Keypress({key: K.Escape, _}), L(_, ti), _) if isAutocompleting(ti, astInfo.state) =>
    acClear(astInfo)
  | (Keypress({key: K.Escape, _}), _, R(_, ti)) if isAutocompleting(ti, astInfo.state) =>
    acClear(astInfo)
  | (Keypress({key: K.Up, _}), _, R(_, ti)) if isAutocompleting(ti, astInfo.state) =>
    acMoveUp(astInfo)
  | (Keypress({key: K.Up, _}), L(_, ti), _) if isAutocompleting(ti, astInfo.state) =>
    acMoveUp(astInfo)
  | (Keypress({key: K.Down, _}), _, R(_, ti)) if isAutocompleting(ti, astInfo.state) =>
    acMoveDown(astInfo)
  | (Keypress({key: K.Down, _}), L(_, ti), _) if isAutocompleting(ti, astInfo.state) =>
    acMoveDown(astInfo)
  /*
   * Autocomplete finish
   */
  | (Keypress({key, _}), L(_, ti), _)
    if isAutocompleting(ti, astInfo.state) &&
    list{K.Enter, K.Tab, K.ShiftTab, K.Space} |> List.member(~value=key) =>
    acEnter(ti, key, astInfo)
  | (Keypress({key, _}), _, R(_, ti))
    if isAutocompleting(ti, astInfo.state) &&
    list{K.Enter, K.Tab, K.ShiftTab, K.Space} |> List.member(~value=key) =>
    acEnter(ti, key, astInfo)
  /* When we type a letter/number after an infix operator, complete and
   * then enter the number/letter. */
  | (InsertText(txt), L(TRightPartial(_, _, _), ti), _) if onEdge && Util.isIdentifierChar(txt) =>
    let astInfo = acEnter(ti, K.Tab, astInfo)
    getLeftTokenAt(astInfo.state.newPos, astInfo |> ASTInfo.activeTokenInfos |> List.reverse)
    |> Option.map(~f=ti => doInsert(~pos, txt, ti, astInfo))
    |> Option.unwrap(~default=astInfo)
  /*
   * Special autocomplete entries
   */
  // Piping, with and without autocomplete menu open
  | (Keypress({key: K.ShiftEnter, _}), left, _) =>
    let doPipeline = (astInfo: ASTInfo.t): ASTInfo.t => {
      let (startPos, endPos) = FluidUtil.getSelectionRange(astInfo.state)
      let topmostSelectionID = getTopmostSelectionID(startPos, endPos, astInfo)

      let defaultTopmostSelection = switch topmostSelectionID {
      | Some(id) => (Some(id), startPos == endPos)
      | None => (None, startPos == endPos)
      }

      let (topmostID, findParent) = if startPos == endPos {
        let tokenAtLeft = getLeftTokenAt(astInfo.state.newPos, ASTInfo.activeTokenInfos(astInfo))

        switch tokenAtLeft {
        | Some(current) if T.isPipeable(current.token) => (Some(T.tid(current.token)), false)
        | _ => defaultTopmostSelection
        }
      } else {
        defaultTopmostSelection
      }

      Option.map(topmostID, ~f=id => {
        let (astInfo, blankId) = createPipe(~findParent, id, astInfo)
        switch blankId {
        | None => astInfo
        | Some(id) => moveToAstRef(ARBlank(id), astInfo)
        }
      }) |> Option.unwrap(~default=astInfo)
    }

    switch left {
    | L(TPartial(_), ti) | L(TFieldPartial(_), ti)
      if Option.is_some(AC.highlighted(astInfo.state.ac)) =>
      astInfo |> acEnter(ti, K.Enter) |> doPipeline
    | _ => doPipeline(astInfo)
    }
  // press dot while in a variable entry
  | (InsertText("."), L(TPartial(_), ti), _)
    if Option.map(~f=AC.isVariable, AC.highlighted(astInfo.state.ac)) == Some(true) =>
    acStartField(ti, astInfo)
  | (InsertText("."), L(TFieldPartial(_), ti), _)
  | (InsertText("."), _, R(TFieldPartial(_), ti))
    if Option.map(~f=AC.isField, AC.highlighted(astInfo.state.ac)) == Some(true) =>
    acStartField(ti, astInfo)
  // ******************
  // CARET NAVIGATION
  // ******************
  // Tab to next blank
  | (Keypress({key: K.Tab, _}), _, R(_, _))
  | (Keypress({key: K.Tab, _}), L(_, _), _) =>
    moveToNextEditable(pos, astInfo)
  | (Keypress({key: K.ShiftTab, _}), _, R(_, _))
  | (Keypress({key: K.ShiftTab, _}), L(_, _), _) =>
    moveToPrevEditable(pos, astInfo)
  // Left/Right movement
  | (Keypress({key: K.GoToEndOfWord(maintainSelection), _}), _, R(_, ti))
  | (Keypress({key: K.GoToEndOfWord(maintainSelection), _}), L(_, ti), _) =>
    if maintainSelection === K.KeepSelection {
      updateSelectionRange(getEndOfWordPos(pos, ti, astInfo), astInfo)
    } else {
      goToEndOfWord(pos, ti, astInfo)
    }
  | (Keypress({key: K.GoToStartOfWord(maintainSelection), _}), _, R(_, ti))
  | (Keypress({key: K.GoToStartOfWord(maintainSelection), _}), L(_, ti), _) =>
    if maintainSelection === K.KeepSelection {
      updateSelectionRange(getStartOfWordPos(pos, ti, astInfo), astInfo)
    } else {
      goToStartOfWord(pos, ti, astInfo)
    }
  | (Keypress({key: K.Left, _}), L(_, ti), _) => astInfo |> doLeft(~pos, ti) |> acMaybeShow(ti)
  | (Keypress({key: K.Right, _}), _, R(_, ti)) =>
    astInfo |> doRight(~pos, ~next=mNext, ti) |> acMaybeShow(ti)
  | (Keypress({key: K.GoToStartOfLine(maintainSelection), _}), _, R(_, ti))
  | (Keypress({key: K.GoToStartOfLine(maintainSelection), _}), L(_, ti), _) =>
    if maintainSelection === K.KeepSelection {
      updateSelectionRange(getStartOfLineCaretPos(ti, astInfo), astInfo)
    } else {
      moveToStartOfLine(ti, astInfo)
    }
  | (Keypress({key: K.GoToEndOfLine(maintainSelection), _}), _, R(_, ti)) =>
    if maintainSelection === K.KeepSelection {
      updateSelectionRange(getEndOfLineCaretPos(ti, astInfo), astInfo)
    } else {
      moveToEndOfLine(ti, astInfo)
    }
  | (Keypress({key: K.Up, _}), _, _) => doUp(~pos, astInfo)
  | (Keypress({key: K.Down, _}), _, _) => doDown(~pos, astInfo)
  // ***********
  // SELECTION
  // ***********
  | (Keypress({key: K.SelectAll, _}), _, R(_, _))
  | (Keypress({key: K.SelectAll, _}), L(_, _), _) =>
    selectAll(~pos, astInfo)
  // ***********
  // OVERWRITE
  // ***********
  | (ReplaceText(txt), _, _) => replaceText(txt, astInfo)
  // ***********
  // DELETION
  // ***********
  // Delete selection
  | (DeleteContentBackward, _, _) | (DeleteContentForward, _, _)
    if Option.isSome(astInfo.state.selectionStart) =>
    deleteSelection(astInfo)
  // Special-case hack for deleting rows of a match or record
  | (DeleteContentBackward, _, R(TRecordFieldname({fieldName: "", _}), ti))
  | (DeleteContentBackward, L(TNewline(_), _), R(TPatternBlank(_), ti)) =>
    doBackspace(~pos, ti, astInfo)
  | (DeleteContentBackward, L(_, ti), _) => doBackspace(~pos, ti, astInfo)
  // Special case for deleting blanks in front of a list
  | (DeleteContentForward, L(TListOpen(_), _), R(TBlank(_), rti)) =>
    /* If L is a TListOpen and R is a TBlank, mNext can be a comma or a list close.
     * In case of a list close, we just replace the expr with the empty list
     */
    switch mNext {
    | Some({token: TListClose(id, _), _}) =>
      astInfo
      |> ASTInfo.setAST(FluidAST.update(~f=_ => EList(id, list{}), id, astInfo.ast))
      |> moveToCaretTarget({astRef: ARList(id, LPOpen), offset: 1})
    | Some(ti) => doDelete(~pos, ti, astInfo)
    | None => doDelete(~pos, rti, astInfo)
    }

  | (DeleteContentForward, _, R(_, ti)) => doDelete(~pos, ti, astInfo)
  | (DeleteSoftLineBackward, _, R(_, ti))
  | (DeleteSoftLineBackward, L(_, ti), _) =>
    /* The behavior of this action is not well specified -- every editor we've seen has slightly different behavior.
           The behavior we use here is: if there is a selection, delete it instead of deleting to start of line (like XCode but not VSCode).
           For expedience, delete to the visual start of line rather than the "real" start of line. This is symmetric with
           K.DeleteToEndOfLine but does not match any code editors we've seen. It does match many non-code text editors. */
    switch getOptionalSelectionRange(astInfo.state) {
    | Some(selRange) => deleteCaretRange(selRange, astInfo)
    | None => deleteCaretRange((pos, getStartOfLineCaretPos(ti, astInfo)), astInfo)
    }
  | (DeleteSoftLineForward, _, R(_, ti)) | (DeleteSoftLineForward, L(_, ti), _) =>
    /* The behavior of this action is not well specified -- every editor we've seen has slightly different behavior.
           The behavior we use here is: if there is a selection, delete it instead of deleting to end of line (like XCode and VSCode).
           For expedience, in the presence of wrapping, delete to the visual end of line rather than the "real" end of line.
           This matches the behavior of XCode and VSCode. Most standard non-code text editors do not implement this command. */
    switch getOptionalSelectionRange(astInfo.state) {
    | Some(selRange) => deleteCaretRange(selRange, astInfo)
    | None => deleteCaretRange((pos, getEndOfLineCaretPos(ti, astInfo)), astInfo)
    }
  | (DeleteWordForward, _, R(_, ti)) =>
    switch getOptionalSelectionRange(astInfo.state) {
    | Some(selRange) => deleteCaretRange(selRange, astInfo)
    | None =>
      let movedState = goToEndOfWord(pos, ti, astInfo)
      let newAstInfo = deleteCaretRange((pos, movedState.state.newPos), astInfo)

      if newAstInfo.ast == astInfo.ast && newAstInfo.state.newPos == pos {
        ASTInfo.modifyState(newAstInfo, ~f=_ => movedState.state)
      } else {
        newAstInfo
      }
    }
  | (DeleteWordBackward, L(_, ti), _) =>
    switch getOptionalSelectionRange(astInfo.state) {
    | Some(selRange) => deleteCaretRange(selRange, astInfo)
    | None =>
      let rangeStart = if T.isStringToken(ti.token) && pos !== ti.startPos {
        getBegOfWordInStrCaretPos(~pos, ti)
      } else {
        ti.startPos
      }

      deleteCaretRange((rangeStart, pos), astInfo)
    }
  // **************************************
  // SKIPPING OVER SYMBOLS BY TYPING THEM
  // **************************************
  /*
   * Skipping over a lambda arrow with '->'
   */
  | (InsertText("-"), L(TLambdaVar(_), _), R(TLambdaArrow(_), ti)) =>
    // ___| -> ___ to ___ |-> ___
    moveOneRight(ti.startPos + 1, astInfo)
  | (InsertText("-"), L(TLambdaArrow(_), _), R(TLambdaArrow(_), ti)) if pos == ti.startPos + 1 =>
    // ___ |-> ___ to ___ -|> ___
    moveOneRight(ti.startPos + 1, astInfo)
  | (InsertText(">"), L(TLambdaArrow(_), _), R(TLambdaArrow(_), ti)) if pos == ti.startPos + 2 =>
    // ___ -|> ___ to ___ -> |___
    moveToNextNonWhitespaceToken(pos, astInfo)
  /*
   * Skipping over specific characters
   */
  | (InsertText("="), _, R(TLetAssignment(_), toTheRight)) => moveTo(toTheRight.endPos, astInfo)
  | (InsertText(":"), _, R(TRecordSep(_), toTheRight)) => moveTo(toTheRight.endPos, astInfo)
  | (Keypress({key: K.Space, _}), _, R(TSep(_), _)) => moveOneRight(pos, astInfo)
  // Pressing } to go over the last }
  | (InsertText("}"), _, R(TRecordClose(_), ti)) if pos == ti.endPos - 1 =>
    moveOneRight(pos, astInfo)
  // Pressing ] to go over the last ]
  | (InsertText("]"), _, R(TListClose(_), ti)) if pos == ti.endPos - 1 => moveOneRight(pos, astInfo)
  // Pressing ) to go over the last )
  | (InsertText(")"), _, R(TTupleClose(_), ti)) if pos == ti.endPos - 1 => moveOneRight(pos, astInfo)
  // Pressing quote to go over the last quote
  | (InsertText("\""), _, R(TPatternString(_), ti))
  | (InsertText("\""), _, R(TString(_), ti))
  | (InsertText("\""), _, R(TStringMLEnd(_), ti)) if pos == ti.endPos - 1 =>
    moveOneRight(pos, astInfo)
  // *************************
  // CREATING NEW CONSTRUCTS
  // *************************
  /* Entering a string escape
   * TODO: Move this to doInsert */
  | (InsertText("\\"), L(TString(_), _), R(TString(_), ti))
    if false /* disable for now */ && pos - ti.startPos !== 0 =>
    startEscapingString(pos, ti, astInfo)


  // Add another element to a List by inserting a `,`
  | (InsertText(","), L(TListOpen(id, _), _), _) if onEdge =>
    let bID = gid()
    let newExpr = EBlank(bID)
    astInfo
    |> ASTInfo.setAST(insertInList(~index=0, id, ~newExpr, astInfo.ast))
    |> moveToCaretTarget({astRef: ARBlank(bID), offset: 0})

  | (InsertText(","), L(TListComma(id, index), _), R(_, ti))
  | (InsertText(","), L(_, ti), R(TListComma(id, index), _)) if onEdge => {
    let astInfo = acEnter(ti, K.Enter, astInfo)

    let blankID = gid()
    let newExpr = EBlank(blankID)

    astInfo
    |> ASTInfo.setAST(insertInList(~index=index + 1, id, ~newExpr, astInfo.ast))
    |> moveToCaretTarget({astRef: ARBlank(blankID), offset: 0})
    }

  | (InsertText(","), L(_, ti), R(TListClose(id, _), _)) if onEdge =>
    let newAstInfo = acEnter(ti, K.Enter, astInfo)

    let blankID = gid()
    let newExpr = EBlank(blankID)

    newAstInfo
    |> ASTInfo.setAST(insertAtListEnd(id, ~newExpr, newAstInfo.ast))
    |> moveToCaretTarget({astRef: ARBlank(blankID), offset: 0})


  // Add another element to a tuple by inserting a `,`
  | (InsertText(","), L(TTupleOpen(id), _), _) if onEdge =>
    // Case: right after a tuple's opening `(`
    let blankID = gid()
    let newExpr = EBlank(blankID)

    astInfo
    |> ASTInfo.setAST(insertInTuple(~index=0, id, ~newExpr, astInfo.ast))
    |> moveToCaretTarget({astRef: ARBlank(blankID), offset: 0})

  | (InsertText(","), L(_, _ti), R(TTupleComma(_, _), _)) if onEdge =>
    // Case: just to the left of a tuple's separator `,`
    moveOneRight(pos, astInfo)

  | (InsertText(","), L(TTupleComma(id, index), _), R(_, ti)) if onEdge =>
    // Case: just to the right of a tuple's `,`
    let indexToInsertInto = index + 1

    let astInfo = acEnter(ti, K.Enter, astInfo)

    let blankID = gid()
    let newExpr = EBlank(blankID)

    astInfo
    |> ASTInfo.setAST(insertInTuple(~index=indexToInsertInto, id, ~newExpr, astInfo.ast))
    |> moveToCaretTarget({astRef: ARBlank(blankID), offset: 0})

  | (InsertText(","), L(_, ti), R(TTupleClose(id), _)) if onEdge =>
    // Case: right before the tuple's closing `)`
    let astInfo = acEnter(ti, K.Enter, astInfo)

    let blankID = gid()
    let newExpr = EBlank(blankID)

    astInfo
    |> ASTInfo.setAST(insertAtTupleEnd(id, ~newExpr, astInfo.ast))
    |> moveToCaretTarget({astRef: ARBlank(blankID), offset: 0})


  // Add another param to a lambda
  | (InsertText(","), L(TLambdaSymbol(id, _), _), _) if onEdge =>
    astInfo
    |> ASTInfo.setAST(insertLambdaVar(~index=0, id, ~name="", astInfo.ast))
    |> moveToCaretTarget({astRef: ARLambda(id, LBPVarName(0)), offset: 0})
  | (InsertText(","), L(TLambdaVar(id, _, index, _, _), _), _) if onEdge =>
    astInfo
    |> ASTInfo.setAST(insertLambdaVar(~index=index + 1, id, ~name="", astInfo.ast))
    |> moveToCaretTarget({astRef: ARLambda(id, LBPVarName(index + 1)), offset: 0})
  | (InsertText(","), _, R(TLambdaVar(id, _, index, _, _), _)) if onEdge =>
    astInfo
    |> ASTInfo.setAST(insertLambdaVar(~index, id, ~name="", astInfo.ast))
    |> moveToCaretTarget({astRef: ARLambda(id, LBPVarName(index)), offset: 0})


  // Field access
  | (InsertText("."), L(TFieldPartial(id, _, _, _, _), _), _) =>
    // When pressing . in a field access partial, commit the partial
    let newPartialID = gid()
    let ast = FluidAST.update(id, astInfo.ast, ~f=x =>
      switch x {
      | EPartial(_, name, EFieldAccess(faid, expr, _)) =>
        let committedAccess = EFieldAccess(faid, expr, name)
        EPartial(newPartialID, "", EFieldAccess(gid(), committedAccess, ""))
      | e => recover("updateKey insert . - unexpected expr " ++ E.show(e), e)
      }
    )

    astInfo
    |> ASTInfo.setAST(ast)
    |> moveToCaretTarget({astRef: ARPartial(newPartialID), offset: 0})

  | (InsertText("."), L(TVariable(id, _, _), toTheLeft), _)
  | (InsertText("."), L(TFieldName(id, _, _, _), toTheLeft), _)
    if onEdge && pos == toTheLeft.endPos =>
    let (newAST, target) = exprToFieldAccess(id, ~partialID=gid(), ~fieldID=gid(), astInfo.ast)

    astInfo |> ASTInfo.setAST(newAST) |> moveToCaretTarget(target)


  // Wrap the current expression in a list
  | (InsertText("["), _, R(TInteger(id, _, _), _))
  | (InsertText("["), _, R(TTrue(id, _), _))
  | (InsertText("["), _, R(TFalse(id, _), _))
  | (InsertText("["), _, R(TNullToken(id, _), _))
  | (InsertText("["), _, R(TFloatWhole(id, _, _), _))
  | (InsertText("["), _, R(TFnName(id, _, _, _, _), _))
  | (InsertText("["), _, R(TVariable(id, _, _), _))
  | (InsertText("["), _, R(TListOpen(id, _), _))
  | (InsertText("["), _, R(TTupleOpen(id), _))
  | (InsertText("["), _, R(TRecordOpen(id, _), _))
  | (InsertText("["), _, R(TConstructorName(id, _), _)) =>
    let newID = gid()
    astInfo
    |> ASTInfo.setAST(FluidAST.update(~f=var => EList(newID, list{var}), id, astInfo.ast))
    |> moveToCaretTarget({astRef: ARList(newID, LPOpen), offset: 1})
  // Strings can be wrapped in lists, but only if we're outside the quote
  | (InsertText("["), _, R(TString(id, _, _), toTheRight))
  | (InsertText("["), _, R(TStringMLStart(id, _, _, _), toTheRight))
    if onEdge && pos == toTheRight.startPos =>
    let newID = gid()
    astInfo
    |> ASTInfo.setAST(FluidAST.update(~f=var => EList(newID, list{var}), id, astInfo.ast))
    |> moveToCaretTarget({astRef: ARList(newID, LPOpen), offset: 1})


  // Infix symbol insertion to create partials
  | (InsertText(infixTxt), L(TPipe(_), ti), _)
  | (InsertText(infixTxt), _, R(TPlaceholder(_), ti))
  | (InsertText(infixTxt), _, R(TBlank(_), ti))
  | (InsertText(infixTxt), L(_, ti), _) if keyIsInfix =>
    doInfixInsert(~pos, infixTxt, ti, astInfo)

  // Typing between empty list symbols []
  | (InsertText(txt), L(TListOpen(id, _), _), R(TListClose(_), _)) =>
    let (newExpr, target) = insertInBlankExpr(txt)
    astInfo
    |> ASTInfo.setAST(insertInList(~index=0, id, ~newExpr, astInfo.ast))
    |> moveToCaretTarget(target)

  // Typing between empty record symbols {}
  | (InsertText(txt), L(TRecordOpen(id, _), _), R(TRecordClose(_), _)) =>
    /* Adds new initial record row with the typed
     * value as the fieldname (if value entered is valid),
     * then move caret to end of fieldname */
    if Util.isIdentifierChar(txt) {
      astInfo
      |> ASTInfo.setAST(addRecordRowAt(~letter=txt, 0, id, astInfo.ast))
      |> moveToAstRef(ARRecord(id, RPFieldname(0)), ~offset=1)
    } else {
      astInfo
    }

  // *********************************
  // INSERT INTO EXISTING CONSTRUCTS
  // *********************************
  | (InsertText(ins), L(TPlaceholder({placeholder, blankID, fnID, _}), _), _)
  | (InsertText(ins), _, R(TPlaceholder({placeholder, blankID, fnID, _}), _)) =>
    /* We need this special case because by the time we get to the general
     * doInsert handling, reconstructing the difference between placeholders
     * and blanks is too challenging. ASTRefs cannot distinguish blanks and placeholders. */
    let (newExpr, newTarget) = insertInPlaceholderExpr(
      ~fnID,
      ~placeholder,
      ~ins,
      astInfo.ast,
      astInfo.props,
    )

    astInfo
    |> ASTInfo.setAST(FluidAST.replace(blankID, ~replacement=newExpr, astInfo.ast))
    |> moveToCaretTarget(newTarget)
  | (Keypress({key: K.Space, _}), _, R(_, toTheRight)) => doInsert(~pos, " ", toTheRight, astInfo)
  | (InsertText(txt), L(_, toTheLeft), _) if T.isAppendable(toTheLeft.token) =>
    doInsert(~pos, txt, toTheLeft, astInfo)
  | (InsertText(txt), _, R(_, toTheRight)) => doInsert(~pos, txt, toTheRight, astInfo)

  // *********
  // Handle K.Enter (hit Enter on keyboard)
  // *********
  /*
   * Caret to right of record open {
   * Add new initial record row and move caret to it. */
  | (Keypress({key: K.Enter, _}), L(TRecordOpen(id, _), _), _) =>
    astInfo
    |> ASTInfo.setAST(addRecordRowAt(0, id, astInfo.ast))
    |> moveToAstRef(ARRecord(id, RPFieldname(0)))
  /*
   * Caret to left of record close }
   * Add new final record but leave caret to left of } */
  | (Keypress({key: K.Enter, _}), _, R(TRecordClose(id, _), _)) =>
    astInfo
    |> recordAction("addRecordRowToBack")
    |> ASTInfo.setAST(addRecordRowToBack(id, astInfo.ast))
    |> moveToAstRef(ARRecord(id, RPClose))
  /*
   * Caret between pipe symbol |> and following expression.
   * Move current pipe expr down by adding new expr above it.
   * Keep caret "the same", only moved down by 1 column. */
  | (Keypress({key: K.Enter, _}), L(TPipe(id, idx, _, _), _), R(_)) =>
    let (astInfo, _) = addPipeExprAt(id, idx + 1, astInfo)
    astInfo |> moveToAstRef(ARPipe(id, idx + 1), ~offset=2)
  /*
   * Caret on end-of-line.
   * Following newline contains a parent and index, meaning we're inside some
   * special construct. Special-case each of those. */
  | (Keypress({key: K.Enter, _}), _, R(TNewline(Some(_, parentId, Some(idx))), ti)) =>
    switch FluidAST.find(parentId, astInfo.ast) {
    | Some(EPipe(_)) =>
      let (astInfo, blankId) = addPipeExprAt(parentId, idx + 1, astInfo)
      moveToCaretTarget(caretTargetForStartOfExpr(blankId, astInfo.ast), astInfo)
    | Some(ERecord(_)) =>
      astInfo
      |> ASTInfo.setAST(addRecordRowAt(idx, parentId, astInfo.ast))
      |> moveToAstRef(ARRecord(parentId, RPFieldname(idx)))
    | Some(EMatch(_)) =>
      let astInfo = addMatchPatternAt(parentId, idx, astInfo)
      astInfo |> moveToCaretTarget(caretTargetForBeginningOfMatchBranch(parentId, idx, astInfo.ast))
    | Some(EFnCall(_)) =>
      /* Pressing enter at the end of an FnCall's expression should just
       * move right. We don't know what's next, so we just want to
       * literally go to whatever's on the other side of the newline.
       * */
      doRight(~pos, ~next=mNext, ti, astInfo)
    | _ => astInfo
    }
  /*
   * Caret at end of line with nothing in newline. */
  | (Keypress({key: K.Enter, _}), L(lt, lti), R(TNewline(None), rti))
    if !(T.isLet(lt) || (isAutocompleting(rti, astInfo.state) || isInIfCondition(lt))) =>
    wrapInLet(lti, astInfo)
  /*
   * Caret at end-of-line with no data in the TNewline.
   * Just move right (ie, * to beginning of next line) */
  | (Keypress({key: K.Enter, _}), L(_), R(TNewline(None), ti)) =>
    doRight(~pos, ~next=mNext, ti, astInfo)
  /*
   * Caret at end-of-line generally adds a let on the line below,
   * unless the next line starts with a blank, in which case we go to it. */
  | (Keypress({key: K.Enter, _}), L(_), R(TNewline(Some(id, _, _)), ti)) =>
    if (
      mNext
      |> Option.map(~f=n =>
        switch n.token {
        | TBlank(_) => true
        | _ => false
        }
      )
      |> Option.unwrap(~default=false)
    ) {
      doRight(~pos, ~next=mNext, ti, astInfo)
    } else {
      let (astInfo, letId) = makeIntoLetBody(id, astInfo)
      astInfo |> moveToAstRef(ARLet(letId, LPVarName))
    }
  /*
   * Caret at beginning of special line.
   * Preceding newline contains a parent and index, meaning we're inside some
   * special construct. Special-case each of those.
   *
   * In the special case of the special case where we're actually at the
   * beginning of the next line _following_ the construct, then we actually
   * want to add a let, not continue the construct. These are the index vs
   * length checks in each case.
   *
   * Keep in mind this is the newline that _ends the previous line_, which means
   * in each case the idx is one more than the number of elements in the construct.
   * Eg, a match with 2 rows will have idx=3 here.
   */
  | (Keypress({key: K.Enter, _}), L(TNewline(Some(_, parentId, Some(idx))), _), R(rTok, _)) =>
    let applyToRightToken = (): ASTInfo.t => {
      let parentID = T.toParentID(rTok)
      let index = T.toIndex(rTok)
      switch (parentID, index, Option.andThen(parentID, ~f=id => FluidAST.find(id, astInfo.ast))) {
      | (Some(parentId), Some(idx), Some(EMatch(_))) =>
        let astInfo = addMatchPatternAt(parentId, idx, astInfo)
        let target = caretTargetForBeginningOfMatchBranch(parentId, idx + 1, astInfo.ast)

        moveToCaretTarget(target, astInfo)
      | (Some(parentId), Some(idx), Some(ERecord(_))) =>
        let ast = addRecordRowAt(idx, parentId, astInfo.ast)
        astInfo
        |> ASTInfo.setAST(ast)
        |> moveToCaretTarget({
          astRef: ARRecord(parentId, RPFieldname(idx + 1)),
          offset: 0,
        })
      | _ =>
        let id = T.tid(rTok)
        let (astInfo, _) = makeIntoLetBody(id, astInfo)
        astInfo |> moveToCaretTarget(caretTargetForStartOfExpr(id, astInfo.ast))
      }
    }

    switch FluidAST.find(parentId, astInfo.ast) {
    | Some(EMatch(_, _, exprs)) =>
      // if a match has n rows, the last newline has idx=(n+1)
      if idx == List.length(exprs) {
        applyToRightToken()
      } else {
        let astInfo = addMatchPatternAt(parentId, idx, astInfo)
        let target = caretTargetForBeginningOfMatchBranch(parentId, idx + 1, astInfo.ast)

        moveToCaretTarget(target, astInfo)
      }
    | Some(EPipe(_, _, _, rest)) =>
      /* exprs[0] is the initial value of the pipeline, but the indexing
       * is zero-based starting at exprs[1] (it indexes the _pipes
       * only_), so need idx+1 here to counteract. */
      if idx + 1 == List.length(rest) + 2 {
        applyToRightToken()
      } else {
        let (astInfo, _) = addPipeExprAt(parentId, idx + 1, astInfo)
        moveToAstRef(ARPipe(parentId, idx + 1), astInfo)
      }
    | Some(ERecord(_)) =>
      /* No length special-case needed because records do not emit a
       * TNewline with index after the final '}'. In this case, we
       * actually hit the next match case instead. */
      astInfo
      |> ASTInfo.setAST(addRecordRowAt(idx, parentId, astInfo.ast))
      |> moveToAstRef(ARRecord(parentId, RPFieldname(idx + 1)))
    | _ => astInfo
    }
  /*
   * Caret at very beginning of tokens or at beginning of non-special line. */
  | (Keypress({key: K.Enter, _}), No, R(t, _))
  | (Keypress({key: K.Enter, _}), L(TNewline(_), _), R(t, _)) =>
    /* In some cases, like |1 + 2, we want to wrap the parent expr (in this case the binop) in a let.
     * This has to be recursive to handle variations on |1*2 + 3.
     * In other cases, we want to wrap just the subexpression, such as an if's then expression. */
    let id = T.tid(t)
    let topID =
      FluidAST.find(id, astInfo.ast)
      |> Option.andThen(~f=directExpr => findAppropriateParentToWrap(directExpr, astInfo.ast))
      |> Option.map(~f=expr => E.toID(expr))
      |> Option.unwrap(~default=id)

    let (astInfo, _) = makeIntoLetBody(topID, astInfo)
    astInfo |> moveToCaretTarget(caretTargetForStartOfExpr(topID, astInfo.ast))
  // Caret at very end of tokens where last line is non-let expression.
  | (Keypress({key: K.Enter, _}), L(token, ti), No) if !T.isLet(token) => wrapInLet(ti, astInfo)
  | _ =>
    // Unknown
    report("Unknown action: " ++ show_fluidInputEvent(inputEvent), astInfo)
  }

  let astInfo = ASTInfo.modifyState(astInfo, ~f=s => {...s, lastInput: inputEvent})

  let astInfo = {
    /* This is a hack to make Enter create a new entry in matches and pipes
     * at the end of an AST. Matches/Pipes generate newlines at the end of
     * the canvas: we don't want those newlines to appear in editor, however,
     * we also can't get rid of them because it's a significant challenge to
     * know what to do in those cases without that information. So instead, we
     * allow them to be created and deal with the consequences.
     *
     * They don't appear in the browser, so we can ignore that.
     *
     * The major consequence is that there is an extra space at the end of the
     * AST (the one after the newline). Users can put their cursor all the way
     * to the end of the AST, and then they press left and the cursor doesn't
     * move (since the browser doesn't display the final newline, the cursor
     * goes to the same spot).
     *
     * We handle this by checking if we're in that situation and moving the
     * cursor back to the right place if so.
     *
     * TODO: there may be ways of getting the cursor to the end without going
     * through this code, if so we need to move it. */
    let activeTokens = astInfo |> ASTInfo.activeTokenInfos
    let text = Printer.tokensToString(activeTokens)
    let last = List.last(activeTokens)
    switch last {
    | Some({token: TNewline(_), _}) if String.length(text) == astInfo.state.newPos =>
      astInfo |> ASTInfo.modifyState(~f=s => {...s, newPos: s.newPos - 1})
    | _ => astInfo
    }
  }

  /* If we were on a partial and have moved off it, we may want to commit that
   * partial. For example, if we fully typed out "String::append", then move
   * away, we want that to become `String::append ___ ___`.
   *
   * We "commit the partial" using the old state, and then we do the action
   * again to make sure we go to the right place for the new canvas.
   *
   * This is done here because the logic is different than clicking. */
  if recursing {
    astInfo
  } else {
    let key = switch inputEvent {
    | Keypress({key, _}) => Some(key)
    | _ => None
    }

    switch (toTheLeft, toTheRight) {
    | (L(TPartial(_, str, _), ti), _)
    | (L(TFieldPartial(_, _, _, str, _), ti), _)
    | (_, R(TPartial(_, str, _), ti))
    | (_, R(TFieldPartial(_, _, _, str, _), ti))
      if /* When pressing an infix character, it's hard to tell whether to commit or
       * not.  If the partial is an int, or a function that returns one, pressing
       * +, -, etc  should lead to committing and then doing the action.
       *
       * However, if the partial is a valid function such as +, then pressing +
       * again could be an attempt to make ++, not `+ + ___`.
       *
       * So if the new function _could_ be valid, don't commit. */
      key == Some(K.Right) || (key == Some(K.Left) || keyIsInfix) =>
      let shouldCommit = switch inputEvent {
      | Keypress({key: K.Right, _}) | Keypress({key: K.Left, _}) => true
      | InsertText(txt) =>
        // if the partial is a valid function name, don't commit
        let newQueryString = str ++ txt
        astInfo.state.ac.completions
        |> List.filter(~f=({item, _}) =>
          String.includes(~substring=newQueryString, AC.asName(item))
        )
        |> \"=="(list{})
      | _ => // unreachable due to when condition on enclosing match branch
        true
      }

      if shouldCommit {
        /* To calculate the new AST, try to commit the old AST with the new
         * position. */
        let committed = commitIfValid(astInfo.state.newPos, ti, origAstInfo)
        /* To find out where the cursor should be, replay the movement
         * command from the old position. */
        let committed = {...committed, state: origAstInfo.state}
        updateKey(~recursing=true, inputEvent, committed)
      } else {
        astInfo
      }
    | (L(TPartial(_, _, _), ti), _) if false /* disable for now */ =>
      maybeCommitStringPartial(pos, ti, astInfo)
    | _ => astInfo
    }
  }
}

/* deleteCaretRange is equivalent to pressing backspace starting from the
 * larger of the two caret positions until the caret reaches the smaller of the
 * caret positions or can no longer move.
 *
 * XXX(JULIAN): This actually moves the caret to the larger side of the range
 * and backspaces until the beginning, which means this hijacks the caret in
 * the state. */
and deleteCaretRange = (caretRange: (int, int), origInfo: ASTInfo.t): ASTInfo.t => {
  let (rangeStart, rangeEnd) = orderRangeFromSmallToBig(caretRange)
  let origInfo = ASTInfo.modifyState(origInfo, ~f=s => {
    ...s,
    newPos: rangeEnd,
    oldPos: s.newPos,
    selectionStart: None,
  })

  let oldInfo = ref(origInfo)
  let nothingChanged = ref(false)
  while !nothingChanged.contents && oldInfo.contents.state.newPos > rangeStart {
    let newInfo = updateKey(DeleteContentBackward, oldInfo.contents)
    if (
      newInfo.state.newPos == oldInfo.contents.state.newPos && newInfo.ast == oldInfo.contents.ast
    ) {
      // stop if nothing changed--guarantees loop termination
      nothingChanged := true
    } else {
      oldInfo := newInfo
    }
  }
  oldInfo.contents
}

/* deleteSelection is equivalent to pressing backspace starting from the larger of the two caret positions
 forming the selection until the caret reaches the smaller of the caret positions or can no longer move. */
and deleteSelection = (astInfo: ASTInfo.t): ASTInfo.t =>
  deleteCaretRange(FluidUtil.getSelectionRange(astInfo.state), astInfo)

and replaceText = (str: string, astInfo: ASTInfo.t): ASTInfo.t =>
  astInfo |> deleteSelection |> updateKey(InsertText(str))

let updateAutocomplete = (m: model, tlid: TLID.t, astInfo: ASTInfo.t): ASTInfo.t =>
  switch ASTInfo.getToken(astInfo) {
  | Some(ti) if T.isAutocompletable(ti.token) =>
    let m = TL.withAST(m, tlid, astInfo.ast)
    ASTInfo.modifyState(astInfo, ~f=s => {
      let newAC = AC.regenerate(m, s.ac, (tlid, ti))
      {...s, ac: newAC}
    })
  | _ => astInfo
  }

let updateMouseClick = (newPos: int, astInfo: ASTInfo.t): ASTInfo.t => {
  let lastPos =
    astInfo
    |> ASTInfo.activeTokenInfos
    |> List.last
    |> Option.map(~f=ti => ti.endPos)
    |> Option.unwrap(~default=0)

  let newPos = if newPos > lastPos {
    lastPos
  } else {
    newPos
  }
  let newPos = // TODO: add tests for clicking in the middle of a pipe (or blank)
  switch getLeftTokenAt(newPos, ASTInfo.activeTokenInfos(astInfo)) {
  | Some(current) if T.isBlank(current.token) => current.startPos
  | Some({token: TPipe(_), _} as current) => current.endPos
  | _ => newPos
  }

  astInfo |> acMaybeCommit(newPos) |> updatePosAndAC(newPos)
}

let shouldDoDefaultAction = (key: K.key): bool =>
  switch key {
  | K.GoToStartOfLine(_)
  | K.GoToEndOfLine(_)
  | K.SelectAll
  | K.GoToStartOfWord(_)
  | K.GoToEndOfWord(_) => false
  | _ => true
  }

let shouldSelect = (key: K.key): bool =>
  switch key {
  | K.GoToStartOfWord(K.KeepSelection)
  | K.GoToEndOfWord(K.KeepSelection)
  | K.GoToStartOfLine(K.KeepSelection)
  | K.GoToEndOfLine(K.KeepSelection)
  | K.SelectAll => true
  | _ => false
  }

@ocaml.doc(" [expressionRange e target] returns the beginning and end of the range
  * from the expression's first and last token by cross-referencing the
  * tokens for the expression with the tokens for the whole editor's expr.
  *
  * This is preferred to just getting all the tokens with the same exprID
  * because the last expression in a token range
  * (e.g. a FnCall `Int::add 1 2`) might be for a sub-expression and have a
  * different ID, (in the above case the last token TInt(2) belongs to the
  * second sub-expr of the FnCall) ")
let expressionRange = (exprID: id, astInfo: ASTInfo.t): option<(int, int)> => {
  let containingTokens = ASTInfo.activeTokenInfos(astInfo)
  let exprTokens =
    FluidAST.find(exprID, astInfo.ast)
    |> Option.map(~f=expr => FluidTokenizer.tokenizeForEditor(astInfo.state.activeEditor, expr))
    |> Option.unwrap(~default=list{})

  let (exprStartToken, exprEndToken) = (
    List.head(exprTokens),
    List.last(exprTokens),
  ) |> Tuple2.mapAll(~f=x =>
    switch x {
    | Some(exprTok) =>
      List.find(containingTokens, ~f=tk => T.matchesContent(exprTok.token, tk.token))
    | _ => None
    }
  )

  switch (exprStartToken, exprEndToken) {
  /* range is from startPos of first token in expr to
   * endPos of last token in expr */
  | (Some({startPos, _}), Some({endPos, _})) => Some(startPos, endPos)
  | _ => None
  }
}

let getExpressionRangeAtCaret = (astInfo: ASTInfo.t): option<(int, int)> =>
  ASTInfo.getToken(astInfo)
  |> // get token that the cursor is currently on
  Option.andThen(~f=t => {
    // get expression that the token belongs to
    let exprID = T.tid(t.token)
    expressionRange(exprID, astInfo)
  })
  |> Option.map(~f=((eStartPos, eEndPos)) => (eStartPos, eEndPos))


let reconstructExprFromRange = (range: (int, int), astInfo: ASTInfo.t): option<
  FluidExpression.t,
> => {
  // prevent duplicates
  let astInfo = ASTInfo.setAST(FluidAST.clone(astInfo.ast), astInfo)
  // a few helpers
  let toBool_ = s =>
    if s == "true" {
      true
    } else if s == "false" {
      false
    } else {
      recover("string bool token should always be convertable to bool", ~debug=s, false)
    }

  let findTokenValue = (tokens, tID, typeName) =>
    List.find(tokens, ~f=((tID', _, typeName')) =>
      tID == tID' && typeName == typeName'
    ) |> Option.map(~f=Tuple3.second)

  let (startPos, endPos) = orderRangeFromSmallToBig(range)
  // main recursive algorithm
  // algo:
  // - find topmost expression by ID and
  // - reconstruct full/subset of expression
  // - recurse into children (that remain in subset) to reconstruct those too
  let rec reconstruct = (~topmostID, (startPos, endPos)): option<E.t> => {
    let topmostExpr =
      topmostID
      |> Option.andThen(~f=id => FluidAST.find(id, astInfo.ast))
      |> Option.unwrap(~default=EBlank(gid()))

    let tokens = // simplify tokens to make them homogenous, easier to parse
    tokensInRange(startPos, endPos, astInfo) |> List.map(~f=ti => {
      let t = ti.token
      let text =
        // trim tokens if they're on the edge of the range
        T.toText(t)
        |> String.dropLeft(
          ~count=if ti.startPos < startPos {
            startPos - ti.startPos
          } else {
            0
          },
        )
        |> String.dropRight(
          ~count=if ti.endPos > endPos {
            ti.endPos - endPos
          } else {
            0
          },
        )
        |> (
          text =>
            // if string, do extra trim to account for quotes, then re-append quotes
            if T.toTypeName(ti.token) == "string" {
              "\"" ++ (Util.trimQuotes(text) ++ "\"")
            } else {
              text
            }
        )

      open T
      (tid(t), text, toTypeName(t))
    })

    // Reconstructs an expression, returning an Option.
    // If it's not within range of the selection, returns None.
    let reconstructExpr = (expr): option<E.t> =>
      switch expr {
      | EPipeTarget(_) => Some(expr)
      | _ =>
        let exprID = E.toID(expr)

        expressionRange(exprID, astInfo)
        |> Option.andThen(~f=((exprStartPos, exprEndPos)) =>
          // ensure expression range is not totally outside selection range
          if exprStartPos > endPos || exprEndPos < startPos {
            None
          } else {
            Some(max(exprStartPos, startPos), min(exprEndPos, endPos))
          }
        )
        |> Option.andThen(~f=reconstruct(~topmostID=Some(exprID)))
      }

    let orDefaultExpr: option<E.t> => E.t = Option.unwrap(~default=EBlank(gid()))

    let id = gid()
    switch topmostExpr {
    | _ if tokens == list{} => None
    // basic, single/fixed-token expressions
    | EInteger(eID, _) =>
      findTokenValue(tokens, eID, "integer")
      |> Option.map(~f=Util.coerceStringTo64BitInt)
      |> Option.map(~f=v => EInteger(gid(), v))
    | EBool(eID, value) =>
      Option.or_(
        findTokenValue(tokens, eID, "true"),
        findTokenValue(tokens, eID, "false"),
      ) |> Option.andThen(~f=newValue =>
        if newValue == "" {
          None
        } else if newValue != string_of_bool(value) {
          Some(EPartial(gid(), newValue, EBool(id, value)))
        } else {
          Some(EBool(id, value))
        }
      )
    | ENull(eID) =>
      findTokenValue(tokens, eID, "null") |> Option.map(~f=newValue =>
        if newValue == "null" {
          ENull(id)
        } else {
          EPartial(gid(), newValue, ENull(id))
        }
      )
    | EString(eID, _) =>
      let merged =
        tokens
        |> List.filter(~f=((_, _, type_)) => type_ != "newline" && type_ != "indent")
        |> List.map(~f=Tuple3.second)
        |> String.join(~sep="")

      if merged == "" {
        None
      } else {
        Some(EString(eID, Util.trimQuotes(merged)))
      }
    | EFloat(eID, _, _, _) =>
      let newWhole = findTokenValue(tokens, eID, "float-whole")
      let pointSelected = findTokenValue(tokens, eID, "float-point") != None
      let newFraction = findTokenValue(tokens, eID, "float-fractional")
      switch (newWhole, pointSelected, newFraction) {
      | (Some(value), true, None) => Some(EFloat(id, Positive, value, "0"))
      | (Some(value), false, None) | (None, false, Some(value)) =>
        Some(EInteger(id, Util.coerceStringTo64BitInt(value)))
      | (None, true, Some(value)) => Some(EFloat(id, Positive, "0", value))
      | (Some(whole), true, Some(fraction)) => Some(EFloat(id, Positive, whole, fraction))
      | (None, true, None) => Some(EFloat(id, Positive, "0", "0"))
      | (_, _, _) => None
      }
    | EBlank(_) => Some(EBlank(id))
    // empty let expr and subsets
    | ELet(eID, _lhs, rhs, body) =>
      let letKeywordSelected = findTokenValue(tokens, eID, "let-keyword") != None

      let newLhs = findTokenValue(tokens, eID, "let-var-name") |> Option.unwrap(~default="")

      switch (reconstructExpr(rhs), reconstructExpr(body)) {
      | (None, None) if newLhs != "" => Some(EPartial(gid(), newLhs, EVariable(gid(), newLhs)))
      | (None, Some(e)) => Some(e)
      | (Some(newRhs), None) => Some(ELet(id, newLhs, newRhs, EBlank(gid())))
      | (Some(newRhs), Some(newBody)) => Some(ELet(id, newLhs, newRhs, newBody))
      | (None, None) if letKeywordSelected => Some(ELet(id, newLhs, EBlank(gid()), EBlank(gid())))
      | (_, _) => None
      }
    | EIf(eID, cond, thenBody, elseBody) =>
      let ifKeywordSelected = findTokenValue(tokens, eID, "if-keyword") != None

      let thenKeywordSelected = findTokenValue(tokens, eID, "if-then-keyword") != None

      let elseKeywordSelected = findTokenValue(tokens, eID, "if-else-keyword") != None

      switch (reconstructExpr(cond), reconstructExpr(thenBody), reconstructExpr(elseBody)) {
      | (newCond, newThenBody, newElseBody)
        if ifKeywordSelected || (thenKeywordSelected || elseKeywordSelected) =>
        Some(
          EIf(
            id,
            newCond |> orDefaultExpr,
            newThenBody |> orDefaultExpr,
            newElseBody |> orDefaultExpr,
          ),
        )
      | (Some(e), None, None) | (None, Some(e), None) | (None, None, Some(e)) => Some(e)
      | _ => None
      }
    | EBinOp(eID, name, expr1, expr2, ster) =>
      let newName = findTokenValue(tokens, eID, "binop") |> Option.unwrap(~default="")

      switch (reconstructExpr(expr1), reconstructExpr(expr2)) {
      | (Some(newExpr1), Some(newExpr2)) if newName == "" =>
        /* since we don't allow empty partials, reconstruct the binop as we would when
         * the binop is manually deleted
         * (by elevating the argument expressions into ELets provided they aren't blanks) */
        switch (newExpr1, newExpr2) {
        | (EBlank(_), EBlank(_)) => None
        | (EBlank(_), e) | (e, EBlank(_)) => Some(ELet(gid(), "", e, EBlank(gid())))
        | (e1, e2) => Some(ELet(gid(), "", e1, ELet(gid(), "", e2, EBlank(gid()))))
        }
      | (None, Some(e)) =>
        let e = EBinOp(id, name, EBlank(gid()), e, ster)
        if newName == "" {
          None
        } else if name != newName {
          Some(EPartial(gid(), newName, e))
        } else {
          Some(e)
        }
      | (Some(e), None) =>
        let e = EBinOp(id, name, e, EBlank(gid()), ster)
        if newName == "" {
          None
        } else if name != newName {
          Some(EPartial(gid(), newName, e))
        } else {
          Some(e)
        }
      | (Some(newExpr1), Some(newExpr2)) =>
        let e = EBinOp(id, name, newExpr1, newExpr2, ster)
        if newName == "" {
          None
        } else if name != newName {
          Some(EPartial(gid(), newName, e))
        } else {
          Some(e)
        }
      | (None, None) if newName != "" =>
        let e = EBinOp(id, name, EBlank(gid()), EBlank(gid()), ster)
        if newName == "" {
          None
        } else if name != newName {
          Some(EPartial(gid(), newName, e))
        } else {
          Some(e)
        }
      | (_, _) => None
      }
    | ELambda(eID, _, body) =>
      /* might be an edge case here where one of the vars is not (fully) selected but
       * is still bound in the body, would be worth turning the EVars in the body to partials somehow */
      let newVars = /* get lambda-var tokens that belong to this expression
       * out of the list of tokens in the selection range */
      tokens |> List.filterMap(~f=x =>
        switch x {
        | (vID, value, "lambda-var") if vID == eID => Some(gid(), value)
        | _ => None
        }
      )

      Some(ELambda(id, newVars, reconstructExpr(body) |> orDefaultExpr))
    | EFieldAccess(eID, e, _) =>
      let newFieldName = findTokenValue(tokens, eID, "field-name") |> Option.unwrap(~default="")

      let fieldOpSelected = findTokenValue(tokens, eID, "field-op") != None
      let e = reconstructExpr(e)
      switch (e, fieldOpSelected, newFieldName) {
      | (None, false, newFieldName) if newFieldName !== "" =>
        Some(EPartial(gid(), newFieldName, EVariable(gid(), newFieldName)))
      | (None, true, newFieldName) if newFieldName !== "" =>
        Some(EFieldAccess(id, EBlank(gid()), newFieldName))
      | (Some(e), true, _) => Some(EFieldAccess(id, e, newFieldName))
      | _ => e
      }
    | EVariable(eID, value) =>
      let newValue = findTokenValue(tokens, eID, "variable") |> Option.unwrap(~default="")

      let e = EVariable(id, value)
      if newValue == "" {
        None
      } else if value != newValue {
        Some(EPartial(gid(), newValue, e))
      } else {
        Some(e)
      }
    | EFnCall(eID, fnName, args, ster) =>
      let newArgs = switch args {
      | list{EPipeTarget(_), ...args} => list{
          EPipeTarget(gid()),
          ...List.map(args, ~f=\">>"(reconstructExpr, orDefaultExpr)),
        }
      | _ => List.map(args, ~f=\">>"(reconstructExpr, orDefaultExpr))
      }

      let newFnName = findTokenValue(tokens, eID, "fn-name") |> Option.unwrap(~default="")

      let newFnVersion = findTokenValue(tokens, eID, "fn-version") |> Option.unwrap(~default="")

      let newFnName = if newFnVersion == "" {
        newFnName
      } else {
        newFnName ++ ("_" ++ newFnVersion)
      }

      let e = EFnCall(id, fnName, newArgs, ster)
      if newFnName == "" {
        None
      } else if fnName != newFnName {
        Some(EPartial(gid(), newFnName, e))
      } else {
        Some(e)
      }
    | EPartial(eID, _, expr) =>
      /* What should we do with the expr? Some of the name is covered by
       * the partial name which breaks the reconstruction algorithm. In
       * addtion, copying a partial without the old expr breaks the whole
       * concept of a partial. So it makes more sense to copy the whole
       * thing. */
      let newName = findTokenValue(tokens, eID, "partial") |> Option.unwrap(~default="")

      Some(EPartial(id, newName, expr))
    | ERightPartial(eID, _, expr) =>
      let expr = reconstructExpr(expr) |> orDefaultExpr
      let newName = findTokenValue(tokens, eID, "partial-right") |> Option.unwrap(~default="")

      Some(ERightPartial(id, newName, expr))
    | ELeftPartial(eID, _, expr) =>
      let expr = reconstructExpr(expr) |> orDefaultExpr
      let newName = findTokenValue(tokens, eID, "partial-left") |> Option.unwrap(~default="")

      Some(ELeftPartial(id, newName, expr))
    | EList(_, exprs) =>
      let newExprs = List.map(exprs, ~f=reconstructExpr) |> Option.values
      Some(EList(id, newExprs))
    | ETuple(_, first, second, theRest) =>
      let results =
        List.map(list{first, second, ...theRest}, ~f=reconstructExpr)
        |> Option.values

      switch results {
        | list{} => recover("unexpected reconstruction of invalid empty tuple", None)
        | list{el} => Some(el)
        | list{fst, snd, ...tail} =>
          Some(ETuple(id, fst, snd, tail))
      }
    | ERecord(id, entries) =>
      let newEntries =
        /* looping through original set of tokens (before transforming them into tuples)
         * so we can get the index field */
        tokensInRange(startPos, endPos, astInfo) |> List.filterMap(~f=ti =>
          switch ti.token {
          | TRecordFieldname({recordID, index, fieldName: newKey, exprID: _, parentBlockID: _})
            if recordID == id /* watch out for nested records */ =>
            List.getAt(~index, entries) |> Option.map(
              ~f=Tuple2.mapEach(
                ~f=/* replace key */ _ => newKey,
                ~g=\">>"(reconstructExpr, orDefaultExpr),
                // reconstruct value expression
              ),
            )
          | _ => None
          }
        )

      Some(ERecord(id, newEntries))
    | EPipe(_, e1, e2, exprs) =>
      list{e1, e2, ...exprs}
      |> List.map(~f=reconstructExpr)
      |> Option.values
      |> (
        x =>
          switch x {
          | list{} => Some(EPipe(id, EBlank(gid()), EBlank(gid()), list{}))
          | list{expr} => Some(EPipe(id, expr, EBlank(gid()), list{}))
          | list{e1, e2, ...rest} => Some(EPipe(id, e1, e2, rest))
          }
      )

    | EConstructor(eID, name, exprs) =>
      let newName = findTokenValue(tokens, eID, "constructor-name") |> Option.unwrap(~default="")

      let newExprs = List.map(exprs, ~f=\">>"(reconstructExpr, orDefaultExpr))
      let e = EConstructor(id, name, newExprs)
      if newName == "" {
        None
      } else if name != newName {
        Some(EPartial(gid(), newName, e))
      } else {
        Some(e)
      }
    | EMatch(_, cond, patternsAndExprs) =>
      let newPatternAndExprs = List.map(patternsAndExprs, ~f=((pattern, expr)) => {
        let toksToPattern = (tokens, pID) =>
          switch tokens |> List.filter(~f=((pID', _, _)) => pID == pID') {
          | list{(id, _, "pattern-blank")} => PBlank(id)
          | list{(id, value, "pattern-integer")} => PInteger(id, Util.coerceStringTo64BitInt(value))
          | list{(id, value, "pattern-variable")} => PVariable(id, value)
          | list{(id, value, "pattern-constructor-name"), ..._subPatternTokens} =>
            // temporarily assuming that PConstructor's sub-pattern tokens are always copied as well
            PConstructor(
              id,
              value,
              switch pattern {
              | PConstructor(_, _, ps) => ps
              | _ => list{}
              },
            )
          | list{(id, value, "pattern-string")} => PString(id, Util.trimQuotes(value))
          | list{(id, value, "pattern-true")} | list{(id, value, "pattern-false")} =>
            PBool(id, toBool_(value))
          | list{(id, _, "pattern-null")} => PNull(id)
          | list{
              (id, whole, "pattern-float-whole"),
              (_, _, "pattern-float-point"),
              (_, fraction, "pattern-float-fractional"),
            } =>
            let (sign, whole) = Sign.split(whole)
            PFloat(id, sign, whole, fraction)
          | list{(id, value, "pattern-float-whole"), (_, _, "pattern-float-point")}
          | list{(id, value, "pattern-float-whole")} =>
            PInteger(id, Util.coerceStringTo64BitInt(value))
          | list{(_, _, "pattern-float-point"), (id, value, "pattern-float-fractional")}
          | list{(id, value, "pattern-float-fractional")} =>
            PInteger(id, Util.coerceStringTo64BitInt(value))
          | _ => PBlank(gid())
          }

        let newPattern = toksToPattern(tokens, P.toID(pattern))
        (newPattern, reconstructExpr(expr) |> orDefaultExpr)
      })

      Some(EMatch(id, reconstructExpr(cond) |> orDefaultExpr, newPatternAndExprs))
    | EFeatureFlag(_, name, cond, disabled, enabled) =>
      // since we don't have any tokens associated with feature flags yet
      Some(
        EFeatureFlag(
          id,
          // should probably do some stuff about if the name token isn't fully selected
          name,
          reconstructExpr(cond) |> orDefaultExpr,
          reconstructExpr(enabled) |> orDefaultExpr,
          reconstructExpr(disabled) |> orDefaultExpr,
        ),
      )
    | EPipeTarget(_) => Some(EPipeTarget(gid()))
    }
  }

  let topmostID = getTopmostSelectionID(startPos, endPos, astInfo)
  reconstruct(~topmostID, (startPos, endPos))
}

let pasteOverSelection = (data: clipboardContents, astInfo: ASTInfo.t): ASTInfo.t => {
  let astInfo = deleteSelection(astInfo)
  let ast = astInfo.ast
  let mTi = ASTInfo.getToken(astInfo)
  let exprID = mTi |> Option.map(~f=ti => ti.token |> T.tid)
  let expr = Option.andThen(exprID, ~f=id => FluidAST.find(id, ast))
  let clipboardExpr = Clipboard.clipboardContentsToExpr(data)
  let text = Clipboard.clipboardContentsToString(data)
  let ct = mTi |> Option.andThen(~f=ti => caretTargetFromTokenInfo(astInfo.state.newPos, ti))

  switch expr {
  | Some(expr) =>
    let clipboardExpr = {
      /* [addPipeTarget pipeId initialExpr] replaces the first arg into which one can pipe with a pipe target having [pipeId]
       * at the root of [initialExpr], if such an arg exists.
       * It is recursive in order to handle root expressions inside partials. */
      let rec addPipeTarget = (pipeId: id, initialExpr: E.t): E.t =>
        switch initialExpr {
        | EFnCall(id, name, args, sendToRail) =>
          let args = switch args {
          | list{_, ...rest} => list{EPipeTarget(pipeId), ...rest}
          | args => args
          }

          EFnCall(id, name, args, sendToRail)
        | EBinOp(id, name, _lhs, rhs, sendToRail) =>
          EBinOp(id, name, EPipeTarget(pipeId), rhs, sendToRail)
        | EPartial(id, text, oldExpr) => EPartial(id, text, addPipeTarget(pipeId, oldExpr))
        | ERightPartial(id, text, oldExpr) =>
          ERightPartial(id, text, addPipeTarget(pipeId, oldExpr))
        | ELeftPartial(id, text, oldExpr) => ELeftPartial(id, text, addPipeTarget(pipeId, oldExpr))
        | _ => initialExpr
        }

      /* [removePipeTarget initialExpr] replaces all pipe targets at the root of [initialExpr] with blanks.
       * It is recursive in order to handle pipe targets inside partials. */
      let rec removePipeTarget = (initialExpr: E.t): E.t =>
        switch initialExpr {
        | EFnCall(id, name, args, sendToRail) =>
          let args = args |> List.map(~f=x =>
            switch x {
            | EPipeTarget(_) => E.newB()
            | arg => arg
            }
          )

          EFnCall(id, name, args, sendToRail)
        | EBinOp(id, name, lhs, rhs, sendToRail) =>
          let (lhs, rhs) = (lhs, rhs) |> Tuple2.mapAll(~f=x =>
            switch x {
            | EPipeTarget(_) => E.newB()
            | arg => arg
            }
          )

          EBinOp(id, name, lhs, rhs, sendToRail)
        | EPartial(id, text, oldExpr) => EPartial(id, text, removePipeTarget(oldExpr))
        | ERightPartial(id, text, oldExpr) => ERightPartial(id, text, removePipeTarget(oldExpr))
        | ELeftPartial(id, text, oldExpr) => ELeftPartial(id, text, removePipeTarget(oldExpr))
        | _ => initialExpr
        }

      switch FluidAST.findParent(E.toID(expr), ast) {
      | Some(EPipe(_, e1, _e2, _rest)) if e1 == expr =>
        // If pasting into the head of a pipe, drop any root-level pipe targets
        clipboardExpr |> Option.map(~f=e => removePipeTarget(e))
      | Some(EPipe(pipeId, _, _, _)) =>
        // If pasting into a child of a pipe, replace first arg of a root-level function with pipe target
        clipboardExpr |> Option.map(~f=e => addPipeTarget(pipeId, e))
      | _ =>
        // If not pasting into a child of a pipe, drop any root-level pipe targets
        clipboardExpr |> Option.map(~f=e => removePipeTarget(e))
      }
    }

    switch (expr, clipboardExpr, ct) {
    | (EBlank(id), Some(cp), _) =>
      // Paste into a blank
      let newAST = FluidAST.replace(~replacement=cp, id, ast)
      let caretTarget = caretTargetForEndOfExpr(E.toID(cp), newAST)
      astInfo |> ASTInfo.setAST(newAST) |> moveToCaretTarget(caretTarget)
    | (EString(id, str), _, Some({astRef: ARString(_, SPOpenQuote), offset})) =>
      let replacement = EString(id, String.insertAt(~value=text, ~index=offset - 1, str))

      let newAST = FluidAST.replace(~replacement, id, ast)
      let caretTarget = if offset == 0 {
        CT.forARStringOpenQuote(id, String.length(text) + 1)
      } else {
        CT.forARStringOpenQuote(id, offset + String.length(text))
      }

      astInfo |> ASTInfo.setAST(newAST) |> moveToCaretTarget(caretTarget)
    | (
        ERecord(id, oldKVs),
        Some(ERecord(_, pastedKVs)),
        Some({astRef: ARRecord(_, RPFieldname(index)), _}),
      ) =>
      /* Since fieldnames can't contain exprs, merge pasted record with existing record,
       * keeping duplicate fieldnames */
      let (first, last) = switch List.getAt(oldKVs, ~index) {
      | Some(
          "",
          EBlank(_),
        ) => /* not adding 1 to index ensures we don't include this entirely empty entry;
         * adding 1 ensures we don't include it after the paste either */
        (List.take(oldKVs, ~count=index), List.drop(oldKVs, ~count=index + 1))
      | _ => // adding 1 to index ensures pasting after the existing entry
        (List.take(oldKVs, ~count=index + 1), List.drop(oldKVs, ~count=index + 1))
      }

      let newKVs = List.flatten(list{first, pastedKVs, last})
      let replacement = ERecord(id, newKVs)
      List.last(pastedKVs)
      |> Option.map(~f=((_, valueExpr)) => {
        let caretTarget = caretTargetForEndOfExpr'(valueExpr)
        let newAST = FluidAST.replace(~replacement, id, ast)
        astInfo |> ASTInfo.setAST(newAST) |> moveToCaretTarget(caretTarget)
      })
      |> Option.unwrap(~default=astInfo)
    | (
        ERecord(_),
        Some(_),
        Some({astRef: ARRecord(_, RPFieldname(_)), _}),
      ) => /* Block pasting arbitrary expr into a record fieldname
       * since keys can't contain exprs */
      astInfo
    | _ =>
      text
      |> String.split(~on="")
      |> List.fold(~initial=astInfo, ~f=(astInfo, str) => {
        let space: FluidKeyboard.keyEvent = {
          key: K.Space,
          shiftKey: false,
          altKey: false,
          metaKey: false,
          ctrlKey: false,
        }

        let enter = {...space, key: K.Enter}
        let action = if str == " " {
          Keypress(space)
        } else if str == "\n" {
          Keypress(enter)
        } else {
          InsertText(str)
        }

        updateKey(action, astInfo)
      })
    }
  | _ => recover("pasting over non-existant handler", astInfo)
  }
}

let getCopySelection = (m: model): clipboardContents =>
  astInfoFromModel(m)
  |> Option.andThen(~f=(astInfo: ASTInfo.t) => {
    let (from, to_) = FluidUtil.getSelectionRange(astInfo.state)
    let text =
      ASTInfo.exprOfActiveEditor(astInfo) |> Printer.eToHumanString |> String.slice(~from, ~to_)

    let json =
      reconstructExprFromRange((from, to_), astInfo) |> Option.map(
        ~f=Clipboard.exprToClipboardContents,
      )

    Some(text, json)
  })
  |> Option.unwrap(~default=("", None))

let buildFeatureFlagEditors = (tlid: TLID.t, ast: FluidAST.t): list<fluidEditor> =>
  ast |> FluidAST.getFeatureFlags |> List.map(~f=e => FeatureFlagEditor(tlid, E.toID(e)))

// ------------------------
// update functions
// ------------------------

let updateMouseDoubleClick = (eventData: fluidMouseDoubleClick, astInfo: ASTInfo.t): ASTInfo.t => {
  let astInfo = ASTInfo.modifyState(astInfo, ~f=s => {
    ...s,
    midClick: false,
    activeEditor: eventData.editor,
  })

  let (selStart, selEnd) = switch eventData.selection {
  | SelectExpressionAt(newPos) =>
    astInfo
    |> ASTInfo.modifyState(~f=s => {...s, newPos: newPos, oldPos: s.newPos})
    |> getExpressionRangeAtCaret
    |> recoverOpt(~default=(0, 0), "no expression range found at caret")
  | SelectTokenAt(selectionStart, selectionEnd) =>
    switch FluidTokenizer.getToken'(
      ASTInfo.activeTokenInfos(astInfo),
      {...astInfo.state, newPos: selectionStart},
    ) {
    | Some({
        token: TFnName(_, displayName, _, _, _),
        startPos,
        _,
      }) => // Highlight the full function name
      (startPos, startPos + String.length(displayName))
    | Some(_) if selectionStart != selectionEnd => // there's an actual selection here, use it
      (selectionStart, selectionEnd)
    | Some({startPos, endPos, _}) => (startPos, endPos)
    | None => (selectionStart, selectionEnd)
    }
  }

  astInfo
  |> ASTInfo.modifyState(~f=s => {
    ...s,
    selectionStart: Some(selStart),
    oldPos: s.newPos,
    newPos: selEnd,
  })
  |> acClear
}

// Handle either a click or the end of a selection drag
let updateMouseUp = (eventData: fluidMouseUp, astInfo: ASTInfo.t): ASTInfo.t => {
  let astInfo =
    astInfo |> ASTInfo.modifyState(~f=s => {...s, midClick: false, activeEditor: eventData.editor})

  switch eventData.selection {
  | ClickAt(pos) => updateMouseClick(pos, astInfo)
  | SelectText(beginSel, endSel) =>
    ASTInfo.modifyState(astInfo, ~f=s => {
      ...s,
      selectionStart: Some(beginSel),
      newPos: endSel,
      oldPos: s.newPos,
    })
  }
}

// We completed a click outside: figure out how to complete it
let updateMouseUpExternal = (tlid: TLID.t, astInfo: ASTInfo.t): ASTInfo.t =>
  switch Entry.getFluidSelectionRange() {
  | Some(startPos, endPos) =>
    let selection = if startPos == endPos {
      ClickAt(startPos)
    } else {
      SelectText(startPos, endPos)
    }

    let eventData: fluidMouseUp = {
      tlid: tlid,
      editor: astInfo.state.activeEditor,
      selection: selection,
    }

    updateMouseUp(eventData, astInfo)
  | None => astInfo
  }

let updateMsg' = (m: model, tlid: TLID.t, astInfo: ASTInfo.t, msg: Types.fluidMsg): ASTInfo.t => {
  let astInfo = switch msg {
  | FluidCloseCmdPalette
  | FluidUpdateAutocomplete => // updateAutocomplete has already been run, so nothing more to do
    astInfo
  | FluidMouseUpExternal => updateMouseUpExternal(tlid, astInfo)
  | FluidMouseUp(eventData) => updateMouseUp(eventData, astInfo)
  | FluidMouseDoubleClick(eventData) => updateMouseDoubleClick(eventData, astInfo)
  | FluidCut => deleteSelection(astInfo)
  | FluidPaste(data) => pasteOverSelection(data, astInfo)
  // handle selection with direction key cases
  /* moving/selecting over expressions or tokens with shift-/alt-direction
   * or shift-/ctrl-direction */
  | FluidInputEvent(Keypress({key, shiftKey: true, altKey: _, ctrlKey: _, metaKey: _}) as ievt)
    if key == K.Right || (key == K.Left || (key == K.Up || key == K.Down)) =>
    /* Ultimately, all we want is for shift to move the end of the
     * selection to where the caret would have been if shift were not held.
     * Since the caret is tracked the same for end of selection and
     * movement, we actually just want to store the start position in
     * selection if there is no selection yet.
     *
     * TODO(JULIAN): We need to refactor updateKey and key handling in
     * general so that modifiers compose more easily with shift
     *
     * XXX(JULIAN): We need to be able to use alt and ctrl and meta to
     * change selection! */
    let newAstInfo = updateKey(ievt, astInfo)
    switch astInfo.state.selectionStart {
    | None =>
      let oldPos = astInfo.state.newPos
      {
        ...newAstInfo,
        state: {
          ...newAstInfo.state,
          newPos: newAstInfo.state.newPos,
          selectionStart: Some(oldPos),
        },
      }
    | Some(pos) => {
        ...newAstInfo,
        state: {
          ...newAstInfo.state,
          newPos: newAstInfo.state.newPos,
          selectionStart: Some(pos),
        },
      }
    }
  | FluidInputEvent(Keypress({key, shiftKey: false, altKey: _, ctrlKey: _, metaKey: _}) as ievt)
    if astInfo.state.selectionStart != None && (key == K.Right || key == K.Left) =>
    /* Aborting a selection using the left and right arrows should
         place the caret on the side of the selection in the direction
         of the pressed arrow key */
    let newPos = {
      let (left, right) = FluidUtil.getSelectionRange(astInfo.state)
      if key == K.Left {
        left
      } else {
        right
      }
    }

    {
      ...astInfo,
      state: {...astInfo.state, lastInput: ievt, newPos: newPos, selectionStart: None},
    }
  | FluidInputEvent(Keypress({key, altKey, metaKey, ctrlKey, shiftKey: _}))
    if (altKey || (metaKey || ctrlKey)) &&
      shouldDoDefaultAction(
        key,
      ) => // To make sure no letters are entered if user is doing a browser default action
    astInfo
  | FluidInputEvent(Keypress({key, shiftKey, _}) as ievt) =>
    let newAstInfo = updateKey(ievt, astInfo)
    let selectionStart = if shouldSelect(key) {
      newAstInfo.state.selectionStart
    } else if shiftKey && !(key == K.ShiftEnter) {
      // We dont want to persist selection on ShiftEnter
      astInfo.state.selectionStart
    } else {
      None
    }

    {...newAstInfo, state: {...newAstInfo.state, selectionStart: selectionStart}}
  | FluidInputEvent(InsertText(str)) if Option.is_some(astInfo.state.selectionStart) =>
    updateKey(ReplaceText(str), astInfo)
  | FluidInputEvent(ievt) => updateKey(ievt, astInfo)
  | FluidAutocompleteClick(entry) =>
    ASTInfo.getToken(astInfo)
    |> Option.map(~f=ti => acClick(entry, ti, astInfo))
    |> Option.unwrap(~default=astInfo)
  | FluidClearErrorDvSrc
  | FluidMouseDown(_)
  | FluidCommandsFilter(_)
  | FluidCommandsClick(_)
  | FluidFocusOnToken(_)
  | FluidUpdateDropdownIndex(_) => astInfo
  }

  let astInfo = updateAutocomplete(m, tlid, astInfo)

  // Js.log2 "ast" (show_ast newAST) ;
  // Js.log2 "tokens" (eToStructure s newAST) ;
  astInfo
}

let updateMsg = (m: model, tlid: TLID.t, ast: FluidAST.t, s: fluidState, msg: Types.fluidMsg): (
  FluidAST.t,
  fluidState,
  tokenInfos,
) => {
  let props = FluidUtil.propsFromModel(m)
  let astInfo = ASTInfo.make(props, ast, s)
  let astInfo = updateMsg'(m, tlid, astInfo, msg)
  (astInfo.ast, astInfo.state, ASTInfo.activeTokenInfos(astInfo))
}

let update = (m: Types.model, msg: Types.fluidMsg): Types.modification => {
  let s = m.fluidState
  let s = {...s, error: None, oldPos: s.newPos, actions: list{}}
  switch msg {
  | FluidUpdateDropdownIndex(index) if FluidCommands.isOpened(m.fluidState.cp) =>
    FluidCommands.cpSetIndex(m, index)
  | FluidUpdateDropdownIndex(index) =>
    ReplaceAllModificationsWithThisOne(
      m => {
        let fluidState = acSetIndex'(index, m.fluidState)
        ({...m, fluidState: fluidState}, Tea.Cmd.none)
      },
    )
  | FluidInputEvent(Keypress({key: K.Undo, _})) => KeyPress.undo_redo(m, false)
  | FluidInputEvent(Keypress({key: K.Redo, _})) => KeyPress.undo_redo(m, true)
  | FluidInputEvent(Keypress({key: K.CommandPalette(heritage), _})) =>
    let maybeOpen = maybeOpenCmd(m)
    let showToast = ReplaceAllModificationsWithThisOne(
      m => (
        {
          ...m,
          toast: {
            toastMessage: Some("Command Palette has been moved to Ctrl-\\."),
            toastPos: None,
          },
        },
        Tea.Cmd.none,
      ),
    )

    switch heritage {
    | K.LegacyShortcut => showToast
    | K.CurrentShortcut => maybeOpen
    }
  | FluidInputEvent(Keypress({key: K.Omnibox, _})) => KeyPress.openOmnibox(m)
  | FluidInputEvent(Keypress(ke)) if FluidCommands.isOpened(m.fluidState.cp) =>
    FluidCommands.updateCmds(m, ke)
  | FluidClearErrorDvSrc => FluidSetState({...m.fluidState, errorDvSrc: SourceNone})
  | FluidFocusOnToken(tlid, id) =>
    // Spec for Show token of expression: https://docs.google.com/document/d/13-jcP5xKe_Du-TMF7m4aPuDNKYjExAUZZ_Dk3MDSUtg/edit#heading=h.h1l570vp6wch
    tlid
    |> TL.get(m)
    |> Option.thenAlso(~f=TL.getAST)
    |> Option.map(~f=((tl, ast)) => {
      let pageMod = if CursorState.tlidOf(m.cursorState) != Some(tlid) {
        SetPage(TL.asPage(tl, true))
      } else {
        NoChange
      }

      let fluidState = {
        let astInfo = ASTInfo.make(FluidUtil.propsFromModel(m), ast, m.fluidState)

        let {state, _}: ASTInfo.t = moveToEndOfNonWhitespaceTarget(id, astInfo)

        {...state, errorDvSrc: SourceId(tlid, id)}
      }

      let moveMod = switch Viewport.moveToToken(id, tl) {
      | (Some(dx), Some(dy)) => MoveCanvasTo({x: dx, y: dy}, AnimateTransition)
      | (Some(dx), None) => MoveCanvasTo({x: dx, y: m.canvasProps.offset.y}, AnimateTransition)
      | (None, Some(dy)) => MoveCanvasTo({x: m.canvasProps.offset.x, y: dy}, AnimateTransition)
      | (None, None) => NoChange
      }

      if moveMod == NoChange && pageMod == NoChange {
        FluidSetState(fluidState)
      } else {
        Many(list{pageMod, moveMod, FluidSetState(fluidState)})
      }
    })
    |> Option.unwrap(~default=NoChange)
  | FluidCloseCmdPalette => FluidCommandsClose
  | FluidAutocompleteClick(FACCreateFunction(name, tlid, id)) =>
    Refactor.createAndInsertNewFunction(m, tlid, id, name)
  | FluidInputEvent(Keypress({key: K.Enter, _}))
  | FluidInputEvent(Keypress({key: K.Space, _}))
  | FluidInputEvent(Keypress({key: K.Tab, _}))
    if AC.highlighted(s.ac)
    |> Option.map(~f=FluidAutocomplete.isCreateFn)
    |> Option.unwrap(~default=false) =>
    switch AC.highlighted(s.ac) {
    | Some(FACCreateFunction(name, tlid, id)) =>
      Refactor.createAndInsertNewFunction(m, tlid, id, name)
    | _ => recover("this should not have happened", NoChange)
    }
  | FluidMouseDown(_)
  | FluidInputEvent(_)
  | FluidPaste(_)
  | FluidCut
  | FluidCommandsFilter(_)
  | FluidCommandsClick(_)
  | FluidAutocompleteClick(_)
  | FluidUpdateAutocomplete
  | FluidMouseDoubleClick(_)
  | FluidMouseUpExternal
  | FluidMouseUp(_) =>
    let tlid = switch msg {
    | FluidMouseUp({tlid, _}) => Some(tlid)
    | _ => CursorState.tlidOf(m.cursorState)
    }

    let tl: option<toplevel> = Option.andThen(tlid, ~f=Toplevel.get(m))
    let ast = Option.andThen(tl, ~f=TL.getAST)
    switch (tl, ast) {
    | (Some(tl), Some(ast)) =>
      let tlid = TL.id(tl)
      let (newAST, newState, newTokens) = updateMsg(m, tlid, ast, s, msg)
      let (eventSpecMod, newAST, newState) = {
        let isFluidEntering = /* Only fire Tab controls if the state is currently in
         * entering, as some keypresses fire in both editors. */
        switch m.cursorState {
        | FluidEntering(_) => true
        | _ => false
        }

        let enter = id => Enter(tlid, id)
        // if tab is wrapping...
        let lastKey = switch newState.lastInput {
        | Keypress({key, _}) => Some(key)
        | _ => None
        }

        if isFluidEntering && (lastKey == Some(K.Tab) && newState.newPos <= newState.oldPos) {
          // get the first blank spec header, or fall back to NoChange
          switch tl {
          | TLHandler({spec, _}) =>
            switch SpecHeaders.firstBlank(spec) {
            | Some(id) => (enter(id), ast, s)
            | None => (NoChange, newAST, newState)
            }
          | _ => (NoChange, newAST, newState)
          }
        } else if (
          isFluidEntering && (lastKey == Some(K.ShiftTab) && newState.newPos >= newState.oldPos)
        ) {
          // get the last blank spec header, or fall back to NoChange
          switch tl {
          | TLHandler({spec, _}) =>
            switch SpecHeaders.lastBlank(spec) {
            | Some(id) => (enter(id), ast, s)
            | None => (NoChange, newAST, newState)
            }
          | _ => (NoChange, newAST, newState)
          }
        } else {
          (NoChange, newAST, newState)
        }
      }

      let cmd = switch newState.ac.index {
      | Some(index) => FluidAutocomplete.focusItem(index)
      | None => Tea.Cmd.none
      }

      let astMod = if ast != newAST {
        let requestAnalysis = switch Analysis.getSelectedTraceID(m, tlid) {
        | Some(traceID) =>
          let m = TL.withAST(m, tlid, newAST)
          MakeCmd(Analysis.requestAnalysis(m, tlid, traceID))
        | None => NoChange
        }

        let astCacheMod = /* We want to update the AST cache for searching, however, we
         * may be editing the feature flag section in which case we'd
         * be putting the wrong information into the cache. As a simple
         * solution for now, only update if we're in the main editor. */
        if s.activeEditor == MainEditor(tlid) {
          UpdateASTCache(tlid, Printer.tokensToString(newTokens))
        } else {
          NoChange
        }

        Many(list{
          ReplaceAllModificationsWithThisOne(m => (TL.withAST(m, tlid, newAST), Tea.Cmd.none)),
          Toplevel.setSelectedAST(m, newAST),
          requestAnalysis,
          astCacheMod,
        })
      } else {
        Types.NoChange
      }

      Types.Many(list{
        ReplaceAllModificationsWithThisOne(m => ({...m, fluidState: newState}, Tea.Cmd.none)),
        astMod,
        eventSpecMod,
        Types.MakeCmd(cmd),
      })
    | _ => NoChange
    }
  }
}

// --------------------
// Scaffolidng
// --------------------

let renderCallback = (m: model): unit =>
  switch m.cursorState {
  | FluidEntering(_) if m.fluidState.midClick == false =>
    if FluidCommands.isOpened(m.fluidState.cp) {
      ()
    } else {
      switch /* This for two different purposes:
         * 1. When a key press mutates the text in the content editable, the browser resets the caret position to the * beginnning of the content editable. Here we set the caret to the correct position from the fluidState
         * 2. We intercept all keyboard caret movement, therefore we need to set the caret to the correct position
         * from the fluidState

         * We do this after a render(not waiting til the next frame) so that the developer does not see the caret
         * flicker to default browser position
 */
      m.fluidState.selectionStart {
      | Some(selStart) =>
        // Updates the browser selection range for 2 in the context of selections
        Entry.setFluidSelectionRange(selStart, m.fluidState.newPos)
      | None => Entry.setFluidCaret(m.fluidState.newPos)
      }
    }
  | _ => ()
  }

let cleanUp = (m: model, tlid: option<TLID.t>): (model, modification) => {
  let rmPartialsMod =
    tlid
    |> Option.andThen(~f=TL.get(m))
    |> Option.thenAlso(~f=tl =>
      // Keep transient state as we're trying to commit it
      astInfoFromModelAndTLID(~removeTransientState=false, m, TL.id(tl))
    )
    |> Option.andThen(~f=((tl, astInfo)) => {
      let newAstInfo = acMaybeCommit(0, astInfo)
      let newAST = newAstInfo.ast |> FluidAST.map(~f=AST.removePartials)
      if newAST != astInfo.ast {
        Some(TL.setASTMod(tl, newAST))
      } else {
        None
      }
    })
    |> Option.unwrap(~default=NoChange)

  let acVisibilityModel = if AC.isOpened(m.fluidState.ac) {
    AC.updateAutocompleteVisibility(m)
  } else if Commands.isOpened(m.fluidState.cp) {
    Commands.updateCommandPaletteVisibility(m)
  } else {
    m
  }

  (acVisibilityModel, rmPartialsMod)
}
