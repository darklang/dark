open Prelude
module T = FluidToken
module Expr = ProgramTypes.Expr
module E = FluidExpression
module Pattern = FluidPattern

type tokenInfo = Types.fluidTokenInfo

type featureFlagTokenization =
  | @ocaml.doc(" FeatureFlagOnlyDisabled is used in the main editor panel to only
          * show the flag's old code ")
  FeatureFlagOnlyDisabled

  | @ocaml.doc(" FeatureFlagConditionAndEnabled is used in the secondary editor
          * panel for editing a flag's condition and new code ")
  FeatureFlagConditionAndEnabled

// --------------------------------------
// Convert FluidExpressions to tokenInfos
// --------------------------------------

module Builder = {
  type t = {
    @ocaml.doc(" [tokens] list is kept reversed while being built up, as adding
            * things to the front of the list is an order of magnitude faster.
            * We were having large slowdowns on large handlers before this. ")
    tokens: list<fluidToken>,
    @ocaml.doc(" [indent] tracks the indent after a newline ")
    indent: int,
    @ocaml.doc(" [xPos] tracks the indent for nesting.
            * `None` indicates it's ready to go after a newline ")
    xPos: option<int>,
    ffTokenization: featureFlagTokenization,
  }

  let rec endsInNewline = (b: t): bool =>
    // The latest token is on the front
    switch b.tokens {
    | list{TNewline(_), ..._} => true
    | list{TIndent(_), ...tail} => endsInNewline({...b, tokens: tail})
    | _ => false
    }

  let empty = {
    tokens: list{},
    xPos: Some(0),
    indent: 0,
    ffTokenization: FeatureFlagOnlyDisabled,
  }

  let lineLimit = 120

  let strLimit = 40

  let listLimit = 60

  let add = (token: fluidToken, b: t): t => {
    let tokenLength = token |> T.toText |> String.length
    let (newTokens, xPos) = // Add new tokens on the front
    if endsInNewline(b) {
      (
        if b.indent != 0 {
          list{token, TIndent(b.indent), ...b.tokens}
        } else {
          list{token, ...b.tokens}
        },
        Some(b.indent + tokenLength),
      )
    } else {
      let newXPos = switch token {
      | TNewline(_) => None
      | _ =>
        let old = Option.unwrap(b.xPos, ~default=b.indent)
        Some(old + tokenLength)
      }

      (list{token, ...b.tokens}, newXPos)
    }

    {...b, tokens: newTokens, xPos: xPos}
  }

  let addIf = (cond: bool, token: fluidToken, b: t): t =>
    if cond {
      add(token, b)
    } else {
      b
    }

  /* Take a list of 'a, and iterate through them, adding them to `b` by
   * calling `f` on them */
  let addIter = (xs: list<'a>, ~f: (int, 'a, t) => t, b: t): t =>
    List.fold(xs, ~initial=(b, 0), ~f=((b, i), x) => (f(i, x, b), i + 1)) |> Tuple2.first

  let addMany = (tokens: list<fluidToken>, b: t): t =>
    List.fold(tokens, ~initial=b, ~f=(acc, t) => add(t, acc))

  @ocaml.doc(" [indentBy ~ident ~f b] calls [f] with a modified [b] having additional
    * indentation by [indent] characters (that is, b.indent + ~indent), then
    * returns the result of that [f b] invocation with the original indent of
    * [b] restored. ")
  let indentBy = (~indent: int, ~f: t => t, b: t): t => {
    let oldIndent = b.indent
    {...b, indent: b.indent + indent} |> f |> (b => {...b, indent: oldIndent})
  }

  let addNested = (~f: t => t, b: t): t => {
    let oldIndent = b.indent
    let newIndent = Option.unwrap(~default=b.indent, b.xPos)
    {...b, indent: newIndent} |> f |> (b => {...b, indent: oldIndent})
  }

  let addNewlineIfNeeded = (nlInfo: option<(id, id, option<int>)>, b: t): t =>
    if endsInNewline(b) {
      b
    } else {
      add(TNewline(nlInfo), b)
    }

  let asTokens = (b: t): list<fluidToken> =>
    // Tokens are stored reversed
    List.reverse(b.tokens)
}

let rec patternToToken = (p: FluidPattern.t, ~idx: int): list<fluidToken> =>
  switch p {
  | PVariable(mid, id, name) => list{TPatternVariable(mid, id, name, idx)}
  | PConstructor(mid, id, name, args) =>
    let args = List.map(args, ~f=a => list{TSep(id, None), ...patternToToken(a, ~idx)})

    List.flatten(list{list{TPatternConstructorName(mid, id, name, idx)}, ...args})
  | PInteger(mid, id, i) => list{TPatternInteger(mid, id, i, idx)}
  | PBool(mid, id, b) =>
    if b {
      list{TPatternTrue(mid, id, idx)}
    } else {
      list{TPatternFalse(mid, id, idx)}
    }
  | PString({matchID: mid, patternID: id, str}) => list{
      TPatternString({matchID: mid, patternID: id, str: str, branchIdx: idx}),
    }
  | PFloat(mID, id, whole, fraction) =>
    let whole = if whole == "" {
      list{}
    } else {
      list{TPatternFloatWhole(mID, id, whole, idx)}
    }

    let fraction = if fraction == "" {
      list{}
    } else {
      list{TPatternFloatFractional(mID, id, fraction, idx)}
    }

    Belt.List.concatMany([whole, list{TPatternFloatPoint(mID, id, idx)}, fraction])
  | PNull(mid, id) => list{TPatternNullToken(mid, id, idx)}
  | PBlank(mid, id) => list{TPatternBlank(mid, id, idx)}
  }

let rec toTokens' = (~parentID=None, e: E.t, b: Builder.t): Builder.t => {
  open Builder
  let ghostPartial = (id, newName, oldName) => {
    let ghostSuffix = String.dropLeft(~count=String.length(newName), oldName)
    if ghostSuffix == "" {
      list{}
    } else {
      list{TPartialGhost(id, ghostSuffix, None)}
    }
  }

  /* placeholderFor = (id * string * int)
   * id: id of the placeholder-containing expr
   * string: name of the placeholder-containing expr
   * int: index of the placeholder within the expr's parameters
   */
  let nest = (
    ~placeholderFor: option<(id, string, int)>=None,
    ~indent,
    e: E.t,
    b: Builder.t,
  ): Builder.t => {
    let tokensFn = b =>
      switch (e, placeholderFor) {
      | (EBlank(id), Some(fnID, fnname, pos)) =>
        let name =
          Functions.global()
          |> Functions.find(fnname)
          |> Option.andThen(~f=fn => List.getAt(~index=pos, fn.fnParameters))
          |> Option.map(~f=p => {name: p.paramName, tipe: tipe2str(p.paramTipe)})

        switch name {
        | None => toTokens'(e, b)
        | Some(placeholder) =>
          add(
            TPlaceholder({
              blankID: id,
              parentBlockID: Some(fnID),
              placeholder: placeholder,
              fnID: fnID,
            }),
            b,
          )
        }
      | _ => toTokens'(e, b)
      }

    b |> indentBy(~indent, ~f=addNested(~f=tokensFn))
  }

  let addArgs = (name: string, id: id, args: list<E.t>, b: Builder.t): Builder.t => {
    let (args, offset) = switch args {
    | list{EPipeTarget(_), ...args} => (args, 1)
    | _ => (args, 0)
    }

    let reflow = {
      let tokens =
        args
        |> List.map(~f=a => toTokens'(a, Builder.empty))
        |> List.map(~f=Builder.asTokens)
        |> List.flatten

      let length =
        (tokens |> List.map(~f=\">>"(T.toText, String.length)))->List.sum(module(Int))
        |> \"+" /* separators, including at the front */(List.length(args))
        |> \"+"(Option.unwrap(~default=0, b.xPos))

      let tooLong = length > lineLimit
      let needsNewlineBreak =
        // newlines aren't disruptive in the last argument
        args
        |> List.initial
        |> Option.unwrap(~default=list{})
        |> List.map(~f=a => toTokens'(a, Builder.empty))
        |> List.map(~f=Builder.asTokens)
        |> List.flatten
        |> List.any(~f=x =>
          switch x {
          | TNewline(_) => true
          | _ => false
          }
        )

      tooLong || needsNewlineBreak
    }

    b |> addIter(args, ~f=(i, e, b) =>
      if reflow {
        b
        |> addNewlineIfNeeded(Some(id, id, Some(offset + i)))
        |> nest(~indent=2, ~placeholderFor=Some(id, name, offset + i), e)
      } else {
        b
        |> add(TSep(E.toID(e), None))
        |> nest(~indent=0, ~placeholderFor=Some(id, name, offset + i), e)
      }
    )
  }

  switch e {
  | EInteger(id, i) => b |> add(TInteger(id, i, parentID))
  | EBool(id, bool') =>
    b |> add(
      if bool' {
        TTrue(id, parentID)
      } else {
        TFalse(id, parentID)
      },
    )
  | ENull(id) => b |> add(TNullToken(id, parentID))
  | EFloat(id, whole, fraction) =>
    let whole = if whole == "" {
      list{}
    } else {
      list{TFloatWhole(id, whole, parentID)}
    }

    let fraction = if fraction == "" {
      list{}
    } else {
      list{TFloatFractional(id, fraction, parentID)}
    }

    b |> addMany(Belt.List.concatMany([whole, list{TFloatPoint(id, parentID)}, fraction]))
  | EBlank(id) => b |> add(TBlank(id, parentID))
  | ELet(id, lhs, rhs, next) =>
    let rhsID = E.toID(rhs)
    b
    |> add(TLetKeyword(id, rhsID, parentID))
    |> add(TLetVarName(id, rhsID, lhs, parentID))
    |> add(TLetAssignment(id, rhsID, parentID))
    |> addNested(~f=toTokens'(rhs))
    |> addNewlineIfNeeded(Some(E.toID(next), id, None))
    |> addNested(~f=toTokens'(next))
  | EString(id, str) =>
    let strings = if String.length(str) > strLimit {
      String.segment(~size=strLimit, str)
    } else {
      list{str}
    }

    switch strings {
    | list{} => add(TString(id, "", parentID), b)
    | list{starting, ...rest} =>
      switch List.reverse(rest) {
      | list{} => add(TString(id, str, parentID), b)
      | list{ending, ...revrest} =>
        b |> addNested(~f=b => {
          let endingOffset = strLimit * (List.length(revrest) + 1)
          b
          |> add(TStringMLStart(id, starting, 0, str))
          |> add(TNewline(None))
          |> addIter(List.reverse(revrest), ~f=(i, s, b) =>
            b |> add(TStringMLMiddle(id, s, strLimit * (i + 1), str)) |> add(TNewline(None))
          )
          |> add(TStringMLEnd(id, ending, endingOffset, str))
        })
      }
    }
  | EIf(id, cond, if', else') =>
    b
    |> add(TIfKeyword(id, parentID))
    |> addNested(~f=toTokens'(cond))
    |> addNewlineIfNeeded(None)
    |> add(TIfThenKeyword(id, parentID))
    |> addNewlineIfNeeded(Some(E.toID(if'), id, None))
    |> nest(~indent=2, if')
    |> addNewlineIfNeeded(None)
    |> add(TIfElseKeyword(id, parentID))
    |> add(TNewline(Some(E.toID(else'), id, None)))
    |> nest(~indent=2, else')
  | EBinOp(id, op, lexpr, rexpr, _ster) =>
    let start = b =>
      switch lexpr {
      | EPipeTarget(_) => b
      | _ =>
        b
        |> nest(~indent=0, ~placeholderFor=Some(id, op, 0), lexpr)
        |> add(TSep(E.toID(lexpr), parentID))
      }

    b
    |> start
    |> addMany(list{TBinOp(id, op, parentID), TSep(id, parentID)})
    |> nest(~indent=0, ~placeholderFor=Some(id, op, 1), rexpr)
  | EPartial(id, newName, EBinOp(_, oldName, lexpr, rexpr, _ster)) =>
    let ghost = ghostPartial(id, newName, FluidUtil.ghostPartialName(oldName))

    let start = b =>
      switch lexpr {
      | EPipeTarget(_) => b
      | _ =>
        b
        |> nest(~indent=0, ~placeholderFor=Some(id, oldName, 0), lexpr)
        |> add(TSep(E.toID(lexpr), parentID))
      }

    b
    |> start
    |> add(TPartial(id, newName, parentID))
    |> addMany(ghost)
    |> add(TSep(id, parentID))
    |> nest(~indent=2, ~placeholderFor=Some(id, oldName, 1), rexpr)
  | EFnCall(id, fnName, args, ster) =>
    let displayName = FluidUtil.fnDisplayName(fnName)
    let versionDisplayName = FluidUtil.versionDisplayName(fnName)
    let partialName = FluidUtil.fnDisplayNameWithVersion(fnName)
    let versionToken = if versionDisplayName == "" {
      list{}
    } else {
      list{TFnVersion(id, partialName, versionDisplayName, fnName)}
    }

    b
    |> add(TFnName(id, partialName, displayName, fnName, ster))
    |> addMany(versionToken)
    |> addArgs(fnName, id, args)
  | EPartial(id, newName, EFnCall(_, oldName, args, _)) =>
    let partial = TPartial(id, newName, parentID)
    let newText = T.toText(partial)
    let oldText = FluidUtil.ghostPartialName(oldName)
    let ghost = ghostPartial(id, newText, oldText)
    b |> add(partial) |> addMany(ghost) |> addArgs(oldName, id, args)
  | EConstructor(id, name, exprs) =>
    b |> add(TConstructorName(id, name)) |> addArgs(name, id, exprs)
  | EPartial(id, newName, EConstructor(_, oldName, exprs)) =>
    let partial = TPartial(id, newName, parentID)
    let newText = T.toText(partial)
    let ghost = ghostPartial(id, newText, oldName)
    b |> add(partial) |> addMany(ghost) |> addArgs(oldName, id, exprs)
  | EFieldAccess(id, expr, fieldname) =>
    let lhsid = E.toID(expr)
    b
    |> addNested(~f=toTokens'(expr, ~parentID))
    |> addMany(list{TFieldOp(id, lhsid, parentID), TFieldName(id, lhsid, fieldname, parentID)})
  | EPartial(id, newFieldname, EFieldAccess(faID, expr, oldFieldname)) =>
    let lhsid = E.toID(expr)
    let partial = TFieldPartial(id, faID, lhsid, newFieldname, parentID)
    let newText = T.toText(partial)
    let ghost = ghostPartial(id, newText, oldFieldname)
    b
    |> addNested(~f=toTokens'(expr))
    |> addMany(list{TFieldOp(id, E.toID(expr), parentID), partial})
    |> addMany(ghost)
  | EVariable(id, name) => b |> add(TVariable(id, name, parentID))
  | ELambda(id, names, body) =>
    let isLast = i => i == List.length(names) - 1
    b
    |> add(TLambdaSymbol(id, parentID))
    |> addIter(names, ~f=(i, (aid, name), b) =>
      b
      |> add(TLambdaVar(id, aid, i, name, parentID))
      |> addIf(!isLast(i), TLambdaComma(id, i, parentID))
      |> addIf(!isLast(i), TSep(aid, parentID))
    )
    |> add(TLambdaArrow(id, parentID))
    |> nest(~indent=2, body)
  | EList(id, exprs) =>
    /*
         With each iteration of the list, we calculate the new line length, if we were to add this new item. If the new line length exceeds the limit, then we add a new line token and an indent by 1 first, before adding the tokenized item to the builder.
 */
    let lastIndex = List.length(exprs) - 1
    let xOffset = b.xPos |> Option.unwrap(~default=0)
    let pid = if lastIndex == -1 {
      None
    } else {
      Some(id)
    }
    b
    |> add(TListOpen(id, pid))
    |> addIter(exprs, ~f=(i, e, b') => {
      let currentLineLength = {
        let commaWidth = if i != lastIndex {
          1
        } else {
          0
        }
        toTokens'(e, b').xPos
        |> Option.map(~f=x => x - xOffset + commaWidth)
        |> Option.unwrap(~default=commaWidth)
      }

      // Even if first element overflows, don't put it in a new line
      let isOverLimit = i > 0 && currentLineLength > listLimit
      // Indent after newlines to match the '[ '
      let indent = if isOverLimit {
        1
      } else {
        0
      }
      b'
      |> addIf(isOverLimit, TNewline(None))
      |> indentBy(~indent, ~f=b' =>
        b'
        |> addNested(~f=toTokens'(~parentID=Some(id), e))
        |> addIf(i != lastIndex, TListComma(id, i))
      )
    })
    |> add(TListClose(id, pid))
  | ERecord(id, fields) =>
    if fields == list{} {
      b |> addMany(list{TRecordOpen(id, None), TRecordClose(id, None)})
    } else {
      let parentBlockID = Some(id)
      b
      |> add(TRecordOpen(id, parentBlockID))
      |> indentBy(~indent=2, ~f=b =>
        addIter(fields, b, ~f=(i, (fieldName, expr), b) => {
          let exprID = E.toID(expr)
          b
          |> addNewlineIfNeeded(Some(id, id, Some(i)))
          |> add(
            TRecordFieldname({
              recordID: id,
              exprID: exprID,
              index: i,
              fieldName: fieldName,
              parentBlockID: parentBlockID,
            }),
          )
          |> add(TRecordSep(id, i, exprID))
          |> addNested(~f=toTokens'(~parentID=Some(id), expr))
        })
      )
      |> addMany(list{
        TNewline(Some(id, id, Some(List.length(fields)))),
        TRecordClose(id, parentBlockID),
      })
    }
  | EPipe(id, exprs) =>
    let length = List.length(exprs)
    switch exprs {
    | list{} => recover("Empty pipe found", ~debug=e, b)
    | list{single} => recover("pipe with single entry found", ~debug=e, toTokens'(single, b))
    | list{head, ...tail} =>
      b
      |> addNested(~f=toTokens'(head))
      |> addNewlineIfNeeded(Some(E.toID(head), id, Some(0)))
      |> addIter(tail, ~f=(i, e, b) =>
        b
        |> add(TPipe(id, i, length, parentID))
        |> addNested(~f=toTokens'(~parentID, e))
        |> addNewlineIfNeeded(Some(E.toID(e), id, Some(i + 1)))
      )
      |> addNewlineIfNeeded(Some(id, id, Some(List.length(tail))))
    }
  | EPipeTarget(_) => recover("should never be making tokens for EPipeTarget", ~debug=e, b)
  | EMatch(id, mexpr, pairs) =>
    b
    |> add(TMatchKeyword(id))
    |> addNested(~f=toTokens'(mexpr))
    |> indentBy(~indent=2, ~f=b =>
      b
      |> addIter(pairs, ~f=(i, (pattern, expr), b) =>
        b
        |> addNewlineIfNeeded(Some(id, id, Some(i)))
        |> addMany(patternToToken(pattern, ~idx=i))
        |> add(
          TMatchBranchArrow({
            matchID: id,
            patternID: Pattern.toID(pattern),
            index: i,
          }),
        )
        |> addNested(~f=toTokens'(expr))
      )
      |> addNewlineIfNeeded(Some(id, id, Some(List.length(pairs))))
    )
  | EPartial(id, str, _) => b |> add(TPartial(id, str, parentID))
  | ERightPartial(id, newOp, expr) =>
    b
    |> addNested(~f=toTokens'(expr))
    |> addMany(list{TSep(id, parentID), TRightPartial(id, newOp, parentID)})
  | ELeftPartial(id, str, expr) =>
    b |> add(TLeftPartial(id, str, parentID)) |> addNested(~f=toTokens'(expr))
  | EFeatureFlag(id, _name, cond, disabled, enabled) =>
    /* Feature flag tokens are displayed in two different editor panels, so
     * they are built differently depending on the current builder option. */
    switch b.ffTokenization {
    | FeatureFlagOnlyDisabled => b |> addNested(~f=toTokens'(disabled))
    | FeatureFlagConditionAndEnabled =>
      b
      |> add(TFlagWhenKeyword(id))
      |> addNested(~f=toTokens'(cond))
      |> addNewlineIfNeeded(None)
      |> add(TFlagEnabledKeyword(id))
      |> addNewlineIfNeeded(Some(E.toID(enabled), id, None))
      |> nest(~indent=2, enabled)
    }
  }
}

let infoize = (tokens): list<tokenInfo> => {
  let (row, col, pos) = (ref(0), ref(0), ref(0))
  List.map(tokens, ~f=token => {
    let length = String.length(T.toText(token))
    let ti = {
      token: token,
      startRow: row.contents,
      startCol: col.contents,
      startPos: pos.contents,
      endPos: pos.contents + length,
      length: length,
    }

    switch token {
    | TNewline(_) =>
      row := row.contents + 1
      col := 0
    | _ => col := col.contents + length
    }
    pos := pos.contents + length
    ti
  })
}

let validateTokens = (tokens: list<fluidToken>): list<fluidToken> => {
  List.forEach(tokens, ~f=t => {
    asserT("invalid token", String.length(T.toText(t)) > 0, ~debug=t)
    ()
  })
  tokens
}

// Remove artifacts of the token generation process
let tidy = (tokens: list<fluidToken>): list<fluidToken> =>
  tokens |> List.filter(~f=x =>
    switch x {
    | TIndent(0) => false
    | _ => true
    }
  )

let tokenizeWithFFTokenization = (
  ffTokenization: featureFlagTokenization,
  e: FluidExpression.t,
): list<tokenInfo> =>
  {...Builder.empty, ffTokenization: ffTokenization}
  |> toTokens'(e)
  |> Builder.asTokens
  |> tidy
  |> validateTokens
  |> infoize

let tokenize: E.t => list<FluidToken.tokenInfo> = tokenizeWithFFTokenization(
  FeatureFlagOnlyDisabled,
)

let tokensForEditor = (e: fluidEditor, ast: FluidAST.t): list<FluidToken.tokenInfo> =>
  switch e {
  | NoEditor => list{}
  | MainEditor(_) => tokenize(FluidAST.toExpr(ast))
  | FeatureFlagEditor(_, id) =>
    FluidAST.find(id, ast)
    |> Option.map(~f=tokenizeWithFFTokenization(FeatureFlagConditionAndEnabled))
    |> recoverOpt(
      "could not find expression id = " ++ (ID.toString(id) ++ " when tokenizing FF editor"),
      ~default=list{},
    )
  }

let tokenizeForEditor = (e: fluidEditor, expr: FluidExpression.t): list<FluidToken.tokenInfo> =>
  switch e {
  | NoEditor => list{}
  | MainEditor(_) => tokenize(expr)
  | FeatureFlagEditor(_) => tokenizeWithFFTokenization(FeatureFlagConditionAndEnabled, expr)
  }

// --------------------------------------
// ASTInfo
// --------------------------------------
type tokenInfos = list<T.tokenInfo>

type neighbour =
  | L(T.t, T.tokenInfo)
  | R(T.t, T.tokenInfo)
  | No

let rec getTokensAtPosition = (~prev=None, ~pos: int, tokens: tokenInfos): (
  option<T.tokenInfo>,
  option<T.tokenInfo>,
  option<T.tokenInfo>,
) => {
  // Get the next token and the remaining tokens, skipping indents.
  let rec getNextToken = (infos: tokenInfos): (option<T.tokenInfo>, tokenInfos) =>
    switch infos {
    | list{ti, ...rest} =>
      if T.isSkippable(ti.token) {
        getNextToken(rest)
      } else {
        (Some(ti), rest)
      }
    | list{} => (None, list{})
    }

  switch getNextToken(tokens) {
  | (None, _remaining) => (prev, None, None)
  | (Some(current), remaining) =>
    if current.endPos > pos {
      let (next, _) = getNextToken(remaining)
      (prev, Some(current), next)
    } else {
      getTokensAtPosition(~prev=Some(current), ~pos, remaining)
    }
  }
}

let getNeighbours = (~pos: int, tokens: tokenInfos): (
  neighbour,
  neighbour,
  option<T.tokenInfo>,
) => {
  let (mPrev, mCurrent, mNext) = getTokensAtPosition(~pos, tokens)
  let toTheRight = switch mCurrent {
  | Some(current) => R(current.token, current)
  | _ => No
  }

  // The token directly before the cursor (skipping whitespace)
  let toTheLeft = switch (mPrev, mCurrent) {
  | (Some(prev), _) if prev.endPos >= pos => L(prev.token, prev)
  // The left might be separated by whitespace
  | (Some(prev), Some(current)) if current.startPos >= pos => L(prev.token, prev)
  | (None, Some(current)) if current.startPos < pos =>
    // We could be in the middle of a token
    L(current.token, current)
  | (None, _) => No
  | (_, Some(current)) => L(current.token, current)
  | (Some(prev), None) =>
    // Last position in the ast
    L(prev.token, prev)
  }

  (toTheLeft, toTheRight, mNext)
}

let getTokenNotWhitespace = (tokens: tokenInfos, s: fluidState): option<T.tokenInfo> => {
  let (left, right, _) = getNeighbours(~pos=s.newPos, tokens)
  switch (left, right) {
  | (L(_, lti), R(TNewline(_), _)) => Some(lti)
  | (L(lt, lti), _) if T.isTextToken(lt) => Some(lti)
  | (_, R(_, rti)) => Some(rti)
  | (L(_, lti), _) => Some(lti)
  | _ => None
  }
}

let getToken' = (tokens: tokenInfos, s: fluidState): option<T.tokenInfo> => {
  let (toTheLeft, toTheRight, _) = getNeighbours(~pos=s.newPos, tokens)

  /* The algorithm that decides what token on when a certain key is pressed is
   * in updateKey. It's pretty complex and it tells us what token a keystroke
   * should apply to. For all other places that need to know what token we're
   * on, this attemps to approximate that.

   * The cursor at newPos is either in a token (eg 3 chars into "myFunction"),
   * or between two tokens (eg 1 char into "4 + 2").

   * If we're between two tokens, we decide by looking at whether the left
   * token is a text token. If it is, it's likely that we're just typing.
   * Otherwise, the important token is probably the right token.
   *
   * Example: `4 + 2`, when the cursor is at position: 0): 4 is to the right,
   * nothing to the left. Choose 4 1): 4 is a text token to the left, choose 4
   * 2): the token to the left is not a text token (it's a TSep), so choose +
   * 3): + is a text token to the left, choose + 4): 2 is to the right, nothing
   * to the left. Choose 2 5): 2 is a text token to the left, choose 2
   *
   * Reminder that this is an approximation. If we find bugs we may need to go
   * much deeper.
 */
  switch (toTheLeft, toTheRight) {
  | (L(_, ti), _) if T.isTextToken(ti.token) => Some(ti)
  | (_, R(_, ti)) => Some(ti)
  | (L(_, ti), _) => Some(ti)
  | _ => None
  }
}

module ASTInfo = {
  type t = {
    ast: FluidAST.t,
    state: fluidState,
    mainTokenInfos: tokenInfos,
    featureFlagTokenInfos: list<(id, tokenInfos)>,
    props: fluidProps,
  }

  let setAST = (ast: FluidAST.t, astInfo: t): t =>
    if astInfo.ast == ast {
      astInfo
    } else {
      let mainTokenInfos = tokenize(FluidAST.toExpr(ast))
      let featureFlagTokenInfos =
        ast
        |> FluidAST.getFeatureFlags
        |> List.map(~f=expr => (
          E.toID(expr),
          tokenizeWithFFTokenization(FeatureFlagConditionAndEnabled, expr),
        ))

      {
        ...astInfo,
        ast: ast,
        mainTokenInfos: mainTokenInfos,
        featureFlagTokenInfos: featureFlagTokenInfos,
      }
    }

  let ffTokenInfosFor = (ffid: id, astInfo: t): option<tokenInfos> =>
    List.find(astInfo.featureFlagTokenInfos, ~f=((id, _)) => ffid == id) |> Option.map(
      ~f=Tuple2.second,
    )

  // Get the correct tokenInfos for the activeEditor
  let activeTokenInfos = (astInfo: t): tokenInfos =>
    switch astInfo.state.activeEditor {
    | NoEditor => list{}
    | MainEditor(_) => astInfo.mainTokenInfos
    | FeatureFlagEditor(_, ffid) => ffTokenInfosFor(ffid, astInfo) |> Option.unwrap(~default=list{})
    }

  let modifyState = (~f: fluidState => fluidState, astInfo: t): t => {
    ...astInfo,
    state: f(astInfo.state),
  }

  let getToken = (astInfo: t): option<T.tokenInfo> =>
    getToken'(activeTokenInfos(astInfo), astInfo.state)

  let getTokenNotWhitespace = (astInfo: t): option<T.tokenInfo> =>
    getTokenNotWhitespace(activeTokenInfos(astInfo), astInfo.state)

  let emptyFor = (props: fluidProps, state: fluidState): t => {
    ast: FluidAST.ofExpr(Expr.EBlank(gid())),
    state: state,
    mainTokenInfos: list{},
    featureFlagTokenInfos: list{},
    props: props,
  }

  let make = (props: fluidProps, ast: FluidAST.t, s: fluidState): t =>
    emptyFor(props, s) |> setAST(ast)

  let exprOfActiveEditor = (astInfo: t): FluidExpression.t =>
    switch astInfo.state.activeEditor {
    | NoEditor => recover("exprOfActiveEditor - none exists", FluidAST.toExpr(astInfo.ast))
    | MainEditor(_) => FluidAST.toExpr(astInfo.ast)
    | FeatureFlagEditor(_, id) =>
      FluidAST.find(id, astInfo.ast) |> recoverOpt(
        "exprOfActiveEditor - cannot find expression for editor",
        ~default=FluidAST.toExpr(astInfo.ast),
      )
    }
}
