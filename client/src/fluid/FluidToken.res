open Prelude

module Html = Tea.Html

type t = FluidTypes.Token.t

type tokenInfo = FluidTypes.TokenInfo.t

let fakeid = ID.fromUInt64(UInt64.max)

let tid = (t: t): id =>
  switch t {
  | TInteger(id, _, _)
  | TFloatWhole(id, _, _)
  | TFloatPoint(id, _)
  | TFloatFractional(id, _, _)
  | TTrue(id, _)
  | TFalse(id, _)
  | TNullToken(id, _)
  | TBlank(id, _)
  | TPlaceholder({blankID: id, _})
  | TPartial(id, _, _)
  | TLeftPartial(id, _, _)
  | TRightPartial(id, _, _)
  | TPartialGhost(id, _, _)
  | TLetKeyword(id, _, _)
  | TLetAssignment(id, _, _)
  | TLetVarName(id, _, _, _)
  | TString(id, _, _)
  | TStringMLStart(id, _, _, _)
  | TStringMLMiddle(id, _, _, _)
  | TStringMLEnd(id, _, _, _)
  | TIfKeyword(id, _)
  | TIfThenKeyword(id, _)
  | TIfElseKeyword(id, _)
  | TBinOp(id, _, _)
  | TFieldOp(id, _, _)
  | TFieldName(id, _, _, _)
  | TFieldPartial(id, _, _, _, _)
  | TVariable(id, _, _)
  | TFnName(id, _, _, _, _)
  | TFnVersion(id, _, _, _)
  | TLambdaVar(id, _, _, _, _)
  | TLambdaArrow(id, _)
  | TLambdaSymbol(id, _)
  | TLambdaComma(id, _, _)
  | TListOpen(id, _)
  | TListClose(id, _)
  | TListComma(id, _)
  | TTupleOpen(id)
  | TTupleClose(id)
  | TTupleComma(id, _)
  | TPipe(id, _, _, _)
  | TRecordOpen(id, _)
  | TRecordClose(id, _)
  | TRecordFieldname({recordID: id, _})
  | TRecordSep(id, _, _)
  | TConstructorName(id, _)
  | TMatchBranchArrow({matchID: id, _})
  | TMatchKeyword(id)
  | TPatternBlank(_, id, _)
  | TPatternInteger(_, id, _, _)
  | TPatternVariable(_, id, _, _)
  | TPatternConstructorName(_, id, _, _)
  | TPatternString({patternID: id, _})
  | TPatternTrue(_, id, _)
  | TPatternFalse(_, id, _)
  | TPatternNullToken(_, id, _)
  | TPatternFloatWhole(_, id, _, _)
  | TPatternFloatPoint(_, id, _)
  | TPatternFloatFractional(_, id, _, _)
  | TSep(id, _)
  | TParenOpen(id)
  | TParenClose(id)
  | TFlagWhenKeyword(id)
  | TFlagEnabledKeyword(id)
  | TNewline(Some(_, id, _)) => id
  | TNewline(None) | TIndent(_) => fakeid
  }

let analysisID = (t: t): id =>
  switch t {
  | TLetVarName(_, id, _, _)
  | TLetKeyword(_, id, _)
  | TLetAssignment(_, id, _)
  | TRecordFieldname({exprID: id, _})
  | TLambdaVar(_, id, _, _, _)
  | TRecordSep(_, _, id)
  | TMatchBranchArrow({patternID: id, _}) => id
  | _ => tid(t)
  }

// TODO(alice) merge these two functions?
let parentExprID = (t: t): id =>
  switch t {
  | TNewline(Some(_, id, _)) => id
  | _ => tid(t)
  }

/* List literals, object literals, and multiline strings are blocks.
 This function returns the ID of the whole list, object, or string expression that this token belongs to, if it does indeed live inside a block.
*/
let parentBlockID = (t: t): option<id> =>
  switch t {
  // The first ID is the ID of the whole string expression
  | TStringMLStart(id, _, _, _)
  | TStringMLMiddle(id, _, _, _)
  | TStringMLEnd(id, _, _, _)
  | TRecordSep(id, _, _) =>
    Some(id)
  // The reason { } and [ ] gets a parentBlockID is so if the list/object is empty, then it's not a multiline block.
  | TRecordOpen(_, pid)
  | TRecordClose(_, pid)
  | TListOpen(_, pid)
  | TListClose(_, pid)
  | TBlank(_, pid)
  | TInteger(_, _, pid)
  | TString(_, _, pid)
  | TTrue(_, pid)
  | TFalse(_, pid)
  | TNullToken(_, pid)
  | TFloatWhole(_, _, pid)
  | TFloatPoint(_, pid)
  | TFloatFractional(_, _, pid)
  | TPartial(_, _, pid)
  | TRightPartial(_, _, pid)
  | TLeftPartial(_, _, pid)
  | TPartialGhost(_, _, pid)
  | TLetKeyword(_, _, pid)
  | TLetVarName(_, _, _, pid)
  | TLetAssignment(_, _, pid)
  | TIfKeyword(_, pid)
  | TIfThenKeyword(_, pid)
  | TIfElseKeyword(_, pid)
  | TBinOp(_, _, pid)
  | TFieldOp(_, _, pid)
  | TFieldName(_, _, _, pid)
  | TFieldPartial(_, _, _, _, pid)
  | TVariable(_, _, pid)
  | TLambdaComma(_, _, pid)
  | TLambdaArrow(_, pid)
  | TLambdaSymbol(_, pid)
  | TLambdaVar(_, _, _, _, pid)
  | TPipe(_, _, _, pid)
  | TSep(_, pid) => pid
  | TRecordFieldname(d) => d.parentBlockID
  | TNewline(Some(_, id, _)) => Some(id)

  | TTupleOpen(id)
  | TTupleClose(id)
  | TListComma(id, _)
  | TTupleComma(id, _) =>
    Some(id)

  | TFnName(_)
  | TFnVersion(_)
  | TMatchKeyword(_)
  | TMatchBranchArrow(_)
  | TPatternVariable(_)
  | TPatternConstructorName(_)
  | TPatternInteger(_)
  | TPatternString(_)
  | TPatternTrue(_)
  | TPatternFalse(_)
  | TPatternNullToken(_)
  | TPatternFloatWhole(_)
  | TPatternFloatPoint(_)
  | TPatternFloatFractional(_)
  | TPatternBlank(_)
  | TConstructorName(_)
  | TParenOpen(_)
  | TParenClose(_)
  | TFlagWhenKeyword(_)
  | TFlagEnabledKeyword(_)
  | TNewline(None)
  | TIndent(_)
  | TPlaceholder(_) =>
    None
  }

let validID = id => id != fakeid

/* Tokens that are 'editable' are considered text tokens
 * If the cursor is to the left of a text token, then pressing a character
 * will append to the end of that token */
let isTextToken = (t: t): bool =>
  switch t {
  | TInteger(_)
  | TLetVarName(_)
  | TBinOp(_)
  | TFieldName(_)
  | TFieldPartial(_)
  | TVariable(_)
  | TConstructorName(_)
  | TFnName(_)
  | TFnVersion(_)
  | TBlank(_)
  | TPlaceholder(_)
  | TPartial(_)
  | TRightPartial(_)
  | TLeftPartial(_)
  | TPartialGhost(_)
  | TRecordFieldname(_)
  | TString(_)
  | TStringMLStart(_)
  | TStringMLMiddle(_)
  | TStringMLEnd(_)
  | TTrue(_)
  | TFalse(_)
  | TNullToken(_)
  | TLambdaVar(_)
  | TFloatWhole(_)
  | TFloatPoint(_)
  | TFloatFractional(_)
  | TPatternInteger(_)
  | TPatternVariable(_)
  | TPatternConstructorName(_)
  | TPatternBlank(_)
  | TPatternString(_)
  | TPatternTrue(_)
  | TPatternFalse(_)
  | TPatternNullToken(_)
  | TPatternFloatWhole(_)
  | TPatternFloatPoint(_)
  | TPatternFloatFractional(_) => true
  | TListOpen(_)
  | TListClose(_)
  | TListComma(_, _)
  | TTupleOpen(_)
  | TTupleClose(_)
  | TTupleComma(_, _)
  | TSep(_)
  | TLetKeyword(_)
  | TRecordOpen(_)
  | TRecordClose(_)
  | TRecordSep(_)
  | TLetAssignment(_)
  | TIfKeyword(_)
  | TIfThenKeyword(_)
  | TIfElseKeyword(_)
  | TFieldOp(_)
  | TNewline(_)
  | TIndent(_)
  | TLambdaSymbol(_)
  | TLambdaComma(_)
  | TMatchKeyword(_)
  | TMatchBranchArrow(_)
  | TPipe(_)
  | TLambdaArrow(_)
  | TParenOpen(_)
  | TParenClose(_)
  | TFlagWhenKeyword(_)
  | TFlagEnabledKeyword(_) => false
  }

let isPipeable = (t: t): bool =>
  switch t {
  | TInteger(_)
  | TLetVarName(_)
  | TListClose(_)
  | TTupleClose(_)
  | TRecordClose(_)
  | TFieldName(_)
  | TVariable(_)
  | TFnName(_)
  | TString(_)
  | TTrue(_)
  | TFalse(_)
  | TNullToken(_)
  | TFloatWhole(_)
  | TFloatPoint(_)
  | TFloatFractional(_)
  | TPatternInteger(_)
  | TPatternVariable(_)
  | TPatternConstructorName(_)
  | TPatternBlank(_)
  | TPatternString(_)
  | TPatternTrue(_)
  | TPatternFalse(_)
  | TPatternNullToken(_)
  | TPatternFloatWhole(_)
  | TPatternFloatPoint(_)
  | TPatternFloatFractional(_)
  | TBlank(_)
  | TPipe(_) => true
  | TFnVersion(_)
  | TPlaceholder(_)
  | TPartial(_)
  | TRightPartial(_)
  | TLeftPartial(_)
  | TPartialGhost(_)
  | TRecordFieldname(_)
  | TConstructorName(_)
  | TBinOp(_)
  | TLambdaVar(_)
  | TStringMLStart(_)
  | TStringMLMiddle(_)
  | TStringMLEnd(_)
  | TFieldPartial(_)
  | TListOpen(_)
  | TListComma(_, _)
  | TTupleOpen(_)
  | TTupleComma(_, _)
  | TSep(_)
  | TLetKeyword(_)
  | TRecordOpen(_)
  | TRecordSep(_)
  | TLetAssignment(_)
  | TIfKeyword(_)
  | TIfThenKeyword(_)
  | TIfElseKeyword(_)
  | TFieldOp(_)
  | TNewline(_)
  | TIndent(_)
  | TLambdaSymbol(_)
  | TLambdaComma(_)
  | TMatchKeyword(_)
  | TMatchBranchArrow(_)
  | TLambdaArrow(_)
  | TParenOpen(_)
  | TParenClose(_)
  | TFlagWhenKeyword(_)
  | TFlagEnabledKeyword(_) => false
  }

let isStringToken = (t: t): bool =>
  switch t {
  | TStringMLStart(_) | TStringMLMiddle(_) | TStringMLEnd(_) | TString(_) => true
  | _ => false
  }

/* if the cursor is at the end of this token, we take it as editing this
 * token, rather than writing the next token. */
let isAppendable = (t: t): bool =>
  switch t {
  // String should really be directly editable, but the extra quote at the end
  // makes it not so; since there's no quote at the end of TStringMLStart or
  // TStringMLMiddle, then they are appendable
  | TString(_) | TPatternString(_) | TStringMLEnd(_) => false
  | _ => isTextToken(t)
  }

let isBlank = (t: t): bool =>
  switch t {
  | TBlank(_)
  | TPlaceholder(_)
  | TRecordFieldname({fieldName: "", _})
  | TVariable(_, "", _)
  | TFieldName(_, _, "", _)
  | TFieldPartial(_, _, _, "", _)
  | TLetVarName(_, _, "", _)
  | TLambdaVar(_, _, _, "", _)
  | TPartial(_, "", _)
  | TRightPartial(_, "", _)
  | TLeftPartial(_, "", _)
  | TPatternBlank(_) => true
  | _ => false
  }

let isKeyword = (t: t) =>
  switch t {
  | TLetKeyword(_)
  | TIfKeyword(_)
  | TIfThenKeyword(_)
  | TIfElseKeyword(_)
  | TMatchKeyword(_)
  | TFlagWhenKeyword(_)
  | TFlagEnabledKeyword(_) => true
  | _ => false
  }

@ocaml.doc(" [isWhitespace t] returns [true] if the token [t] is a whitespace token (separator, newline, indent)
 * and [false] otherwise. ")
let isWhitespace = (t: t): bool =>
  switch t {
  | TSep(_) | TNewline(_) | TIndent(_) => true
  | TInteger(_)
  | TFloatWhole(_)
  | TFloatPoint(_)
  | TFloatFractional(_)
  | TTrue(_)
  | TFalse(_)
  | TNullToken(_)
  | TBlank(_)
  | TPlaceholder(_)
  | TPartial(_)
  | TLeftPartial(_)
  | TRightPartial(_)
  | TPartialGhost(_)
  | TLetKeyword(_)
  | TLetAssignment(_)
  | TLetVarName(_)
  | TString(_)
  | TStringMLStart(_)
  | TStringMLMiddle(_)
  | TStringMLEnd(_)
  | TIfKeyword(_)
  | TIfThenKeyword(_)
  | TIfElseKeyword(_)
  | TBinOp(_)
  | TFieldOp(_)
  | TFieldName(_)
  | TFieldPartial(_)
  | TVariable(_)
  | TFnName(_)
  | TFnVersion(_)
  | TLambdaVar(_)
  | TLambdaArrow(_)
  | TLambdaSymbol(_)
  | TLambdaComma(_)
  | TListOpen(_)
  | TListClose(_)
  | TListComma(_)
  | TTupleOpen(_)
  | TTupleClose(_)
  | TTupleComma(_)
  | TPipe(_)
  | TRecordOpen(_)
  | TRecordClose(_)
  | TRecordFieldname(_)
  | TRecordSep(_)
  | TConstructorName(_)
  | TMatchBranchArrow(_)
  | TMatchKeyword(_)
  | TPatternBlank(_)
  | TPatternInteger(_)
  | TPatternVariable(_)
  | TPatternConstructorName(_)
  | TPatternString(_)
  | TPatternTrue(_)
  | TPatternFalse(_)
  | TPatternNullToken(_)
  | TPatternFloatWhole(_)
  | TPatternFloatPoint(_)
  | TPatternFloatFractional(_)
  | TParenOpen(_)
  | TParenClose(_)
  | TFlagWhenKeyword(_)
  | TFlagEnabledKeyword(_) => false
  }

let isSkippable = (t: t): bool =>
  switch t {
  | TIndent(_) => true
  | _ => false
  }

let isAtom = (t: t): bool =>
  switch t {
  | TMatchBranchArrow(_) | TPipe(_) | TLambdaArrow(_) => true
  | _ => isKeyword(t) || isBlank(t)
  }

let isNewline = (t: t): bool =>
  switch t {
  | TNewline(_) => true
  | _ => false
  }

let isLet = (t: t): bool =>
  switch t {
  | TLetAssignment(_) | TLetVarName(_) => true
  | _ => false
  }

let isAutocompletable = (t: t): bool =>
  switch t {
  | TBlank(_)
  | TPlaceholder(_)
  | TPartial(_)
  | TFieldPartial(_)
  | TRightPartial(_)
  | TLeftPartial(_)
  | TPatternBlank(_)
  | /* since patterns have no partial but commit as variables
   * automatically, allow intermediate variables to
   * be autocompletable to other expressions */
  TPatternVariable(_) => true
  | _ => false
  }

// Is this token something we can highlight as DError or DIncomplete?
let isErrorDisplayable = (t: t): bool =>
  isTextToken(t) &&
  switch t {
  | TFnVersion(_) => false
  | _ => true
  }

let isFieldPartial = (t: t): bool =>
  switch t {
  | TFieldPartial(_) => true
  | _ => false
  }

let isMultilineString = (t: FluidTypes.Token.t): bool =>
  switch t {
  | TStringMLStart(_) | TStringMLMiddle(_) | TStringMLEnd(_) => true
  | _ => false
  }

let isListSymbol = (t: FluidTypes.Token.t): bool =>
  switch t {
  | TListOpen(_) | TListClose(_) | TListComma(_) => true
  | _ => false
  }

let isTupleSymbol = (t: FluidTypes.Token.t): bool =>
  switch t {
  | TTupleOpen(_) | TTupleClose(_) | TTupleComma(_) => true
  | _ => false
  }

let toText = (t: t): string => {
  let shouldntBeEmpty = name => {
    if name == "" {
      asserT(~debug=FluidTypes.Token.show(t), "shouldn't be empty", name != "")
    }
    name
  }

  let canBeEmpty = name =>
    if name == "" {
      "   "
    } else {
      name
    }
  switch t {
  | TInteger(_, i, _) => Int64.to_string(i)
  | TFloatWhole(_, w, _) => shouldntBeEmpty(w)
  | TFloatPoint(_) => "."
  | TFloatFractional(_, f, _) => f
  | TString(_, str, _) => "\"" ++ (str ++ "\"")
  | TStringMLStart(_, str, _, _) => "\"" ++ str
  | TStringMLMiddle(_, str, _, _) => str
  | TStringMLEnd(_, str, _, _) => str ++ "\""
  | TTrue(_) => "true"
  | TFalse(_) => "false"
  | TNullToken(_) => "null"
  | TBlank(_) => "   "
  | TPlaceholder({placeholder: {name, tipe}, _}) => " " ++ (name ++ (" : " ++ (tipe ++ " ")))
  | TPartial(_, str, _) => shouldntBeEmpty(str)
  | TRightPartial(_, str, _) => shouldntBeEmpty(str)
  | TLeftPartial(_, str, _) => shouldntBeEmpty(str)
  | TPartialGhost(_, str, _) => shouldntBeEmpty(str)
  | TSep(_) => " "
  | TNewline(_) => "\n"
  | TLetKeyword(_) => "let "
  | TLetAssignment(_) => " = "
  | TLetVarName(_, _, name, _) => canBeEmpty(name)
  | TIfKeyword(_) => "if "
  | TIfThenKeyword(_) => "then"
  | TIfElseKeyword(_) => "else"
  | TBinOp(_, op, _) => shouldntBeEmpty(op)
  | TFieldOp(_) => "."
  | TFieldPartial(_, _, _, name, _) => canBeEmpty(name)
  | TFieldName(_, _, name, _) =>
    /* Although we typically use TFieldPartial for empty fields, when
     * there's a new field we won't have a fieldname for it. */
    canBeEmpty(name)
  | TVariable(_, name, _) => canBeEmpty(name)
  | TFnName(_, _, displayName, _, _) | TFnVersion(_, _, displayName, _) =>
    shouldntBeEmpty(displayName)
  | TLambdaVar(_, _, _, name, _) => canBeEmpty(name)
  | TLambdaSymbol(_) => "\\"
  | TLambdaComma(_) => ","
  | TLambdaArrow(_) => " -> "
  | TIndent(indent) => shouldntBeEmpty(Caml.String.make(indent, ' '))
  /* We dont want this to be transparent, so have these make their presence
   * known */
  | TListOpen(_) => "["
  | TListClose(_) => "]"
  | TListComma(_, _) => ","
  | TTupleOpen(_) => "("
  | TTupleClose(_) => ")"
  | TTupleComma(_, _) => ","
  | TRecordOpen(_) => "{"
  | TRecordClose(_) => "}"
  | TRecordFieldname(f) => canBeEmpty(f.fieldName)
  | TRecordSep(_) => " : "
  | TConstructorName(_, name) => canBeEmpty(name)
  | TPipe(_) => "|>"
  | TMatchKeyword(_) => "match "
  | TMatchBranchArrow(_) => " -> "
  | TPatternInteger(_, _, i, _) => Int64.to_string(i)
  | TPatternFloatWhole(_, _, w, _) => shouldntBeEmpty(w)
  | TPatternFloatPoint(_) => "."
  | TPatternFloatFractional(_, _, f, _) => f
  | TPatternString({str, _}) => "\"" ++ (str ++ "\"")
  | TPatternTrue(_) => "true"
  | TPatternFalse(_) => "false"
  | TPatternNullToken(_) => "null"
  | TPatternBlank(_) => "   "
  | TPatternVariable(_, _, name, _) => canBeEmpty(name)
  | TPatternConstructorName(_, _, name, _) => canBeEmpty(name)
  | TParenOpen(_) => "("
  | TParenClose(_) => ")"
  | TFlagWhenKeyword(_) => "when "
  | TFlagEnabledKeyword(_) => "enabled"
  }
}

let toTestText = (t: t): string => {
  let result = switch t {
  | TPlaceholder({placeholder: {name, tipe}, _}) =>
    let count = 1 + String.length(name) + 3 + String.length(tipe) + 1
    Caml.String.make(count, '_')
  | TBlank(_) => "___"
  | TPartialGhost(_, str, _) =>
    switch String.length(str) {
    | 0 => "@EMPTY@"
    | 1 => "@"
    | 2 => "@@"
    | _ =>
      let str = str |> String.dropLeft(~count=1) |> String.dropRight(~count=1)

      "@" ++ (str ++ "@")
    }
  | _ =>
    if isBlank(t) {
      "***"
    } else {
      toText(t)
    }
  }

  asserT(~debug=t, "wrong length toTestText", String.length(result) == String.length(toText(t)))
  result
}

let toIndex = (t: t): option<int> =>
  switch t {
  | TStringMLMiddle(_, _, index, _)
  | TLambdaVar(_, _, index, _, _)
  | TLambdaComma(_, index, _)
  | TPipe(_, _, index, _)
  | TRecordFieldname({index, _})
  | TRecordSep(_, index, _)
  | TListComma(_, index)
  | TTupleComma(_, index)
  | TNewline(Some(_, _, Some(index)))
  | TPatternBlank(_, _, index)
  | TPatternInteger(_, _, _, index)
  | TPatternVariable(_, _, _, index)
  | TPatternConstructorName(_, _, _, index)
  | TPatternString({branchIdx: index, _})
  | TPatternTrue(_, _, index)
  | TPatternFalse(_, _, index)
  | TPatternNullToken(_, _, index)
  | TPatternFloatWhole(_, _, _, index)
  | TPatternFloatPoint(_, _, index)
  | TPatternFloatFractional(_, _, _, index) =>
    Some(index)
  | _ => None
  }

let toParentID = (t: t): option<id> =>
  switch t {
  | TRecordFieldname({recordID: id, _})
  | TPatternBlank(id, _, _)
  | TPatternInteger(id, _, _, _)
  | TPatternVariable(id, _, _, _)
  | TPatternConstructorName(id, _, _, _)
  | TPatternString({matchID: id, _})
  | TPatternTrue(id, _, _)
  | TPatternFalse(id, _, _)
  | TPatternNullToken(id, _, _)
  | TPatternFloatWhole(id, _, _, _)
  | TPatternFloatPoint(id, _, _)
  | TPatternFloatFractional(id, _, _, _) =>
    Some(id)
  | _ => None
  }

let toTypeName = (t: t): string =>
  switch t {
  | TInteger(_) => "integer"
  | TFloatWhole(_) => "float-whole"
  | TFloatPoint(_) => "float-point"
  | TFloatFractional(_) => "float-fractional"
  | TString(_) => "string"
  | TStringMLStart(_) => "string-ml-start"
  | TStringMLMiddle(_) => "string-ml-middle"
  | TStringMLEnd(_) => "string-ml-end"
  | TTrue(_) => "true"
  | TFalse(_) => "false"
  | TNullToken(_) => "null"
  | TBlank(_) => "blank"
  | TPlaceholder(_) => "placeholder"
  | TPartial(_) => "partial"
  | TRightPartial(_) => "partial-right"
  | TLeftPartial(_) => "partial-left"
  | TPartialGhost(_) => "partial-ghost"
  | TLetKeyword(_) => "let-keyword"
  | TLetAssignment(_) => "let-assignment"
  | TLetVarName(_) => "let-var-name"
  | TSep(_) => "sep"
  | TIndent(_) => "indent"
  | TNewline(_) => "newline"
  | TIfKeyword(_) => "if-keyword"
  | TIfThenKeyword(_) => "if-then-keyword"
  | TIfElseKeyword(_) => "if-else-keyword"
  | TBinOp(_) => "binop"
  | TFieldOp(_) => "field-op"
  | TFieldName(_) => "field-name"
  | TFieldPartial(_) => "field-partial"
  | TVariable(_) => "variable"
  | TFnName(_) => "fn-name"
  | TFnVersion(_) => "fn-version"
  | TLambdaVar(_) => "lambda-var"
  | TLambdaSymbol(_) => "lambda-symbol"
  | TLambdaArrow(_) => "lambda-arrow"
  | TLambdaComma(_) => "lambda-comma"
  | TListOpen(_) => "list-open"
  | TListClose(_) => "list-close"
  | TListComma(_, _) => "list-comma"
  | TTupleOpen(_) => "tuple-open"
  | TTupleClose(_) => "tuple-close"
  | TTupleComma(_, _) => "tuple-comma"
  | TRecordOpen(_) => "record-open"
  | TRecordClose(_) => "record-close"
  | TRecordFieldname(_) => "record-fieldname"
  | TRecordSep(_) => "record-sep"
  | TConstructorName(_) => "constructor-name"
  | TPipe(_) => "pipe-symbol"
  | TMatchKeyword(_) => "match-keyword"
  | TMatchBranchArrow(_) => "match-branch-arrow"
  | TPatternBlank(_) => "pattern-blank"
  | TPatternInteger(_) => "pattern-integer"
  | TPatternVariable(_) => "pattern-variable"
  | TPatternConstructorName(_) => "pattern-constructor-name"
  | TPatternString(_) => "pattern-string"
  | TPatternTrue(_) => "pattern-true"
  | TPatternFalse(_) => "pattern-false"
  | TPatternNullToken(_) => "pattern-null"
  | TPatternFloatWhole(_) => "pattern-float-whole"
  | TPatternFloatPoint(_) => "pattern-float-point"
  | TPatternFloatFractional(_) => "pattern-float-fractional"
  | TParenOpen(_) => "paren-open"
  | TParenClose(_) => "paren-close"
  | TFlagWhenKeyword(_) => "ff-cond"
  | TFlagEnabledKeyword(_) => "ff-enabled"
  }

let toCategoryName = (t: t): string =>
  switch t {
  | TInteger(_) => "integer"
  | TString(_) | TStringMLStart(_) | TStringMLMiddle(_) | TStringMLEnd(_) => "string"
  | TVariable(_) | TNewline(_) | TSep(_) | TBlank(_) | TPlaceholder(_) => ""
  | TPartial(_) | TRightPartial(_) | TLeftPartial(_) | TPartialGhost(_) => "partial"
  | TFloatWhole(_) | TFloatPoint(_) | TFloatFractional(_) => "float"
  | TTrue(_) | TFalse(_) => "boolean"
  | TNullToken(_) => "null"
  | TFnName(_) | TFnVersion(_) | TBinOp(_) => "function"
  | TLetKeyword(_) | TLetAssignment(_) | TLetVarName(_) => "let"
  | TIndent(_) => "indent"
  | TIfKeyword(_) | TIfThenKeyword(_) | TIfElseKeyword(_) => "if"
  | TFieldOp(_) | TFieldName(_) | TFieldPartial(_) => "field"
  | TLambdaVar(_) | TLambdaSymbol(_) | TLambdaArrow(_) | TLambdaComma(_) => "lambda"
  | TListOpen(_) | TListClose(_) | TListComma(_) => "list"
  | TTupleOpen(_) | TTupleClose(_) | TTupleComma(_) => "tuple"
  | TPipe(_) => "pipe"
  | TConstructorName(_) => "constructor"
  | TRecordOpen(_) | TRecordClose(_) | TRecordFieldname(_) | TRecordSep(_) => "record"
  | TMatchKeyword(_) | TMatchBranchArrow(_) => "match"
  | TPatternBlank(_)
  | TPatternInteger(_)
  | TPatternVariable(_)
  | TPatternConstructorName(_)
  | TPatternString(_)
  | TPatternTrue(_)
  | TPatternFalse(_)
  | TPatternNullToken(_)
  | TPatternFloatWhole(_)
  | TPatternFloatPoint(_)
  | TPatternFloatFractional(_) => "pattern"
  | TParenOpen(_) | TParenClose(_) => "paren"
  | TFlagWhenKeyword(_) | TFlagEnabledKeyword(_) => "flag"
  }

let toDebugInfo = (t: t): string =>
  switch t {
  | TStringMLStart(_, _, offset, _)
  | TStringMLMiddle(_, _, offset, _)
  | TStringMLEnd(_, _, offset, _) =>
    "offset=" ++ string_of_int(offset)
  | TNewline(Some(_, pid, Some(idx))) =>
    "parent=" ++ (ID.toString(pid) ++ (" idx=" ++ string_of_int(idx)))
  | TNewline(Some(_, pid, None)) => "parent=" ++ (ID.toString(pid) ++ " idx=none")
  | TNewline(None) => "no parent"
  | TPipe(_, idx, len, _) => Printf.sprintf("idx=%d len=%d", idx, len)
  | TMatchBranchArrow({index: idx, _}) => "idx=" ++ string_of_int(idx)
  | TPatternBlank(mid, _, idx)
  | TPatternInteger(mid, _, _, idx)
  | TPatternVariable(mid, _, _, idx)
  | TPatternConstructorName(mid, _, _, idx)
  | TPatternString({matchID: mid, branchIdx: idx, _})
  | TPatternTrue(mid, _, idx)
  | TPatternFalse(mid, _, idx)
  | TPatternNullToken(mid, _, idx)
  | TPatternFloatWhole(mid, _, _, idx)
  | TPatternFloatPoint(mid, _, idx)
  | TPatternFloatFractional(mid, _, _, idx) =>
    "match=" ++ (ID.toString(mid) ++ (" idx=" ++ string_of_int(idx)))
  | _ => ""
  }

let toCssClasses = (t: t): list<string> => {
  let empty = if isBlank(t) {
    list{"fluid-empty"}
  } else {
    list{}
  }
  let keyword = if isKeyword(t) {
    list{"fluid-keyword"}
  } else {
    list{}
  }
  let typename = list{"fluid-" ++ toTypeName(t)}
  let category = {
    let name = toCategoryName(t)
    if name == "" {
      list{}
    } else {
      list{"fluid-category-" ++ name}
    }
  }

  Belt.List.concatMany([empty, keyword, typename, category])
}

let show_tokenInfo = (ti: tokenInfo) =>
  Html.dl(
    list{},
    list{
      Html.dt(list{}, list{Html.text("pos")}),
      Html.dd(list{}, list{Html.text(Printf.sprintf("(%d, %d)", ti.startPos, ti.endPos))}),
      Html.dt(list{}, list{Html.text("len")}),
      Html.dd(list{}, list{Html.text(Printf.sprintf("%d", ti.length))}),
      Html.dt(list{}, list{Html.text("tok")}),
      Html.dd(list{}, list{Html.text(toText(ti.token))}),
      Html.dt(list{}, list{Html.text("id")}),
      Html.dd(list{}, list{Html.text(tid(ti.token) |> ID.toString)}),
      Html.dt(list{}, list{Html.text("aid")}),
      Html.dd(list{}, list{Html.text(analysisID(ti.token) |> ID.toString)}),
      Html.dt(list{}, list{Html.text("type")}),
      Html.dd(list{}, list{Html.text(toTypeName(ti.token))}),
      Html.dt(list{}, list{Html.text("debug")}),
      Html.dd(list{}, list{Html.text(toDebugInfo(ti.token))}),
    },
  )

/* Since tokens don't have unique IDs, it is hard to look at two tokens streams
 * and find which tokens represent the same thing. You can use toText and ID,
 * but that doesn't work where the content has changed, which is a thing we
 * want to check for. */
let matches = (t1: t, t2: t): bool =>
  tid(t1) == tid(t2) &&
    (toTypeName(t1) == toTypeName(t2) &&
    (toIndex(t1) == toIndex(t2) && t1 != /* Matches too many things */ TNewline(None)))

// Matches everything except parentBlockID
let matchesContent = (t1: t, t2: t): bool =>
  switch (t1, t2) {
  | (TStringMLStart(id1, seg1, ind1, str1), TStringMLStart(id2, seg2, ind2, str2))
  | (TStringMLMiddle(id1, seg1, ind1, str1), TStringMLMiddle(id2, seg2, ind2, str2))
  | (TStringMLEnd(id1, seg1, ind1, str1), TStringMLEnd(id2, seg2, ind2, str2)) =>
    id1 == id2 && (seg1 == seg2 && (ind1 == ind2 && str1 == str2))
  | (TListOpen(id1, _), TListOpen(id2, _))
  | (TListClose(id1, _), TListClose(id2, _))
  | (TTupleOpen(id1), TTupleOpen(id2))
  | (TTupleClose(id1), TTupleClose(id2))
  | (TRecordOpen(id1, _), TRecordOpen(id2, _))
  | (TRecordClose(id1, _), TRecordClose(id2, _))
  | (TTrue(id1, _), TTrue(id2, _))
  | (TFalse(id1, _), TFalse(id2, _))
  | (TNullToken(id1, _), TNullToken(id2, _))
  | (TFloatPoint(id1, _), TFloatPoint(id2, _))
  | (TLetKeyword(id1, _, _), TLetKeyword(id2, _, _))
  | (TLetAssignment(id1, _, _), TLetAssignment(id2, _, _))
  | (TIfKeyword(id1, _), TIfKeyword(id2, _))
  | (TIfThenKeyword(id1, _), TIfThenKeyword(id2, _))
  | (TIfElseKeyword(id1, _), TIfElseKeyword(id2, _))
  | (TBlank(id1, _), TBlank(id2, _))
  | (TLambdaArrow(id1, _), TLambdaArrow(id2, _))
  | (TLambdaSymbol(id1, _), TLambdaSymbol(id2, _))
  | (TSep(id1, _), TSep(id2, _))
  | (TMatchKeyword(id1), TMatchKeyword(id2))
  | (TParenOpen(id1), TParenOpen(id2))
  | (TParenClose(id1), TParenClose(id2))
  | (TFlagWhenKeyword(id1), TFlagWhenKeyword(id2))
  | (TFlagEnabledKeyword(id1), TFlagEnabledKeyword(id2)) =>
    id1 == id2
  | (TListComma(id1, ind1), TListComma(id2, ind2))
  | (TTupleComma(id1, ind1), TTupleComma(id2, ind2))
  | (TRecordSep(id1, ind1, _), TRecordSep(id2, ind2, _))
  | (TLambdaComma(id1, ind1, _), TLambdaComma(id2, ind2, _)) =>
    id1 == id2 && ind1 == ind2
  | (TInteger(id1, val1, _), TInteger(id2, val2, _)) => id1 == id2 && val1 == val2
  | (TFloatWhole(id1, val1, _), TFloatWhole(id2, val2, _))
  | (TFloatFractional(id1, val1, _), TFloatFractional(id2, val2, _))
  | (TPartial(id1, val1, _), TPartial(id2, val2, _))
  | (TRightPartial(id1, val1, _), TRightPartial(id2, val2, _))
  | (TPartialGhost(id1, val1, _), TPartialGhost(id2, val2, _))
  | (TString(id1, val1, _), TString(id2, val2, _))
  | (TLetVarName(id1, _, val1, _), TLetVarName(id2, _, val2, _))
  | (TBinOp(id1, val1, _), TBinOp(id2, val2, _))
  | (TVariable(id1, val1, _), TVariable(id2, val2, _))
  | (TConstructorName(id1, val1), TConstructorName(id2, val2)) =>
    id1 == id2 && val1 == val2
  | (TFieldOp(id1, l1, _), TFieldOp(id2, l2, _)) => id1 == id2 && l1 == l2
  | (TFieldName(id1, l1, val1, _), TFieldName(id2, l2, val2, _))
  | (TFieldPartial(id1, l1, _, val1, _), TFieldPartial(id2, l2, _, val2, _)) =>
    id1 == id2 && (l1 == l2 && val1 == val2)
  | (TLambdaVar(id1, _, ind1, val1, _), TLambdaVar(id2, _, ind2, val2, _)) =>
    id1 == id2 && (ind1 == ind2 && val1 == val2)
  | (TPipe(id1, order1, nest1, _), TPipe(id2, order2, nest2, _)) =>
    id1 == id2 && (order1 == order2 && nest1 == nest2)
  | (TRecordFieldname(d1), TRecordFieldname(d2)) =>
    d1.recordID == d2.recordID &&
      (d1.exprID == d2.exprID &&
      (d1.index == d2.index && d1.fieldName == d2.fieldName))
  | (TNewline(props1), TNewline(props2)) => props1 == props2
  | (TMatchBranchArrow(d1), TMatchBranchArrow(d2)) =>
    d1.matchID == d2.matchID && (d1.patternID == d2.patternID && d1.index == d2.index)
  | (TPatternString(d1), TPatternString(d2)) =>
    d1.matchID == d2.matchID &&
      (d1.patternID == d2.patternID &&
      (d1.str == d2.str && d1.branchIdx == d2.branchIdx))
  | (TFnName(id1, _, _, fullname1, rail1), TFnName(id2, _, _, fullname2, rail2)) =>
    id1 == id2 && (fullname1 == fullname2 && rail1 == rail2)
  | (TFnVersion(id1, _, _, fullname1), TFnVersion(id2, _, _, fullname2)) =>
    id1 == id2 && fullname1 == fullname2
  | (TPatternVariable(m1, p1, val1, ind1), TPatternVariable(m2, p2, val2, ind2))
  | (TPatternConstructorName(m1, p1, val1, ind1), TPatternConstructorName(m2, p2, val2, ind2)) =>
    m1 == m2 && (p1 == p2 && (val1 == val2 && ind1 == ind2))
  | (TPatternInteger(m1, p1, val1, ind1), TPatternInteger(m2, p2, val2, ind2)) =>
    m1 == m2 && (p1 == p2 && (val1 == val2 && ind1 == ind2))
  | (TPatternTrue(p1, id1, ind1), TPatternTrue(p2, id2, ind2))
  | (TPatternFalse(p1, id1, ind1), TPatternFalse(p2, id2, ind2))
  | (TPatternNullToken(p1, id1, ind1), TPatternNullToken(p2, id2, ind2))
  | (TPatternFloatPoint(p1, id1, ind1), TPatternFloatPoint(p2, id2, ind2))
  | (TPatternBlank(p1, id1, ind1), TPatternBlank(p2, id2, ind2)) =>
    p1 == p2 && (id1 == id2 && ind1 == ind2)
  | (TPatternFloatWhole(p1, id1, val1, ind1), TPatternFloatWhole(p2, id2, val2, ind2))
  | (TPatternFloatFractional(p1, id1, val1, ind1), TPatternFloatFractional(p2, id2, val2, ind2)) =>
    p1 == p2 && (id1 == id2 && (val1 == val2 && ind1 == ind2))
  | (TIndent(ind1), TIndent(ind2)) => ind1 == ind2
  | (TPlaceholder(d1), TPlaceholder(d2)) =>
    d1.blankID == d2.blankID && (d1.fnID == d2.fnID && d1.placeholder == d2.placeholder)
  | (TInteger(_), _)
  | (TString(_), _)
  | (TStringMLStart(_), _)
  | (TStringMLMiddle(_), _)
  | (TStringMLEnd(_), _)
  | (TBlank(_), _)
  | (TPlaceholder(_), _)
  | (TTrue(_), _)
  | (TFalse(_), _)
  | (TNullToken(_), _)
  | (TFloatWhole(_), _)
  | (TFloatPoint(_), _)
  | (TFloatFractional(_), _)
  | (TPartial(_), _)
  | (TLeftPartial(_), _)
  | (TRightPartial(_), _)
  | (TPartialGhost(_), _)
  | (TSep(_), _)
  | (TNewline(_), _)
  | (TIndent(_), _)
  | (TLetKeyword(_), _)
  | (TLetVarName(_), _)
  | (TLetAssignment(_), _)
  | (TIfKeyword(_), _)
  | (TIfThenKeyword(_), _)
  | (TIfElseKeyword(_), _)
  | (TBinOp(_), _)
  | (TFieldOp(_), _)
  | (TFieldName(_), _)
  | (TFieldPartial(_), _)
  | (TVariable(_), _)
  | (TFnName(_), _)
  | (TFnVersion(_), _)
  | (TLambdaComma(_), _)
  | (TLambdaArrow(_), _)
  | (TLambdaSymbol(_), _)
  | (TLambdaVar(_), _)
  | (TListOpen(_), _)
  | (TListClose(_), _)
  | (TListComma(_), _)
  | (TTupleOpen(_), _)
  | (TTupleClose(_), _)
  | (TTupleComma(_), _)
  | (TPipe(_), _)
  | (TRecordOpen(_), _)
  | (TRecordFieldname(_), _)
  | (TRecordSep(_), _)
  | (TRecordClose(_), _)
  | (TMatchKeyword(_), _)
  | (TMatchBranchArrow(_), _)
  | (TPatternVariable(_), _)
  | (TPatternConstructorName(_), _)
  | (TPatternInteger(_), _)
  | (TPatternString(_), _)
  | (TPatternTrue(_), _)
  | (TPatternFalse(_), _)
  | (TPatternNullToken(_), _)
  | (TPatternFloatWhole(_), _)
  | (TPatternFloatPoint(_), _)
  | (TPatternFloatFractional(_), _)
  | (TPatternBlank(_), _)
  | (TConstructorName(_), _)
  | (TParenOpen(_), _)
  | (TParenClose(_), _)
  | (TFlagWhenKeyword(_), _)
  | (TFlagEnabledKeyword(_), _) => false
  }
