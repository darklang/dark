open Prelude

@ppx.deriving(show({with_path: false}))
type rec t = FluidCursorTypes.CaretTarget.t

@ocaml.doc(" [forARStringOpenQuote id offset] produces an ARString caretTarget
* pointing to an [offset] into the open quote of the string with [id].
* [offset] may NOT be negative as it cannot represent something out of string bounds. ")
let forARStringOpenQuote = (id: id, offset: int): t => {
  Recover.asserT("unexpected openquote offset", ~debug=offset, offset == 0 || offset == 1)
  {
    astRef: ARString(id, SPOpenQuote),
    offset: offset,
  }
}

@ocaml.doc(" [forARStringText id offset] produces an ARString caretTarget
* pointing to an [offset] into the text of the string with [id].
* [offset] may be negative but cannot represent something out of string bounds. ")
let forARStringBody = (id: id, offset: int, str: string): t => {
  if str == "" {
    forARStringOpenQuote(id, 1)
  } else {
    Recover.asserT(
      "unexpected string body offset",
      ~debug=(offset, str),
      offset >= 0 && 0 < String.length(str),
    )
    {
      astRef: ARString(id, SPBody),
      offset: offset,
    }
  }
}

@ocaml.doc(" [forARStringCloseQuote id offset] produces an ARString caretTarget
* pointing to an [offset] into the close quote of the string with [id]. It uses the
* [fullStr] of the string (excluding visual quotes) to compute the target.
* [offset] may be negative but cannot represent something out of string bounds. ")
let forARStringCloseQuote = (id: id, offset: int): t => {
  Recover.asserT("unexpected closequote offset", ~debug=offset, offset == 0 || offset == 1)
  {astRef: ARString(id, SPCloseQuote), offset: offset}
}

@ocaml.doc(" [forMPPStringOpenQuote id offset] produces an ARMPattern MPPString caretTarget
* pointing to an [offset] into the open quote of the pattern string with [id].
* [offset] may NOT be negative as it cannot represent something out of string bounds. ")
let forMPPStringOpenQuote = (id: id, offset: int): t => {
  astRef: ARMPattern(id, MPPString),
  offset: offset,
}

@ocaml.doc(" [forMPPStringText id offset] produces an ARMPattern MPPString caretTarget
* pointing to an [offset] into the text of the pattern string with [id].
* [offset] may be negative but cannot represent something out of string bounds. ")
let forMPPStringText = (id: id, offset: int): t => {
  astRef: ARMPattern(id, MPPString),
  offset: 1 + offset,
}

@ocaml.doc(" [forMPPStringCloseQuote id offset] produces an ARMPattern MPPString caretTarget
* pointing to an [offset] into the close quote of the pattern string with [id]. It uses the
* [fullStr] of the string (excluding visual quotes) to compute the target.
* [offset] may be negative but cannot represent something out of string bounds. ")
let forMPPStringCloseQuote = (id: id, offset: int, fullStr: string): t => {
  let lenPlusOpenQuote = 1 + String.length(fullStr)
  {
    astRef: ARMPattern(id, MPPString),
    offset: lenPlusOpenQuote + offset,
  }
}

@ocaml.doc(" [forEndOfExpr' expr] produces a caretTarget corresponding
 * to the very end of the expr. The concept of \"very end\" is related to an
 * understanding of the tokenization of the expr, even though this function
 * doesn't explicitly depend on any tokenization functions. ")
let rec forEndOfExpr': fluidExpr => t = expr =>
  switch expr {
  | EVariable(id, str) => {astRef: ARVariable(id), offset: String.length(str)}
  | EFieldAccess(id, _, fieldName) => {
      astRef: ARFieldAccess(id, FAPFieldname),
      offset: String.length(fieldName),
    }
  | EInteger(id, value) => {astRef: ARInteger(id), offset: String.length(Int64.to_string(value))}
  | EBool(id, true) => {astRef: ARBool(id), offset: String.length("true")}
  | EBool(id, false) => {astRef: ARBool(id), offset: String.length("false")}
  | EString(id, _) => forARStringCloseQuote(id, 1)
  | EFloat(id, _, _, decimalStr) => {
      astRef: ARFloat(id, FPFractional),
      offset: String.length(decimalStr),
    }
  | ENull(id) => {astRef: ARNull(id), offset: String.length("null")}
  | EBlank(id) => {astRef: ARBlank(id), offset: 0}
  | ELet(_, _, _, bodyExpr) => forEndOfExpr'(bodyExpr)
  | EIf(_, _, _, elseExpr) => forEndOfExpr'(elseExpr)
  | EBinOp(_, _, _, rhsExpr, _) => forEndOfExpr'(rhsExpr)
  | ELambda(_, _, bodyExpr) => forEndOfExpr'(bodyExpr)
  | EFnCall(id, fnName, argExprs, _) =>
    /* Caret targets don't make sense for EPipeTargets, so we
     * return a caret target for the end of the last fn argument
     * that isn't an EPipeTarget, or the end of the extended
     * function name, if there are no non-EPipeTargets. */
    argExprs
    |> List.reverse
    |> List.find(~f=e =>
      switch e {
      | ProgramTypes.Expr.EPipeTarget(_) => false
      | _ => true
      }
    )
    |> Option.map(~f=lastNonPipeTarget => forEndOfExpr'(lastNonPipeTarget))
    |> Option.unwrap(
      ~default=(
        {
          astRef: ARFnCall(id),
          offset: fnName
          |> FQFnName.toString
          |> FluidUtil.fnDisplayNameWithVersion
          |> String.length,
        }: t
      ),
    )
  | EPartial(_, _, EBinOp(_, _, _, rhsExpr, _)) =>
    /* We need this so that (for example) when we backspace a binop containing a binop within a partial,
     * we can keep hitting backspace to delete the whole thing. This isn't (currently) needed for
     * other types of partials because deleting non-binop partials deletes their args,
     * whereas deleting binop partials merges and hoists the args. */
    forEndOfExpr'(rhsExpr)
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
    | Some(lastExpr) => forEndOfExpr'(lastExpr)
    | None => {astRef: ARPipe(id, 0), offset: String.length("|>")}
    }
  | EMatch(_, matchedExpr, matchItems) =>
    switch List.last(matchItems) {
    | Some(_, branchBody) => forEndOfExpr'(branchBody)
    | None => forEndOfExpr'(matchedExpr)
    }
  | EConstructor(id, name, containedExprs) =>
    switch List.last(containedExprs) {
    | Some(lastExpr) => forEndOfExpr'(lastExpr)
    | None => {astRef: ARConstructor(id), offset: String.length(name)}
    }
  | ECharacter(_)
  | EFeatureFlag(_, _, _, _, _)
  | EPipeTarget(_) =>
    recover(
      "we don't yet support forEndOfExpr' for this",
      ~debug=show_fluidExpr(expr),
      ({astRef: ARInvalid, offset: 0}: t),
    )
  }

/* [forEndOfExpr id ast] produces a caretTarget corresponding
 * to the "very end" of the expr identified by id within the [ast].
 * The concept of "very end" depends on forEndOfExpr'.
 */
let forEndOfExpr = (astPartId: id, ast: FluidAST.t): t =>
  switch FluidAST.findExpr(astPartId, ast) {
  | Some(expr) => forEndOfExpr'(expr)
  | None =>
    recover(
      "forEndOfExpr got an id outside of the AST",
      ~debug=astPartId,
      ({astRef: ARInvalid, offset: 0}: t),
    )
  }

/* [forStartOfExpr' expr] produces a caretTarget corresponding
 * to the very beginning of the [expr]. The concept of "very beginning" is related to an
 * understanding of the tokenization of the expr, even though this function
 * doesn't explicitly depend on any tokenization functions. */
let rec forStartOfExpr': fluidExpr => t = expr =>
  switch expr {
  | EInteger(id, _) => {astRef: ARInteger(id), offset: 0}
  | EBool(id, _) => {astRef: ARBool(id), offset: 0}
  | EString(id, _) => forARStringOpenQuote(id, 0)
  | EFloat(id, _, _, _) => {astRef: ARFloat(id, FPWhole), offset: 0}
  | ENull(id) => {astRef: ARNull(id), offset: 0}
  | EBlank(id) => {astRef: ARBlank(id), offset: 0}
  | ELet(id, _, _, _) => {astRef: ARLet(id, LPKeyword), offset: 0}
  | EIf(id, _, _, _) => {astRef: ARIf(id, IPIfKeyword), offset: 0}
  | EMatch(id, _, _) => {astRef: ARMatch(id, MPKeyword), offset: 0}
  | EBinOp(_, _, lhsExpr, _, _) => forStartOfExpr'(lhsExpr)
  | EFnCall(id, _, _, _) => {astRef: ARFnCall(id), offset: 0}
  | ELambda(id, _, _) => {astRef: ARLambda(id, LBPSymbol), offset: 0}
  | EFieldAccess(_, expr, _) => forStartOfExpr'(expr)
  | EVariable(id, _) => {astRef: ARVariable(id), offset: 0}
  | EPartial(id, _, _) => {astRef: ARPartial(id), offset: 0}
  | ERightPartial(id, _, _) => {astRef: ARRightPartial(id), offset: 0}
  | ELeftPartial(id, _, _) => {astRef: ARLeftPartial(id), offset: 0}
  | EList(id, _) => {astRef: ARList(id, LPOpen), offset: 0}
  | ETuple(id, _, _, _) => {astRef: ARTuple(id, TPOpen), offset: 0}
  | ERecord(id, _) => {astRef: ARRecord(id, RPOpen), offset: 0}
  | EPipe(_, e1, _, _) => forStartOfExpr'(e1)
  | EConstructor(id, _, _) => {astRef: ARConstructor(id), offset: 0}
  | ECharacter(_)
  | EFeatureFlag(_)
  | EPipeTarget(_) =>
    recover(
      "unhandled expr in forStartOfExpr'",
      ~debug=show_fluidExpr(expr),
      ({astRef: ARInvalid, offset: 0}: t),
    )
  }

/* [forStartOfExpr id ast] produces a caretTarget corresponding
 * to the "very beginning" of the expr identified by [id] within the [ast].
 * The concept of "very beginning" depends on forStartOfExpr'.
 */
let forStartOfExpr = (astPartId: id, ast: FluidAST.t): t =>
  switch FluidAST.findExpr(astPartId, ast) {
  | Some(expr) => forStartOfExpr'(expr)
  | None =>
    recover(
      "forStartOfExpr got an id outside of the AST",
      ~debug=astPartId,
      ({astRef: ARInvalid, offset: 0}: t),
    )
  }

@ocaml.doc("returns a caretTarget representing caret placement at the very
  start of the expression in `matchPattern`")
let forStartOfMP = (matchPattern: fluidMatchPattern): t =>
  switch matchPattern {
  | MPVariable(id, _) => {astRef: ARMPattern(id, MPPVariable), offset: 0}
  | MPConstructor(id, _, _) => {astRef: ARMPattern(id, MPPConstructor), offset: 0}
  | MPInteger(id, _) => {astRef: ARMPattern(id, MPPInteger), offset: 0}
  | MPBool(id, _) => {astRef: ARMPattern(id, MPPBool), offset: 0}
  | MPString(id, _) => forMPPStringOpenQuote(id, 0)
  | MPCharacter(_, _) =>
    recover(
      "echar unsupported in forStartOfMP",
      {FluidCursorTypes.CaretTarget.astRef: ARInvalid, offset: 0},
    )
  | MPFloat(id, _, _, _) => {astRef: ARMPattern(id, MPPFloat(FPWhole)), offset: 0}
  | MPNull(id) => {astRef: ARMPattern(id, MPPNull), offset: 0}
  | MPBlank(id) => {astRef: ARMPattern(id, MPPBlank), offset: 0}
  | MPTuple(id, _, _, _) => {astRef: ARMPattern(id, MPPTuple(TPOpen)), offset: 0}
  }

@ocaml.doc("returns a caretTarget representing caret placement at the very end
  of the expression in `matchPattern`.

  The concept of 'very end' is related to an understanding of the tokenization
  of the AST, even though this function doesn't explicitly depend on any
  tokenization functions.")
let rec forEndOfMP = (matchPattern: fluidMatchPattern): t =>
  switch matchPattern {
  | MPVariable(id, varName) => {
      astRef: ARMPattern(id, MPPVariable),
      offset: String.length(varName),
    }
  | MPConstructor(id, name, args) =>
    switch List.last(args) {
    | Some(lastSubpattern) => forEndOfMP(lastSubpattern)
    | None => {astRef: ARMPattern(id, MPPConstructor), offset: String.length(name)}
    }
  | MPTuple(_id, first, second, theRest) =>
    let allSubpatterns = list{first, second, ...theRest}
    switch List.last(allSubpatterns) {
    | Some(lastSubpattern) => forEndOfMP(lastSubpattern)
    | None =>
      recover(
        "no sub-patterns found within tuple match pattern",
        {FluidCursorTypes.CaretTarget.astRef: ARInvalid, offset: 0},
      )
    }
  | MPInteger(id, value) => {
      astRef: ARMPattern(id, MPPInteger),
      offset: String.length(Int64.to_string(value)),
    }
  | MPBool(id, true) => {astRef: ARMPattern(id, MPPBool), offset: String.length("true")}
  | MPBool(id, false) => {astRef: ARMPattern(id, MPPBool), offset: String.length("false")}
  | MPString(id, str) => forMPPStringCloseQuote(id, 1, str) // end of close quote
  | MPCharacter(_) =>
    recover(
      "echar unsupported in forStartOfMP",
      {FluidCursorTypes.CaretTarget.astRef: ARInvalid, offset: 0},
    )
  | MPFloat(id, _, _, frac) => {
      astRef: ARMPattern(id, MPPFloat(FPFractional)),
      offset: String.length(frac),
    }
  | MPNull(id) => {astRef: ARMPattern(id, MPPNull), offset: String.length("null")}
  | MPBlank(id) => {astRef: ARMPattern(id, MPPBlank), offset: 0}
  }

@ocaml.doc("returns a caretTarget representing caret placement at the start of
  the first blank sub-pattern, if any. If there are no blanks, the cursor is
  set at the end of the pattern")
let rec forFirstInputOfMP = (matchPattern: fluidMatchPattern): t =>
  switch matchPattern {
  | MPBlank(id) => {astRef: ARMPattern(id, MPPBlank), offset: 0}
  | MPNull(id) => {astRef: ARMPattern(id, MPPNull), offset: String.length("null")}
  | MPBool(id, true) => {astRef: ARMPattern(id, MPPBool), offset: String.length("true")}
  | MPBool(id, false) => {astRef: ARMPattern(id, MPPBool), offset: String.length("false")}
  | MPString(id, str) => forMPPStringCloseQuote(id, 1, str) // end of close quote
  | MPInteger(id, value) => {
      astRef: ARMPattern(id, MPPInteger),
      offset: String.length(Int64.to_string(value)),
    }
  | MPFloat(id, _, _, frac) => {
      astRef: ARMPattern(id, MPPFloat(FPFractional)),
      offset: String.length(frac),
    }
  | MPVariable(id, varName) => {
      astRef: ARMPattern(id, MPPVariable),
      offset: String.length(varName),
    }
  | MPConstructor(id, name, args) =>
    switch List.last(args) {
    | Some(lastArg) => forFirstInputOfMP(lastArg)
    | None => {astRef: ARMPattern(id, MPPConstructor), offset: String.length(name)}
    }
  | MPTuple(_id, first, second, theRest) =>
    let allSubpatterns = list{first, second, ...theRest}
    switch List.head(allSubpatterns) {
    | Some(lastSubpattern) => forFirstInputOfMP(lastSubpattern)
    | None =>
      recover(
        "no sub-patterns found within tuple match pattern",
        {FluidCursorTypes.CaretTarget.astRef: ARInvalid, offset: 0},
      )
    }
  | MPCharacter(_) =>
    recover(
      "echar unsupported in forStartOfMP",
      {FluidCursorTypes.CaretTarget.astRef: ARInvalid, offset: 0},
    )
  }

@ocaml.doc("returns a caretTarget representing caret placement at the very
  start of the match branch identified by `matchID` and `index` within the
  `ast`. It is an error to pass an id of a non-match or an index outside the
  match.

  'very start' is based on the definition of forStartOfMP")
let forBeginningOfMatchBranch = (matchID: id, index: int, ast: FluidAST.t): t => {
  let maybeTarget = switch FluidAST.findExpr(matchID, ast) {
  | Some(EMatch(_, _, branches)) =>
    branches |> List.getAt(~index) |> Option.map(~f=((mp, _)) => forStartOfMP(mp))
  | _ => None
  }

  maybeTarget |> recoverOpt(
    "forBeginningOfMatchBranch got an invalid id/idx",
    ~debug=(matchID, index),
    ~default=({astRef: ARInvalid, offset: 0}: t),
  )
}

@ocaml.doc("returns a caretTarget representing caret placement at the end of
  the match pattern in the branch identified by `matchID` and `index` within
  the `ast`. It is an error to pass an id of a non-match or an index outside
  the match.

  'end' is based on the definition of forEndOfMP")
let forEndOfMatchPattern = (ast: FluidAST.t, matchID: id, index: int): t => {
  let maybeTarget = switch FluidAST.findExpr(matchID, ast) {
  | Some(EMatch(_, _, branches)) =>
    branches |> List.getAt(~index) |> Option.map(~f=((mp, _)) => forEndOfMP(mp))
  | _ => None
  }

  maybeTarget |> recoverOpt(
    "forEndOfMatchPattern got an invalid id/index",
    ~debug=(matchID, index),
    ~default=({astRef: ARInvalid, offset: 0}: t),
  )
}
