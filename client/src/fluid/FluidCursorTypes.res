@ppx.deriving(show({with_path: false}))
type rec id = ID.t

// -----------------------------
// Referencing parts of an AST
// at the caret level
// -----------------------------
module AstRef = {
  @ppx.deriving(show({with_path: false}))
  type rec astFloatPart =
    | FPWhole
    | FPPoint
    | FPFractional

  @ppx.deriving(show({with_path: false}))
  type rec astStringPart = SPOpenQuote

  @ppx.deriving(show({with_path: false}))
  type rec astLetPart =
    | LPKeyword
    | LPVarName
    | LPAssignment

  @ppx.deriving(show({with_path: false}))
  type rec astIfPart =
    | IPIfKeyword
    | IPThenKeyword
    | IPElseKeyword

  @ppx.deriving(show({with_path: false}))
  type rec astLambdaPart =
    | LBPSymbol
    | LBPVarName(/* index of the var */ int)
    | LBPComma(/* index of the var */ int)
    | LBPArrow

  @ppx.deriving(show({with_path: false}))
  type rec astFieldAccessPart =
    | FAPFieldname
    | FAPFieldOp

  @ppx.deriving(show({with_path: false}))
  type rec astRecordPart =
    | RPOpen
    | RPFieldname(/* index of the <fieldname,value> pair */ int)
    | RPFieldSep(/* index of the <fieldname,value> pair */ int)
    | RPClose

  @ppx.deriving(show({with_path: false}))
  type rec astListPart =
    | LPOpen
    | LPClose
    | LPComma(int)

  @ppx.deriving(show({with_path: false}))
  type rec astTuplePart =
    | TPOpen
    | TPClose
    | TPComma(int)

  @ppx.deriving(show({with_path: false}))
  type rec astPatternMatchPart =
    // PMP = Pattern-Match Part
    | PMPKeyword
    | PMPBranchArrow(/* index of the branch */ int)

  @ppx.deriving(show({with_path: false}))
  type rec astMatchPatternPart =
    // MPP = MatchPatternPart
    | MPPVariable
    | MPPConstructor
    | MPPTuple(astTuplePart)
    | MPPInteger
    | MPPBool
    | MPPString(astStringPart)
    | MPPFloat(astFloatPart)
    | MPPNull
    | MPPBlank

  @ppx.deriving(show({with_path: false}))
  type rec astFlagPart =
    | FPWhenKeyword
    | FPEnabledKeyword

  /* An astRef represents a reference to a specific part of an AST node,
   such as a specific Record Fieldname rather than just the record.
   Why not use a fluidToken for this purpose?
   A single construct such as a string might map to multiple fluidTokens,
   but when describing a part of the ast (for example with caretTarget),
   we often don't want to care about the details of the tokenization;
   we can represent concepts like "the caret position at the end of this
   string" without needing to know if it is a TString relative to a combination
   of TStringMLStart, TStringMLMiddle, TStringMLEnd.

   The IDs below all refer to the AST node id

   NOTE(JULIAN): We intentionally do not have any astRefs that include
   parts that refer to an part of the AST that contains nested expressions.
   In such cases (for example the value or body of a let), it makes more sense
   to generate a more specific astRef within the nested expression.
 */
  @ppx.deriving(show({with_path: false}))
  type rec t =
    | ARInteger(id)
    | ARBool(id)
    | ARString(id, astStringPart)
    | ARFloat(id, astFloatPart)
    | ARNull(id)
    | ARBlank(id)
    | ARLet(id, astLetPart)
    | ARIf(id, astIfPart)
    | ARBinOp(id) // matches the operator
    | ARFieldAccess(id, astFieldAccessPart)
    | ARVariable(id)
    | ARFnCall(id) // Matches the fn name+version
    | ARPartial(id)
    | ARRightPartial(id)
    | ARLeftPartial(id)
    | ARList(id, astListPart)
    | ARTuple(id, astTuplePart)
    | ARRecord(id, astRecordPart)
    | ARPipe(id, int) // index of the pipe
    | ARConstructor(id) // name of the constructor
    | ARMatch(id, astPatternMatchPart)
    | ARLambda(id, astLambdaPart)
    // MP = MatchPattern
    | ARMPattern(id, astMatchPatternPart)
    | ARFlag(id, astFlagPart)
    // for use if something that should never happen happened
    | ARInvalid
}

module CaretTarget = {
  /* A caretTarget represents a distinct caret location within the AST.
   By combining a reference to part of the AST and a caret offset
   into that part of the AST, we can uniquely represent a place
   for the caret to jump during AST transformations, even ones that
   drastically change the token stream. */
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    astRef: AstRef.t,
    offset: int,
  }
}
