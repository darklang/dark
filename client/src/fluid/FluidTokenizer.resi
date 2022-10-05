type featureFlagTokenization =
  | @ocaml.doc("used in the main editor panel to only show the flag's old code ")
  FeatureFlagOnlyDisabled

  | @ocaml.doc("used in the secondary editor panel for editing a flag's condition and new code")
  FeatureFlagConditionAndEnabled

@@ocaml.text("takes a match pattern `p` and converts it to a list of
  fluidTokens.

  ~idx is the zero-based index of the match pattern in the enclosing match")
let matchPatternToTokens: (ID.t, ~idx: int, FluidMatchPattern.t) => list<FluidTypes.Token.t>

let tokenizeExpr: FluidExpression.t => list<FluidToken.tokenInfo>

@ocaml.doc("returns the given expression, tokenized with the rules of the specified editor")
let tokenizeExprForEditor: (FluidTypes.Editor.t, FluidExpression.t) => list<FluidToken.tokenInfo>

@ocaml.doc("returns the given match pattern, tokenized with the rules of the specified editor")
let tokenizeMatchPatternForEditor: (
  FluidTypes.Editor.t,
  ID.t,
  int,
  FluidMatchPattern.t,
) => list<FluidToken.tokenInfo>

@ocaml.doc("the same as tokenizeExprForEditor, but for the contributor debugger")
let tokenizeExprForDebugger: (FluidTypes.Editor.t, FluidAST.t) => list<FluidToken.tokenInfo>

let getTokensAtPosition: (
  ~prev: option<FluidToken.tokenInfo>=?,
  ~pos: int,
  list<FluidToken.tokenInfo>,
) => (option<FluidToken.tokenInfo>, option<FluidToken.tokenInfo>, option<FluidToken.tokenInfo>)

type neighbour =
  | L(FluidToken.t, FluidToken.tokenInfo)
  | R(FluidToken.t, FluidToken.tokenInfo)
  | No

let getNeighbours: (
  ~pos: int,
  list<FluidToken.tokenInfo>,
) => (neighbour, neighbour, option<FluidToken.tokenInfo>)

let getToken': (list<FluidToken.tokenInfo>, AppTypes.fluidState) => option<FluidToken.tokenInfo>

/* This is slightly different from getToken. Here we simply want the token
 * closest to the caret that is a not TNewline nor TSep. It is used for
 * figuring out where your caret is, to determine whether certain rendering
 * behavior should be applicable */
let getTokenNotWhitespace: (
  list<FluidToken.tokenInfo>,
  AppTypes.fluidState,
) => option<FluidToken.tokenInfo>

module ASTInfo: {
  type t = {
    ast: FluidAST.t,
    state: AppTypes.fluidState,
    mainTokenInfos: list<FluidToken.tokenInfo>,
    featureFlagTokenInfos: list<(ID.t, list<FluidToken.tokenInfo>)>,
  }

  let setAST: (FluidAST.t, t) => t

  let ffTokenInfosFor: (ID.t, t) => option<list<FluidToken.tokenInfo>>

  let activeTokenInfos: t => list<FluidToken.tokenInfo>

  let modifyState: (~f: AppTypes.fluidState => AppTypes.fluidState, t) => t

  let getToken: t => option<FluidToken.tokenInfo>

  let getTokenNotWhitespace: t => option<FluidToken.tokenInfo>

  let emptyFor: AppTypes.fluidState => t

  let make: (FluidAST.t, AppTypes.fluidState) => t

  let exprOfActiveEditor: t => FluidExpression.t
}
