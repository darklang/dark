type featureFlagTokenization =
  | @ocaml.doc(" FeatureFlagOnlyDisabled is used in the main editor panel to only
          * show the flag's old code ")
  FeatureFlagOnlyDisabled

  | @ocaml.doc(" FeatureFlagConditionAndEnabled is used in the secondary editor
          * panel for editing a flag's condition and new code ")
  FeatureFlagConditionAndEnabled

let tokenizeWithFFTokenization: (
  featureFlagTokenization,
  FluidExpression.t,
) => list<FluidToken.tokenInfo>

let tokenize: FluidExpression.t => list<FluidToken.tokenInfo>

@ocaml.doc(" returns the tokens that should populate the given editor ")
let tokensForEditor: (Types.fluidEditor, FluidAST.t) => list<FluidToken.tokenInfo>

@ocaml.doc(" returns the given expression, tokenized with the rules of the specified editor ")
let tokenizeForEditor: (Types.fluidEditor, FluidExpression.t) => list<FluidToken.tokenInfo>

@@ocaml.text(" patternToToken takes a match pattern `p` and converts it to a list of
    fluidTokens.

    ~idx is the zero-based index of the pattern in the enclosing match ")

let patternToToken: (ID.t, FluidPattern.t, ~idx: int) => list<Types.fluidToken>

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

let getToken': (list<FluidToken.tokenInfo>, Types.fluidState) => option<FluidToken.tokenInfo>

/* This is slightly different from getToken. Here we simply want the token
 * closest to the caret that is a not TNewline nor TSep. It is used for
 * figuring out where your caret is, to determine whether certain rendering
 * behavior should be applicable */
let getTokenNotWhitespace: (
  list<FluidToken.tokenInfo>,
  Types.fluidState,
) => option<FluidToken.tokenInfo>

module ASTInfo: {
  type t = {
    ast: FluidAST.t,
    state: Types.fluidState,
    mainTokenInfos: list<FluidToken.tokenInfo>,
    featureFlagTokenInfos: list<(ID.t, list<FluidToken.tokenInfo>)>,
    props: Types.fluidProps,
  }

  let setAST: (FluidAST.t, t) => t

  let ffTokenInfosFor: (ID.t, t) => option<list<FluidToken.tokenInfo>>

  let activeTokenInfos: t => list<FluidToken.tokenInfo>

  let modifyState: (~f: Types.fluidState => Types.fluidState, t) => t

  let getToken: t => option<FluidToken.tokenInfo>

  let getTokenNotWhitespace: t => option<FluidToken.tokenInfo>

  let emptyFor: (Types.fluidProps, Types.fluidState) => t

  let make: (Types.fluidProps, FluidAST.t, Types.fluidState) => t

  let exprOfActiveEditor: t => FluidExpression.t
}
