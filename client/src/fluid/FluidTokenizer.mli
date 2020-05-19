type featureFlagTokenization =
  | FeatureFlagOnlyDisabled
      (** FeatureFlagOnlyDisabled is used in the main editor panel to only
          * show the flag's old code *)
  | FeatureFlagConditionAndEnabled
      (** FeatureFlagConditionAndEnabled is used in the secondary editor
          * panel for editing a flag's condition and new code *)

val tokenizeWithFFTokenization :
  featureFlagTokenization -> FluidExpression.t -> FluidToken.tokenInfo list

val tokenize : FluidExpression.t -> FluidToken.tokenInfo list

(** returns the tokens that should populate the given editor *)
val tokensForEditor :
  Types.fluidEditor -> FluidAST.t -> FluidToken.tokenInfo list

(** returns the given expression, tokenized with the rules of the specified editor *)
val tokenizeForEditor :
  Types.fluidEditor -> FluidExpression.t -> FluidToken.tokenInfo list

(** patternToToken takes a match pattern `p` and converts it to a list of
    fluidTokens.

    ~idx is the zero-based index of the pattern in the enclosing match *)

val patternToToken : FluidPattern.t -> idx:int -> Types.fluidToken list

val getTokensAtPosition :
     ?prev:FluidToken.tokenInfo option
  -> pos:int
  -> FluidToken.tokenInfo list
  -> FluidToken.tokenInfo option
     * FluidToken.tokenInfo option
     * FluidToken.tokenInfo option

type neighbour =
  | L of FluidToken.t * FluidToken.tokenInfo
  | R of FluidToken.t * FluidToken.tokenInfo
  | No

val getNeighbours :
     pos:int
  -> FluidToken.tokenInfo list
  -> neighbour * neighbour * FluidToken.tokenInfo option

val getToken' :
  FluidToken.tokenInfo list -> Types.fluidState -> FluidToken.tokenInfo option

(* This is slightly different from getToken. Here we simply want the token
 * closest to the caret that is a not TNewline nor TSep. It is used for
 * figuring out where your caret is, to determine whether certain rendering
 * behavior should be applicable *)
val getTokenNotWhitespace :
  FluidToken.tokenInfo list -> Types.fluidState -> FluidToken.tokenInfo option

module ASTInfo : sig
  type t =
    { ast : FluidAST.t
    ; state : Types.fluidState
    ; mainTokenInfos : FluidToken.tokenInfo list
    ; featureFlagTokenInfos : (ID.t * FluidToken.tokenInfo list) list
    ; props : Types.fluidProps }

  val setAST : FluidAST.t -> t -> t

  val ffTokenInfosFor : ID.t -> t -> FluidToken.tokenInfo list option

  val activeTokenInfos : t -> FluidToken.tokenInfo list

  val modifyState : f:(Types.fluidState -> Types.fluidState) -> t -> t

  val getToken : t -> FluidToken.tokenInfo option

  val getTokenNotWhitespace : t -> FluidToken.tokenInfo option

  val emptyFor : Types.fluidProps -> Types.fluidState -> t

  val make : Types.fluidProps -> FluidAST.t -> Types.fluidState -> t

  val exprOfActiveEditor : t -> FluidExpression.t
end
