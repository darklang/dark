type varBind = string Types.blankOr

type field = string Types.blankOr

type key = string Types.blankOr

type lambdaParameter = string Types.blankOr

type nPattern =
  | PVariable of string
  | PLiteral of string
  | PConstructor of string * pattern list

and pattern = nPattern Types.blankOr

val toFluidPattern : Types.id -> pattern -> Types.fluidPattern

val fromFluidPattern : FluidPattern.t -> pattern

type expr = nExpr Types.blankOr

and nExpr =
  | If of expr * expr * expr
  | FnCall of Types.fnName Types.blankOr * expr list * Types.sendToRail
  | Variable of string
  | Let of varBind * expr * expr
  | Lambda of lambdaParameter list * expr
  | Value of string
  | ObjectLiteral of (key * expr) list
  | ListLiteral of expr list
  | Thread of expr list
  | FieldAccess of expr * field
  | FeatureFlag of string Types.blankOr * expr * expr * expr
  | Match of expr * (pattern * expr) list
  | Constructor of string Types.blankOr * expr list
  | FluidPartial of string * expr
  | FluidRightPartial of string * expr

(** [toNExpr e] recursively converts [e] to the corresponding [nExpr Types.blankOr] *)
val fromFluidExpr : FluidExpression.t -> expr

(** [toFluid e] recursively converts a corresponding [nExpr Types.blankOr] to [e] *)
val toFluidExpr : expr -> FluidExpression.t

(** The assertion in toFluidExpr will eventually go away as the server will
  * also use fluidExprs and so there won't be a need to convert. But we
  * sometimes need to call this from a place without the functions available,
  * and we don't want to set off rollbar. *)
val toFluidExprNoAssertion : expr -> FluidExpression.t
