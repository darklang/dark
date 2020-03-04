type varBind = string Types.blankOr

type field = string Types.blankOr

type key = string Types.blankOr

type lambdaParameter = string Types.blankOr

type nPattern =
  | PVariable of string
  | PLiteral of string
  | PConstructor of string * pattern list

and pattern = nPattern Types.blankOr

val toFluidPattern : ID.t -> pattern -> FluidPattern.t

val fromFluidPattern : FluidPattern.t -> pattern

type expr = nExpr Types.blankOr

and nExpr =
  | If of expr * expr * expr
  | FnCall of
      Types.fnName Types.blankOr * expr list * FluidExpression.sendToRail
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

(** We use this to convert to the old Expr type, and also to convert to
 * tokens. *)
val functions : Types.function_ list ref

(** [toNExpr e] recursively converts [e] to the corresponding [nExpr Types.blankOr] *)
val fromFluidExpr : FluidExpression.t -> expr

(** [toFluid e] recursively converts a corresponding [nExpr Types.blankOr] to [e] *)
val toFluidExpr : expr -> FluidExpression.t
