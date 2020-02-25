(** FluidAST.t is a way to differentiate between the entire AST of a handler/function
  * and a subtree of it. *)
type t = Root of FluidExpression.t [@@deriving show {with_path = false}, eq]

(** [toExpr r] returns the expression of [r] *)
val toExpr : t -> FluidExpression.t

(** [ofExpr r] returns the expression of [r]
  * WARNING: this is dangerous, as it allows treating an arbitrary expression
  * as if it represents the entire AST. Be sure the expression is actually the
  * root of the AST before using this. It is much safer to use the functions
  * provided in this module instead.*)
val ofExpr : FluidExpression.t -> t

val toID : t -> Shared.id

(** [map ast ~f] passes [ast] to [f] as an expression and returns the result of
 * [f e] as an AST. *)
val map : f:(FluidExpression.t -> FluidExpression.t) -> t -> t

(** [replace ast ~replacement] is like FluidExpression.replace *)
val replace : replacement:FluidExpression.t -> Shared.id -> t -> t

(** [update ast ~f] is like FluidExpression.update *)
val update :
     ?failIfMissing:bool
  -> f:(FluidExpression.t -> FluidExpression.t)
  -> Shared.id
  -> t
  -> t

val filter : t -> f:(FluidExpression.t -> bool) -> FluidExpression.t list

val blanks : t -> FluidExpression.t list

val ids : t -> Shared.id list

val find : Shared.id -> t -> FluidExpression.t option

val findParent : Shared.id -> t -> FluidExpression.t option

val ancestors : Shared.id -> t -> FluidExpression.t list

val clone : t -> t
