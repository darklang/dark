(** FluidAST.t is a way to differentiate between the entire AST of a
  * handler/function and a subtree of it. You should prefer working with this
  * type whenever you know you have a full handler AST and want to either
  * execute it or persist it to storage somehow.
  *
  * Many of these functions are simply wrappers around functions of
  * FluidExpression, but by using these functions you can ensure you never
  * accidentally treat a sub-tree of an AST as the entire tree. Functions that
  * return the whole AST return a new FluidAST.t whereas functions that return
  * subtrees return FluidExpression.t.
  *
  * If you need new a new FluidAST function that already exists in
  * FluidExpression, it is usually safe to wrap the existing function with
  * [map] or [toExpr] as appropriate. *)
type t

val show : t -> string

(** [pp] allows @deriving show to work *)
val pp : Format.formatter -> t -> unit

(** [toExpr ast] returns the expression of [ast] *)
val toExpr : t -> FluidExpression.t

(** [ofExpr e] returns the FluidExpression.t [e] wrapped in a [t].
  * WARNING: this is dangerous, as it allows treating an arbitrary expression
  * as if it represents the entire AST. Be sure the expression is actually the
  * root of the AST before using this. It is much safer to use the functions
  * provided in this module instead.*)
val ofExpr : FluidExpression.t -> t

(** [toID ast] returns the id of the first (root) expression of [ast]. *)
val toID : t -> Shared.id

(** [map ast ~f] passes [ast] to [f] as an expression and returns the result of
  * [f e] as an AST. *)
val map : f:(FluidExpression.t -> FluidExpression.t) -> t -> t

(** [replace ast ~replacement] finds the expression with the id of [target] in
  * [ast] and returns a new AST with it replaced by [replacement].
  *
  * See FluidExpression.replace *)
val replace : replacement:FluidExpression.t -> Shared.id -> t -> t

(** [update f target ast] recursively searches [ast] for the expression having
  * an id of [target].
  *
  * If found, replaces the expression with the result of [f e] and returns the
  * new AST. If not found, will asserT before returning the unmodified [ast].
  *
  * Passing failIfMissing=false will skip the asserT and silently return an
  * unmodified AST.
  *
  * See FluidExpression.update *)
val update :
     ?failIfMissing:bool
  -> f:(FluidExpression.t -> FluidExpression.t)
  -> Shared.id
  -> t
  -> t

(** [filter ast ~f] recursively calls [f] on every expression within [ast],
  * returning a list of all expressions for which [f e] is true.
  *
  * See FluidExpression.filter *)
val filter : t -> f:(FluidExpression.t -> bool) -> FluidExpression.t list

(** [blanks ast] returns a list of all EBlank expressions within [ast].
 *
 * See FluidExpression.blanks *)
val blanks : t -> FluidExpression.t list

(** [ids ast] returns a list of the id of every expression in [ast].
  *
  * See FluidExpression.ids *)
val ids : t -> Shared.id list

(** [find target ast] recursively finds the expression having an id of [target]
  * in [ast] and returns it if found.
  *
  * See FluidExpression.find *)
val find : Shared.id -> t -> FluidExpression.t option

(** [findParent target ast] recursively finds the expression having an id of
  * [target] in [ast] and then returns the parent of that expression.
  *
  * See FluidExpression.findParent *)
val findParent : Shared.id -> t -> FluidExpression.t option

(** [ancestors target ast] finds the expression having an id of [target] in
  * [ast] and then returns the list of ancestors (parent, parent of parent, etc)
  * of that expression.
  *
  * See FluidExpression.ancestors *)
val ancestors : Shared.id -> t -> FluidExpression.t list

(** [getFeatureFlags ast] returns a list of all feature flags in the AST. *)
val getFeatureFlags : t -> FluidExpression.t list

(** [clone ast] returns a clone of [ast]. That is, a structural copy of the AST
  * but with every expression having a newly generated id.
  *
  * See FluidExpression.clone *)
val clone : t -> t

(** [testEqualIgnoringIds a b] compares the structure and values of two ASTs,
  * ignoring the actual IDs of the expressions.
  *
  * Find the implementation in the FluidExpression function of the same name. *)
val testEqualIgnoringIds : t -> t -> bool
