type t = Types.fluidExpr

val toNexpr : t -> Types.expr
(** [toNexpr e] recursively converts [e] to the corresponding [nExpr blankOr] *)

val id : t -> Types.id
(** [id e] returns the id of [e] *)

val show : t -> string
(** [show e] returns a string representation of [e]. *)

val walk : f:(t -> t) -> t -> t
(** [walk f ast] is a helper for recursively walking an expression tree. It
    returns a new ast with every subexpression e replaced by [f e]. To use
    effectively, [f] must call [walk]. *)

val find : Types.id -> t -> t option
(** [find target ast] recursively finds the expression having an id of [target]
   and returns it if found. *)

val findParent : Types.id -> t -> t option
(** [findParent target ast] recursively finds the expression having an id of
    [target] and then returns the parent of that expression. *)

val isEmpty : t -> bool
(** [isEmpty e] returns true if e is an EBlank or a collection (ERecord or
    EList) with only EBlanks inside. *)

val hasEmptyWithId : Types.id -> t -> bool
(** [hasEmptyWithId target ast] recursively finds the expression having an id
    of [target] and returns true if that expression exists and [isEmpty]. *)

val isBlank : t -> bool
(** [isBlank e] returns true iff [e] is an EBlank. *)

val update : ?failIfMissing:bool -> f:(t -> t) -> Types.id -> t -> t
(** [update f target ast] recursively searches [ast] for an expression e
    having an id of [target].

    If found, replaces the expression with the result of [f e] and returns the new ast.
    If not found, will assertT before returning the unmodified [ast]. *)

val replace : replacement:t -> Types.id -> t -> t
(** [replace replacement target ast] finds the expression with id of [target] in the [ast] and replaces it with [replacement]. *)
