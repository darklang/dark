type sendToRail =
  | Rail
  | NoRail
[@@deriving show {with_path = false}, eq]

type t =
  (* ints in Bucklescript only support 32 bit ints but we want 63 bit int
   * support *)
  | EInteger of Shared.id * string
  | EBool of Shared.id * bool
  | EString of Shared.id * string
  | EFloat of Shared.id * string * string
  | ENull of Shared.id
  | EBlank of Shared.id
  | ELet of Shared.id * string * t * t
  | EIf of Shared.id * t * t * t
  | EBinOp of Shared.id * string * t * t * sendToRail
  (* the Shared.id in the varname list is the analysis Shared.id, used to get a livevalue
   * from the analysis engine *)
  | ELambda of Shared.id * (Shared.analysisID * string) list * t
  | EFieldAccess of Shared.id * t * string
  | EVariable of Shared.id * string
  | EFnCall of Shared.id * string * t list * sendToRail
  | EPartial of Shared.id * string * t
  | ERightPartial of Shared.id * string * t
  | EList of Shared.id * t list
  (* The Shared.id in the list is extra for the fieldname *)
  | ERecord of Shared.id * (string * t) list
  | EPipe of Shared.id * t list
  (* Constructors include `Just`, `Nothing`, `Error`, `Ok`.  In practice the
   * expr list is currently always length 1 (for `Just`, `Error`, and `Ok`)
   * or length 0 (for `Nothing`).
   *)
  | EConstructor of Shared.id * string * t list
  | EMatch of Shared.id * t * (FluidPattern.t * t) list
  (* Placeholder that indicates the target of the Thread. May be movable at
   * some point *)
  | EPipeTarget of Shared.id
  (* EFeatureFlag: Shared.id, flagName, condExpr, caseAExpr, caseBExpr *)
  | EFeatureFlag of Shared.id * string * t * t * t
[@@deriving show {with_path = false}, eq]

type fluidPatOrExpr =
  | Expr of t
  | Pat of FluidPattern.t
[@@deriving show {with_path = false}, eq]

val toID : t -> Shared.id

(** Generate a new EBlank *)
val newB : unit -> t

(** Deprecated, this is difficult to use correctly (you have to call back to
    deprecatedWalk from within [f]). Use preTraversal or postTraversal instead.
    [walk f ast] is a helper for recursively walking an expression tree. It
    returns a new ast with every subexpression e replaced by [f e]. To use
    effectively, [f] must call [walk]. *)
val deprecatedWalk : f:(t -> t) -> t -> t

(** [preTraversal f ast] walks the entire AST from top to bottom, calling f on
 * each expression. It returns a new AST with every subexpression e replaced by
 * [f e].  Unlike walk, it does not require you to call preTraversal again. After
 * calling [f], the result is then recursed into; if this isn't what you want
 * call postTraversal. *)
val preTraversal : f:(t -> t) -> t -> t

(** [postTraversal f ast] walks the entire AST from bottom to top, calling f on
 * each function. It returns a new AST with every subexpression e replaced by
 * [f e].  Unlike walk, it does not require you to call preorder again. After
 * calling [f], the result is NOT recursed into; if this isn't what you want
 * call preTraversal. *)
val postTraversal : f:(t -> t) -> t -> t

(** [filterMap f ast] calls f on every expression, keeping any Some results
 * of f, returning them in a list. Recurses into expressions: if a child and
 * its parent (or grandparent, etc) both match, then both will be in the
 * result list. *)
val filterMap : f:(t -> 'a option) -> t -> 'a list

(** [filter f ast] calls f on every expression, returning a list of all
 * expressions for which [f e] is true. Recurses into expressions:
 * if a child and its parent (or grandparent, etc) both match, then both will
 * be in the result list.  *)
val filter : f:(t -> bool) -> t -> t list

(** [findExprOrPat target within] recursively finds the subtree
    with the Shared.id = [target] inside the [within] tree, returning the subtree
    wrapped in fluidPatOrExpr, or None if there is no subtree with the Shared.id [target] *)
val findExprOrPat : Shared.id -> fluidPatOrExpr -> fluidPatOrExpr option

(** [find target ast] recursively finds the expression having an Shared.id of [target]
   and returns it if found. *)
val find : Shared.id -> t -> t option

(** [findParent target ast] recursively finds the expression having an Shared.id of
    [target] and then returns the parent of that expression. *)
val findParent : Shared.id -> t -> t option

(** [isEmpty e] returns true if e is an EBlank or a collection (ERecord or
    EList) with only EBlanks inside. *)
val isEmpty : t -> bool

(** [hasEmptyWithId target ast] recursively finds the expression having an Shared.id
    of [target] and returns true if that expression exists and [isEmpty]. *)
val hasEmptyWithId : Shared.id -> t -> bool

(** [isBlank e] returns true iff [e] is an EBlank. *)
val isBlank : t -> bool

(** [blanks e] returns all children [c] of [e] where [isBlank c] is true *)
val blanks : t -> t list

(** [ids e] returns the id of [e] and all its children *)
val ids : t -> Shared.id list

(** [children e] returns a list of all the direct children of [e] *)
val children : t -> t list

(** [decendants e] returns a list of the IDs of all decendants (children,
 * grandchildren, etc) of [e] in an unspecified order *)
val decendants : t -> Shared.id list

val ancestors : Shared.id -> t -> t list

(** [update f target ast] recursively searches [ast] for an expression e
    having an Shared.id of [target].

    If found, replaces the expression with the result of [f e] and returns the new ast.
    If not found, will assertT before returning the unmodified [ast]. *)
val update : ?failIfMissing:bool -> f:(t -> t) -> Shared.id -> t -> t

(** [replace replacement target ast] finds the expression with Shared.id of [target] in the [ast] and replaces it with [replacement]. *)
val replace : replacement:t -> Shared.id -> t -> t

val removeVariableUse : string -> t -> t

val renameVariableUses : oldName:string -> newName:string -> t -> t

val updateVariableUses : string -> f:(t -> t) -> t -> t

val clone : t -> t

(** [testEqualIgnoringIds a b] compares the structure and values of two ASTs,
  * ignoring the actual IDs of the expressions.
  *
  * NB: Only usable for tests right now. If you want to use for non-tests,
  * you'll need to complete the implementation and add tests *)
val testEqualIgnoringIds : t -> t -> bool
