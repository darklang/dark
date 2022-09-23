@ppx.deriving(show({with_path: false})) type rec t = ProgramTypes.Expr.t

@ppx.deriving(show({with_path: false}))
type fluidMatchPatOrExpr =
  | Expr(ProgramTypes.Expr.t)
  | MatchPat(ID.t, FluidMatchPattern.t)

let toID: ProgramTypes.Expr.t => ID.t

@ocaml.doc(" Generate a new EBlank ")
let newB: unit => ProgramTypes.Expr.t

@ocaml.doc(" Deprecated, this is difficult to use correctly (you have to call back to
    deprecatedWalk from within [f]). Use preTraversal or postTraversal instead.
    [walk f ast] is a helper for recursively walking an expression tree. It
    returns a new ast with every subexpression e replaced by [f e]. To use
    effectively, [f] must call [walk]. ")
let deprecatedWalk: (
  ~f: ProgramTypes.Expr.t => ProgramTypes.Expr.t,
  ProgramTypes.Expr.t,
) => ProgramTypes.Expr.t

@ocaml.doc(" [preTraversal f ast] walks the entire AST from top to bottom, calling f on
 * each expression. It returns a new AST with every subexpression e replaced by
 * [f e].  Unlike walk, it does not require you to call preTraversal again. After
 * calling [f], the result is then recursed into; if this isn't what you want
 * call postTraversal. ")
let preTraversal: (
  ~f: ProgramTypes.Expr.t => ProgramTypes.Expr.t,
  ProgramTypes.Expr.t,
) => ProgramTypes.Expr.t

@ocaml.doc(" [postTraversal f ast] walks the entire AST from bottom to top, calling f on
 * each function. It returns a new AST with every subexpression e replaced by
 * [f e].  Unlike walk, it does not require you to call preorder again. After
 * calling [f], the result is NOT recursed into; if this isn't what you want
 * call preTraversal. ")
let postTraversal: (
  ~f: ProgramTypes.Expr.t => ProgramTypes.Expr.t,
  ProgramTypes.Expr.t,
) => ProgramTypes.Expr.t

@ocaml.doc(" [filterMap f ast] calls f on every expression, keeping any Some results
 * of f, returning them in a list. Recurses into expressions: if a child and
 * its parent (or grandparent, etc) both match, then both will be in the
 * result list. ")
let filterMap: (~f: ProgramTypes.Expr.t => option<'a>, ProgramTypes.Expr.t) => list<'a>

@ocaml.doc(" [filter f ast] calls f on every expression, returning a list of all
 * expressions for which [f e] is true. Recurses into expressions:
 * if a child and its parent (or grandparent, etc) both match, then both will
 * be in the result list.  ")
let filter: (~f: ProgramTypes.Expr.t => bool, ProgramTypes.Expr.t) => list<ProgramTypes.Expr.t>

@ocaml.doc(" [findExprOrPat target within] recursively finds the subtree
    with the ID.t = [target] inside the [within] tree, returning the subtree
    wrapped in fluidMatchPatOrExpr, or None if there is no subtree with the ID.t [target] ")
let findExprOrPat: (ID.t, fluidMatchPatOrExpr) => option<fluidMatchPatOrExpr>

@ocaml.doc(" [find target ast] recursively finds the expression having an ID.t of [target]
   and returns it if found. ")
let find: (ID.t, ProgramTypes.Expr.t) => option<ProgramTypes.Expr.t>

@ocaml.doc(" [findParent target ast] recursively finds the expression having an ID.t of
    [target] and then returns the parent of that expression. ")
let findParent: (ID.t, ProgramTypes.Expr.t) => option<ProgramTypes.Expr.t>

@ocaml.doc(" [isEmpty e] returns true if e is an EBlank or a collection (ERecord or
    EList) with only EBlanks inside. ")
let isEmpty: ProgramTypes.Expr.t => bool

@ocaml.doc(" [hasEmptyWithId target ast] recursively finds the expression having an ID.t
    of [target] and returns true if that expression exists and [isEmpty]. ")
let hasEmptyWithId: (ID.t, ProgramTypes.Expr.t) => bool

@ocaml.doc(" [isBlank e] returns true iff [e] is an EBlank. ")
let isBlank: ProgramTypes.Expr.t => bool

@ocaml.doc(" [blanks e] returns all children [c] of [e] where [isBlank c] is true ")
let blanks: ProgramTypes.Expr.t => list<ProgramTypes.Expr.t>

@ocaml.doc(" [ids e] returns the id of [e] and all its children ")
let ids: ProgramTypes.Expr.t => list<ID.t>

@ocaml.doc(" [children e] returns a list of all the direct children of [e] ")
let children: ProgramTypes.Expr.t => list<ProgramTypes.Expr.t>

@ocaml.doc(" [decendants e] returns a list of the IDs of all decendants (children,
 * grandchildren, etc) of [e] in an unspecified order ")
let decendants: ProgramTypes.Expr.t => list<ID.t>

let ancestors: (ID.t, ProgramTypes.Expr.t) => list<ProgramTypes.Expr.t>

@ocaml.doc(" [update f target ast] recursively searches [ast] for an expression e
    having an ID.t of [target].

    If found, replaces the expression with the result of [f e] and returns the new ast.
    If not found, will assertT before returning the unmodified [ast]. ")
let update: (
  ~failIfMissing: bool=?,
  ~f: ProgramTypes.Expr.t => ProgramTypes.Expr.t,
  ID.t,
  ProgramTypes.Expr.t,
) => ProgramTypes.Expr.t

@ocaml.doc(
  " [replace replacement target ast] finds the expression with ID.t of [target] in the [ast] and replaces it with [replacement]. "
)
let replace: (~replacement: ProgramTypes.Expr.t, ID.t, ProgramTypes.Expr.t) => ProgramTypes.Expr.t

let removeVariableUse: (string, ProgramTypes.Expr.t) => ProgramTypes.Expr.t

let renameVariableUses: (
  ~oldName: string,
  ~newName: string,
  ProgramTypes.Expr.t,
) => ProgramTypes.Expr.t

let updateVariableUses: (
  string,
  ~f: ProgramTypes.Expr.t => ProgramTypes.Expr.t,
  ProgramTypes.Expr.t,
) => ProgramTypes.Expr.t

let clone: ProgramTypes.Expr.t => ProgramTypes.Expr.t

@ocaml.doc(" [testEqualIgnoringIds a b] compares the structure and values of two ASTs,
  * ignoring the actual IDs of the expressions.
  *
  * NB: Only usable for tests right now. If you want to use for non-tests,
  * you'll need to complete the implementation and add tests ")
let testEqualIgnoringIds: (ProgramTypes.Expr.t, ProgramTypes.Expr.t) => bool

@ocaml.doc(" toHumanReadable returns a string of the expression tokenized into a
 * human-readable S-exp-like formatting that contains newlines and truncated
 * strings, which is meant to closely mirror the actual AST and aid in
 * debugging.
 *
 * Do not attempt to parse this or use it in comparisons of any sort, as its
 * format is not considered stable. Again, it is only for debugging with your
 * squishy human eyes. ")
let toHumanReadable: (ProgramTypes.Expr.t, bool) => string
