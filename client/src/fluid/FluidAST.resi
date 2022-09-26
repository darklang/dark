@ocaml.doc(" FluidAST.t is a way to differentiate between the entire AST of a
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
  * [map] or [toExpr] as appropriate. ")
type t = ProgramTypes.AST.t

let show: t => string

@ocaml.doc(" [pp] allows @deriving show to work ")
let pp: (Format.formatter, t) => unit

@ocaml.doc(" [toExpr ast] returns the expression of [ast] ")
let toExpr: t => FluidExpression.t

@ocaml.doc(" [ofExpr e] returns the FluidExpression.t [e] wrapped in a [t].
  * WARNING: this is dangerous, as it allows treating an arbitrary expression
  * as if it represents the entire AST. Be sure the expression is actually the
  * root of the AST before using this. It is much safer to use the functions
  * provided in this module instead.")
let ofExpr: FluidExpression.t => t

@ocaml.doc(" [toID ast] returns the id of the first (root) expression of [ast]. ")
let toID: t => ID.t

@ocaml.doc(" [map ast ~f] passes [ast] to [f] as an expression and returns the result of
  * [f e] as an AST. ")
let map: (~f: FluidExpression.t => FluidExpression.t, t) => t

@ocaml.doc(" [replace ast ~replacement] finds the expression with the id of [target] in
  * [ast] and returns a new AST with it replaced by [replacement].
  *
  * See FluidExpression.replace ")
let replace: (~replacement: FluidExpression.t, ID.t, t) => t

@ocaml.doc(" [update f target ast] recursively searches [ast] for the expression having
  * an id of [target].
  *
  * If found, replaces the expression with the result of [f e] and returns the
  * new AST. If not found, will asserT before returning the unmodified [ast].
  *
  * Passing failIfMissing=false will skip the asserT and silently return an
  * unmodified AST.
  *
  * See FluidExpression.update ")
let update: (~failIfMissing: bool=?, ~f: FluidExpression.t => FluidExpression.t, ID.t, t) => t

@ocaml.doc(" [filter ast ~f] recursively calls [f] on every expression within [ast],
  * returning a list of all expressions for which [f e] is true.
  *
  * See FluidExpression.filter ")
let filter: (t, ~f: FluidExpression.t => bool) => list<FluidExpression.t>

@ocaml.doc(" [blanks ast] returns a list of all EBlank expressions within [ast].
 *
 * See FluidExpression.blanks ")
let blanks: t => list<FluidExpression.t>

@ocaml.doc(" [ids ast] returns a list of the id of every expression in [ast].
  *
  * See FluidExpression.ids ")
let ids: t => list<ID.t>

@ocaml.doc("[findExpr target ast] recursively finds the expression having an id
  of [target] in [ast] and returns it if found.

  See FluidExpression.find")
let findExpr: (ID.t, t) => option<FluidExpression.t>

@ocaml.doc("[findMP target ast] recursively finds the match pattern having an
  id of [target] in [ast] and returns it if found")
let findMP: (ID.t, t) => option<FluidMatchPattern.t>

@ocaml.doc(" [findExprParent target ast] recursively finds the expression having an id of
  * [target] in [ast] and then returns the parent of that expression.
  *
  * See FluidExpression.findParent ")
let findExprParent: (ID.t, t) => option<FluidExpression.t>

@ocaml.doc(" [ancestors target ast] finds the expression having an id of [target] in
  * [ast] and then returns the list of ancestors (parent, parent of parent, etc)
  * of that expression.
  *
  * See FluidExpression.ancestors ")
let exprAncestors: (ID.t, t) => list<FluidExpression.t>

@ocaml.doc(" [getFeatureFlags ast] returns a list of all feature flags in the AST. ")
let getFeatureFlags: t => list<FluidExpression.t>

@ocaml.doc(" [clone ast] returns a clone of [ast]. That is, a structural copy of the AST
  * but with every expression having a newly generated id.
  *
  * See FluidExpression.clone ")
let clone: t => t

@ocaml.doc(" [testEqualIgnoringIds a b] compares the structure and values of two ASTs,
  * ignoring the actual IDs of the expressions.
  *
  * Find the implementation in the FluidExpression function of the same name. ")
let testEqualIgnoringIds: (t, t) => bool

@ocaml.doc(" [updateMatchPattern f matchID patID ast] recursively searches [ast] for a
  * match expression containing a pattern with id [patID].
  *
  * If found, replaces the pattern with the result of [f p] and returns the
  * new AST. If not found, will asserT before returning the unmodified [ast].
  *
  * Passing failIfMissing=false will skip the asserT and silently return an
  * unmodified AST.
  *
  * See FluidExpression.update ")
let updateMatchPattern: (~f: FluidMatchPattern.t => FluidMatchPattern.t, ID.t, ID.t, t) => t

@ocaml.doc(" [replacePattern ~newPat matchID patID ast] recursively searches [ast]
  * for a match expression containing a pattern with id [patID].
  *
  * If found, replaces the pattern with newPat and returns the new AST. If not
  * found, will asserT before returning the unmodified [ast].
  *
  * Passing failIfMissing=false will skip the asserT and silently return an
  * unmodified AST.
  *
  * See FluidExpression.update ")
let replaceMatchPattern: (~newPat: FluidMatchPattern.t, ID.t, ID.t, t) => t
