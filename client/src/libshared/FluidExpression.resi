@ppx.deriving(show({with_path: false}))
type rec sendToRail =
  | Rail
  | NoRail

@ppx.deriving(show({with_path: false}))
type rec t =
  /* ints in Bucklescript only support 32 bit ints but we want 63 bit int
   * support */
  | EInteger(ID.t, string)
  | EBool(ID.t, bool)
  | EString(ID.t, string)
  | EFloat(ID.t, string, string)
  | ENull(ID.t)
  | EBlank(ID.t)
  | ELet(ID.t, string, t, t)
  | EIf(ID.t, t, t, t)
  | EBinOp(ID.t, string, t, t, sendToRail)
  /* the ID.t in the varname list is the analysis ID.t, used to get a livevalue
   * from the analysis engine */
  | ELambda(ID.t, list<(ID.t, string)>, t)
  | EFieldAccess(ID.t, t, string)
  | EVariable(ID.t, string)
  | EFnCall(ID.t, string, list<t>, sendToRail)
  /* An EPartial holds the intermediate state of user-input when changing from
   * one expression to another. The [string] is the exact text that has been
   * entered and the [t] is the old expression that is being changed.
   *
   * Examples:
   * - When filling in an EBlank by typing `Str` an EPartial (id, "Str", EBlank (...)) is used.
   * - When changing the EFnCall of "String::append" by deleting a character
   *   from the end, an EPartial (id, "String::appen", EFnCall _) would
   *   be created.
   *
   * EPartial is usually rendered as just the string part, but sometimes when
   * wrapping certain kinds of expressions will be rendered in unique ways.
   * Eg, an EPartial wrapping an EFnCall will render the arguments of the old
   * EFnCall expression after the string. See FluidPrinter for specifics. */
  | EPartial(ID.t, string, t)
  /* An ERightPartial is used while in the process of adding an EBinOp,
   * allowing for typing multiple characters as operators (eg, "++") after an
   * expression. The [string] holds the typed characters while the [t] holds
   * the LHS of the binop.
   *
   * Example:
   * Typing `"foo" ++` creates ERightPartial (id, "++", EString (_, "foo"))
   * until the autocomplete of "++" is accepted, transforming the ERightPartial
   * into a proper EBinOp.
   *
   * ERightPartial is rendered as the old expression followed by the string. */
  | ERightPartial(ID.t, string, t)
  /* ELeftPartial allows typing to prepend a construct to an existing
   * expression. The [string] holds the typed text, while the [t] holds the
   * existing expression to the right.
   *
   * Example:
   * On an existing line with `String::append "a" "b"` (a EFnCall), typing `if` at the beginning of the line
   * will create a ELeftPartial (id, "if", EFnCall _). Accepting autocomplete
   * of `if` would wrap the EFnCall into an EIf.
   *
   * ELeftPartial is rendered as the string followed by the normal rendering of the old expression. */
  | ELeftPartial(ID.t, string, t)
  | EList(ID.t, list<t>)
  /* The ID.t in the list is extra for the fieldname */
  | ERecord(ID.t, list<(string, t)>)
  | EPipe(ID.t, list<t>)
  /* Constructors include `Just`, `Nothing`, `Error`, `Ok`.  In practice the
   * expr list is currently always length 1 (for `Just`, `Error`, and `Ok`)
   * or length 0 (for `Nothing`).
   */
  | EConstructor(ID.t, string, list<t>)
  | EMatch(ID.t, t, list<(FluidPattern.t, t)>)
  /* Placeholder that indicates the target of the Thread. May be movable at
   * some point */
  | EPipeTarget(ID.t)
  /* EFeatureFlag: ID.t, flagName, condExpr, caseAExpr, caseBExpr */
  | EFeatureFlag(ID.t, string, t, t, t)

@ppx.deriving(show({with_path: false}))
type fluidPatOrExpr =
  | Expr(t)
  | Pat(FluidPattern.t)

let toID: t => ID.t

@ocaml.doc(" Generate a new EBlank ")
let newB: unit => t

@ocaml.doc(" Deprecated, this is difficult to use correctly (you have to call back to
    deprecatedWalk from within [f]). Use preTraversal or postTraversal instead.
    [walk f ast] is a helper for recursively walking an expression tree. It
    returns a new ast with every subexpression e replaced by [f e]. To use
    effectively, [f] must call [walk]. ")
let deprecatedWalk: (~f: t => t, t) => t

@ocaml.doc(" [preTraversal f ast] walks the entire AST from top to bottom, calling f on
 * each expression. It returns a new AST with every subexpression e replaced by
 * [f e].  Unlike walk, it does not require you to call preTraversal again. After
 * calling [f], the result is then recursed into; if this isn't what you want
 * call postTraversal. ")
let preTraversal: (~f: t => t, t) => t

@ocaml.doc(" [postTraversal f ast] walks the entire AST from bottom to top, calling f on
 * each function. It returns a new AST with every subexpression e replaced by
 * [f e].  Unlike walk, it does not require you to call preorder again. After
 * calling [f], the result is NOT recursed into; if this isn't what you want
 * call preTraversal. ")
let postTraversal: (~f: t => t, t) => t

@ocaml.doc(" [filterMap f ast] calls f on every expression, keeping any Some results
 * of f, returning them in a list. Recurses into expressions: if a child and
 * its parent (or grandparent, etc) both match, then both will be in the
 * result list. ")
let filterMap: (~f: t => option<'a>, t) => list<'a>

@ocaml.doc(" [filter f ast] calls f on every expression, returning a list of all
 * expressions for which [f e] is true. Recurses into expressions:
 * if a child and its parent (or grandparent, etc) both match, then both will
 * be in the result list.  ")
let filter: (~f: t => bool, t) => list<t>

@ocaml.doc(" [findExprOrPat target within] recursively finds the subtree
    with the ID.t = [target] inside the [within] tree, returning the subtree
    wrapped in fluidPatOrExpr, or None if there is no subtree with the ID.t [target] ")
let findExprOrPat: (ID.t, fluidPatOrExpr) => option<fluidPatOrExpr>

@ocaml.doc(" [find target ast] recursively finds the expression having an ID.t of [target]
   and returns it if found. ")
let find: (ID.t, t) => option<t>

@ocaml.doc(" [findParent target ast] recursively finds the expression having an ID.t of
    [target] and then returns the parent of that expression. ")
let findParent: (ID.t, t) => option<t>

@ocaml.doc(" [isEmpty e] returns true if e is an EBlank or a collection (ERecord or
    EList) with only EBlanks inside. ")
let isEmpty: t => bool

@ocaml.doc(" [hasEmptyWithId target ast] recursively finds the expression having an ID.t
    of [target] and returns true if that expression exists and [isEmpty]. ")
let hasEmptyWithId: (ID.t, t) => bool

@ocaml.doc(" [isBlank e] returns true iff [e] is an EBlank. ")
let isBlank: t => bool

@ocaml.doc(" [blanks e] returns all children [c] of [e] where [isBlank c] is true ")
let blanks: t => list<t>

@ocaml.doc(" [ids e] returns the id of [e] and all its children ")
let ids: t => list<ID.t>

@ocaml.doc(" [children e] returns a list of all the direct children of [e] ")
let children: t => list<t>

@ocaml.doc(" [decendants e] returns a list of the IDs of all decendants (children,
 * grandchildren, etc) of [e] in an unspecified order ")
let decendants: t => list<ID.t>

let ancestors: (ID.t, t) => list<t>

@ocaml.doc(" [update f target ast] recursively searches [ast] for an expression e
    having an ID.t of [target].

    If found, replaces the expression with the result of [f e] and returns the new ast.
    If not found, will assertT before returning the unmodified [ast]. ")
let update: (~failIfMissing: bool=?, ~f: t => t, ID.t, t) => t

@ocaml.doc(
  " [replace replacement target ast] finds the expression with ID.t of [target] in the [ast] and replaces it with [replacement]. "
)
let replace: (~replacement: t, ID.t, t) => t

let removeVariableUse: (string, t) => t

let renameVariableUses: (~oldName: string, ~newName: string, t) => t

let updateVariableUses: (string, ~f: t => t, t) => t

let clone: t => t

@ocaml.doc(" [testEqualIgnoringIds a b] compares the structure and values of two ASTs,
  * ignoring the actual IDs of the expressions.
  *
  * NB: Only usable for tests right now. If you want to use for non-tests,
  * you'll need to complete the implementation and add tests ")
let testEqualIgnoringIds: (t, t) => bool

@ocaml.doc(" toHumanReadable returns a string of the expression tokenized into a
 * human-readable S-exp-like formatting that contains newlines and truncated
 * strings, which is meant to closely mirror the actual AST and aid in
 * debugging.
 *
 * Do not attempt to parse this or use it in comparisons of any sort, as its
 * format is not considered stable. Again, it is only for debugging with your
 * squishy human eyes. ")
let toHumanReadable: t => string
