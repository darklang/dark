type id = ID.t
type tlid = TLID.t

module Pattern = {
  @ppx.deriving(show({with_path: false}))
  type rec t =
    // match id, then pattern id
    | FPVariable(ID.t, ID.t, string)
    | FPConstructor(ID.t, ID.t, string, list<t>)
    // TODO: support char
    // Currently we support u62s; we will support s63s. ints in Bucklescript only support 32 bit ints but we want 63 bit int support
    | FPInteger(ID.t, ID.t, string)
    | FPBool(ID.t, ID.t, bool)
    | FPString({matchID: ID.t, patternID: ID.t, str: string})
    | FPFloat(ID.t, ID.t, string, string)
    | FPNull(ID.t, ID.t)
    | FPBlank(ID.t, ID.t)
}

module Expr = {
  @ppx.deriving(show({with_path: false}))
  type rec sendToRail =
    | Rail
    | NoRail

  // CLEANUP: move comments to LibExecution.ProgramTypes
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
    | EMatch(ID.t, t, list<(Pattern.t, t)>)
    /* Placeholder that indicates the target of the Thread. May be movable at
     * some point */
    | EPipeTarget(ID.t)
    /* EFeatureFlag: ID.t, flagName, condExpr, caseAExpr, caseBExpr */
    | EFeatureFlag(ID.t, string, t, t, t)
}

module DType = {
  @ppx.deriving(show({with_path: false}))
  type rec t =
    | TInt
    | TStr
    | TCharacter
    | TBool
    | TFloat
    | TObj
    | TList
    | TAny
    | TNull
    | TBlock
    | TIncomplete
    | TError
    | TResp
    | TDB
    | TDate
    | TPassword
    | TUuid
    | TOption
    | TErrorRail
    | TResult
    | TDbList(t)
    | TUserType(string, int)
    | TBytes
}

module AST = {
  @ppx.deriving(show({with_path: false}))
  type rec t = Root(Expr.t)
}
