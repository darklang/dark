@ppx.deriving(show({with_path: false}))
type rec t =
  | FPVariable(Shared.id, Shared.id, string)
  | FPConstructor(Shared.id, Shared.id, string, list<t>)
  /* TODO: support char */
  /* Currently we support u62s; we will support s63s. ints in Bucklescript only support 32 bit ints but we want 63 bit int support */
  | FPInteger(Shared.id, Shared.id, string)
  | FPBool(Shared.id, Shared.id, bool)
  | FPString({matchID: Shared.id, patternID: Shared.id, str: string})
  | FPFloat(Shared.id, Shared.id, string, string)
  | FPNull(Shared.id, Shared.id)
  | FPBlank(Shared.id, Shared.id)

let toID: t => Shared.id

/* Returns the ids of all the patterns in this pattern. Includes this pattern's
 * ID, does not include the matchID */
let ids: t => list<Shared.id>

let toMatchID: t => Shared.id

let clone: (Shared.id, t) => t

let variableNames: t => list<string>

let hasVariableNamed: (string, t) => bool

@ocaml.doc(" [findPattern patID within] returns Some pattern
  with Shared.id = [patID] in the [within] tree, or None. ")
let findPattern: (Shared.id, t) => option<t>

@ocaml.doc(" [preTraversal f pattern] walks the entire pattern from top to bottom,
  * calling f on each pattern. It returns a new patterm with every subpattern p
  * replaced by [f p]. After calling [f], the result is then recursed into; if
  * this isn't what you want call postTraversal. ")
let preTraversal: (~f: t => t, t) => t

@ocaml.doc(" [postTraversal f pattern] walks the entire pattern from bottom to top,
  * calling f on each pattern. It returns a new pattern with every subpattern p
  * replaced by [f p]. After calling [f], the result is NOT recursed into; if
  * this isn't what you want call preTraversal. ")
let postTraversal: (~f: t => t, t) => t
