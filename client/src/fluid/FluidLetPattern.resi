type t = ProgramTypes.LetPattern.t

let toID: t => ID.t

/* Returns the ids of all the patterns in this pattern. Includes this pattern's
 * ID, does not include the letID */
let ids: t => list<ID.t>

let clone: t => t

let variableNames: t => list<string>

let hasVariableNamed: (string, t) => bool

@ocaml.doc("[findLetPattern patID within] returns Some pattern
  with ID.t = [patID] in the [within] tree, or None.")
let findLetPattern: (ID.t, t) => option<t>

@ocaml.doc("[isPatternBlank e] returns true iff [e] is an LPBlank.")
let isBlank: t => bool

@ocaml.doc("[preTraversal f pattern] walks the entire pattern from top to bottom,
  * calling f on each pattern. It returns a new patterm with every subpattern p
  * replaced by [f p]. After calling [f], the result is then recursed into; if
  * this isn't what you want call postTraversal.")
let preTraversal: (~f: t => t, t) => t

@ocaml.doc("[postTraversal f pattern] walks the entire pattern from bottom to top,
  * calling f on each pattern. It returns a new pattern with every subpattern p
  * replaced by [f p]. After calling [f], the result is NOT recursed into; if
  * this isn't what you want call preTraversal.")
let postTraversal: (~f: t => t, t) => t
