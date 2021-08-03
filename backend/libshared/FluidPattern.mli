type t =
  | FPVariable of Shared.id * Shared.id * string
  | FPConstructor of Shared.id * Shared.id * string * t list
  (* TODO: support char *)
  (* Currently we support u62s; we will support s63s. ints in Bucklescript only support 32 bit ints but we want 63 bit int support *)
  | FPInteger of Shared.id * Shared.id * string
  | FPBool of Shared.id * Shared.id * bool
  | FPString of
      { matchID : Shared.id
      ; patternID : Shared.id
      ; str : string }
  | FPFloat of Shared.id * Shared.id * string * string
  | FPNull of Shared.id * Shared.id
  | FPBlank of Shared.id * Shared.id
[@@deriving show {with_path = false}, eq, ord, yojson {optional = true}]

val toID : t -> Shared.id

(* Returns the ids of all the patterns in this pattern. Includes this pattern's
 * ID, does not include the matchID *)
val ids : t -> Shared.id list

val toMatchID : t -> Shared.id

val clone : Shared.id -> t -> t

val variableNames : t -> string list

val hasVariableNamed : string -> t -> bool

(** [findPattern patID within] returns Some pattern
  with Shared.id = [patID] in the [within] tree, or None. *)
val findPattern : Shared.id -> t -> t option

(** [preTraversal f pattern] walks the entire pattern from top to bottom,
  * calling f on each pattern. It returns a new patterm with every subpattern p
  * replaced by [f p]. After calling [f], the result is then recursed into; if
  * this isn't what you want call postTraversal. *)
val preTraversal : f:(t -> t) -> t -> t

(** [postTraversal f pattern] walks the entire pattern from bottom to top,
  * calling f on each pattern. It returns a new pattern with every subpattern p
  * replaced by [f p]. After calling [f], the result is NOT recursed into; if
  * this isn't what you want call preTraversal. *)
val postTraversal : f:(t -> t) -> t -> t
