type t = Types.functionsType [@@ppx.deriving show {with_path = false}]

type props = Types.functionsProps [@@ppx.deriving show {with_path = false}]

(* Returns the function named `name`. Returns Nothing if the function
  * can't be found - this shouldn't happen in theory but often does
  * in practice; for example, someone might delete a function and
  * then do a local undo. *)
val find : string -> t -> Types.function_ option

(* For legacy reasons, we stash a global reference to the result of the latest
 * update. This was only to be used by OldExpr, but it snuck in elsewhere. DO
 * NOT USE! *)
val global : unit -> t

val empty : t

val asFunctions : t -> Types.function_ list

val builtins : t -> Types.function_ list

val setBuiltins : Types.function_ list -> props -> t -> t

val setPackages : Types.packageFns -> props -> t -> t

(* Update the cached function data; should be called anytime the functions
 * change. *)
val update : props -> t -> t

(* For testing *)
val testCalculateUnsafeUserFunctions : props -> t -> Tc.Set.String.t
