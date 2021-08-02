(* This is designed to be used as Json.Encode in Prelude. Don't use it
 * directly unless you have to. *)

include module type of Json.Encode

val variant : string -> Js.Json.t list -> Js.Json.t

(* CLEANUP : change name of function *)
val tcStrSet : Tc.Set.String.t -> Js.Json.t

(* CLEANUP : change name of function *)
val tcStrDict : ('a -> Js.Json.t) -> 'a Tc.Map.String.t -> Js.Json.t

val beltStrDict : ('a -> Js.Json.t) -> 'a Belt.Map.String.t -> Js.Json.t
