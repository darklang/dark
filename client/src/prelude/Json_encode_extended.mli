(* This is designed to be used as Json.Encode in Prelude. Don't use it
 * directly unless you have to. *)

include module type of Json.Encode

val variant : string -> Js.Json.t list -> Js.Json.t

val tcStrSet : Tc.StrSet.t -> Js.Json.t

val tcStrDict : ('a -> Js.Json.t) -> 'a Tc.StrDict.t -> Js.Json.t
