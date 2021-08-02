(* This is designed to be used as Json.Decode in prelude. Don't use it
 * directly unless you have to. *)

include module type of Json.Decode

val variant5 :
     ('b -> 'c -> 'd -> 'e -> 'f -> 'a)
  -> 'b decoder
  -> 'c decoder
  -> 'd decoder
  -> 'e decoder
  -> 'f decoder
  -> 'a decoder

val variant4 :
     ('b -> 'c -> 'd -> 'e -> 'a)
  -> 'b decoder
  -> 'c decoder
  -> 'd decoder
  -> 'e decoder
  -> 'a decoder

val variant3 :
  ('b -> 'c -> 'd -> 'a) -> 'b decoder -> 'c decoder -> 'd decoder -> 'a decoder

val variant2 : ('b -> 'c -> 'a) -> 'b decoder -> 'c decoder -> 'a decoder

val variant1 : ('b -> 'a) -> 'b decoder -> 'a decoder

val variant0 : 'a -> 'a decoder

(* For variants with record types, provide a constructor and three fields with
 * the field types, and decode them. For example, for the type:
 * FPString { matchID: id; patternID : id; str : string }
 *
 * the decoder would be
 *
 * recordVariant3
 *    (fun matchID patternID str -> P.FPString {matchID; patternID; str})
 *    ("matchID", id)
 *    ("patternID", id)
 *    ("str", string)
 *)
val recordVariant3 :
     ('b -> 'c -> 'd -> 'a)
  -> string * 'b decoder
  -> string * 'c decoder
  -> string * 'd decoder
  -> 'a decoder

val variants : (string * 'a decoder) list -> 'a decoder

val result : 'ok decoder -> 'error decoder -> ('ok, 'error) result decoder

val succeed : 'a -> 'a decoder

val index : int -> 'a decoder -> 'a decoder

val tryDecode2 : 'a decoder -> 'a decoder -> 'a decoder

val strDict : 'a decoder -> 'a Tc.Map.String.t decoder

val beltStrDict : 'a decoder -> 'a Belt.Map.String.t decoder

val strSet : Tc.Set.String.t decoder

val decodeString : 'a decoder -> string -> ('a, string) Tc.Result.t
