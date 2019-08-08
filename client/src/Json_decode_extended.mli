include module type of Json.Decode

val variant4 :
     ('b -> 'c -> 'd -> 'e -> 'a)
  -> 'b decoder
  -> 'c decoder
  -> 'd decoder
  -> 'e decoder
  -> 'a decoder

val variant3 :
     ('b -> 'c -> 'd -> 'a)
  -> 'b decoder
  -> 'c decoder
  -> 'd decoder
  -> 'a decoder

val variant2 : ('b -> 'c -> 'a) -> 'b decoder -> 'c decoder -> 'a decoder

val variant1 : ('b -> 'a) -> 'b decoder -> 'a decoder

val variant0 : 'a -> 'a decoder

val variants : (string * 'a decoder) list -> 'a decoder

val succeed : 'a -> 'a decoder

val index : int -> 'a decoder -> 'a decoder

val dict : 'a decoder -> 'a Tc.StrDict.t decoder

val tcStrSet : Tc.StrSet.t decoder

val decodeString : 'a decoder -> string -> (string, 'a) Tc.Result.t
