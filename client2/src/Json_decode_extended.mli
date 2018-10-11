include (module type of Json.Decode)

val decodeVariant4 : ('b -> 'c -> 'd -> 'e -> 'a) -> 'b decoder -> 'c decoder -> 'd decoder -> 'e decoder -> 'a decoder
val decodeVariant3 : ('b -> 'c -> 'd -> 'a) -> 'b decoder -> 'c decoder -> 'd decoder -> 'a decoder
val decodeVariant2 : ('b -> 'c -> 'a) -> 'b decoder -> 'c decoder -> 'a decoder
val decodeVariant1 : ('b -> 'a) -> 'b decoder -> 'a decoder
val decodeVariant0 : 'a -> 'a decoder

val decodeVariants : ((string * 'a decoder) list) -> 'a decoder

val succeed : 'a -> 'a decoder
val index : int -> 'a decoder -> 'a decoder

