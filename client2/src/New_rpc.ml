open! Porting

module Decoders = struct
  let rec tipe json =
    let open Json_decode_extended in
    let dv0 = decodeVariant0 in
    let dv1 = decodeVariant1 in
    let dv2 = decodeVariant2 in
    decodeVariants [
      ("TInt", dv0 Types.tint)
    ]
end

module Encoders = struct
end
