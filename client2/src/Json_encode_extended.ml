include Json.Encode

let encodeVariant constructor vals =
  jsonArray (Array.of_list ((string constructor) :: vals))
