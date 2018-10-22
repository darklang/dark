include Json.Encode

let variant constructor vals =
  jsonArray (Array.of_list ((string constructor) :: vals))
