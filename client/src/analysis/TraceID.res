@ppx.deriving(show({with_path: false}))
type rec t = string

let encode = Json.Encode.string

let decode = Json.Decode.string
