// A TraceID is the identifier of a 'trace' - the recording of some code being
// evaluated, incl. input and output

@ppx.deriving(show({with_path: false}))
type rec t = string

let encode = Json.Encode.string

let decode = Json.Decode.string
