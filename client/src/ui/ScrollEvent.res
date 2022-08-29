// TODO: can this be replaced with a type from a library?

@ppx.deriving(show({with_path: false}))
type rec t = {timestamp: float}

let decode = (j: Js.Json.t): t => {
  open Json.Decode
  {
    timestamp: field("timeStamp", Json.Decode.float, j),
  }
}
