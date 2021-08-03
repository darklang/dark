let gid: unit => ID.t

type jsonType = Js.Json.t

module Rollbar: {
  let init: jsonType => unit

  let send: (string, option<string>, jsonType) => unit
}

let reportError: (string, 'm) => unit
