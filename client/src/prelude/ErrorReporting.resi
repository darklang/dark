module Rollbar: {
  let init: Js.Json.t => unit

  let send: (string, option<string>, Js.Json.t) => unit
}

let reportError: (string, 'm) => unit
