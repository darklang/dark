module Rollbar = {
  type rollbar_mod = {@meth "init": Js.Json.t => unit}

  type rollbar_instance = {
    @meth
    "error": (string, Js.nullable<string>, Js.null<int>, Js.null<string>, Js.Json.t) => unit,
    @meth
    "critical": (string, Js.nullable<string>, Js.null<int>, Js.null<string>, Js.Json.t) => unit,
  }

  @module external rollbar: rollbar_mod = "rollbar"

  let init = (rollbarConfig: Js.Json.t) => {
    rollbar["init"](rollbarConfig)
    Js.log("Inited rollbar")
  }

  let send = (msg: string, url: option<string>, custom: Js.Json.t): unit => {
    let url = Js.Nullable.fromOption(url)
    let rb: rollbar_instance = /* There's a better way of doing this but I couldn't get it to work:
     * https://bucklescript.github.io/docs/en/embed-raw-javascript#detect-global-variables
     * */
    %raw(` (typeof window === 'undefined') ? self.rollbar : window.rollbar `)

    /* Note that this prints an exception in test as the rollbar field doesn't
     * exist. */
    rb["error"](msg, url, Js.null, Js.null, custom)
  }
}

let reportError = (msg: string, msgVal: 'm): unit => {
  Js.Console.error3("An unexpected but recoverable error happened: ", msg, msgVal)
  Js.Console.trace()
  Rollbar.send(
    msg,
    None,
    /* It seems ridiculous to convert to JSON strings, and then parse, to
     * get the right type to send through, but I can't figure out a
     * different way to do it. */
    msgVal
    |> Js.Json.stringifyAny
    |> Tc.Option.map(~f=Js.Json.parseExn)
    |> Tc.Option.unwrap(~default=Js.Json.null),
  )
  ()
}
