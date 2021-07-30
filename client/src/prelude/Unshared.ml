let gid () = Js_math.random_int 0 2147483647 |> string_of_int |> ID.fromString

type jsonType = Js.Json.t

module Rollbar = struct
  type rollbar_mod = < init : Js.Json.t -> unit [@bs.meth] > Js.t

  type rollbar_instance =
    < error :
           string
        -> string Js.nullable
        -> int Js.null
        -> string Js.null
        -> Js.Json.t
        -> unit [@bs.meth]
    ; critical :
           string
        -> string Js.nullable
        -> int Js.null
        -> string Js.null
        -> Js.Json.t
        -> unit [@bs.meth] >
    Js.t

  external rollbar : rollbar_mod = "rollbar" [@@bs.module]

  let init (rollbarConfig : Js.Json.t) =
    rollbar##init rollbarConfig ;
    Js.log "Inited rollbar"


  let send (msg : string) (url : string option) (custom : Js.Json.t) : unit =
    let url = Js.Nullable.fromOption url in
    let (rb : rollbar_instance) =
      (* There's a better way of doing this but I couldn't get it to work:
     * https://bucklescript.github.io/docs/en/embed-raw-javascript#detect-global-variables
     * *)
      [%raw
        {| (typeof window === 'undefined') ? self.rollbar : window.rollbar |}]
    in
    (* Note that this prints an exception in test as the rollbar field doesn't
     * exist. *)
    rb##error msg url Js.null Js.null custom
end

let reportError (msg : string) (msgVal : 'm) : unit =
  Js.log3 "An unexpected but recoverable error happened: " msg msgVal ;
  Js.Console.trace () ;
  Rollbar.send
    msg
    None
    (* It seems ridiculous to convert to JSON strings, and then parse, to
     * get the right type to send through, but I can't figure out a
     * different way to do it. *)
    ( msgVal
    |> Js.Json.stringifyAny
    |> Tc.Option.map ~f:Js.Json.parseExn
    |> Tc.Option.unwrap ~default:Js.Json.null ) ;
  ()
