type rollbar_mod = < init : Js.Json.t -> unit [@bs.meth] > Js.t

type rollbar_instance =
  < error :
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
  rb##error msg url Js.null Js.null custom
