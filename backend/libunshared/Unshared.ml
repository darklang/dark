(* can't use 52 bit interval as js_of_ocaml complained, so just use largest 31
 * bit for now. *)
let max = Base.Int63.of_int 2147483647

let gid () : Base.Int63.t = Base.Int63.random max

type jsonType = Yojson.Safe.t

module Rollbar = struct
  (* Do nothing for now *)
  let init _ = ()

  let send (msg : string) (url : string option) _custom : unit = ()
end

let reportError (msg : string) (msgVal : 'm) : unit =
  Rollbar.send msg None `Null
