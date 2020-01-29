let gid () : Base.Int63.t = Base.Int63.random Base.Int63.max_value

type jsonType = Yojson.Safe.t

module Rollbar = struct
  (* Do nothing for now *)
  let init _ = ()

  let send (msg : string) (url : string option) _custom : unit = ()
end

let reportError (msg : string) (msgVal : 'm) : unit =
  Rollbar.send msg None `Null
