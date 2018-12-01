(* We don't have an internal standard library, but should. This file it
 * potentially the start of one. I named it GMap because it needs some prefix
 * (not D!), and G is a letter. *)

module String = struct
  include Belt.Map.String
  let pp
      (_: Format.formatter -> 'value -> unit)
      (_: Format.formatter)
      (_: 'value t)
    =
    ()
end

module Int = struct
  include Belt.Map.Int
  let pp
      (_: Format.formatter -> 'value -> unit)
      (_: Format.formatter)
      (_: 'value t)
    =
    ()
end

