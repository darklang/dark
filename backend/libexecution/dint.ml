module Int63 = Prelude.Int63

type t = Int63.t [@@deriving compare, eq, yojson, show]

let of_int = Int63.of_int

let to_int = Int63.to_int

let to_int63 t = t

let of_int63 t = t

let to_int_exn = Int63.to_int_exn

let of_string_exn = Int63.of_string

let to_string = Int63.to_string

let to_float = Int63.to_float

let of_float = Int63.of_float

let random i = Int63.random i

(* This is NOT the same as Javascript's % *)
let modulo = Int63.( % )

let rem = Int63.rem

let ( + ) = Int63.( + )

let ( - ) = Int63.( - )

let ( / ) = Int63.( / )

let ( * ) = Int63.( * )

let pow = Int63.pow

let abs = Int63.abs

let negate = Int63.neg

let one = Int63.one

let zero = Int63.zero

let max = Int63.max

let min = Int63.min

let clamp = Int63.clamp

let init () =
  (* TODO: init random *)
  ()
