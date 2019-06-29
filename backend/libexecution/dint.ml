module Int63 = Prelude.Int63

type t = Int63.t [@@deriving compare, eq, yojson, show]

let of_int = Int63.of_int

let to_int = Int63.to_int

let to_int_exn = Int63.to_int_exn

let of_string_exn = Int63.of_string

let to_string = Int63.to_string

let to_float = Int63.to_float

let of_float = Int63.of_float

let random i = Int63.random i

let ( % ) = Int63.( % )

let ( + ) = Int63.( + )

let ( - ) = Int63.( - )

let ( / ) = Int63.( / )

let ( * ) = Int63.( * )

let pow = Int63.pow

let one = Int63.one

let zero = Int63.zero

let init () =
  (* TODO: init random *)
  ()
