module T = struct
  module Nested = struct
    type t = TLID of string [@@ppx.deriving show {with_path = false}]
    let compare (TLID id1 : t) (TLID id2 : t) =
      compare id1 id2
  end

  include Nested
  include Tablecloth.Comparator.Make (Nested)

  let fromString (tlid : string) = TLID tlid

  let toString (TLID tlid : t) = tlid
end

include T

module Set = struct
  include Tc.Set.Of (T)

  let pp = Tc.Set.pp T.pp

  let fromArray a = Tablecloth.Set.Poly.fromArray a |. Obj.magic

  let empty = fromArray [||]

  let singleton value = fromArray [|value|]

  let fromList l = fromArray (Array.of_list l)
end

(* CLEANUP: rename to map *)
module Dict = struct
  include Tc.Map.Of (T)

  let pp
      (valueFormatter : Format.formatter -> 'value -> unit)
      (fmt : Format.formatter)
      (map : 'value t) : unit =
    Tc.Map.pp T.pp valueFormatter fmt map


  let fromArray a = Tablecloth.Map.Poly.fromArray a |. Obj.magic

  let empty = fromArray [||]

  let singleton ~key ~value = fromArray [|(key, value)|]

  let fromList l = fromArray (Array.of_list l)
end
