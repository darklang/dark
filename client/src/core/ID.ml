module T = struct
  module Nested = struct
    type t = UnsharedTypes.id [@@ppx.deriving show, ord]
  end

  let fromString (id : string) = UnsharedTypes.ID id

  let toString (UnsharedTypes.ID id : UnsharedTypes.id) = id

  include Nested
  include Tablecloth.Comparator.Make (Nested)
end

include T

module Set = struct
  include Tc.Set.Of (T)

  let pp = Tc.Set.pp T.pp

  let fromArray a = Tc.Set.fromArray (module T) a

  let empty = Tc.Set.empty (module T)

  let singleton value = Tc.Set.singleton (module T) value

  let fromList l = Tc.Set.fromList (module T) l
end

module Map = struct
  include Tc.Map.Of (T)

  let pp
      (valueFormatter : Format.formatter -> 'value -> unit)
      (fmt : Format.formatter)
      (map : 'value t) : unit =
    Tc.Map.pp T.pp valueFormatter fmt map


  let fromArray a = Tablecloth.Map.Poly.fromArray a |. Obj.magic

  let empty = fromArray [||]

  let singleton ~key ~value = Tc.Map.singleton (module T) ~key ~value

  let fromList l = Tc.Map.fromList (module T) l
end
