module T = struct
  type t = UnsharedTypes.id [@@ppx.deriving show]

  let toString (UnsharedTypes.ID str) = str

  let fromString str = UnsharedTypes.ID str

  let empty = UnsharedTypes.ID ""
end

include T
module Set = Tc.Set (T)
