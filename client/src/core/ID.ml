module T = struct
  type t = UnsharedTypes.id [@@deriving show]

  let toString (UnsharedTypes.ID str) = str

  let fromString str = UnsharedTypes.ID str

  let empty = UnsharedTypes.ID ""
end

include T
module Set = Tc.Set (T)
module Dict = Tc.Dict (T)
