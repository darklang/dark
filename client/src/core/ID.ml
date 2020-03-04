module T = struct
  type t = UnsharedTypes.id [@@deriving show]

  let toString (UnsharedTypes.ID str) = str

  let fromString str = UnsharedTypes.ID str
end

include T
module Set = Tc.Set (T)
