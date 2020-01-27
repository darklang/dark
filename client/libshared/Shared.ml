module UnsharedTypes = struct
  type id = ID of string [@@deriving show {with_path = false}]

  type analysisID = id [@@deriving show {with_path = false}]
end

module Unshared = struct
  let gid () =
    UnsharedTypes.ID (Js_math.random_int 0 2147483647 |> string_of_int)
end

type id = UnsharedTypes.id [@@deriving show {with_path = false}]

type analysisID = id [@@deriving show {with_path = false}]

let gid = Unshared.gid
