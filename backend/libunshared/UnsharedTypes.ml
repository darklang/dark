type int63t = Core_kernel.Int63.t [@@deriving show, eq, ord, bin_io]

module Int63 = struct
  type t = int63t [@@deriving show, eq, ord, bin_io]

  include (Core_kernel.Int63 : module type of Core_kernel.Int63 with type t := t)

  let to_yojson (i : t) : Yojson.Safe.t =
    match to_int i with Some i -> `Int i | None -> `Intlit (to_string i)


  let of_yojson (json : Yojson.Safe.t) : (t, string) result =
    match json with
    | `Int i ->
        Ok (of_int i)
    | `Intlit i | `String i ->
        Ok (of_string i)
    | _ ->
        Error "Int63.t of_yojson"
end

type id = Int63.t [@@deriving show {with_path = false}, eq, ord, yojson, bin_io]

type analysisID = id
[@@deriving show {with_path = false}, eq, ord, yojson, bin_io]
