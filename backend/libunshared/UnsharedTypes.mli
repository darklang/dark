module Int63 : sig
  type t = Core_kernel.Int63.t [@@deriving show {with_path = false}, eq, bin_io]

  include module type of Core_kernel.Int63 with type t := t

  val to_yojson : t -> Yojson.Safe.t

  val of_yojson : Yojson.Safe.t -> (t, string) result
end

type id = Int63.t [@@deriving show {with_path = false}, eq, yojson, bin_io]

type analysisID = id [@@deriving show {with_path = false}, eq, yojson]
