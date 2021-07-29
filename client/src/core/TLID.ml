module T = struct
  type t = TLID of string [@@ppx.deriving show {with_path = false}]

  let toString (TLID str) = str

  let fromString str = TLID str

  let empty = TLID ""
end

include T
module Set = Tc.Set (T)

module Dict = struct
  include Tc.Dict (T)

  (* TODO: convert the tlid key back to being called key *)
  let get ~(tlid : T.t) (dict : 'a t) : 'a option = get ~key:tlid dict

  let insert ~(tlid : T.t) ~(value : 'a) (dict : 'a t) : 'a t =
    insert ~key:tlid ~value dict


  let tlids (dict : 'a t) : T.t list = dict |> keys

  let updateIfPresent ~(tlid : T.t) ~(f : 'v -> 'v) (dict : 'a t) : 'a t =
    updateIfPresent ~key:tlid ~f dict


  let update ~(tlid : T.t) ~(f : 'v option -> 'v option) (dict : 'a t) : 'a t =
    update ~key:tlid ~f dict


  let remove ~(tlid : T.t) (dict : 'a t) : 'a t = remove ~key:tlid dict

  let removeMany ~(tlids : T.t list) (dict : 'a t) : 'a t =
    removeMany ~keys:tlids dict
end
