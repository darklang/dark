module T = struct
  type t = TLID of string [@@deriving show]

  let toString (TLID str) = str

  let fromString str = TLID str
end

include T
module Set = Tc.Set (T)

module Dict = struct
  include Tc.Dict (T)

  (* TODO: convert the tlid key back to being called key *)
  let get ~(tlid : T.t) (dict : 'value t) : 'value option = get ~key:tlid dict

  let insert ~(tlid : T.t) ~(value : 'value) (dict : 'value t) : 'value t =
    insert ~key:tlid ~value dict


  let tlids (dict : 'value t) : T.t list = dict |> keys

  let updateIfPresent ~(tlid : T.t) ~(f : 'v -> 'v) (dict : 'value t) : 'value t
      =
    updateIfPresent ~key:tlid ~f dict


  let update ~(tlid : T.t) ~(f : 'v option -> 'v option) (dict : 'value t) :
      'value t =
    update ~key:tlid ~f dict


  let remove ~(tlid : T.t) (dict : 'value t) : 'value t = remove ~key:tlid dict

  let removeMany ~(tlids : T.t list) (dict : 'value t) : 'value t =
    removeMany ~keys:tlids dict
end
