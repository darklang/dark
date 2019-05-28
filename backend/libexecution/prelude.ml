(* This file is a local variant of Tablecloth. It allows us to add, modify,
 * etc, tablecloth function and type definitions, to be upstreamed later. *)
module Caml = struct
  module String = String
  module List = List
  module Char = Char
  module Array = Array
end

(* This allows us override submodules. See
 * http://gallium.inria.fr/blog/overriding-submodules/ *)
include (
  Tablecloth :
    module type of Tablecloth
    (* with module StrSet := Tablecloth.StrSet *)
    (*  and module IntSet := Tablecloth.IntSet *)
    with module StrDict := Tablecloth.StrDict
    (* with module Option := Tablecloth.Option *)
    (* with module String := Tablecloth.String *)
     and module Result := Tablecloth.Result
    (* and module List := Tablecloth.List  *) )

module Result = struct
  include Tablecloth.Result

  let ok_or_internal_exception (msg : string) (t : (string, 'a) t) : 'a =
    match t with
    | Ok a ->
        a
    | Error err ->
        Exception.internal ~info:[("error", err)] msg
end

module StrDict = struct
  include Tablecloth.StrDict
  module XMap = Base.Map.M (Base.String)

  let iter ~(f : key -> 'value -> unit) (dict : 'value t) : unit =
    Base.Map.iteri ~f:(fun ~key ~data -> f key data) dict


  let to_list (dict : 'value t) : (key * 'value) list = Base.Map.to_alist dict

  let compare
      ~(f : 'value -> 'value -> int) (dict1 : 'value t) (dict2 : 'value t) =
    Base.Map.compare_direct f dict1 dict2


  (* This is for when you believe there should never be duplicate
     * values, and want an exception to prove this invariant. *)
  let from_list_exn (l : (key * 'value) list) : 'value t =
    Base.Map.of_alist_exn (module Base.String) l


  let from_list_unique (l : (key * 'value) list) : (string, 'value t) Result.t
      =
    match Base.Map.of_alist (module Base.String) l with
    | `Duplicate_key k ->
        Result.fail ("Duplicate key: " ^ k)
    | `Ok dict ->
        Result.succeed dict


  let equal = Base.Map.equal

  let values = Base.Map.data

  let get = Base.Map.find

  let filter l ~f = Base.Map.filter l ~f

  let insert ~(key : key) ~(value : 'value) (dict : 'value t) : 'value t =
    Base.Map.update dict key ~f:(fun _ -> value)


  let insert_no_override ~(key : key) ~(value : 'value) (dict : 'value t) :
      'value t =
    Base.Map.change dict key ~f:(fun old ->
        if old = None then Some value else old )


  let singleton k v = from_list [(k, v)]

  let is_empty = Base.Map.is_empty

  let foldl
      ~(init : 'a) ~(f : key:key -> value:'value -> 'a -> 'a) (dict : 'value t)
      : 'a =
    Base.Map.fold ~init ~f:(fun ~key ~data -> f ~key ~value:data) dict


  let mapi ~(f : key:key -> value:'value -> 'a) (dict : 'value t) : 'value t =
    Base.Map.mapi ~f:(fun ~key ~data -> f ~key ~value:data) dict
end
