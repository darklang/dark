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
      with module Option := Tablecloth.Option
      (* with module String := Tablecloth.String *)
      with module Result := Tablecloth.Result
      with module List := Tablecloth.List )

module Result = struct
  include Tablecloth.Result

  let or_ (ra : ('a, 'b) t) (rb : ('a, 'b) t) : ('a, 'b) t =
    match ra with Error _ -> rb | Ok _ -> ra


  let orElse (ra : ('a, 'b) t) (rb : ('a, 'b) t) : ('a, 'b) t =
    match rb with Error _ -> ra | Ok _ -> rb


  let and_ (ra : ('a, 'b) t) (rb : ('a, 'b) t) : ('a, 'b) t =
    match (ra, rb) with
    | Ok a, Ok _ ->
        Ok a
    | Ok _, Error b ->
        Error b
    | Error a, Ok _ ->
        Error a
    | Error a, Error _ ->
        Error a
end

module List = struct
  include Tablecloth.List

  let findMap t ~f = Base.List.find_map t ~f

  let find_map = findMap
end

module Option = struct
  include Tablecloth.Option

  let exec ~(f : 'a -> unit) (v : 'a option) : unit =
    match v with Some v -> f v | None -> ()


  (* TODO: remove *)
  let valueExn (value : 'a option) : 'a =
    match value with Some v -> v | None -> raise Not_found


  let value_exn = valueExn

  let orLazy (v : 'a option) (v2 : unit -> 'a option) : 'a option =
    match v with Some v -> Some v | None -> v2 ()


  let or_else = orElse

  let orElseLazy (v : unit -> 'a option) (v2 : 'a option) : 'a option =
    match v2 with Some v2 -> Some v2 | None -> v ()


  let or_else_lazy = orElseLazy

  let pair (a : 'a option) (b : 'b option) : ('a * 'b) option =
    match (a, b) with Some a, Some b -> Some (a, b) | _ -> None


  let map2 (a : 'a option) (b : 'b option) ~(f : 'a -> 'b -> 'c) : 'c option =
    match (a, b) with Some a, Some b -> Some (f a b) | _ -> None


  let andThen2 (a : 'a option) (b : 'b option) ~(f : 'a -> 'b -> 'c option) :
      'c option =
    match (a, b) with Some a, Some b -> f a b | _ -> None


  let and_then2 = andThen2

  let isSomeEqualTo ~(value : 'a) (o : 'a option) : bool = Some value = o

  let is_some_equal_to = isSomeEqualTo

  (* If a is some, then apply fn to a, return both a and the result.
    if either a or b is none, then return none
  *)
  let thenAlso (a : 'a option) ~(f : 'a -> 'b option) : ('a * 'b) option =
    let b = andThen ~f a in
    pair a b


  let then_also = thenAlso
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


  let from_list_unique (l : (key * 'value) list) : (string, 'value t) Result.t =
    match Base.Map.of_alist (module Base.String) l with
    | `Duplicate_key k ->
        Result.fail ("Duplicate key: " ^ k)
    | `Ok dict ->
        Result.succeed dict


  let equal = Base.Map.equal

  let values = Base.Map.data

  let get ~(key : key) (dict : 'value t) = Base.Map.find dict key

  let filter l ~f = Base.Map.filter l ~f

  let insert ~(key : key) ~(value : 'value) (dict : 'value t) : 'value t =
    Base.Map.update dict key ~f:(fun _ -> value)


  let insert_no_override ~(key : key) ~(value : 'value) (dict : 'value t) :
      'value t =
    Base.Map.change dict key ~f:(fun old ->
        if old = None then Some value else old)


  let singleton k v = from_list [(k, v)]

  let is_empty = Base.Map.is_empty

  let foldl
      ~(init : 'a) ~(f : key:key -> value:'value -> 'a -> 'a) (dict : 'value t)
      : 'a =
    Base.Map.fold ~init ~f:(fun ~key ~data -> f ~key ~value:data) dict


  let mapi ~(f : key:key -> value:'value -> 'a) (dict : 'value t) : 'value t =
    Base.Map.mapi ~f:(fun ~key ~data -> f ~key ~value:data) dict
end
