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
    (* with module IntSet := Tablecloth.IntSet *)
    with module StrDict := Tablecloth.StrDict
    with module Option := Tablecloth.Option
    with module Int := Tablecloth.Int
    with module String := Tablecloth.String
    (*  and module Result := Tablecloth.Result *)
     and module List := Tablecloth.List )

module Option = struct
  include Tablecloth.Option

  let exec ~(f : 'a -> unit) (v : 'a option) : unit =
    match v with Some v -> f v | None -> ()


  let valueExn (value : 'a option) : 'a =
    match value with Some v -> v | None -> raise Not_found


  let orLazy (v : 'a option) (v2 : unit -> 'a option) : 'a option =
    match v with Some v -> Some v | None -> v2 ()


  let orElseLazy (v : unit -> 'a option) (v2 : 'a option) : 'a option =
    match v2 with Some v2 -> Some v2 | None -> v ()
end

module List = struct
  include Tablecloth.List

  let rec dropLeft ~(count : int) (l : 'a list) : 'a list =
    if count <= 0
    then l
    else
      match l with
      | [] ->
          []
      | [_] ->
          []
      | _ :: rest ->
          dropLeft ~count:(count - 1) rest


  let dropRight ~(count : int) (l : 'a list) : 'a list =
    l |> reverse |> dropLeft ~count |> reverse


  let range (start : int) (end_ : int) : 'a list =
    let length = end_ - start in
    if length < 0 then [] else Belt.List.makeBy length (fun i -> i + start)
end

module Float = struct
  let toString (f : float) : string = Js.Float.toString f
end

module Int = struct
  include Tablecloth.Int

  let toString (i : int) : string = Js.Int.toString i
end

module String = struct
  include Tablecloth.String

  let splitAt ~(index : int) (s : string) : string * string =
    (slice ~from:0 ~to_:index s, slice ~from:index ~to_:(length s) s)


  let left ~(count : int) (s : string) : string = slice ~from:0 ~to_:count s
end

module StrDict = struct
  include Tablecloth.StrDict

  let values (dict : 'a t) : 'a list =
    Belt.Map.String.valuesToArray dict |> Array.toList


  let updateIfPresent ~(key : key) ~(f : 'v -> 'v) (dict : 'value t) : 'value t
      =
    update ~key ~f:(Option.map ~f) dict


  let mergeLeft (dict1 : 'v t) (dict2 : 'v t) : 'v t =
    Tablecloth.StrDict.merge
      ~f:(fun (_key : string) (v1 : 'v option) (v2 : 'v option) ->
        match (v1, v2) with Some _, _ -> v1 | None, _ -> v2 )
      dict1
      dict2


  let mergeRight (dict1 : 'v t) (dict2 : 'v t) : 'v t =
    Tablecloth.StrDict.merge
      ~f:(fun (_key : string) (v1 : 'v option) (v2 : 'v option) ->
        match (v1, v2) with _, Some _ -> v2 | _, None -> v1 )
      dict1
      dict2


  let count (dict : 'v t) = Belt.Map.String.size dict

  let mapValues (dict : 'v t) ~(f : 'v -> 'x) : 'x list =
    dict |> values |> List.map ~f


  let filterMapValues (dict : 'v t) ~(f : 'v -> 'x option) : 'x list =
    dict |> values |> List.filterMap ~f


  let remove (dict : 'v t) ~(key : key) : 'v t =
    Belt.Map.String.remove dict key


  let removeMany (dict : 'v t) ~(keys : key list) : 'v t =
    Belt.Map.String.removeMany dict (Belt.List.toArray keys)
end
