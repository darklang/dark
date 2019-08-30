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
    with module StrSet := Tablecloth.StrSet
    (* with module IntSet := Tablecloth.IntSet *)
    with module StrDict := Tablecloth.StrDict
    with module Option := Tablecloth.Option
    with module Int := Tablecloth.Int
    with module String := Tablecloth.String
     and module Result := Tablecloth.Result
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

module Result = struct
  include Tablecloth.Result

  let isOk (v : ('err, 'ok) t) : bool =
    match v with Ok _ -> true | _ -> false
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

  let rec segment ~(size : int) (s : string) : string list =
    let front, back = splitAt ~index:size s in
    if back = "" then [front] else front :: segment ~size back


  let replaceChunk ~(from : int) ~(to_ : int) ~(replacement : string) s :
      string =
    slice ~from:0 ~to_:from s ^ replacement ^ slice ~from:to_ ~to_:(length s) s
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


  let mapWithKey (dict : 'v t) ~(f : key:string -> 'v -> 'x) : 'x t =
    Belt.Map.String.mapWithKey dict (fun key v -> f ~key v)


  let remove (dict : 'v t) ~(key : key) : 'v t =
    Belt.Map.String.remove dict key


  let removeMany (dict : 'v t) ~(keys : key list) : 'v t =
    Belt.Map.String.removeMany dict (Belt.List.toArray keys)


  let singleton ~(key : key) ~(value : 'v) : 'v t = empty |> insert ~key ~value
end

module type Key = sig
  type t

  val toString : t -> string

  val fromString : string -> t
end

module Dict (K : Key) = struct
  (* We don't include StrDict as forgetting to wrap a function gives
   * hard-to-decipher error messages *)
  type 'value t = 'value StrDict.t

  let empty = StrDict.empty

  let mergeLeft = StrDict.mergeLeft

  let mergeRight = StrDict.mergeRight

  let map = StrDict.map

  let filterMapValues = StrDict.filterMapValues

  let mapValues = StrDict.mapValues

  let values = StrDict.values

  let get ~(key : K.t) (dict : 'value t) : 'value option =
    StrDict.get ~key:(K.toString key) dict


  let insert ~(key : K.t) ~(value : 'value) (dict : 'value t) : 'value t =
    StrDict.insert ~key:(K.toString key) ~value dict


  let keys (dict : 'value t) : K.t list =
    dict |> StrDict.keys |> List.map ~f:K.fromString


  let updateIfPresent ~(key : K.t) ~(f : 'v -> 'v) (dict : 'value t) : 'value t
      =
    StrDict.updateIfPresent ~key:(K.toString key) ~f dict


  let update ~(key : K.t) ~(f : 'v option -> 'v option) (dict : 'value t) :
      'value t =
    StrDict.update ~key:(K.toString key) ~f dict


  let remove ~(key : K.t) (dict : 'value t) : 'value t =
    StrDict.remove ~key:(K.toString key) dict


  let removeMany ~(keys : K.t list) (dict : 'value t) : 'value t =
    let keys = List.map keys ~f:K.toString in
    StrDict.removeMany ~keys dict


  let fromList (values : (K.t * 'value) list) : 'value t =
    values
    |> List.map ~f:(fun (key, v) -> (K.toString key, v))
    |> StrDict.fromList


  let toList (values : 'value t) : (K.t * 'value) list =
    values
    |> StrDict.toList
    |> List.map ~f:(fun (key, v) -> (K.fromString key, v))


  let pp = StrDict.pp

  let count = StrDict.count

  let mapWithKey (dict : 'v t) ~(f : key:K.t -> 'v -> 'x) : 'x t =
    StrDict.mapWithKey dict ~f:(fun ~key v -> f ~key:(K.fromString key) v)
end

module StrSet = struct
  include Tablecloth.StrSet

  let removeMany ~(values : string list) (set : t) : t =
    Belt.Set.String.removeMany set (Array.fromList values)
end

module Set (K : Key) = struct
  (* We don't include StrSet as forgetting to wrap a function gives
   * hard-to-decipher error messages *)
  type value = StrSet.value

  type t = StrSet.t

  let add ~(value : K.t) (set : t) : t =
    StrSet.add ~value:(K.toString value) set


  let set = add

  let toList (set : t) : K.t list =
    set |> StrSet.toList |> List.map ~f:K.fromString


  let remove ~(value : K.t) (set : t) : t =
    StrSet.remove ~value:(K.toString value) set


  let removeMany ~(values : K.t list) (set : t) : t =
    let values = List.map values ~f:K.toString in
    StrSet.removeMany ~values set


  let fromList (values : K.t list) : t =
    values |> List.map ~f:K.toString |> StrSet.fromList


  let empty = StrSet.empty

  let pp = StrSet.pp
end
