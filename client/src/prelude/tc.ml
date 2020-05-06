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
      with module Array := Tablecloth.Array
      with module Result := Tablecloth.Result
      with module List := Tablecloth.List )

module Array = struct
  include Tablecloth.Array

  let iter ~(f : 'a -> unit) (arr : 'a array) : unit = Belt.Array.forEach arr f
end

module Option = struct
  include Tablecloth.Option

  let exec ~(f : 'a -> unit) (v : 'a option) : unit =
    match v with Some v -> f v | None -> ()


  (* TODO: remove *)
  let valueExn (value : 'a option) : 'a =
    match value with Some v -> v | None -> raise Not_found


  let orLazy (v : 'a option) (v2 : unit -> 'a option) : 'a option =
    match v with Some v -> Some v | None -> v2 ()


  let orElseLazy (v : unit -> 'a option) (v2 : 'a option) : 'a option =
    match v2 with Some v2 -> Some v2 | None -> v ()


  let pair (a : 'a option) (b : 'b option) : ('a * 'b) option =
    match (a, b) with Some a, Some b -> Some (a, b) | _ -> None


  let map2 (a : 'a option) (b : 'b option) ~(f : 'a -> 'b -> 'c) : 'c option =
    match (a, b) with Some a, Some b -> Some (f a b) | _ -> None


  let andThen2 (a : 'a option) (b : 'b option) ~(f : 'a -> 'b -> 'c option) :
      'c option =
    match (a, b) with Some a, Some b -> f a b | _ -> None


  let isSomeEqualTo ~(value : 'a) (o : 'a option) : bool = Some value = o

  (* If a is some, then apply fn to a, return both a and the result.
    if either a or b is none, then return none
  *)
  let thenAlso (a : 'a option) ~(f : 'a -> 'b option) : ('a * 'b) option =
    let b = andThen ~f a in
    pair a b


  let withDefaultLazy (a : 'a option) ~(default : unit -> 'a) : 'a =
    match a with Some a -> a | None -> default ()
end

module Result = struct
  include Tablecloth.Result

  let isOk (v : ('err, 'ok) t) : bool = match v with Ok _ -> true | _ -> false
end

module Either = struct
  type ('a, 'b) t =
    | Left of 'a
    | Right of 'b
end

module List = struct
  include Tablecloth.List

  (* From https://github.com/janestreet/base/blob/eaab227499b36bb90c2537bc6358a2d5caf75227/src/list.ml#L247 *)
  let findMap t ~f =
    let rec loop = function
      | [] ->
          None
      | x :: l ->
        (match f x with None -> loop l | Some _ as r -> r)
    in
    loop t


  let findWithIndex ~(f : int -> 'a -> bool) (l : 'a list) : int option =
    let rec findIndexHelper
        ~(i : int) ~(predicate : int -> 'a -> bool) (l : 'a list) : int option =
      match l with
      | [] ->
          None
      | x :: rest ->
          if predicate i x
          then Some i
          else findIndexHelper ~i:(i + 1) ~predicate rest
    in
    findIndexHelper ~i:0 ~predicate:f l


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


  (* Takes everything before and after, but not including nexted element *)
  let splitOn ~(index : int) (l : 'a list) : 'a list * 'a list =
    (take ~count:index l, drop ~count:(index + 1) l)


  (* Moves item in oldPos into the position at newPos, pushing the element already at newPos down. Ex:
    l = [a b c d]
    moveInto 3 1 l, takes d and moves it between a & b. => [a d b c]
    NOTE: This is not swapping the elements in newPos & oldPos
  *)
  let moveInto ~(oldPos : int) ~(newPos : int) (l : 'a list) : 'a list =
    match getAt ~index:oldPos l with
    | Some value ->
        (* Checks to see if we need to offset the newPos by -1, after removing the element at oldPos *)
        let index = if newPos > oldPos then newPos - 1 else newPos in
        l |> removeAt ~index:oldPos |> insertAt ~index ~value
    | None ->
        l


  (* Partition into two lists, of potentially different type, using function
   * `f`.  Returns value in the first list for `Left` and second list for
   * `Right`. *)
  let partitionMap ~(f : 'c -> ('a, 'b) Either.t) (items : 'c list) :
      'a list * 'b list =
    Tablecloth.List.foldr
      ~init:([], [])
      ~f:(fun item (lefts, rights) ->
        match f item with
        | Left a ->
            (a :: lefts, rights)
        | Right b ->
            (lefts, b :: rights))
      items
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


  let replaceChunk ~(from : int) ~(to_ : int) ~(replacement : string) s : string
      =
    slice ~from:0 ~to_:from s ^ replacement ^ slice ~from:to_ ~to_:(length s) s


  (* returns the index of the last occurrence of character c in string s before position i+1 or None if c does not occur in s before position i+1. *)
  let rindex_from_opt ~(pos : int) (s : string) (c : char) : int option =
    String.rindex_from_opt s pos c


  (* returns the index of the first occurrence of character c in string s after position i or None if c does not occur in s after position i.
 *)
  let index_from_opt ~(pos : int) (s : string) (c : char) : int option =
    String.index_from_opt s pos c
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
        match (v1, v2) with Some _, _ -> v1 | None, _ -> v2)
      dict1
      dict2


  let mergeRight (dict1 : 'v t) (dict2 : 'v t) : 'v t =
    Tablecloth.StrDict.merge
      ~f:(fun (_key : string) (v1 : 'v option) (v2 : 'v option) ->
        match (v1, v2) with _, Some _ -> v2 | _, None -> v1)
      dict1
      dict2


  let count (dict : 'v t) = Belt.Map.String.size dict

  let mapValues (dict : 'v t) ~(f : 'v -> 'x) : 'x list =
    dict |> values |> List.map ~f


  let filterMapValues (dict : 'v t) ~(f : 'v -> 'x option) : 'x list =
    dict |> values |> List.filterMap ~f


  let mapWithKey (dict : 'v t) ~(f : key:string -> 'v -> 'x) : 'x t =
    Belt.Map.String.mapWithKey dict (fun key v -> f ~key v)


  let remove (dict : 'v t) ~(key : key) : 'v t = Belt.Map.String.remove dict key

  let removeMany (dict : 'v t) ~(keys : key list) : 'v t =
    Belt.Map.String.removeMany dict (Belt.List.toArray keys)


  let singleton ~(key : key) ~(value : 'v) : 'v t = empty |> insert ~key ~value

  let has ~key (dict : 'v t) : bool = Belt.Map.String.has dict key
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


  let eq = Belt.Set.String.eq
end

module Set (K : Key) = struct
  (* We don't include StrSet as forgetting to wrap a function gives
   * hard-to-decipher error messages *)
  type value = StrSet.value

  type t = StrSet.t

  let add ~(value : K.t) (set : t) : t =
    StrSet.add ~value:(K.toString value) set


  let addMany ~(values : K.t list) (set : t) : t =
    List.foldl values ~init:set ~f:(fun v acc ->
        StrSet.add ~value:(K.toString v) acc)


  let set = add

  let toList (set : t) : K.t list =
    set |> StrSet.toList |> List.map ~f:K.fromString


  let member ~(value : K.t) (set : t) : bool =
    StrSet.member ~value:(K.toString value) set


  let remove ~(value : K.t) (set : t) : t =
    StrSet.remove ~value:(K.toString value) set


  let removeMany ~(values : K.t list) (set : t) : t =
    let values = List.map values ~f:K.toString in
    StrSet.removeMany ~values set


  let fromList (values : K.t list) : t =
    values |> List.map ~f:K.toString |> StrSet.fromList


  let empty = StrSet.empty

  let pp = StrSet.pp

  let eq = StrSet.eq
end
