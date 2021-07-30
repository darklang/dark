(* This file is a local variant of Tablecloth. It allows us to add, modify,
 * etc, tablecloth function and type definitions, to be upstreamed later. *)
module Caml = struct
  module String = String
  module List = List
  module Char = Char
  module Array = Array
  module Map = Map
  module Set = Set
end

(* This allows us override submodules. See
 * http://gallium.inria.fr/blog/overriding-submodules/ *)
include (
  Tablecloth :
    module type of Tablecloth
      with module Option := Tablecloth.Option
      with module Int := Tablecloth.Int
      with module Float := Tablecloth.Float
      with module String := Tablecloth.String
      with module Array := Tablecloth.Array
      with module Result := Tablecloth.Result
      with module List := Tablecloth.List
      with module Set := Tablecloth.Set
      with module Map := Tablecloth.Map )

let ( <| ) a b = a b

let ( >> ) (f1 : 'a -> 'b) (f2 : 'b -> 'c) : 'a -> 'c = fun x -> x |> f1 |> f2

let ( << ) (f1 : 'b -> 'c) (f2 : 'a -> 'b) : 'a -> 'c = fun x -> x |> f2 |> f1

let identity (value : 'a) : 'a = value

module Array = struct
  include Tablecloth.Array

  let iter ~(f : 'a -> unit) (arr : 'a array) : unit = Belt.Array.forEach arr f
end

module Option = struct
  include Tablecloth.Option

  let exec ~(f : 'a -> unit) (v : 'a option) : unit =
    match v with Some v -> f v | None -> ()


  let values (l : 'a option list) : 'a list =
    let valuesHelper (l : 'a list) (item : 'a option) : 'a list =
      match item with None -> l | Some v -> v :: l
    in
    Tablecloth.List.foldRight ~f:valuesHelper ~initial:[] l


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


  let unwrapLazy (a : 'a option) ~(default : unit -> 'a) : 'a =
    match a with Some a -> a | None -> default ()
end

module Either = struct
  type ('a, 'b) t =
    | Left of 'a
    | Right of 'b
end

module List = struct
  include Tablecloth.List

  (* CLEANUP: tablecloth version is wrong *)
  let insertAt ~(index : int) ~(value : 'a) (l : 'a list) : 'a list =
    let front, back = splitAt ~index l in
    append front (value :: back)

  let getBy ~(f : 'a -> bool) (l : 'a list) : 'a option = Belt.List.getBy l f

  let uniqueBy ~(f : 'a -> string) (l : 'a list) : 'a list =
    let rec uniqueHelper
        (f : 'a -> string)
        (existing : Belt.Set.String.t)
        (remaining : 'a list)
        (accumulator : 'a list) =
      match remaining with
      | [] ->
          reverse accumulator
      | first :: rest ->
          let computedFirst = f first in
          if Belt.Set.String.has existing computedFirst
          then uniqueHelper f existing rest accumulator
          else
            uniqueHelper
              f
              (Belt.Set.String.add existing computedFirst)
              rest
              (first :: accumulator)
    in
    uniqueHelper f Belt.Set.String.empty l []


  let sortBy ~(f : 'a -> 'b) (l : 'a list) : 'a list =
    Belt.List.sort l (fun a b ->
        let a' = f a in
        let b' = f b in
        if a' = b' then 0 else if a' < b' then -1 else 1)


  let sortWith (f : 'a -> 'a -> int) (l : 'a list) : 'a list =
    Belt.List.sort l f


  (* From https://github.com/janestreet/base/blob/eaab227499b36bb90c2537bc6358a2d5caf75227/src/list.ml#L247 *)
  let findMap t ~f =
    let rec loop = function
      | [] ->
          None
      | x :: l ->
        (match f x with None -> loop l | Some _ as r -> r)
    in
    loop t


  let member ~(value : 'v) (l : 'v list) : bool =
    Tablecloth.List.includes ~equal:( = ) l value


  let flatten = List.flatten

  let fold ~initial ~f list = Tablecloth.List.fold ~initial ~f list

  let foldr ~init ~f list = Tablecloth.List.fold_right ~initial:init ~f list

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
        let index =
          (* Checks to see if we need to offset the newPos by -1, after removing the element at oldPos *)
          if newPos > oldPos
          then
            let len = List.length l in
            (* Clamp at list length to prevent overflow *)
            if newPos > len then len - 1 else newPos - 1
          else newPos
        in
        l |> removeAt ~index:oldPos |> insertAt ~index ~value
    | None ->
        l


  (* Partition into two lists, of potentially different type, using function
   * `f`.  Returns value in the first list for `Left` and second list for
   * `Right`. *)
  let partitionMap ~(f : 'c -> ('a, 'b) Either.t) (items : 'c list) :
      'a list * 'b list =
    Tablecloth.List.foldRight
      ~initial:([], [])
      ~f:(fun (lefts, rights) item ->
        match f item with
        | Left a ->
            (a :: lefts, rights)
        | Right b ->
            (lefts, b :: rights))
      items


  let elemIndex ~(value : 'a) (l : 'a list) : int option =
    l
    |> Tablecloth.List.findIndex ~f:(fun _i v -> v = value)
    |> Option.map ~f:Tuple2.first
end

module Result = struct
  include Tablecloth.Result

  let isOk (v : ('ok, 'err) t) : bool = match v with Ok _ -> true | _ -> false

  let combine (l : ('ok, 'err) t list) : ('ok list, 'err) t =
    List.foldRight ~f:(map2 ~f:(fun accum r -> r :: accum)) ~initial:(Ok []) l
end

module Float = struct
  include Tablecloth.Float

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


  let join ~(sep : string) (strings : string list) =
    Js.Array.joinWith sep (List.toArray strings)
end

module Map = struct
  (* Include this way to allow adding to String and Int submodules *)
  include (
    Tablecloth.Map :
      module type of Tablecloth.Map
        with module String := Tablecloth.Map.String
        with module Int := Tablecloth.Map.Int )

  let updateIfPresent
      ~(key : 'key) ~(f : 'value -> 'value) (dict : ('key, 'value, 'id) t) :
      ('key, 'value, 'id) t =
    Tablecloth.Map.update ~key ~f:(Option.map ~f) dict


  let mergeLeft (dict1 : ('key, 'value, 'id) t) (dict2 : ('key, 'value, 'id) t)
      : ('key, 'value, 'id) t =
    Tablecloth.Map.merge
      ~f:(fun (_key : 'key) (v1 : 'v option) (v2 : 'v option) ->
        match (v1, v2) with Some _, _ -> v1 | None, _ -> v2)
      dict1
      dict2


  let mergeRight (dict1 : ('key, 'value, 'id) t) (dict2 : ('key, 'value, 'id) t)
      : ('key, 'value, 'id) t =
    Tablecloth.Map.merge
      ~f:(fun (_key : 'key) (v1 : 'v option) (v2 : 'v option) ->
        match (v1, v2) with _, Some _ -> v2 | _, None -> v1)
      dict1
      dict2


  let count (dict : ('key, 'value, 'id) t) = Tablecloth.Map.length dict

  let mapValues (dict : ('key, 'value, 'id) t) ~(f : 'value -> 'x) : 'x list =
    dict |> Tablecloth.Map.values |> List.map ~f


  let filterMapValues (dict : ('key, 'value, 'id) t) ~(f : 'value -> 'x option)
      : 'x list =
    dict |> Tablecloth.Map.values |> List.filterMap ~f


  let mapWithKey (dict : ('key, 'value, 'id) t) ~(f : key:'key -> 'value -> 'x)
      : ('key, 'x, 'id) t =
    Tablecloth.Map.mapWithIndex ~f:(fun key v -> f ~key v) dict


  let remove (dict : ('key, 'value, 'id) t) ~(key : 'key) :
      ('key, 'value, 'id) t =
    Tablecloth.Map.remove dict key


  let removeMany (dict : ('key, 'value, 'id) t) ~(keys : 'key list) :
      ('key, 'value, 'id) t =
    Belt.Map.removeMany dict (Belt.List.toArray keys)


  let has ~key (dict : ('key, 'value, 'id) t) : bool =
    Tablecloth.Map.includes dict key


  let get ~key (dict : ('key, 'value, 'id) t) : 'value option =
    Tablecloth.Map.get dict key


  let toString (d : ('key, 'value, 'id) t) =
    d
    |> toList
    |> List.map ~f:(fun (k, v) -> "\"" ^ k ^ "\": \"" ^ Js.String.make v ^ "\"")
    |> List.join ~sep:", "
    |> fun s -> "{" ^ s ^ "}"


  let pp
      (keyFormatter : Format.formatter -> 'key -> unit)
      (valueFormatter : Format.formatter -> 'value -> unit)
      (fmt : Format.formatter)
      (map : ('key, 'value, 'id) t) =
    Format.pp_print_string fmt "{ " ;
    Tablecloth.Map.forEachWithIndex map ~f:(fun ~key ~value ->
        keyFormatter fmt key ;
        Format.pp_print_string fmt ": " ;
        valueFormatter fmt value ;
        Format.pp_print_string fmt ",  ") ;
    Format.pp_print_string fmt "}" ;
    ()


  module String = struct
    include Tablecloth.Map.String

    let pp
        (valueFormatter : Format.formatter -> 'value -> unit)
        (fmt : Format.formatter)
        (map : 'value t) =
      pp Format.pp_print_string valueFormatter fmt map

    (* Js.String.make gives us "[object Object]", so we actually want our own toString. Not perfect, but slightly nicer (e.g., for App.ml's DisplayAndReportHttpError, info's values are all strings, which this handles) *)
  end

  module Int = struct
    include Tablecloth.Map.Int

    let pp
        (valueFormatter : Format.formatter -> 'value -> unit)
        (fmt : Format.formatter)
        (map : 'value t) =
      pp Format.pp_print_int valueFormatter fmt map
  end
end

module Set = struct
  (* Include this way to allow adding to String and Int submodules *)
  include (
    Tablecloth.Set :
      module type of Tablecloth.Set
        with module String := Tablecloth.Set.String
        with module Int := Tablecloth.Set.Int )

  let pp
      (valueFormatter : Format.formatter -> 'key -> unit)
      (fmt : Format.formatter)
      (set : ('key, 'id) t) =
    Format.pp_print_string fmt "{ " ;
    Tablecloth.Set.forEach set ~f:(fun v ->
        valueFormatter fmt v ;
        Format.pp_print_string fmt ", ") ;
    Format.pp_print_string fmt " }" ;
    ()


  module String = struct
    include Tablecloth.Set.String

    let pp (fmt : Format.formatter) (set : t) =
      pp Format.pp_print_string fmt set
  end

  module Int = struct
    include Tablecloth.Set.Int

    let pp (fmt : Format.formatter) (set : t) = pp Format.pp_print_int fmt set
  end

  let removeMany ~(values : 'key list) (set : ('key, 'id) t) : ('key, 'id) t =
    Belt.Set.removeMany set (Array.fromList values)


  let addMany ~(values : 'key list) (set : ('key, 'id) t) : ('key, 'id) t =
    Tablecloth.List.fold values ~initial:set ~f:(fun acc v ->
        Tablecloth.Set.add acc v)


  let add ~(value : 'key) (set : ('key, 'id) t) : ('key, 'id) t =
    Tablecloth.Set.add set value


  let set = add

  let toList (set : ('key, 'id) t) : 'key list = set |> Tablecloth.Set.toList

  let member ~(value : 'key) (set : ('key, 'id) t) : bool =
    Tablecloth.Set.includes set value


  let remove ~(value : 'key) (set : ('key, 'id) t) : ('key, 'id) t =
    Tablecloth.Set.remove set value


  (* let removeMany ~(values : 'key list) (set :  ('key, 'id) t ) :  ('key, 'id) t = *)
  (*   Belt.Set.removeMany set (List.toArray values) *)

  let empty = Tablecloth.Set.empty

  let eq = Belt.Set.String.eq
end
