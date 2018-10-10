type size = { width : int; height : int }
module Native = struct
  module Window = struct
    let window : Dom.window = [%bs.raw "window"]
    external getWidth : Dom.window -> int = "innerWidth" [@@bs.get]
    external getHeight : Dom.window -> int = "innerHeight" [@@bs.get]
    let size () : size =
      { width = getWidth window
      ; height = getHeight window
      }
  end

  module Random = struct
    let random () : int = Random.int 2147483647
  end

  module Cache = struct
    let set _k _v = Some "todo: Cache_set"
    let get _k = "todo: Cache_get"
    let clear _k = ()
  end
end

module PageVisibility = struct
  type visibility = Hidden
end

let (++) (a: string) (b: string) = a ^ b

module Debug = struct
  let crash (str: string) : 'a =
    failwith str
end

let toString (v : 'a) : string =
  Js.String.make v

module Option = struct
  type 'a t = 'a option
  let andThen (fn: 'a -> 'b option) (o: 'a option) : 'b option =
    match o with
    | None -> None
    | Some x -> fn x
  let orElse  (ma : 'a option) (mb: 'a option) : ('a option) =
    match mb with
    | None -> ma
    | Some _ -> mb
  let map (f: 'a -> 'b) (o: 'a option) : 'b option =
    Belt.Option.map o f
  let withDefault (a: 'a) (o: 'a option) : 'a =
    Belt.Option.getWithDefault o a
end

module Result = struct
  type ('err, 'ok) t = ('ok, 'err) Belt.Result.t
  let withDefault (default: 'ok) (r: ('err, 'ok) t) : 'ok =
    Belt.Result.getWithDefault r default
end
type ('err, 'ok) result = ('err, 'ok) Result.t



module Regex = struct
  let regex s : Js.Re.t = Js.Re.fromString ("/" ^ s ^ "/")
  let contains (re: Js.Re.t) (s: string) : bool = Js.Re.test s re
  let replace (re: string) (repl: string) (str: string) =
    Js.String.replaceByRe (regex re) repl str
end

let to_option (value: 'a) (sentinel: 'a) : 'a option =
  if value = sentinel
  then None
  else Some value


module List = struct
  let map (fn: 'a -> 'b) (l: 'a list) : 'b list =
    Belt.List.map l fn
  let indexedMap (fn: 'int -> 'a -> 'b) (l: 'a list) : 'b list =
    Belt.List.mapWithIndex l fn
  let map2 (fn: 'a -> 'b -> 'c) (a: 'a list) (b: 'b list) : 'c list =
    Belt.List.mapReverse2 a b fn |> Belt.List.reverse
  let getBy (fn: 'a -> bool) (l: 'a list) : 'a option =
    Belt.List.getBy l fn
  let elemIndex (a: 'a) (l : 'a list) : int option =
    l
    |> Array.of_list
    |> Js.Array.findIndex ((=) a)
    |> to_option (-1)
  let rec last (l : 'a list) : 'a option =
    match l with
    | [] -> None
    | [a] -> Some a
    | _ :: tail -> last tail
  let member (i: 'a) (l : 'a list) : bool =
    Belt.List.has l i (=)
  let uniqueBy (f: 'a -> string) (l: 'a list) : 'a list =
    let rec uniqueHelp
        (f: 'a -> string)
        (existing: Belt.Set.String.t)
        (remaining: 'a list)
        (accumulator: 'a list) =
      match remaining with
      | [] -> List.rev accumulator
      | first :: rest ->
        let computedFirst = f first in
        if Belt.Set.String.has existing computedFirst
        then uniqueHelp f existing rest accumulator
        else
          uniqueHelp
            f
            (Belt.Set.String.add existing computedFirst)
            rest
            (first :: accumulator)
    in
    uniqueHelp f Belt.Set.String.empty l []
  let find (f: 'a -> bool) (l: 'a list) : 'a option =
    Belt.List.getBy l f

  let getAt (i: int) (l: 'a list) : 'a option =
    Belt.List.get l i

  let head (l: 'a list) : 'a option =
    Belt.List.head l
  let drop (count: int) (l: 'a list) : 'a list =
    Belt.List.drop l count
    |. Belt.Option.getWithDefault []
  let init (l: 'a list) : 'a list option =
    match List.rev l with
    | _ :: rest -> Some (List.rev rest)
    | [] -> None
  let filterMap (fn: 'a -> bool) (l: 'a list) : 'a list =
    Belt.List.keep l fn

end

module Char = struct
  let toCode (c: char) : int = Char.code c
  let fromCode (i: int) : char = Char.chr i
end

module Tuple2 = struct
  let create a b = (a,b)
end

module Tuple = struct
  let mapSecond (fn: 'b -> 'c) ((a,b): 'a * 'b) : 'a * 'c =
    (a, fn b)

  let create a b = (a,b)
end


module String = struct
  let toInt (s: string) : (string, int) result =
    try
      Ok (int_of_string s)
    with e ->
      Error (Printexc.to_string e)

  let toFloat (s: string) : (string, float) result =
    try
      Ok (float_of_string s)
    with e ->
      Error (Printexc.to_string e)

  let uncons (s: string) : (char * string) option =
    match s with
    | "" -> None
    | s -> Some (String.get s 0, String.sub s 1 (String.length s - 1))
  let dropLeft (from: int) (s: string) : string =
    Js.String.substr ~from s
  let dropRight (from: int) (s: string) : string =
    Js.String.sliceToEnd ~from s
  let split (delimiter : string) (s: string) : string list =
    Js.String.split delimiter s
    |> Belt.List.fromArray
  let join (sep : string) (l: string list) : string =
    String.concat sep l
  let endsWith (needle: string) (haystack: string) =
    Js.String.endsWith needle haystack
  let startsWith (needle: string) (haystack: string) =
    Js.String.startsWith needle haystack
  let toLower (s: string) : string =
    String.lowercase s
  let toUpper (s: string) : string =
    String.uppercase s
  let contains (needle: string) (haystack: string) : bool =
    Js.String.includes needle haystack
  let repeat (count: int) (s: string) : string =
    Js.String.repeat count s
  let fromList (l : char list) : string =
    l
    |> List.map Char.toCode
    |> List.map Js.String.fromCharCode
    |> String.concat ""
end


