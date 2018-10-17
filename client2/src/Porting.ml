let (++) (a: string) (b: string) = a ^ b


module PageVisibility = struct
  type visibility = Hidden
end

let (<|) a b = a b
let (>>) (f1: 'a -> 'b) (f2: 'b -> 'c) : 'a -> 'c =
  fun x -> x |> f1 |> f2
let (<<) (f1: 'b -> 'c) (f2: 'a -> 'b) : 'a -> 'c =
  fun x -> x |> f2 |> f1

module Debug = struct
  let crash (str: string) : 'a =
    failwith str
end

let toString (v : 'a) : string =
  Js.String.make v

module Result = struct
  type ('err, 'ok) t = ('ok, 'err) Belt.Result.t
  let withDefault (default: 'ok) (r: ('err, 'ok) t) : 'ok =
    Belt.Result.getWithDefault r default
  let map2 (fn: 'a -> 'b -> 'c) (a: ('err, 'a) t) (b: ('err, 'b) t) : ('err, 'c) t =
    match a,b with
    | Ok a, Ok b -> Ok (fn a b)
    | Error a, Ok _ -> Error a
    | Ok _, Error b -> Error b
    | Error a, Error b -> Error a
end
type ('err, 'ok) result = ('err, 'ok) Result.t



module Regex = struct
  let regex s : Js.Re.t = Js.Re.fromString ("/" ^ s ^ "/")
  let contains (re: Js.Re.t) (s: string) : bool = Js.Re.test s re
  let replace (re: string) (repl: string) (str: string) =
    Js.String.replaceByRe (regex re) repl str
  let matches (re: Js.Re.t) (s: string) : Js.Re.result option = Js.Re.exec s re
end

let toOption (value: 'a) (sentinel: 'a) : 'a option =
  if value = sentinel
  then None
  else Some value

let identity (value: 'a) : 'a =
  value

(* let deOption (msg: string) (value: 'a option) : 'a = *)
(*   match value with *)
(*   | Some v -> v *)
(*   | None -> failwith msg *)


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
    |> toOption (-1)
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
  let any (fn: 'a -> bool) (l: 'a list) : bool =
    List.exists fn l
  let head (l: 'a list) : 'a option =
    Belt.List.head l
  let drop (count: int) (l: 'a list) : 'a list =
    Belt.List.drop l count
    |. Belt.Option.getWithDefault []
  let init (l: 'a list) : 'a list option =
    match List.rev l with
    | _ :: rest -> Some (List.rev rest)
    | [] -> None
  let filterMap (fn: 'a -> 'b option) (l: 'a list) : 'b list =
    Belt.List.keepMap l fn
  let filter (fn: 'a -> bool) (l: 'a list) : 'a list =
    Belt.List.keep l fn
  let concat (ls: 'a list list) : 'a list =
    ls
    |> Belt.List.toArray
    |> Belt.List.concatMany
  let partition (fn: 'a -> bool) (l: 'a list) : 'a list * 'a list =
    List.partition fn l
  let foldr (fn: 'a -> 'b -> 'b) (init: 'b) (l: 'a list) : 'b =
    List.fold_right fn l init
  let foldl (fn: 'a -> 'b -> 'b) (init: 'b) (l: 'a list) : 'b =
    List.fold_right fn (List.rev l) init
  let rec findIndexHelp (index : int) (predicate : 'a -> bool) (list : 'a list) :
    int option =
    match list with
    | [] -> None
    | x :: xs ->
      if predicate x then Some index
      else findIndexHelp (index + 1) predicate xs
  let findIndex (fn: 'a -> bool) (l: 'a list) : int option =
    findIndexHelp 0 fn l
  let take (count: int) (l: 'a list) : 'a list =
    Belt.List.take l count
    |. Belt.Option.getWithDefault []
  let updateAt (index : int) (fn : 'a -> 'a) (list : 'a list) : 'a list =
    if index < 0 then list
    else
      let head = take index list in
      let tail = drop index list in
      match tail with x :: xs -> head @ (fn x :: xs) | _ -> list
  let length (l: 'a list) : int =
    List.length l
  let reverse (l: 'a list) : 'a list =
    List.rev l
  let rec dropWhile (predicate : 'a -> bool) (list : 'a list) : 'a list =
    match list with
    | [] -> []
    | x :: xs -> if predicate x then dropWhile predicate xs else list
  let isEmpty (l: 'a list) : bool =
    l = []
  let cons (item: 'a) (l: 'a list) : 'a list =
    item :: l
  let takeWhile (predicate : 'a -> bool) (l : 'a list) : 'a list =
    let rec takeWhileMemo memo list =
      match list with
      | [] -> List.rev memo
      | x :: xs ->
        if predicate x then takeWhileMemo (x :: memo) xs else List.rev memo
    in
    takeWhileMemo [] l
  let all (fn: 'a -> bool) (l: 'a list) : bool =
    Belt.List.every l fn
  let tail (l: 'a list) : 'a list option =
    match l with
    | [] -> None
    | _ :: rest -> Some rest
  let append (l1: 'a list) (l2: 'a list) : 'a list =
    l1 @ l2
  let removeAt (index : int) (l : 'a list) : 'a list =
    if index < 0 then l
    else
      let head = take index l in
      let tail = drop index l |> tail in
      match tail with None -> l | Some t -> append head t
end

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
  let foldrValues (item : 'a option) (list : 'a list) : 'a list =
    match item with None -> list | Some v -> v :: list
  let values (l : 'a option list) : 'a list = List.foldr foldrValues [] l
  let toList (o: 'a option) : 'a list =
    match o with
    | None -> []
    | Some o -> [o]
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
  let second ((a,b): 'a * 'b) : 'b =
    b
  let first ((a,b): 'a * 'b) : 'a =
    a

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
  let fromInt (i : int) : string =
    Printf.sprintf "%d" i
end

module IntSet = struct
  module Set = Belt.Set.Int
  type t = Set.t
  type value = Set.value
  let fromList (l: value list) : t =
    l
    |> Belt.List.toArray
    |> Set.fromArray
  let member (i: value) (set: t) : bool =
    Set.has set i
  let diff (set1: t) (set2: t) : t =
    Set.diff set1 set2
  let isEmpty (s: t) : bool =
    Set.isEmpty s
  let toList (s: t) : value list =
    Set.toList s
end

module StrSet = struct
  module Set = Belt.Set.String
  type t = Set.t
  type value = Set.value
  let fromList (l: value list) : t =
    l
    |> Belt.List.toArray
    |> Set.fromArray
  let member (i: value) (set: t) : bool =
    Set.has set i
  let diff (set1: t) (set2: t) : t =
    Set.diff set1 set2
  let isEmpty (s: t) : bool =
    Set.isEmpty s
  let toList (s: t) : value list =
    Set.toList s
end


module StrDict = struct
  module Map = Belt.Map.String
  type key = Map.key
  type 'value t = 'value Map.t
  let toList = Map.toList
  let empty = Map.empty
  let fromList (l: ('key * 'value) list) : 'value t =
    l
    |> Belt.List.toArray
    |> Map.fromArray
  let get (k: key) (v: 'value t) : 'value option =
    Map.get v k
  let insert (k: key) (v: 'value) (map: 'value t) : 'value t =
    Map.set map k v

end

module IntDict = struct
  module Map = Belt.Map.Int
  type key = Map.key
  type 'value t = 'value Map.t
  let toList = Map.toList
  let empty = Map.empty
  let fromList (l: ('key * 'value) list) : 'value t =
    l
    |> Belt.List.toArray
    |> Map.fromArray
  let get (k: key) (v: 'value t) : 'value option =
    Map.get v k
  let insert (k: key) (v: 'value) (map: 'value t) : 'value t =
    Map.set map k v
  let update (k: key) (fn: 'v option -> 'v option) (map: 'value t) : 'value t =
    Map.update map k fn
end

module Html = struct
  include Tea.Html
  type 'a html = 'a Vdom.t
end

module Native = struct
  type size = { width : int; height : int }

  type bounding_rect =
    { x : float
    ; y : float
    ; width : float
    ; height : float
    ; left : float
    ; top : float
    ; right : float
    ; bottom : float
    }

  type rect =
    { id : int
    ; bottom : float
    ; height : float
    ; left : float
    ; right : float
    ; top : float
    ; width : float
    ; x : float
    ; y : float
    }

  type ast_positions = { atoms : rect list; nested : rect list }


  exception NativeCodeError of string

  module Ext = struct
    let window : Dom.window = [%bs.raw "window"]

    external getWidth :
      Dom.window -> int =
      "innerWidth" [@@bs.get]

    external getHeight :
      Dom.window -> int =
      "innerHeight" [@@bs.get]

    external getElementsByClassName :
      (string -> Dom.element list) =
      "getElementsByClassName" [@@bs.val][@@bs.scope "document"]

    external querySelectorAll :
      (Dom.element -> string -> Dom.element list) =
      "querySelectorAll" [@@bs.val]

    external getBoundingClientRect :
      Dom.element -> bounding_rect =
      "getBoundingClientRect" [@@bs.val]

    external classList :
      Dom.element -> string list =
      "classList" [@@bs.get]
  end

  module Window = struct
    let size () : size =
      { width = Ext.getWidth Ext.window
      ; height = Ext.getHeight Ext.window
      }
  end

  module Random = struct
    let random () : int = Js_math.random_int 0 2147483647
  end

  module Cache = struct
    let set _k v =
      Dom.Storage.setItem
        (String.fromInt _k)
        (Js.Json.stringify v)
        Dom.Storage.sessionStorage
    let get _k =
      Dom.Storage.getItem
        (String.fromInt _k)
        Dom.Storage.sessionStorage
    let clear _k =
      Dom.Storage.clear
        Dom.Storage.sessionStorage
  end

  module Size = struct

    let getId (n : Dom.element) : int  =
      let classes = Ext.classList n in
      let r =  Regex.regex "id-(\\d+)" in
      List.find (fun c -> Regex.contains r c) classes
      |> function
        | Some cls ->
          Regex.matches r cls
          |> (function
            | Some res ->
              Js.Nullable.toOption (Js.Re.captures res).(1)
              |> (function
                | Some id -> int_of_string id
                | None -> raise (NativeCodeError "Native.size.getId : cannot convert string to int"))
            | None -> raise (NativeCodeError "Native.size.getId : cannot find expr id"))
        | None -> raise (NativeCodeError "Native.size.getId : cannot find expr id")

    let find (tl: Dom.element) (nested: bool) : rect list =
      let selector =
        if nested
        then ".blankOr.nested"
        else ".blankOr:not(.nested)"
      in
      let matches = Ext.querySelectorAll tl selector in
      List.map
        (fun n ->
          let rect = Ext.getBoundingClientRect n in
          let blankId = getId n in
          { id = blankId
          ; x = rect.x
          ; y = rect.y
          ; width = rect.width
          ; height = rect.height
          ; left = rect.left
          ; top = rect.top
          ; right = rect.right
          ; bottom = rect.bottom
          }
        ) matches

    let positions (tlid: int) : ast_positions =
      let selector = Printf.sprintf "toplevel tl-%d" tlid in
      let elems = Ext.getElementsByClassName selector in
      List.head elems
      |> function
        | Some tl ->
          { atoms = find tl false
          ; nested = find tl true
          }
        | None ->
          raise (NativeCodeError "Native.Size.positions : Cannot find toplevels")
  end

  module Location = struct
    external queryString: string = "search" [@@bs.val][@@bs.scope "window", "location"]
    external hashString: string = "hash" [@@bs.val][@@bs.scope "window", "location"]
    (* TODO write string query parser *)
  end
end


module Rollbar = struct
  external send : (string -> unit) = "error" [@@bs.val][@@bs.scope "window", "Rollbar"]
end
