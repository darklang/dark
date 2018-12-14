let ( ++ ) (a : string) (b : string) = a ^ b

let registerGlobal name key tagger decoder =
  let open Vdom in
  let enableCall callbacks_base =
    let callbacks = ref callbacks_base in
    let fn ev =
      let open Tea_json.Decoder in
      let open Tea_result in
      match decodeEvent decoder ev with
      | Error _ ->
          None
      | Ok pos ->
          Some (tagger pos)
    in
    let handler = EventHandlerCallback (key, fn) in
    let elem = Web_node.document_node in
    let cache = eventHandler_Register callbacks elem name handler in
    fun () ->
      let _ = eventHandler_Unregister elem name cache in
      ()
  in
  Tea_sub.registration key enableCall


module PageVisibility = struct
  type visibility =
    | Hidden
    | Visible
  [@@deriving show]
end

let ( <| ) a b = a b

let ( >> ) (f1 : 'a -> 'b) (f2 : 'b -> 'c) : 'a -> 'c = fun x -> x |> f1 |> f2

let ( << ) (f1 : 'b -> 'c) (f2 : 'a -> 'b) : 'a -> 'c = fun x -> x |> f2 |> f1

module Debug = struct
  let crash (str : string) : 'a = failwith str

  let log ?(f : 'a -> string = Js.String.make) (msg : string) (data : 'a) : 'a
      =
    Js.log2 msg (f data) ;
    data


  let loG ?(f : 'a -> string = Js.String.make) (msg : string) (data : 'a) :
      unit =
    Js.log2 msg (f data)
end

let toOption ~(sentinel : 'a) (value : 'a) : 'a option =
  if value = sentinel then None else Some value


let identity (value : 'a) : 'a = value

module List = struct
  let flatten = Belt.List.flatten

  let sum (l : int list) : int = Belt.List.reduce l 0 ( + )

  let floatSum (l : float list) : float = Belt.List.reduce l 0.0 ( +. )

  let map (fn : 'a -> 'b) (l : 'a list) : 'b list = Belt.List.map l fn

  let indexedMap (fn : 'int -> 'a -> 'b) (l : 'a list) : 'b list =
    Belt.List.mapWithIndex l fn


  let map2 (fn : 'a -> 'b -> 'c) (a : 'a list) (b : 'b list) : 'c list =
    Belt.List.mapReverse2 a b fn |> Belt.List.reverse


  let getBy (fn : 'a -> bool) (l : 'a list) : 'a option = Belt.List.getBy l fn

  let elemIndex (a : 'a) (l : 'a list) : int option =
    l
    |> Array.of_list
    |> Js.Array.findIndex (( = ) a)
    |> toOption ~sentinel:(-1)


  let rec last (l : 'a list) : 'a option =
    match l with [] -> None | [a] -> Some a | _ :: tail -> last tail


  let member (i : 'a) (l : 'a list) : bool = Belt.List.has l i ( = )

  let uniqueBy (f : 'a -> string) (l : 'a list) : 'a list =
    let rec uniqueHelp
        (f : 'a -> string)
        (existing : Belt.Set.String.t)
        (remaining : 'a list)
        (accumulator : 'a list) =
      match remaining with
      | [] ->
          List.rev accumulator
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


  let find (f : 'a -> bool) (l : 'a list) : 'a option = Belt.List.getBy l f

  let getAt (i : int) (l : 'a list) : 'a option = Belt.List.get l i

  let any (fn : 'a -> bool) (l : 'a list) : bool = List.exists fn l

  let head (l : 'a list) : 'a option = Belt.List.head l

  let drop (count : int) (l : 'a list) : 'a list =
    Belt.List.drop l count |. Belt.Option.getWithDefault []


  let init (l : 'a list) : 'a list option =
    match List.rev l with _ :: rest -> Some (List.rev rest) | [] -> None


  let filterMap (fn : 'a -> 'b option) (l : 'a list) : 'b list =
    Belt.List.keepMap l fn


  let filter (fn : 'a -> bool) (l : 'a list) : 'a list = Belt.List.keep l fn

  let concat (ls : 'a list list) : 'a list =
    ls |> Belt.List.toArray |> Belt.List.concatMany


  let partition (fn : 'a -> bool) (l : 'a list) : 'a list * 'a list =
    List.partition fn l


  let foldr (fn : 'a -> 'b -> 'b) (init : 'b) (l : 'a list) : 'b =
    List.fold_right fn l init


  let foldl (fn : 'a -> 'b -> 'b) (init : 'b) (l : 'a list) : 'b =
    List.fold_right fn (List.rev l) init


  let rec findIndexHelp (index : int) (predicate : 'a -> bool) (list : 'a list)
      : int option =
    match list with
    | [] ->
        None
    | x :: xs ->
        if predicate x
        then Some index
        else findIndexHelp (index + 1) predicate xs


  let findIndex (fn : 'a -> bool) (l : 'a list) : int option =
    findIndexHelp 0 fn l


  let take (count : int) (l : 'a list) : 'a list =
    Belt.List.take l count |. Belt.Option.getWithDefault []


  let updateAt (index : int) (fn : 'a -> 'a) (list : 'a list) : 'a list =
    if index < 0
    then list
    else
      let head = take index list in
      let tail = drop index list in
      match tail with x :: xs -> head @ (fn x :: xs) | _ -> list


  let length (l : 'a list) : int = List.length l

  let reverse (l : 'a list) : 'a list = List.rev l

  let rec dropWhile (predicate : 'a -> bool) (list : 'a list) : 'a list =
    match list with
    | [] ->
        []
    | x :: xs ->
        if predicate x then dropWhile predicate xs else list


  let isEmpty (l : 'a list) : bool = l = []

  let cons (item : 'a) (l : 'a list) : 'a list = item :: l

  let takeWhile (predicate : 'a -> bool) (l : 'a list) : 'a list =
    let rec takeWhileMemo memo list =
      match list with
      | [] ->
          List.rev memo
      | x :: xs ->
          if predicate x then takeWhileMemo (x :: memo) xs else List.rev memo
    in
    takeWhileMemo [] l


  let all (fn : 'a -> bool) (l : 'a list) : bool = Belt.List.every l fn

  let tail (l : 'a list) : 'a list option =
    match l with [] -> None | _ :: rest -> Some rest


  let append (l1 : 'a list) (l2 : 'a list) : 'a list = l1 @ l2

  let removeAt (index : int) (l : 'a list) : 'a list =
    if index < 0
    then l
    else
      let head = take index l in
      let tail = drop index l |> tail in
      match tail with None -> l | Some t -> append head t


  let minimumBy (f : 'a -> 'comparable) (ls : 'a list) : 'a option =
    let minBy x (y, fy) =
      let fx = f x in
      if fx < fy then (x, fx) else (y, fy)
    in
    match ls with
    | [l_] ->
        Some l_
    | l_ :: ls_ ->
        Some (fst <| foldl minBy (l_, f l_) ls_)
    | _ ->
        None


  let maximum (list : 'comparable list) : 'comparable option =
    match list with x :: xs -> Some (foldl max x xs) | _ -> None


  let sortBy (fn : 'a -> 'b) (l : 'a list) : 'a list =
    Belt.List.sort l (fun a b ->
        let a' = fn a in
        let b' = fn b in
        if a' = b' then 0 else if a' < b' then -1 else 1 )


  let span (p : 'a -> bool) (xs : 'a list) : 'a list * 'a list =
    (takeWhile p xs, dropWhile p xs)


  let rec groupWhile (eq : 'a -> 'a -> bool) (xs_ : 'a list) : 'a list list =
    match xs_ with
    | [] ->
        []
    | x :: xs ->
        let ys, zs = span (eq x) xs in
        (x :: ys) :: groupWhile eq zs


  let splitAt (n : int) (xs : 'a list) : 'a list * 'a list =
    (take n xs, drop n xs)


  let insertAt (n : int) (xs : 'a list) (newVal : 'a) : 'a list =
    take n xs @ (newVal :: drop n xs)


  let splitWhen (predicate : 'a -> bool) (list : 'a list) :
      ('a list * 'a list) option =
    findIndex predicate list |. Belt.Option.map (fun i -> splitAt i list)


  let intersperse (sep : 'a) (xs : 'a list) : 'a list =
    match xs with
    | [] ->
        []
    | hd :: tl ->
        let step x rest = sep :: x :: rest in
        let spersed = foldr step [] tl in
        hd :: spersed


  let initialize (n : int) (f : int -> 'a) : 'a list =
    let rec step i acc = if i < 0 then acc else step (i - 1) (f i :: acc) in
    step (n - 1) []


  let sortWith (fn : 'a -> 'a -> int) (l : 'a list) : 'a list =
    Belt.List.sort l fn


  let fromArray (arr : 'a Js.Array.t) : 'a list = Belt.List.fromArray arr

  let iter ~(f : 'a -> unit) (l : 'a list) : unit = List.iter f l

  let for_all2_exn (l1 : 'a list) (l2 : 'b list) (f : 'a -> 'b -> bool) : bool
      =
    let n1 = length l1 in
    let n2 = length l2 in
    if n1 <> n2
    then
      raise
        (Invalid_argument
           (Printf.sprintf "length mismatch in for_all2_exn: %d <> %d " n1 n2))
    else List.for_all2 f l1 l2


  let mapi = List.mapi
end

module Result = struct
  type ('err, 'ok) t = ('ok, 'err) Belt.Result.t

  let withDefault (default : 'ok) (r : ('err, 'ok) t) : 'ok =
    Belt.Result.getWithDefault r default


  let map2 (fn : 'a -> 'b -> 'c) (a : ('err, 'a) t) (b : ('err, 'b) t) :
      ('err, 'c) t =
    match (a, b) with
    | Ok a, Ok b ->
        Ok (fn a b)
    | Error a, Ok _ ->
        Error a
    | Ok _, Error b ->
        Error b
    | Error a, Error _ ->
        Error a


  let combine (l : ('x, 'a) t list) : ('x, 'a list) t =
    List.foldr (map2 (fun a b -> a :: b)) (Ok []) l


  let map (fn : 'ok -> 'value) (r : ('err, 'ok) t) : ('err, 'value) t =
    Belt.Result.map r fn


  let toOption (r : ('err, 'ok) t) : 'ok option =
    match r with Ok v -> Some v | _ -> None


  let pp
      (_ : Format.formatter -> 'err -> unit)
      (_ : Format.formatter -> 'ok -> unit)
      (_ : Format.formatter)
      (_ : ('err, 'ok) t) =
    ()
end

type ('err, 'ok) result = ('err, 'ok) Result.t [@@deriving show]

module Base64 = struct
  let encode (str : string) : string = Webapi.Base64.btoa str

  let decode (b64 : string) : (string, string) result =
    try Ok (Webapi.Base64.atob b64) with e -> Error (Printexc.to_string e)
end

module Regex = struct
  let regex s : Js.Re.t = Js.Re.fromStringWithFlags ~flags:"g" s

  let contains (re : Js.Re.t) (s : string) : bool = Js.Re.test s re

  let replace (re : string) (repl : string) (str : string) =
    Js.String.replaceByRe (regex re) repl str


  let matches (re : Js.Re.t) (s : string) : Js.Re.result option =
    Js.Re.exec s re
end

module Option = struct
  type 'a t = 'a option

  let andThen (fn : 'a -> 'b option) (o : 'a option) : 'b option =
    match o with None -> None | Some x -> fn x


  let or_ (ma : 'a option) (mb : 'a option) : 'a option =
    match ma with None -> mb | Some _ -> ma


  let orElse (ma : 'a option) (mb : 'a option) : 'a option =
    match mb with None -> ma | Some _ -> mb


  let map (f : 'a -> 'b) (o : 'a option) : 'b option = Belt.Option.map o f

  let withDefault (a : 'a) (o : 'a option) : 'a =
    Belt.Option.getWithDefault o a


  let foldrValues (item : 'a option) (list : 'a list) : 'a list =
    match item with None -> list | Some v -> v :: list


  let values (l : 'a option list) : 'a list = List.foldr foldrValues [] l

  let toList (o : 'a option) : 'a list =
    match o with None -> [] | Some o -> [o]


  let isSome = Belt.Option.isSome
end

module Char = struct
  let toCode (c : char) : int = Char.code c

  let fromCode (i : int) : char = Char.chr i
end

module Tuple2 = struct
  let create a b = (a, b)
end

module Tuple = struct
  let mapSecond (fn : 'b -> 'c) ((a, b) : 'a * 'b) : 'a * 'c = (a, fn b)

  let second ((_, b) : 'a * 'b) : 'b = b

  let first ((a, _) : 'a * 'b) : 'a = a

  let create a b = (a, b)
end

module String = struct
  let length = String.length

  let toInt (s : string) : (string, int) result =
    try Ok (int_of_string s) with e -> Error (Printexc.to_string e)


  let toFloat (s : string) : (string, float) result =
    try Ok (float_of_string s) with e -> Error (Printexc.to_string e)


  let uncons (s : string) : (char * string) option =
    match s with
    | "" ->
        None
    | s ->
        Some (s.[0], String.sub s 1 (String.length s - 1))


  let dropLeft (from : int) (s : string) : string = Js.String.substr ~from s

  let dropRight (num : int) (s : string) : string =
    if num < 1 then s else Js.String.slice ~from:0 ~to_:(-num) s


  let split (delimiter : string) (s : string) : string list =
    Js.String.split delimiter s |> Belt.List.fromArray


  let join (sep : string) (l : string list) : string = String.concat sep l

  let endsWith (needle : string) (haystack : string) =
    Js.String.endsWith needle haystack


  let startsWith (needle : string) (haystack : string) =
    Js.String.startsWith needle haystack


  let toLower (s : string) : string = String.lowercase s

  let toUpper (s : string) : string = String.uppercase s

  let isCapitalized (s : string) : bool = s = String.capitalize s

  let contains (needle : string) (haystack : string) : bool =
    Js.String.includes needle haystack


  let repeat (count : int) (s : string) : string = Js.String.repeat count s

  let fromList (l : char list) : string =
    l
    |> List.map Char.toCode
    |> List.map Js.String.fromCharCode
    |> String.concat ""


  let toList (s : string) : char list =
    s |> Js.String.castToArrayLike |> Js.Array.from |> Belt.List.fromArray


  let fromInt (i : int) : string = Printf.sprintf "%d" i

  let concat = String.concat ""

  let fromChar (c : char) : string = c |> Char.toCode |> Js.String.fromCharCode

  let slice from to_ str = Js.String.slice ~from ~to_ str

  let trim = Js.String.trim

  let insertAt (newStr : string) (pos : int) (origStr : string) : string =
    Js.String.slice ~from:0 ~to_:pos origStr
    ^ newStr
    ^ Js.String.sliceToEnd ~from:pos origStr
end

module IntSet = struct
  module Set = Belt.Set.Int

  type t = Set.t

  type value = Set.value

  let fromList (l : value list) : t = l |> Belt.List.toArray |> Set.fromArray

  let member (i : value) (set : t) : bool = Set.has set i

  let diff (set1 : t) (set2 : t) : t = Set.diff set1 set2

  let isEmpty (s : t) : bool = Set.isEmpty s

  let toList (s : t) : value list = Set.toList s

  let ofList (s : value list) : t = s |> Array.of_list |> Set.fromArray

  let add = Set.add

  let union = Set.union

  let empty = Set.empty
end

module StrSet = struct
  module Set = Belt.Set.String

  type t = Set.t

  type value = Set.value

  let fromList (l : value list) : t = l |> Belt.List.toArray |> Set.fromArray

  let member (i : value) (set : t) : bool = Set.has set i

  let diff (set1 : t) (set2 : t) : t = Set.diff set1 set2

  let isEmpty (s : t) : bool = Set.isEmpty s

  let toList (s : t) : value list = Set.toList s

  let ofList (s : value list) : t = s |> Array.of_list |> Set.fromArray

  let add = Set.add

  let union = Set.union

  let empty = Set.empty
end

module StrDict = struct
  module Map = Belt.Map.String

  type key = Map.key

  type 'value t = 'value Map.t

  let toList = Map.toList

  let empty = Map.empty

  let fromList (l : ('key * 'value) list) : 'value t =
    l |> Belt.List.toArray |> Map.fromArray


  let get (k : key) (v : 'value t) : 'value option = Map.get v k

  let insert (k : key) (v : 'value) (map : 'value t) : 'value t =
    Map.set map k v


  let keys m : key list = Map.keysToArray m |> Belt.List.fromArray

  let update (k : key) (fn : 'v option -> 'v option) (map : 'value t) :
      'value t =
    Map.update map k fn


  let map = Map.map

  (* Js.String.make gives us "[object Object]", so we actually want our own
     toString. Not perfect, but slightly nicer (e.g., for App.ml's
     DisplayAndReportHttpError, info's values are all strings, which this
     handles) *)
  let toString d =
    d
    |> toList
    |> List.map (fun (k, v) -> "\"" ^ k ^ "\": \"" ^ Js.String.make v ^ "\"")
    |> String.join ", "
    |> fun s -> "{" ^ s ^ "}"
end

module IntDict = struct
  module Map = Belt.Map.Int

  type key = Map.key

  type 'value t = 'value Map.t

  let toList = Map.toList

  let empty = Map.empty

  let fromList (l : ('key * 'value) list) : 'value t =
    l |> Belt.List.toArray |> Map.fromArray


  let get (k : key) (v : 'value t) : 'value option = Map.get v k

  let insert (k : key) (v : 'value) (map : 'value t) : 'value t =
    Map.set map k v


  let update (k : key) (fn : 'v option -> 'v option) (map : 'value t) :
      'value t =
    Map.update map k fn


  let keys m : key list = Map.keysToArray m |> Belt.List.fromArray

  let map = Map.map

  let fromStrDict ~(default : key) (d : 'value StrDict.t) : 'value t =
    d
    |> StrDict.toList
    |> List.map (fun (k, v) ->
           (k |> String.toInt |> Result.withDefault default, v) )
    |> Belt.List.toArray
    |> Map.fromArray
end

module Html = struct
  include Tea.Html

  type 'a html = 'a Vdom.t
end

module Native = struct
  type size =
    { width : int
    ; height : int }

  type rect =
    { id : string
    ; top : int
    ; left : int
    ; right : int
    ; bottom : int }

  type list_pos =
    { atoms : rect list
    ; nested : rect list }

  type jsRect = string Js.Dict.t

  type jsRectArr = jsRect array Js.Dict.t

  type jsPointObj = int Js.Dict.t

  exception NativeCodeError of string

  module Ext = struct
    let window : Dom.window = [%bs.raw "window"]

    external getWidth : Dom.window -> int = "innerWidth" [@@bs.get]

    external getHeight : Dom.window -> int = "innerHeight" [@@bs.get]

    external astPositions : string -> jsRectArr = "positions"
      [@@bs.val] [@@bs.scope "window", "Dark", "ast"]

    type blankOrId = string

    external findCaretPos : unit -> jsPointObj = "findCaretPos"
      [@@bs.val] [@@bs.scope "window", "Dark", "caret"]

    external findLogicalOffset :
      blankOrId -> int -> int -> int
      = "findLogicalOffset"
      [@@bs.val] [@@bs.scope "window", "Dark", "caret"]

    external moveCaretLeft : unit -> bool = "moveCaretLeft"
      [@@bs.val] [@@bs.scope "window", "Dark", "caret"]

    external moveCaretRight : unit -> bool = "moveCaretRight"
      [@@bs.val] [@@bs.scope "window", "Dark", "caret"]
  end

  module Window = struct
    let size () : size =
      {width = Ext.getWidth Ext.window; height = Ext.getHeight Ext.window}
  end

  module Random = struct
    let random () : int = Js_math.random_int 0 2147483647
  end

  module Size = struct
    let _convert (key : string) (pos : jsRectArr) : rect list =
      Js.Dict.unsafeGet pos key
      |> List.fromArray
      |> List.map (fun jsRect ->
             { id = Js.Dict.unsafeGet jsRect "id"
             ; top = int_of_string (Js.Dict.unsafeGet jsRect "top")
             ; left = int_of_string (Js.Dict.unsafeGet jsRect "left")
             ; right = int_of_string (Js.Dict.unsafeGet jsRect "right")
             ; bottom = int_of_string (Js.Dict.unsafeGet jsRect "bottom") } )


    let positions (tlid : string) : list_pos =
      let pos = Ext.astPositions tlid in
      {atoms = _convert "atoms" pos; nested = _convert "nested" pos}
  end

  module Location = struct
    external queryString : string = "search"
      [@@bs.val] [@@bs.scope "window", "location"]

    external hashString : string = "hash"
      [@@bs.val] [@@bs.scope "window", "location"]

    (* TODO write string query parser *)
  end
end

module Window = struct
  module OnResize = struct
    let decode =
      let open Tea.Json.Decoder in
      let decodeDetail =
        map2
          (fun width height -> (width, height))
          (field "width" int)
          (field "height" int)
      in
      map (fun msg -> msg) (field "detail" decodeDetail)


    let listen ~key tagger = registerGlobal "windowResize" key tagger decode
  end

  module OnFocusChange = struct
    let decode =
      let open Tea.Json.Decoder in
      map (fun visible -> visible) (field "detail" bool)


    let listen ~key tagger =
      registerGlobal "windowFocusChange" key tagger decode
  end
end

module Rollbar = struct
  external rollbarError :
    string -> string Js.nullable -> 'a -> 'a -> Js.Json.t -> unit
    = "error"
    [@@bs.val] [@@bs.scope "window", "Rollbar"]

  let send (msg : string) (url : string option) (custom : Js.Json.t) : unit =
    let url = Js.Nullable.fromOption url in
    rollbarError msg url Js.null Js.null custom
end

module DisplayClientError = struct
  let decode =
    let open Tea.Json.Decoder in
    map (fun msg -> msg) (field "detail" string)


  let listen ~key tagger = registerGlobal "displayError" key tagger decode
end

module OnWheel = struct
  let decode =
    let open Tea.Json.Decoder in
    map2 (fun dX dY -> (dX, dY)) (field "deltaX" int) (field "deltaY" int)


  let listen ~key tagger = registerGlobal "wheel" key tagger decode
end

module DarkMouse = struct
  let moves ~key tagger =
    registerGlobal "mousemove" key tagger Tea.Mouse.position
end
