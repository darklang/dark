/// Non-empty list
module NEList

type NEList<'a> = { head : 'a; tail : List<'a> }

let length (l : NEList<'a>) : int = 1 + List.length l.tail
let toList (l : NEList<'a>) : List<'a> = l.head :: l.tail

let iter (f : 'a -> unit) (l : NEList<'a>) : unit =
  f l.head
  List.iter f l.tail

let iter2 (f : 'a -> 'b -> unit) (l1 : NEList<'a>) (l2 : NEList<'b>) : unit =
  f l1.head l2.head
  List.iter2 f l1.tail l2.tail

let iteri2 (f : int -> 'a -> 'b -> unit) (l1 : NEList<'a>) (l2 : NEList<'b>) : unit =
  let rec loop (i : int) (l1 : List<'a>) (l2 : List<'b>) : unit =
    match l1, l2 with
    | [], [] -> ()
    | [], _
    | _, [] ->
      Exception.raiseInternal "NEList.iteri2: lists have different lengths" []
    | x1 :: xs1, x2 :: xs2 ->
      f i x1 x2
      loop (i + 1) xs1 xs2
  f 0 l1.head l2.head
  loop 1 l1.tail l2.tail

let map (f : 'a -> 'b) (l : NEList<'a>) : NEList<'b> =
  { head = f l.head; tail = List.map f l.tail }

let mapWithIndex (f : int -> 'a -> 'b) (l : NEList<'a>) : NEList<'b> =
  let rec loop (i : int) (l : List<'a>) : List<'b> =
    match l with
    | [] -> []
    | x :: xs -> f i x :: loop (i + 1) xs
  { head = f 0 l.head; tail = loop 1 l.tail }

let map2 (f : 'a -> 'b -> 'c) (l1 : NEList<'a>) (l2 : NEList<'b>) : NEList<'c> =
  let rec loop (l1 : List<'a>) (l2 : List<'b>) : List<'c> =
    match l1, l2 with
    | [], [] -> []
    | [], _
    | _, [] ->
      System.Console.WriteLine((l1, l2))
      Exception.raiseInternal "NEList.map2: lists have different lengths" []
    | x1 :: xs1, x2 :: xs2 -> f x1 x2 :: loop xs1 xs2
  { head = f l1.head l2.head; tail = loop l1.tail l2.tail }

let map2WithIndex
  (f : int -> 'a -> 'b -> 'c)
  (l1 : NEList<'a>)
  (l2 : NEList<'b>)
  : NEList<'c> =
  let rec loop (i : int) (l1 : List<'a>) (l2 : List<'b>) : List<'c> =
    match l1, l2 with
    | [], [] -> []
    | [], _
    | _, [] ->
      Exception.raiseInternal "NEList.map2WithIndex: lists have different lengths" []
    | x1 :: xs1, x2 :: xs2 -> f i x1 x2 :: loop (i + 1) xs1 xs2
  { head = f 0 l1.head l2.head; tail = loop 1 l1.tail l2.tail }

let ofList (head : 'a) (tail : List<'a>) : NEList<'a> = { head = head; tail = tail }

let ofListUnsafe
  (msg : string)
  (metadata : Exception.Metadata)
  (l : List<'a>)
  : NEList<'a> =
  match l with
  | [] -> Exception.raiseInternal msg metadata
  | head :: tail -> { head = head; tail = tail }

let ofSeq (head : 'a) (seq : seq<'a>) : NEList<'a> =
  { head = head; tail = List.ofSeq seq }

let singleton (head : 'a) : NEList<'a> = { head = head; tail = [] }

let doubleton (head : 'a) (tail : 'a) : NEList<'a> = { head = head; tail = [ tail ] }

let push (head : 'a) (l : NEList<'a>) : NEList<'a> =
  { head = head; tail = l.head :: l.tail }

let pushBack (tail : 'a) (l : NEList<'a>) : NEList<'a> =
  { head = l.head; tail = l.tail @ [ tail ] }

let prependList (list : List<'a>) (l : NEList<'a>) : NEList<'a> =
  match list with
  | [] -> l
  | head :: tail -> { head = head; tail = tail @ toList l }

let reverse (l : NEList<'a>) : NEList<'a> =
  match l |> toList |> List.rev with
  | [] -> Exception.raiseInternal "Unexpected empty NEList" []
  | head :: tail -> { head = head; tail = tail }

let find (f : 'a -> bool) (l : NEList<'a>) : Option<'a> =
  if f l.head then Some l.head else List.tryFind f l.tail

let forall (f : 'a -> bool) (l : NEList<'a>) : bool =
  f l.head && List.forall f l.tail

let forall2 (f : 'a -> 'b -> bool) (l1 : NEList<'a>) (l2 : NEList<'b>) : bool =
  f l1.head l2.head && List.forall2 f l1.tail l2.tail


let filter (f : 'a -> bool) (l : NEList<'a>) : List<'a> =
  if f l.head then l.head :: List.filter f l.tail else List.filter f l.tail

let filterMap (f : 'a -> Option<'b>) (l : NEList<'a>) : List<'b> =
  match f l.head with
  | Some v -> v :: List.choose f l.tail
  | None -> List.choose f l.tail

let last (l : NEList<'a>) : 'a =
  match l.tail with
  | [] -> l.head
  | _ -> List.last l.tail

let initial (l : NEList<'a>) : List<'a> =
  let rec listInitial (l : List<'a>) : List<'a> =
    match l with
    | [] -> []
    | [ _ ] -> [] // drop the last one
    | head :: head2 :: tail -> head :: listInitial (head2 :: tail)
  match l.tail with
  | [] -> [] // drop head
  | _ -> l.head :: (listInitial l.tail)

let fold (f : 'state -> 'a -> 'state) (initial : 'state) (l : NEList<'a>) : 'state =
  let state = f initial l.head
  List.fold f state l.tail

let splitLast (l : NEList<'a>) : (List<'a> * 'a) = initial l, last l

let zip (l1 : NEList<'a>) (l2 : NEList<'b>) : NEList<'a * 'b> =
  { head = (l1.head, l2.head); tail = List.zip l1.tail l2.tail }
