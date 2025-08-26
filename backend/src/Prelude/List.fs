module List

let splitLast (l : List<'a>) : Option<List<'a> * 'a> =
  match List.rev l with
  | [] -> None
  | head :: tail -> Some(List.rev tail, head)

let filterMap (f : 'a -> Option<'b>) (xs : List<'a>) : List<'b> = List.choose f xs

let mapWithIndex (f : int -> 'a -> 'b) (l : List<'a>) : List<'b> = List.mapi f l

let filterWithIndex (f : int -> 'a -> bool) (l : List<'a>) : List<'a> =
  l
  |> List.fold
    (fun (i, acc) x -> if f i x then (i + 1, x :: acc) else (i + 1, acc))
    (0, [])
  |> snd

let foldWithIndex (f : int -> 'b -> 'a -> 'b) (initial : 'b) (l : List<'a>) : 'b =
  List.fold
    (fun ((i, accum) : (int * 'b)) (item : 'a) -> (i + 1, f i accum item))
    (0, initial)
    l
  |> Tuple2.second


let any (f : 'a -> bool) (l : List<'a>) : bool = List.exists f l

let all (f : 'a -> bool) (l : List<'a>) : bool = List.forall f l

let find (f : 'a -> bool) (l : List<'a>) : 'a option = List.tryFind f l

let foldRight (f : 'b -> 'a -> 'b) (initial : 'b) (l : List<'a>) : 'b =
  List.foldBack (fun item accum -> f accum item) l initial

let intersperse (sep : 'a) (l : List<'a>) : List<'a> =
  match l with
  | [] -> []
  | [ x ] -> [ x ]
  | x :: rest -> x :: foldRight (fun acc x -> sep :: x :: acc) [] rest : 'a list

let flatten (l : List<List<'a>>) : List<'a> = List.concat l

let count (f : 'a -> bool) (l : List<'a>) : int = List.filter f l |> List.length

let initial (l : List<'a>) : List<'a> =
  match List.rev l with
  | [] -> []
  | _ :: tail -> List.rev tail

let groupBy (f : 'a -> 'b) (l : List<'a>) : Map<'b, List<'a>> =
  List.groupBy f l |> Map

let head (l : List<'a>) : Option<'a> = List.tryHead l

let reverse (l : List<'a>) : List<'a> = List.rev l

let last (l : List<'a>) : Option<'a> = List.tryLast l

let join (sep : string) (l : List<string>) : string = String.concat sep l

let rec range from to' = if from >= to' then [] else from :: range (from + 1) to'

let fromArray (arr : array<'a>) : List<'a> = List.ofArray arr

let getAt (index : int) (l : List<'a>) : Option<'a> =
  if index < 0 then None else List.tryItem index l

let rec zipUntilEitherEnds list1 list2 =
  match list1, list2 with
  | x :: xs, y :: ys -> (x, y) :: zipUntilEitherEnds xs ys
  | _, _ -> []
