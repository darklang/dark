module List

let splitLast (l : List<'a>) : Option<List<'a> * 'a> =
  match List.rev l with
  | [] -> None
  | head :: tail -> Some(List.rev tail, head)

let filterMap (f : 'a -> Option<'b>) (xs : List<'a>) : List<'b> = List.choose f xs

let mapWithIndex (f : int -> 'a -> 'b) (l : List<'a>) : List<'b> =
  List.mapi f l

let any (f : 'a -> bool) (l : List<'a>) : bool = List.exists f l

let all (f : 'a -> bool) (l : List<'a>) : bool = List.forall f l

let find (f : 'a -> bool) (l : List<'a>) : 'a option = List.tryFind f l

let foldRight (f : 'b -> 'a -> 'b) (initial : 'b) (l : List<'a>) : 'b =
  List.foldBack (fun item accum -> f accum item) l initial

let intersperse (sep : 'a) (l : List<'a>) : List<'a> =
  (match l with
   | [] -> []
   | [ x ] -> [ x ]
   | x :: rest -> x :: foldRight (fun acc x -> sep :: x :: acc) [] rest : 'a list)

let flatten (l : List<List<'a>>) : List<'a> = List.concat l

let count (f : 'a -> bool) (l : List<'a>) : int = List.filter f l |> List.length

let initial (l : List<'a>) : List<'a> =
  match List.rev l with
  | [] -> []
  | _ :: tail -> List.rev tail
