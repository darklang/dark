module List

let splitLast (l : List<'a>) : Option<List<'a> * 'a> =
  match List.rev l with
  | [] -> None
  | head :: tail -> Some(List.rev tail, head)
