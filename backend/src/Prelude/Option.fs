module Option

let unwrap (default' : 'a) (t : Option<'a>) : 'a =
  match t with
  | None -> default'
  | Some value -> value
