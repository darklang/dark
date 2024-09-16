module Option

let unwrap (default' : 'a) (t : Option<'a>) : 'a =
  match t with
  | None -> default'
  | Some value -> value


let getUnsafe (msg : string) (metadata : Exception.Metadata) (t : Option<'a>) : 'a =
  match t with
  | None -> Exception.raiseInternal msg metadata
  | Some value -> value
