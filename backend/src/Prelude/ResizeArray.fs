module ResizeArray

type T<'v> = ResizeArray<'v>
let empty () = T()

let iter (f : 'v -> unit) (l : T<'v>) : unit =
  for v in l do
    f v

let map (f : 'v -> 'v2) (l : T<'v>) : T<'v2> =
  let result = T<'v2>(l.Count)
  for v in l do
    result.Add(f v)
  result

let append (v : 'v) (list : T<'v>) : unit = list.Add(v)

let toList (l : T<'v>) : List<'v> = List.ofSeq l

let toSeq (l : T<'v>) : seq<'v> = l :> seq<'v>
