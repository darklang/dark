module ResizeArray

type T<'v> = ResizeArray<'v>
let empty () = T()

let iter (f : 'v -> unit) (l : T<'v>) : unit =
  FSharpx.Collections.ResizeArray.iter f l

let map (f : 'v -> 'v2) (l : T<'v>) : T<'v2> =
  FSharpx.Collections.ResizeArray.map f l

let append (v : 'v) (list : T<'v>) : unit = list.Add(v)

let toList (l : T<'v>) : List<'v> = FSharpx.Collections.ResizeArray.toList l

let toSeq (l : T<'v>) : seq<'v> = FSharpx.Collections.ResizeArray.toSeq l
