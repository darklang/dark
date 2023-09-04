module Dictionary

type T<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

let get (k : 'k) (t : T<'k, 'v>) : Option<'v> = FSharpPlus.Dictionary.tryGetValue k t

let containsKey (k : 'k) (t : T<'k, 'v>) : bool = t.ContainsKey k

let add (k : 'k) (v : 'v) (d : T<'k, 'v>) : unit =
  d[k] <- v
  ()

let empty () : T<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>()

let keys = FSharpPlus.Dictionary.keys
let values = FSharpPlus.Dictionary.values

let toList (d : T<'k, 'v>) : List<'k * 'v> =
  seq {
    let mutable e = d.GetEnumerator()

    while e.MoveNext() do
      yield (e.Current.Key, e.Current.Value)
  }
  |> Seq.toList

let fromList (l : List<'k * 'v>) : T<'k, 'v> =
  let result = empty ()
  List.iter (fun (k, v) -> result[k] <- v) l
  result

let toMap (d : T<'k, 'v>) : Map<'k, 'v> = d |> toList |> Map.ofList
