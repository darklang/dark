module HashSet

type HashSet<'v> = System.Collections.Generic.HashSet<'v>

let add (v : 'v) (s : HashSet<'v>) : unit =
  let (_ : bool) = s.Add v
  ()

let empty () : HashSet<'v> = System.Collections.Generic.HashSet<'v>()

let toList (d : HashSet<'v>) : List<'v> =
  seq {
    let mutable e = d.GetEnumerator()

    while e.MoveNext() do
      yield e.Current
  }
  |> Seq.toList
