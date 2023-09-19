module Map

let mergeFavoringRight (m1 : Map<'k, 'v>) (m2 : Map<'k, 'v>) : Map<'k, 'v> =
  FSharpPlus.Map.union m2 m1

let mergeFavoringLeft (m1 : Map<'k, 'v>) (m2 : Map<'k, 'v>) : Map<'k, 'v> =
  FSharpPlus.Map.union m1 m2

let ofNEList (l : NEList.NEList<'k * 'v>) : Map<'k, 'v> =
  NEList.fold (fun m (k, v) -> Map.add k v m) Map.empty l

let fromListBy (f : 'v -> 'k) (l : List<'v>) : Map<'k, 'v> =
  List.fold (fun (m : Map<'k, 'v>) v -> m.Add(f v, v)) Map.empty l

let fromList (l : List<'k * 'v>) : Map<'k, 'v> = Map.ofList l

let filterWithIndex (f : 'a -> 'b -> bool) (m : Map<'a, 'b>) : Map<'a, 'b> =
  Map.filter f m

let filter (f : 'v -> bool) (m : Map<'k, 'v>) : Map<'k, 'v> =
  Map.filter (fun _ v -> f v) m

let keys (map : Map<'k, 'v>) : List<'k> =
  seq {
    for KeyValue(key, _) in map do
      yield key
  }
  |> List.ofSeq

let values (map : Map<'k, 'v>) : List<'v> =
  seq {
    for KeyValue(_, value) in map do
      yield value
  }
  |> List.ofSeq

let all (f : 'v -> bool) (m : Map<'k, 'v>) : bool = Map.forall (fun _ v -> f v) m

let map (f : 'a -> 'b) (m : Map<'k, 'a>) : Map<'k, 'b> = Map.map (fun _ v -> f v) m
let mapWithIndex (f : 'k -> 'a -> 'b) (m : Map<'k, 'a>) : Map<'k, 'b> = Map.map f m

let get (k : 'k) (m : Map<'k, 'v>) : Option<'v> = Map.find k m

let findUnsafe (k : 'k) (m : Map<'k, 'v>) : 'v = Map.find k m

let find (k : 'k) (m : Map<'k, 'v>) : Option<'v> = Map.find k m


let update
  (key : 'a)
  (f : Option<'b> -> Option<'b>)
  (m : Map<'a, 'b>)
  : Map<'a, 'b> =
  Map.change key f m

let iterWithIndex (f : 'k -> 'v -> unit) (m : Map<'k, 'v>) : unit = Map.iter f m
