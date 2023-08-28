module Map

let mergeFavoringRight (m1 : Map<'k, 'v>) (m2 : Map<'k, 'v>) : Map<'k, 'v> =
  FSharpPlus.Map.union m2 m1

let mergeFavoringLeft (m1 : Map<'k, 'v>) (m2 : Map<'k, 'v>) : Map<'k, 'v> =
  FSharpPlus.Map.union m1 m2

let ofNEList (l : NEList.NEList<'k * 'v>) : Map<'k, 'v> =
  NEList.fold (fun m (k, v) -> Map.add k v m) Map.empty l
