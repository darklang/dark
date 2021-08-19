module Tablecloth.Set

// A collection of unique values

type t<'a when 'a : comparison> = Set<'a>

let empty = Set.empty

let singleton (element : 'a) : 'a t = Set.singleton element

let fromArray (elements : 'a array) : 'a t = Set.ofArray elements

let from_array s = fromArray s

let fromList (elements : 'a list) : 'a t = Set.ofList elements

let from_list s = fromList s

let length s = Set.count s

let isEmpty s = Set.isEmpty s

let is_empty s = isEmpty s

let includes (v : 'a) (s : 'a t) = Set.contains v s

// let ( .?{} ) (set : ('element, _) t) (element : 'element) : bool =
//   includes set element

let add v s = Set.add s v

let remove v s = Set.remove s v

let difference s1 s2 = Set.difference s1 s2

let intersection s1 s2 = Set.intersect s1 s2

let union s1 s2 = Set.union s1 s2

let filter f s = Set.filter f s

let partition f s = Set.partition f s

let find f s =
  Set.fold
    (fun accum elem ->
      match accum with
      | Some v -> Some v
      | None -> if f elem then Some elem else None)
    None
    s

let all f s = Set.forall f s

let any f s = Set.exists f s

let forEach f s = Set.iter f s

let for_each f s = forEach f s

let fold (initial : 'b) (f : 'b -> 'a -> 'b) (s : 'a t) : 'b = Set.fold f initial s

let toArray s = Set.toArray s

let to_array s = toArray s

let toList s = Set.toList s

let to_list s = toList s
