type 'a t = 'a option ref

let create () : 'a t =
  ref None

let cache1 keyFn expensiveFn =
  let cache = create () in
  (fun arg ->
     let cacheKey = keyFn arg in
     match !cache with
     | Some (key, result) when cacheKey = key -> result
     | _ ->
       let result = expensiveFn arg in
       cache := Some (cacheKey, result);
       result)


