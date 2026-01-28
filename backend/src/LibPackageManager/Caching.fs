module LibPackageManager.Caching

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Collections.Concurrent

open Prelude


/// All caches that have been created, so we can clear them on edit
let private allCaches = ResizeArray<unit -> unit>()

/// Clear all package manager caches. Call this after edits are saved.
let clearAllCaches () : unit =
  for clearFn in allCaches do
    clearFn ()


let withCache (f : 'key -> Ply<Option<'value>>) =
  let cache = ConcurrentDictionary<'key, 'value>()

  // Register this cache for clearing
  allCaches.Add(fun () -> cache.Clear())

  fun (key : 'key) ->
    uply {
      let mutable cached = Unchecked.defaultof<'value>
      let inCache = cache.TryGetValue(key, &cached)
      if inCache then
        return Some cached
      else
        //debuG "missed" key
        let! result = f key
        match result with
        | Some v -> cache.TryAdd(key, v) |> ignore<bool>
        | None -> ()
        return result
    }
