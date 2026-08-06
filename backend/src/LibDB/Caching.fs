module LibDB.Caching

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Collections.Concurrent

open Prelude


let withCache (f : 'key -> Ply<Option<'value>>) =
  // Holds the `Some` wrapper rather than the value, so a hit hands back the same option object instead
  // of building a fresh one. `None` still isn't cached: a lookup that missed the store might succeed
  // later, and this cache has no way to hear about it.
  let cache = ConcurrentDictionary<'key, Option<'value>>()
  fun (key : 'key) ->
    let mutable cached = Unchecked.defaultof<Option<'value>>
    if cache.TryGetValue(key, &cached) then
      // Deliberately outside the computation expression. Once a script is warm nearly every call lands
      // here, and entering the builder to hand over a value already in hand costs more than the lookup.
      Ply cached
    else
      uply {
        let! result = f key
        match result with
        | Some _ -> cache.TryAdd(key, result) |> ignore<bool>
        | None -> ()
        return result
      }
