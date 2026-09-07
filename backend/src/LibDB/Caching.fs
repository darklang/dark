module LibDB.Caching

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Collections.Concurrent

open Prelude


/// Bumped by every statement that writes the store (see `Sqlite`).
///
/// The caches below remember lookups that FAILED, and a failed lookup is only
/// worth remembering while the store it failed against is unchanged. Rather than
/// ask each cache to subscribe to the writes that could affect it, the whole
/// negative side is invalidated whenever anything is written at all. That is
/// coarse on purpose: writes are rare next to lookups, and a correctness rule of
/// "any write, all negatives" is one nobody has to remember to extend when a new
/// write path appears.
let mutable private generation = 0

let storeGeneration () : int = System.Threading.Volatile.Read &generation

let bumpStoreGeneration () : unit =
  System.Threading.Interlocked.Increment &generation |> ignore<int>


/// Memoise a store lookup, hits and misses both.
///
/// `name` only feeds telemetry (`cache.<name>.hit` / `.hitNone` / `.missNone` /
/// `.missSome`), which is how the hit rate on each of these becomes a number
/// instead of an assumption. Gated, so it costs nothing when telemetry is off.
let withCacheNamed (name : string) (f : 'key -> Ply<Option<'value>>) =
  // Holds the `Some` wrapper rather than the value, so a hit hands back the same option object instead
  // of building a fresh one.
  let cache = ConcurrentDictionary<'key, Option<'value>>()

  // Misses are kept apart from hits, because they are the ones that expire. Name
  // resolution asks about candidate locations that mostly do not exist (a `List.map`
  // in a module tries the module, then each enclosing one, then the root), so on a
  // fluid testfile the failures outnumber the successes three to one, and before this
  // every one of them was a fresh SQLite query.
  let negCache = ConcurrentDictionary<'key, unit>()
  let mutable negGeneration = storeGeneration ()

  fun (key : 'key) ->
    let gen = storeGeneration ()
    if gen <> negGeneration then
      negCache.Clear()
      negGeneration <- gen

    let mutable cached = Unchecked.defaultof<Option<'value>>
    if cache.TryGetValue(key, &cached) then
      // Deliberately outside the computation expression. Once a script is warm nearly every call lands
      // here, and entering the builder to hand over a value already in hand costs more than the lookup.
      if Telemetry.isEnabled () then Telemetry.count $"cache.{name}.hit"
      Ply cached
    elif negCache.ContainsKey key then
      if Telemetry.isEnabled () then Telemetry.count $"cache.{name}.hitNone"
      Ply None
    else
      uply {
        let! result = f key
        match result with
        | Some _ ->
          if Telemetry.isEnabled () then Telemetry.count $"cache.{name}.missSome"
          cache.TryAdd(key, result) |> ignore<bool>
        | None ->
          if Telemetry.isEnabled () then Telemetry.count $"cache.{name}.missNone"
          // Only if the store hasn't moved under us while the lookup was in flight.
          if storeGeneration () = negGeneration then
            negCache.TryAdd(key, ()) |> ignore<bool>
        return result
      }

let withCache (f : 'key -> Ply<Option<'value>>) = withCacheNamed "anon" f


/// Same, for a lookup that answers with a list. The empty list is the negative case:
/// a hash with no location on this branch chain is exactly the answer that changes
/// when someone publishes, and exactly the answer worth remembering until they do.
let withCacheListNamed (name : string) (f : 'key -> Ply<List<'value>>) =
  let cache = ConcurrentDictionary<'key, List<'value>>()
  let negCache = ConcurrentDictionary<'key, unit>()
  let mutable negGeneration = storeGeneration ()

  fun (key : 'key) ->
    let gen = storeGeneration ()
    if gen <> negGeneration then
      negCache.Clear()
      negGeneration <- gen

    let mutable cached = Unchecked.defaultof<List<'value>>
    if cache.TryGetValue(key, &cached) then
      if Telemetry.isEnabled () then Telemetry.count $"cache.{name}.hit"
      Ply cached
    elif negCache.ContainsKey key then
      if Telemetry.isEnabled () then Telemetry.count $"cache.{name}.hitNone"
      Ply []
    else
      uply {
        let! result = f key
        match result with
        | [] ->
          if Telemetry.isEnabled () then Telemetry.count $"cache.{name}.missNone"
          if storeGeneration () = negGeneration then
            negCache.TryAdd(key, ()) |> ignore<bool>
        | _ ->
          if Telemetry.isEnabled () then Telemetry.count $"cache.{name}.missSome"
          cache.TryAdd(key, result) |> ignore<bool>
        return result
      }
