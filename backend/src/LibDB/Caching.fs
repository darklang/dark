module LibDB.Caching

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Collections.Concurrent

open Prelude


/// Every cache built by `withCache`, so they can all be dropped at once.
///
/// A cache here answers a question about the STORE, and the store changes underneath a
/// long-lived process (the REPL, the LSP, any daemon). Without a way to drop them, the
/// first answer a process gives for a name is the only answer it will ever give. A
/// one-shot `dark` invocation never notices, because it exits before the second question.
let private clearActions = ConcurrentBag<unit -> unit>()

/// Sign a cache up to be dropped by `invalidateAll`. `withCache` does this for itself;
/// hand-rolled caches (the Harmful set) have to say so, or they go stale for the life of
/// the process.
let register (clear : unit -> unit) : unit = clearActions.Add clear

/// Drop every cache. Called whenever ops fold or `locations` is written directly.
///
/// Blunt on purpose. These caches all derive from the same tables, an author touches an
/// unpredictable set of names (propagation repoints things you didn't name), and re-reading
/// a row from SQLite is cheap next to answering with a stale version.
let invalidateAll () : unit =
  for clear in clearActions do
    clear ()

let withCache (f : 'key -> Ply<Option<'value>>) =
  // Holds the `Some` wrapper rather than the value, so a hit hands back the same option object instead
  // of building a fresh one. `None` still isn't cached: a lookup that missed the store might succeed
  // later, and this cache has no way to hear about it.
  let cache = ConcurrentDictionary<'key, Option<'value>>()
  clearActions.Add(fun () -> cache.Clear())

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
