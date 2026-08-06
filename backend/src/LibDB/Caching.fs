module LibDB.Caching

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Collections.Concurrent

open Prelude


/// Every cache built by `withCache`, so they can all be dropped at once.
///
/// A cache here answers a question about the STORE, and the store changes underneath
/// a long-lived process: the REPL is one process for a whole session, and so are the
/// LSP and any daemon. Without a way to drop them, the first answer a process gives
/// for a name is the only answer it will ever give -- which meant authoring a fn,
/// running it, fixing it and running it again showed you the version you just
/// replaced.
///
/// A one-shot `dark` invocation never noticed, because it exits before the second
/// question.
let private clearActions = ConcurrentBag<unit -> unit>()

/// Sign a cache up to be dropped by `invalidateAll`.
///
/// `withCache` does this for itself. Hand-rolled caches (the Harmful set) have to
/// say so, and the one that existed had an `invalidate` function written for exactly
/// this hazard that nothing ever called -- so marking a fn harmful in a long-lived
/// process didn't take effect until you restarted it.
let register (clear : unit -> unit) : unit = clearActions.Add clear

/// Drop every cache. Called whenever ops fold or `locations` is written directly.
///
/// Blunt on purpose. These caches all derive from the same tables, an author touches
/// an unpredictable set of names (propagation repoints things you didn't name), and
/// re-reading a row from SQLite is cheap next to answering with a version that is no
/// longer what the name means.
let invalidateAll () : unit =
  for clear in clearActions do
    clear ()

let withCache (f : 'key -> Ply<Option<'value>>) =
  // Holds the `Some` wrapper rather than the value, so a hit hands back the same option object instead
  // of building a fresh one. `None` still isn't cached: a lookup that missed the store might succeed
  // later, and this cache has no way to hear about it.
  let cache = ConcurrentDictionary<'key, Option<'value>>()
  // Registered for `invalidateAll`. Without this the cache would outlive the fold that changed what the
  // answer is -- which is the whole reason this file has an invalidation list.
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
