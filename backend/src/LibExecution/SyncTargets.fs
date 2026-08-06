/// The origins this instance may reach with the SSRF guards OFF.
///
/// `httpGetUnsafeBytes` and its siblings exist so a peer's sync server behind loopback / RFC-1918 / a
/// tailnet is reachable at all, which the guarded client bans. They are registered in the GENERAL builtin
/// set, so any Dark the CLI runs could call them -- including a package pulled from a peer. That is a real
/// hole rather than a theoretical one: a function that arrives over sync, run by the person who pulled it,
/// could read services on that person's own loopback.
///
/// So the transport is unsafe only TOWARDS the places this instance was told to sync with. Everywhere else
/// it falls back to the guarded client, which bans internal addresses as usual. What counts as "told":
///
///   - the relay stored in local config, which is what `dark sync setup` / `connect` write, and
///   - a URL typed on the command line of the running process.
///
/// Pulled code can influence neither: it cannot rewrite argv, and writing config is itself a local act.
/// The config side is read through a hook rather than a snapshot, so a relay stored DURING this process
/// (which is exactly what `dark sync setup` does before its first push) counts immediately.
module LibExecution.SyncTargets

open Prelude

/// scheme://host:port, lowercased, with the default port made explicit so `http://h` and `http://h:80`
/// compare equal. None when the string is not a URL we can reason about, which is a refusal, not a pass.
let originOf (url : string) : string option =
  match System.Uri.TryCreate(url, System.UriKind.Absolute) with
  | true, uri ->
    let scheme = uri.Scheme.ToLowerInvariant()
    let host = uri.Host.ToLowerInvariant()
    if host = "" then None else Some $"{scheme}://{host}:{uri.Port}"
  | _ -> None

/// URLs named on the command line of this process. Set once at startup.
let mutable private fromArgv : Set<string> = Set.empty

/// Where the stored relay comes from. A hook because this module sits below `LibDB` and must not depend
/// on it, and because the answer can change within one process.
let mutable private storedTargets : unit -> string list = fun () -> []

let setFromArgv (urls : string seq) : unit =
  fromArgv <- urls |> Seq.choose originOf |> Set.ofSeq

let setStoredLookup (lookup : unit -> string list) : unit = storedTargets <- lookup

/// May the unsafe transport be used for this URL?
let isAllowed (url : string) : bool =
  match originOf url with
  | None -> false
  | Some origin ->
    Set.contains origin fromArgv
    || (storedTargets () |> List.choose originOf |> List.contains origin)

/// What a refusal should say. Names the origin asked for, since the caller may not have built the URL
/// itself, and points at the one thing that changes the answer. No "refused:" prefix: every caller here
/// wraps errors in its own words, and two prefixes read like two failures.
let refusalMessage (url : string) : string =
  let where = originOf url |> Option.defaultValue url
  $"{where} is not this instance's sync target, so the unguarded transport is not available for it. It is "
  + "only for the relay you configured (`dark sync setup`) or a URL you passed on the command line."
