/// The origins this instance may reach with the SSRF guards OFF.
///
/// `httpGetUnsafeBytes` turns the guards off so a server behind loopback, RFC-1918
/// or a tailnet is reachable at all, which the guarded client bans. It sits in the
/// GENERAL builtin set, so any Dark the CLI runs can call it, including code that
/// arrived from somewhere else. That is a real hole rather than a theoretical
/// one: a function someone else wrote, run here, can read services on this machine's
/// own loopback.
///
/// So the guards come off only towards origins this machine was pointed at, and a
/// url anywhere else is refused outright rather than fetched. Ordinary HTTP is
/// unaffected: `httpClientRequest` is a different builtin, and still guards all of
/// its own traffic.
///
/// What counts as pointed at:
///
///   - the peers stored locally, which is what `dark sync connect` writes, and
///   - a URL typed on the command line of the running process.
///
/// Neither is something fetched code can influence: it cannot rewrite argv, and
/// storing a peer is a local act. The stored side is read through a hook rather than
/// a snapshot, so an origin added DURING this process counts immediately -- which is
/// what `dark sync connect <url>` does before it probes that url.
module LibExecution.UnguardedOrigins

/// scheme://host:port, lowercased, with the default port made explicit so `http://h`
/// and `http://h:80` compare equal. None when the string is not a URL we can reason
/// about, which is a refusal, not a pass.
let originOf (url : string) : string option =
  match System.Uri.TryCreate(url, System.UriKind.Absolute) with
  | true, uri ->
    let scheme = uri.Scheme.ToLowerInvariant()
    let host = uri.Host.ToLowerInvariant()
    if host = "" then None else Some $"{scheme}://{host}:{uri.Port}"
  | _ -> None

/// URLs named on the command line of this process. Set once at startup.
let mutable private fromArgv : Set<string> = Set.empty

/// Where the stored origins come from. A hook because this module sits below `LibDB`
/// and must not depend on it, and because the answer can change within one process.
let mutable private stored : unit -> string list = fun () -> []

let setFromArgv (urls : string seq) : unit =
  fromArgv <- urls |> Seq.choose originOf |> Set.ofSeq

let setStoredLookup (lookup : unit -> string list) : unit = stored <- lookup

/// May the guards be skipped for this URL?
let isAllowed (url : string) : bool =
  match originOf url with
  | None -> false
  | Some origin ->
    Set.contains origin fromArgv
    || (stored () |> List.choose originOf |> List.contains origin)

/// What a refusal should say. Names the origin asked for, since the caller may not
/// have built the URL itself, and then both of the things that would change the
/// answer. No "refused:" prefix: every caller here wraps errors in its own words, and
/// two prefixes read like two failures.
let refusalMessage (url : string) : string =
  let where = originOf url |> Option.defaultValue url
  $"{where} is not a peer you sync with, so it cannot be fetched with the network "
  + "protections off. Add it with `dark sync connect`, or pass its URL on the "
  + "command line."
