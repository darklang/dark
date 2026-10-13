/// The write secret for this instance's relay, attached at the transport.
///
/// The sync transport (`http*UnsafeBytes*`) reaches origins the guarded client bans;
/// WHO may call it is `requireBundledCaller`'s job and WHERE it may reach is the
/// instance policy's, so this module no longer keeps an origin allowlist. What
/// remains is the one thing neither of those can do: attach the stored write secret
/// to a request, F#-side, so the credential never reaches Dark -- where any code the
/// CLI runs, including a package pulled from a peer, could read it. Dark still
/// decides WHEN to push.
module LibExecution.UnguardedOrigins

/// scheme://host:port, lowercased, with the default port made explicit so `http://h`
/// and `http://h:80` agree.
let originOf (url : string) : string option =
  match System.Uri.TryCreate(url, System.UriKind.Absolute) with
  | true, uri ->
    let scheme = uri.Scheme.ToLowerInvariant()
    let host = uri.Host.ToLowerInvariant()
    Some $"{scheme}://{host}:{uri.Port}"
  | _ -> None

/// The write secret by origin. A hook because this module sits below `LibDB`,
/// where the config lives.
let mutable private secretLookup : string -> string option = fun _ -> None

let setSecretLookup (lookup : string -> string option) : unit =
  secretLookup <- lookup

/// The `Authorization` header for <param url>, when a secret is stored for this
/// instance's relay and the url is one of its origins. Empty otherwise, so an
/// ordinary request carries nothing.
///
/// A header rather than a query parameter: a query string ends up in access logs and
/// proxy traces, a poor place for the one string that grants write access.
let authHeadersFor (url : string) : List<string * string> =
  match originOf url with
  | None -> []
  | Some origin ->
    match secretLookup origin with
    | Some secret when secret <> "" -> [ "Authorization", $"Bearer {secret}" ]
    | _ -> []
