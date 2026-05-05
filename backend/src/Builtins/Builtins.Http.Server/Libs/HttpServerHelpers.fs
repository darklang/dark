/// Stateless helper functions used by the HTTP server's per-request
/// pipeline — body reading, header extraction, URL canonicalization,
/// header injection, request logging.
///
/// Pulled out of `Libs/HttpServer.fs` so the listener-loop and
/// per-request handler can each focus on their own concern.
module Builtins.Http.Server.Libs.HttpServerHelpers

open System.IO
open System.Net
open System.Threading.Tasks

open Prelude


/// Default request body cap (30 MB).
let defaultMaxBodyBytes : int64 = 30L * 1024L * 1024L

/// HSTS header value matching the historical default for HTTP services.
let hstsHeaderValue = "max-age=31536000; includeSubDomains; preload"


/// Read the request body up to `maxBytes`. Returns `Error()` when the
/// declared `Content-Length` already exceeds the cap, OR when the actual
/// stream grows past it (covers chunked encoding where `Content-Length` is
/// absent / -1).
let readRequestBodyWithLimit
  (req : HttpListenerRequest)
  (maxBytes : int64)
  : Task<Result<byte[], unit>> =
  task {
    if req.ContentLength64 > maxBytes then
      return Error()
    else
      use ms = new MemoryStream()
      let buffer = Array.zeroCreate 8192
      let mutable totalRead = 0L
      let mutable keepReading = true
      let mutable overLimit = false
      while keepReading && not overLimit do
        let! n = req.InputStream.ReadAsync(buffer, 0, buffer.Length)
        if n = 0 then
          keepReading <- false
        else
          totalRead <- totalRead + int64 n
          if totalRead > maxBytes then
            overLimit <- true
          else
            do! ms.WriteAsync(buffer, 0, n)
      if overLimit then return Error() else return Ok(ms.ToArray())
  }


/// Flatten HttpListener's NameValueCollection of headers into the (key, value)
/// list shape that Builtins.Http.Server.Http.Request expects. A single header key
/// with multiple values becomes multiple entries.
let extractHeaders (req : HttpListenerRequest) : List<string * string> =
  let headers = ResizeArray<string * string>()
  for key in req.Headers.AllKeys do
    if not (isNull key) then
      let values = req.Headers.GetValues(key)
      if not (isNull values) then
        for value in values do
          headers.Add(key, value)
  // CLEANUP once Dark-side handlers stop relying on x-http-method.
  ("x-http-method", req.HttpMethod) :: List.ofSeq headers


/// If `X-Forwarded-Proto: https` is present, rewrite the URL's scheme to
/// `https://` and port to 443. Matches the canonicalize-URL convention used
/// behind a TLS-terminating load balancer. Lower-cases the lookup since the
/// request-shape converter lowercases header keys before this.
let canonicalizeUrlFromForwardedProto
  (url : string)
  (headers : List<string * string>)
  : string =
  let isHttps =
    headers
    |> List.exists (fun (k, v) ->
      String.equalsCaseInsensitive k "x-forwarded-proto"
      && String.equalsCaseInsensitive v "https")
  if isHttps then
    try
      let uri = System.UriBuilder(url)
      uri.Port <- 443
      uri.Scheme <- "https"
      string uri.Uri
    with _ ->
      url
  else
    url


/// Add `Server: darklang` and HSTS to the response headers unless the handler
/// already set them. Header keys may be lowercased (the response-shape
/// converter lowercases them on the way out), so we compare case-insensitively.
let maybeInjectStandardHeaders
  (inject : bool)
  (headers : List<string * string>)
  : List<string * string> =
  if not inject then
    headers
  else
    let hasKey name =
      headers |> List.exists (fun (k, _) -> String.equalsCaseInsensitive k name)
    let extras =
      [ if not (hasKey "server") then ("Server", "darklang")
        if not (hasKey "strict-transport-security") then
          ("Strict-Transport-Security", hstsHeaderValue) ]
    headers @ extras


/// Emit a per-request log line + telemetry event. Caller passes the
/// already-set status code (post-handler) and the start timestamp.
let logRequest
  (ctx : HttpListenerContext)
  (status : int)
  (started : System.DateTime)
  : unit =
  let durationMs = (System.DateTime.UtcNow - started).TotalMilliseconds |> int64
  let methodStr = ctx.Request.HttpMethod
  let pathAndQuery =
    try
      ctx.Request.Url.PathAndQuery
    with _ ->
      "?"
  print $"[HttpServer] {methodStr} {pathAndQuery} {status} {durationMs}ms"
  Telemetry.event
    "httpserver.request"
    [ "method", methodStr
      "path", pathAndQuery
      "status", string status
      "duration_ms", string durationMs ]
