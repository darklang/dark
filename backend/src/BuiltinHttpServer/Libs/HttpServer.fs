/// HTTP Server builtin
///
/// Starts a server on the given port and defers to Darklang code to
/// handle the request.
///
/// The F# side handles:
/// - Starting the HTTP listener
/// - Reading requests with a body-size limit
/// - Optional X-Forwarded-Proto → https URL canonicalization (production
///   behind a TLS-terminating proxy)
/// - Converting HTTP requests to Darklang Request type
/// - Calling the Darklang handler function
/// - Converting Darklang Response back to HTTP
/// - Optional auto-injection of standard headers (Server, HSTS) when the
///   handler didn't set them
/// - SIGINT (Ctrl+C) → drain in-flight + exit
///
/// All routing and handler management lives in Darklang.
///
/// Implementation note: this used to be ASP.NET Kestrel via
/// WebApplication.CreateBuilder. It was rewritten on top of
/// System.Net.HttpListener to drop the Microsoft.AspNetCore.App
/// FrameworkReference from the CLI's AOT graph. HttpListener is in
/// the BCL, AOT-clean, and gives us the same surface this file uses.
module BuiltinHttpServer.Libs.HttpServer

open System
open System.Net
open System.IO
open System.Threading
open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module Execution = LibExecution.Execution
module Blob = LibExecution.Blob
module Http = BuiltinHttpServer.Http


/// Default request body cap (30 MB), matching Kestrel's default.
let defaultMaxBodyBytes : int64 = 30L * 1024L * 1024L

/// HSTS header value matching the historical default for HTTP services.
let private hstsHeaderValue =
  "max-age=31536000; includeSubDomains; preload"


/// Execute a handler (named fn or lambda) against the given request. Lambdas
/// registered anywhere under this `exeState` (including the caller VM) are
/// findable here since `lambdaInstrCache` lives on `exeState`.
let private executeHandler
  (exeState : ExecutionState)
  (handler : Applicable)
  (arg : Dval)
  : Task<Dval> =
  task {
    let! result = Execution.executeApplicable exeState handler (NEList.singleton arg)
    match result with
    | Ok dval -> return dval
    | Error(rte, _callStack) ->
      // runtimeErrorToString returns Task<ExecutionResult> (Result-wrapped Dval).
      // Unwrap so we don't render the F# Result type signature into the body.
      let! errorStrResult = Execution.runtimeErrorToString exeState rte
      let errorStr =
        match errorStrResult with
        | Ok(DString s) -> s
        | Ok other -> string other
        | Error _ -> string rte
      return DString $"Handler error: {errorStr}"
  }


/// Read the request body up to `maxBytes`. Returns `Error()` when the
/// declared `Content-Length` already exceeds the cap, OR when the actual
/// stream grows past it (covers chunked encoding where `Content-Length` is
/// absent / -1).
let private readRequestBodyWithLimit
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
/// list shape that BuiltinHttpServer.Http.Request expects. A single header key
/// with multiple values becomes multiple entries (matching the prior ASP.NET
/// behavior).
let private extractHeaders (req : HttpListenerRequest) : List<string * string> =
  let headers = ResizeArray<string * string>()
  for key in req.Headers.AllKeys do
    if not (isNull key) then
      let values = req.Headers.GetValues(key)
      if not (isNull values) then
        for value in values do
          headers.Add(key, value)
  // x-http-method preserved from the prior ASP.NET implementation. CLEANUP
  // once Dark-side handlers stop relying on it.
  ("x-http-method", req.HttpMethod) :: List.ofSeq headers


/// If `X-Forwarded-Proto: https` is present, rewrite the URL's scheme to
/// `https://` and port to 443. Matches the canonicalize-URL convention used
/// behind a TLS-terminating load balancer. Lower-cases the lookup since the
/// request-shape converter lowercases header keys before this.
let private canonicalizeUrlFromForwardedProto
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
/// already set them. Header keys may be lowercased (LibHttpMiddleware lowercases
/// them on the way out), so we compare case-insensitively.
let private maybeInjectStandardHeaders
  (inject : bool)
  (headers : List<string * string>)
  : List<string * string> =
  if not inject then
    headers
  else
    let hasKey name =
      headers
      |> List.exists (fun (k, _) -> String.equalsCaseInsensitive k name)
    let extras =
      [ if not (hasKey "server") then ("Server", "darklang")
        if not (hasKey "strict-transport-security") then
          ("Strict-Transport-Security", hstsHeaderValue) ]
    headers @ extras


/// Process a single request: parse → dispatch → write response. Errors are
/// caught and returned as 500s so a single bad request can't kill the loop.
let private handleRequest
  (exeState : ExecutionState)
  (handler : Applicable)
  (maxBodyBytes : int64)
  (injectStandardHeaders : bool)
  (canonicalizeFromForwardedProto : bool)
  (ctx : HttpListenerContext)
  : Task<unit> =
  task {
    // Push a fresh blob-scope per request so ephemeral blobs minted by the
    // handler (and by any toHttpResponse materialisation) are reclaimed once
    // the response is sent. Without this, a long-lived http-server VM leaks
    // blobStore entries across requests.
    LibExecution.Blob.pushScope exeState
    try
      try
        let! bodyResult = readRequestBodyWithLimit ctx.Request maxBodyBytes
        match bodyResult with
        | Error() ->
          ctx.Response.StatusCode <- 413
          let msg = UTF8.toBytes "413 Payload Too Large"
          ctx.Response.ContentLength64 <- int64 msg.Length
          do! ctx.Response.OutputStream.WriteAsync(msg, 0, msg.Length)
        | Ok reqBody ->
          let reqHeaders = extractHeaders ctx.Request
          let rawUrl = ctx.Request.Url.ToString()
          let url =
            if canonicalizeFromForwardedProto then
              canonicalizeUrlFromForwardedProto rawUrl reqHeaders
            else
              rawUrl

          let requestDval = Http.Request.fromRequest exeState url reqHeaders reqBody

          let! result = executeHandler exeState handler requestDval
          let! response = Http.Response.toHttpResponse exeState result

          let respHeaders =
            maybeInjectStandardHeaders injectStandardHeaders response.headers

          ctx.Response.StatusCode <- response.statusCode
          for (key, value) in respHeaders do
            ctx.Response.Headers.Add(key, value)
          ctx.Response.ContentLength64 <- int64 response.body.Length
          do!
            ctx.Response.OutputStream.WriteAsync(
              response.body,
              0,
              response.body.Length
            )
      with ex ->
        ctx.Response.StatusCode <- 500
        let errorBytes = UTF8.toBytes $"Internal server error: {ex.Message}"
        ctx.Response.ContentLength64 <- int64 errorBytes.Length
        do! ctx.Response.OutputStream.WriteAsync(errorBytes, 0, errorBytes.Length)
    finally
      LibExecution.Blob.popScope exeState
      try
        ctx.Response.OutputStream.Close()
        ctx.Response.Close()
      with _ ->
        ()
  }


/// Run the HTTP listener loop until `cancellationToken` fires. Public so
/// tests can drive a per-test listener with their own CancellationToken.
/// Starts an HttpListener on `port`, dispatches every request to `handler`
/// via `handleRequest`, and exits when cancellation is requested. On
/// cancellation, calls `listener.Stop()` to unblock `GetContextAsync`.
let runListener
  (exeState : ExecutionState)
  (port : int64)
  (handler : Applicable)
  (maxBodyBytes : int64)
  (injectStandardHeaders : bool)
  (canonicalizeFromForwardedProto : bool)
  (cancellationToken : CancellationToken)
  : Task<unit> =
  task {
    let listener = new HttpListener()
    listener.Prefixes.Add($"http://*:{port}/")
    listener.Start()

    Telemetry.event
      "httpserver.listening"
      [ "port", string port
        "maxBodyBytes", string maxBodyBytes
        "injectStandardHeaders", string injectStandardHeaders
        "canonicalizeFromForwardedProto",
        string canonicalizeFromForwardedProto ]

    // Cancellation → listener.Stop() unblocks any pending GetContextAsync
    // call by raising HttpListenerException / ObjectDisposedException, which
    // the loop below catches and treats as normal exit.
    use _registration =
      cancellationToken.Register(fun () ->
        try
          listener.Stop()
        with _ ->
          ())

    while not cancellationToken.IsCancellationRequested do
      try
        let! ctx = listener.GetContextAsync()
        // Fire-and-forget: errors are caught inside handleRequest;
        // anything escaping that try/catch is also swallowed here.
        Task.Run(fun () ->
          task {
            try
              do!
                handleRequest
                  exeState
                  handler
                  maxBodyBytes
                  injectStandardHeaders
                  canonicalizeFromForwardedProto
                  ctx
            with _ ->
              ()
          }
          :> Task)
        |> ignore<Task>
      with
      | :? HttpListenerException -> ()
      | :? ObjectDisposedException -> ()

    try
      listener.Close()
    with _ ->
      ()

    Telemetry.event "httpserver.shutdown" [ "port", string port ]
  }


let fns () : List<BuiltInFn> =
  [ { name = fn "httpServerServe" 0
      typeParams = []
      parameters =
        [ Param.make "port" TInt64 "TCP port to listen on"
          Param.makeWithArgs
            "handler"
            // CLEANUP real types
            (TFn(NEList.singleton (TVariable "request"), TVariable "response"))
            "Handler function: request -> response"
            [ "request" ]
          Param.make
            "maxBodyBytes"
            TInt64
            "Maximum request body size in bytes (over-limit → 413)"
          Param.make
            "injectStandardHeaders"
            TBool
            "If true, auto-add `Server: darklang` and HSTS to responses unless the handler set them"
          Param.make
            "canonicalizeFromForwardedProto"
            TBool
            "If true, rewrite request.url to https:// when X-Forwarded-Proto: https is present" ]
      returnType = TUnit
      description =
        "Start an HTTP server. Calls handler for each request. Blocks until SIGINT."
      fn =
        (function
        | exeState,
          _,
          _,
          [ DInt64 port
            DApplicable handler
            DInt64 maxBodyBytes
            DBool injectStandardHeaders
            DBool canonicalizeFromForwardedProto ] ->
          uply {
            use _serveSpan =
              Telemetry.span "httpserver.serve" [ "port", string port ]

            print $"[HttpServer] Listening on port {port}"

            // Graceful shutdown: SIGINT (Ctrl+C) → cancel.
            // Already-accepted requests drain by virtue of being fire-and-
            // forget Tasks that we don't actively cancel.
            let cts = new CancellationTokenSource()
            let cancelHandler =
              ConsoleCancelEventHandler(fun _ args ->
                args.Cancel <- true
                cts.Cancel())
            Console.CancelKeyPress.AddHandler cancelHandler

            let listenerTask =
              runListener
                exeState
                port
                handler
                maxBodyBytes
                injectStandardHeaders
                canonicalizeFromForwardedProto
                cts.Token

            // Block until listener exits (cancellation).
            listenerTask.Wait()

            Console.CancelKeyPress.RemoveHandler cancelHandler

            return DUnit
          }

        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
