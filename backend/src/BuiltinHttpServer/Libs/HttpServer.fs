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
module BuiltinHttpServer.Libs.HttpServer

open System
open System.Net
open System.IO
open System.Threading
open System.Threading.Tasks

open Fumble
open LibSqlite.Db

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module Execution = LibExecution.Execution
module Blob = LibExecution.Blob
module Http = BuiltinHttpServer.Http
module AT = LibExecution.AnalysisTypes
module Tracing = LibDB.Tracing
module VT = LibExecution.ValueType
module PT = LibExecution.ProgramTypes


/// Default request body cap (30 MB).
let defaultMaxBodyBytes : int64 = 30L * 1024L * 1024L

/// HSTS header value matching the historical default for HTTP services.
let private hstsHeaderValue = "max-age=31536000; includeSubDomains; preload"


/// Render an ExecutionResult as a Dval, surfacing errors as a DString
/// the toHttpResponse path turns into a 500.
let private resultToDval
  (exeState : ExecutionState)
  (result : Result<Dval, RuntimeError.Error * CallStack>)
  : Task<Dval> =
  task {
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


/// Execute a static handler Applicable (lambda or named fn) against
/// the given request. Lambdas registered anywhere under this `exeState`
/// (including the caller VM) are findable here since `lambdaInstrCache`
/// lives on `exeState`.
let private executeHandler
  (exeState : ExecutionState)
  (handler : Applicable)
  (arg : Dval)
  : Task<Dval> =
  task {
    let! result = Execution.executeApplicable exeState handler (NEList.singleton arg)
    return! resultToDval exeState result
  }


/// Re-resolve a fn by FQ path on each call and dispatch the request
/// through it. Enables hot-reload-on-commit: any time the user `commit`s
/// a new version of the named fn, the next request picks it up.
///
/// 404-driven dev: when the path doesn't resolve, returns a hand-crafted
/// 404 Response Dval pointing at the missing fn. The user can `commit`
/// the fn and the next request will hit it.
let private executeHandlerByPath
  (exeState : ExecutionState)
  (branchId : System.Guid)
  (path : string)
  (arg : Dval)
  : Task<Dval> =
  task {
    let parts = path.Split('.') |> Array.toList
    match parts with
    | owner :: rest when not (List.isEmpty rest) ->
      let modules, name =
        match List.rev rest with
        | last :: revMods -> List.rev revMods, last
        | [] ->
          // Shouldn't happen given the `not (List.isEmpty rest)` guard.
          [], ""
      let location : LibExecution.ProgramTypes.PackageLocation =
        { owner = owner; modules = modules; name = name }
      let! branchChain = LibDB.PackageManager.Branches.getBranchChain branchId
      let! hashOpt =
        LibDB.PackageManager.ProgramTypes.Fn.find branchChain location
        |> Ply.toTask
      match hashOpt with
      | Some hash ->
        let (PT.Hash hashStr) = hash
        let fqName = FQFnName.fqPackage hashStr
        let! result =
          Execution.executeFunction exeState fqName [] (NEList.singleton arg)
        return! resultToDval exeState result
      | None ->
        // 404 with a dev-friendly hint. Look for nearby fn names so the
        // 404 doubles as "did you mean?" — caller probably mistyped or
        // is iterating on a name they haven't committed yet.
        let! suggestions =
          let modulesStr = String.concat "." modules
          let likePattern =
            if modulesStr = "" then
              // Top-level: search the owner's fns by name fragment.
              $"%%{name}%%"
            else
              // With modules: prefer matches under the same module path,
              // fall back to anything matching the leaf name.
              $"%%{modulesStr}%%"
          Sql.query
            """
            SELECT owner, modules, name
            FROM locations
            WHERE owner = @owner
              AND item_type = 'fn'
              AND unlisted_at IS NULL
              AND ((modules = @modules) OR (modules LIKE @likePattern) OR (name LIKE @namePattern))
            ORDER BY (modules = @modules) DESC, length(modules) ASC
            LIMIT 5
            """
          |> Sql.parameters
            [ "owner", Sql.string owner
              "modules", Sql.string modulesStr
              "likePattern", Sql.string likePattern
              "namePattern", Sql.string $"%%{name}%%" ]
          |> Sql.executeAsync (fun read ->
            let o = read.string "owner"
            let m = read.string "modules"
            let n = read.string "name"
            if m = "" then $"{o}.{n}" else $"{o}.{m}.{n}")

        let suggestionsBlock =
          match suggestions with
          | [] -> ""
          | items ->
            "Did you mean one of these?\n"
            + (items |> List.map (fun s -> $"  - {s}") |> String.concat "\n")
            + "\n\n"

        let body =
          $"404 Not Found\n\nNo handler defined at `{path}` on this branch.\n\n"
          + suggestionsBlock
          + $"Define it with `fn {path} <args> = <body>` and `commit`, then retry."
        let typeName =
          FQTypeName.fqPackage (LibExecution.PackageRefs.Type.Stdlib.Http.response ())
        let fields =
          [ ("statusCode", DInt64 404L)
            ("headers",
             Dval.list
               (KTTuple(VT.string, VT.string, []))
               [ DTuple(
                   DString "Content-Type",
                   DString "text/plain; charset=utf-8",
                   []
                 ) ])
            ("body", Blob.newEphemeral exeState (UTF8.toBytes body)) ]
        return DRecord(typeName, typeName, [], Map fields)
    | _ ->
      // Malformed path — surface as a Handler error.
      return DString $"Handler error: malformed path '{path}'"
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
/// with multiple values becomes multiple entries.
let private extractHeaders (req : HttpListenerRequest) : List<string * string> =
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
/// already set them. Header keys may be lowercased (the response-shape
/// converter lowercases them on the way out), so we compare case-insensitively.
let private maybeInjectStandardHeaders
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
let private logRequest
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


/// Process a single request: parse → dispatch → write response. Errors are
/// caught and returned as 500s so a single bad request can't kill the loop.
///
/// `dispatch` is the per-request "run the user code" closure. Static-handler
/// mode wraps `executeHandler`; named/hot-reload mode wraps
/// `executeHandlerByPath`. handleRequest stays oblivious to which.
let private handleRequest
  (exeState : ExecutionState)
  (dispatch : ExecutionState -> Dval -> Task<Dval>)
  (maxBodyBytes : int64)
  (injectStandardHeaders : bool)
  (canonicalizeFromForwardedProto : bool)
  (logRequests : bool)
  (ctx : HttpListenerContext)
  : Task<unit> =
  task {
    let started = System.DateTime.UtcNow
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

          // Per-request tracer. Same shape as `darklang eval/run` produces
          // (`Tracing.createCliTracer` in `BuiltinCliHost/Libs/Cli.fs`), so
          // `traces list/view/replay` shows HTTP traces alongside CLI traces
          // with no consumer-side changes. The trace description carries the
          // method + path so per-endpoint browsing (Step 2 in the plan)
          // already has somewhere to live.
          let traceID = AT.TraceID.create ()
          let traceDesc =
            try
              $"{ctx.Request.HttpMethod} {ctx.Request.Url.PathAndQuery}"
            with _ ->
              "(http request)"
          let tracer =
            Tracing.createCliTracer
              exeState.program.canvasID
              traceID
              traceDesc
              "request"
              requestDval
          let perRequestState =
            { exeState with tracing = tracer.executionTracing }

          let! result = dispatch perRequestState requestDval
          let! response = Http.Response.toHttpResponse perRequestState result
          tracer.storeTraceResults ()

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
      if logRequests then
        try
          logRequest ctx ctx.Response.StatusCode started
        with _ ->
          ()
      try
        ctx.Response.OutputStream.Close()
        ctx.Response.Close()
      with _ ->
        ()
  }


/// Internal implementation of the listener loop, parameterised over
/// the per-request dispatch closure. `runListener` (static handler) and
/// `runListenerByPath` (hot-reload-by-path) both wrap this with their
/// own dispatch closure and share the rest of the implementation.
let private runListenerImpl
  (exeState : ExecutionState)
  (port : int64)
  (dispatch : ExecutionState -> Dval -> Task<Dval>)
  (maxBodyBytes : int64)
  (injectStandardHeaders : bool)
  (canonicalizeFromForwardedProto : bool)
  (logRequests : bool)
  (cancellationToken : CancellationToken)
  : Task<unit> =
  task {
    // ──────────────────────────────────────────────────────────────────
    // TODO: replace `HttpListener` with raw `System.Net.Sockets.TcpListener`
    // + a hand-rolled HTTP/1.1 parser.
    //
    // Why: HttpListener adds ~80 ms per new connection on loopback
    // (verified via dotnet-trace; CPU idle, time is in HttpListener-
    // internal blocking). A 60-LOC `TcpListener` PoC proves that
    // overhead disappears entirely. Wins both performance and the
    // long-term "thin .NET surface" goal — `TcpListener` has obvious
    // equivalents in Rust/Go/OCaml/native-C; `HttpListener` does not.
    // Do NOT switch to Kestrel — that re-introduces ~8 MB of
    // `Microsoft.AspNetCore.*`.
    //
    // Effort: ~300 LOC for a working server, 1–2 days. Then 0.5–1 day
    // of adversarial fixtures before declaring it production-ready.
    //
    // ────────── Security considerations for the swap ──────────
    // HttpListener gives us (mostly free) defenses we'd need to write
    // ourselves. Treat as security-relevant work, not pure perf.
    //
    // What HttpListener handles for us today:
    //  1. HTTP request smuggling. RFC 7230 ambiguity around
    //     `Transfer-Encoding: chunked` + `Content-Length: N` arriving
    //     together is a classic CVE class. Hand-rolled parsers must
    //     pick a consistent rule — reject the combo with 400 is safest
    //     (RFC 7230 §3.3.3 lets us either drop C-L or refuse).
    //  2. Slow loris. Slow byte-by-byte client sends pin a thread per
    //     connection. HttpListener has internal idle timeouts; we'd
    //     need our own (e.g. 30 s of no bytes → close).
    //  3. Concurrent connection cap. HttpListener bounds live conns;
    //     raw TcpListener + Task.Run-per-accept is OOM-able under
    //     load without an explicit SemaphoreSlim.
    //  4. Malformed input. Overlong headers, invalid encodings, CRLF
    //     injection, multi-line/folded headers. HttpListener returns
    //     400 cleanly; our parser would need explicit handling and
    //     fuzz tests for the same shapes.
    //  5. URL canonicalization for path traversal patterns.
    //
    // What we already own (no regression risk):
    //  - Body size limit: `readRequestBodyWithLimit` enforces today.
    //  - SSRF guard for outbound: `BuiltinExecution.Libs.HttpClient.
    //    LocalAccess` + `strictConfig`.
    //  - Per-request blob scope: memory hygiene already in place.
    //
    // Why "bounded" rather than scary:
    //  - HttpListener on .NET/Linux is the *managed* implementation,
    //    not http.sys. It's just .NET-team code with normal testing —
    //    not Apache/nginx-class hardened. We're not giving up
    //    battle-tested infra.
    //  - Most HTTP-smuggling vectors need a proxy in front to bite.
    //    `darklang serve` behind nginx/Cloudflare/ALB has the
    //    upstream proxy normalize requests; smuggling is mitigated.
    //    CLI-on-localhost = low risk. Public-internet-direct =
    //    highest risk; that's the deployment that needs the
    //    adversarial fixture suite.
    //  - HTTP/1.1 is a closed grammar with finite parsing surface.
    //    Fuzz-testable end-to-end. Easier to audit than a network
    //    protocol like SSH.
    //
    // Practical checklist for the swap PR:
    //  □ Reject `T-E: chunked` + `C-L: N` combo with 400.
    //  □ Per-connection idle timeout (default 30 s).
    //  □ Concurrent-connection cap via SemaphoreSlim.
    //  □ Reject overlong status lines / headers (e.g. >8 KB).
    //  □ Strict header-key/value charset (no embedded CRLF).
    //  □ Adversarial fixture suite alongside the existing httphandler
    //    fixtures: malformed requests → 400, T-E+C-L combos → 400,
    //    oversize body → 413 (already covered), slow-loris → close.
    //
    // See `notes/merge-readiness-report.md` for the dotnet-trace
    // numbers and the 60-LOC PoC details.
    // ──────────────────────────────────────────────────────────────────
    let listener = new HttpListener()
    listener.Prefixes.Add($"http://*:{port}/")
    listener.Start()

    Telemetry.event
      "httpserver.listening"
      [ "port", string port
        "maxBodyBytes", string maxBodyBytes
        "injectStandardHeaders", string injectStandardHeaders
        "canonicalizeFromForwardedProto", string canonicalizeFromForwardedProto
        "logRequests", string logRequests ]

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
                  dispatch
                  maxBodyBytes
                  injectStandardHeaders
                  canonicalizeFromForwardedProto
                  logRequests
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


/// Static-handler listener: the user passed an Applicable (lambda or
/// named fn) and expects that exact resolved handler for the lifetime of
/// the server. Public — tests drive a per-test listener with this.
let runListener
  (exeState : ExecutionState)
  (port : int64)
  (handler : Applicable)
  (maxBodyBytes : int64)
  (injectStandardHeaders : bool)
  (canonicalizeFromForwardedProto : bool)
  (logRequests : bool)
  (cancellationToken : CancellationToken)
  : Task<unit> =
  let dispatch (state : ExecutionState) (arg : Dval) : Task<Dval> =
    executeHandler state handler arg
  runListenerImpl
    exeState
    port
    dispatch
    maxBodyBytes
    injectStandardHeaders
    canonicalizeFromForwardedProto
    logRequests
    cancellationToken


/// Hot-reload-on-commit listener: the user passed an FQ path string. On
/// each request, the path is re-resolved against the current branch's
/// package store, so a `commit` of a new fn version is picked up by the
/// next request. Unresolved paths return a dev-friendly 404 with a
/// "define it and retry" hint.
let runListenerByPath
  (exeState : ExecutionState)
  (port : int64)
  (path : string)
  (maxBodyBytes : int64)
  (injectStandardHeaders : bool)
  (canonicalizeFromForwardedProto : bool)
  (logRequests : bool)
  (cancellationToken : CancellationToken)
  : Task<unit> =
  let branchId = exeState.branchId
  let dispatch (state : ExecutionState) (arg : Dval) : Task<Dval> =
    executeHandlerByPath state branchId path arg
  runListenerImpl
    exeState
    port
    dispatch
    maxBodyBytes
    injectStandardHeaders
    canonicalizeFromForwardedProto
    logRequests
    cancellationToken


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
            "If true, rewrite request.url to https:// when X-Forwarded-Proto: https is present"
          Param.make
            "logRequests"
            TBool
            "If true, emit a per-request stdout line and Telemetry.event 'httpserver.request' with method/path/status/duration_ms" ]
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
            DBool canonicalizeFromForwardedProto
            DBool logRequests ] ->
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
                logRequests
                cts.Token

            // Block until listener exits (cancellation).
            listenerTask.Wait()

            Console.CancelKeyPress.RemoveHandler cancelHandler

            return DUnit
          }

        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "httpServerServeNamed" 0
      typeParams = []
      parameters =
        [ Param.make "port" TInt64 "TCP port to listen on"
          Param.make
            "handlerPath"
            TString
            "Fully-qualified package path (e.g. `Darklang.MyApp.handler`). Re-resolved on every request, so a `commit` of a new fn version is picked up immediately by the next request."
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
            "If true, rewrite request.url to https:// when X-Forwarded-Proto: https is present"
          Param.make
            "logRequests"
            TBool
            "If true, emit a per-request stdout line and Telemetry.event 'httpserver.request' with method/path/status/duration_ms" ]
      returnType = TUnit
      description =
        "Start an HTTP server with hot-reload-on-commit. Resolves <param handlerPath> against the current branch on every request. Blocks until SIGINT."
      fn =
        (function
        | exeState,
          _,
          _,
          [ DInt64 port
            DString handlerPath
            DInt64 maxBodyBytes
            DBool injectStandardHeaders
            DBool canonicalizeFromForwardedProto
            DBool logRequests ] ->
          uply {
            use _serveSpan =
              Telemetry.span
                "httpserver.serve.named"
                [ "port", string port; "path", handlerPath ]

            print
              $"[HttpServer] Listening on port {port} (hot-reload: {handlerPath})"

            let cts = new CancellationTokenSource()
            let cancelHandler =
              ConsoleCancelEventHandler(fun _ args ->
                args.Cancel <- true
                cts.Cancel())
            Console.CancelKeyPress.AddHandler cancelHandler

            let listenerTask =
              runListenerByPath
                exeState
                port
                handlerPath
                maxBodyBytes
                injectStandardHeaders
                canonicalizeFromForwardedProto
                logRequests
                cts.Token

            listenerTask.Wait()

            Console.CancelKeyPress.RemoveHandler cancelHandler

            return DUnit
          }

        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
