/// HTTP server builtin: starts a listener, hands every request to a Dark
/// handler fn, writes the response. All routing lives Dark-side.
module Builtins.Http.Server.Libs.HttpServer

open System
open System.IO
open System.Net
open System.Threading
open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open LibExecution.Effects

module Dval = LibExecution.Dval
module Execution = LibExecution.Execution
module NR = LibExecution.RuntimeTypes.NameResolution
module Http = Builtins.Http.Server.Http
module AT = LibExecution.AnalysisTypes
module Tracing = LibDB.Tracing


/// Default request body cap (30 MB).
let defaultMaxBodyBytes : int64 = 30L * 1024L * 1024L

/// HSTS header value matching the historical default for HTTP services.
let private hstsHeaderValue = "max-age=31536000; includeSubDomains; preload"


// ───────── pure stateless helpers ─────────

/// Read the request body up to `maxBytes`. Returns `Error()` when the
/// declared `Content-Length` exceeds the cap, OR when the actual stream
/// grows past it (covers chunked encoding where C-L is absent / -1).
let private readRequestBodyWithLimit
  (req : HttpListenerRequest)
  (maxBytes : int64)
  : Task<Result<byte[], unit>> =
  if req.ContentLength64 > maxBytes then
    Task.FromResult(Error())
  // A GET has no body. Without this arm, discovering that costs a read buffer, a `MemoryStream` and
  // its internal buffer, on every request.
  elif req.ContentLength64 = 0L then
    Task.FromResult(Ok [||])
  // A declared length means the size is known, so read straight into an array of exactly that size:
  // no MemoryStream, no copy out of it, no scratch buffer.
  elif req.ContentLength64 > 0L then
    task {
      let body = Array.zeroCreate (int req.ContentLength64)
      let mutable read = 0
      let mutable eof = false
      while read < body.Length && not eof do
        let! n = req.InputStream.ReadAsync(body, read, body.Length - read)
        if n = 0 then eof <- true else read <- read + n
      // A client that declared more than it sent gets what arrived.
      return Ok(if read = body.Length then body else Array.sub body 0 read)
    }
  else
    // Chunked, so the length is unknown (`ContentLength64` is -1) and it has to be accumulated.
    task {
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


/// Bodies below this are left alone: gzip's own framing plus the header costs more than it saves,
/// and it makes every small response allocate a stream for nothing.
let private compressionFloor = 1024


/// Compress a response body IF the client asked for it, returning the body to send and the
/// `Content-Encoding` to declare.
///
/// **Brotli when offered, gzip otherwise.** Both were measured on a real sync page rather than chosen by
/// reputation, and the reason brotli wins here is specific: gzip's window is 32KB and a page is ~2.9MB,
/// so gzip cannot see that the same hashes and names recur throughout it. A large-window coder can.
///
///     raw          2,883,465
///     gzip         499,655   5.8x   14ms
///     brotli       280,064  10.3x   19ms
let private maybeCompress
  (req : HttpListenerRequest)
  (body : byte[])
  : byte[] * Option<string> =
  let accepts =
    match req.Headers["Accept-Encoding"] with
    | null -> ""
    | value -> value.ToLowerInvariant()

  let compressWith (makeStream : System.IO.Stream -> System.IO.Stream) : byte[] =
    use out = new System.IO.MemoryStream()

    (use coder = makeStream out
     coder.Write(body, 0, body.Length))

    out.ToArray()

  if body.Length < compressionFloor then
    body, None
  elif accepts.Contains "br" then
    let encode (out : System.IO.Stream) : System.IO.Stream =
      new System.IO.Compression.BrotliStream(
        out,
        System.IO.Compression.CompressionLevel.Optimal
      )

    compressWith encode, Some "br"
  elif accepts.Contains "gzip" then
    let encode (out : System.IO.Stream) : System.IO.Stream =
      new System.IO.Compression.GZipStream(
        out,
        System.IO.Compression.CompressionLevel.Fastest
      )

    compressWith encode, Some "gzip"
  else
    body, None


/// Flatten HttpListener's NameValueCollection into the (key, value) list
/// shape that `Http.Request` expects. Multi-value keys become multiple
/// entries. The final `x-http-method` entry is a CLEANUP — Dark handlers
/// still read it.
let private extractHeaders (req : HttpListenerRequest) : List<string * string> =
  let headers = ResizeArray<string * string>()
  for key in req.Headers.AllKeys do
    if not (isNull key) then
      let values = req.Headers.GetValues(key)
      if not (isNull values) then
        for value in values do
          headers.Add(key, value)
  ("x-http-method", req.HttpMethod) :: List.ofSeq headers


/// If `X-Forwarded-Proto: https` is present, rewrite scheme → https / port → 443.
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


/// Add `Server: darklang` + HSTS unless the handler already set them.
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


// ───────── how a request finds its handler ─────────

/// A server that follows edits keeps a Dark value between requests and asks Dark, per request, which
/// handler to run. The DECISION is Dark's (`Stdlib.Live`: did the store move, is the newest version
/// usable, else the last one that was); only the holding is here, because a Dark value cannot outlive
/// the call that made it and requests are separate calls.
///
/// `step : 's -> ('s * Result<Request -> Response, String>)`. Two requests in flight at once may both
/// run it against the same state and both write back; the step is idempotent (a poll and, at most, a
/// check of one declaration), so the race costs a repeated check and never a wrong answer.
type private LiveRouting =
  {
    step : Applicable
    mutable state : Dval
    /// The guest state built for the router hash last handed out. Rebuilt when the hash moves, since
    /// the router is the approval root and the root is a hash.
    mutable guest : Option<Hash * ExecutionState>
  }

type private Routing =
  | Fixed of Applicable
  | Live of LiveRouting

/// The hash a named applicable calls, for the approval root. A lambda has none.
let private rootOf (handler : Applicable) : List<Hash> =
  match handler with
  | AppNamedFn named ->
    match named.name with
    | FQFnName.Package hash -> [ hash ]
    | FQFnName.Builtin _ -> []
  | AppLambda _ -> []

/// The guest state a handler runs under: the router is the approval root, the instance policy the
/// ceiling, and the frame that called `serve` the outer bound.
let private guestStateFor
  (exeState : ExecutionState)
  (invokerAccess : LibExecution.Permissions.Access)
  (handler : Applicable)
  : ExecutionState =
  let guest =
    LibDB.PolicyStore.guestState
      exeState.accountID
      LibExecution.Permissions.Policy.allowAll
      []
      (rootOf handler)
      exeState
  { guest with
      access =
        guest.access |> LibExecution.Permissions.Access.constrainBy invokerAccess }

/// Which handler this request runs, and under what state. `Error` carries what Dark said when no
/// version is usable, which becomes a 503 rather than a crash.
let private resolveRouting
  (serverState : ExecutionState)
  (invokerAccess : LibExecution.Permissions.Access)
  (routing : Routing)
  : Task<Result<ExecutionState * Applicable, string>> =
  task {
    match routing with
    | Fixed handler -> return Ok(serverState, handler)
    | Live live ->
      let! stepped =
        Execution.executeApplicable
          serverState
          serverState.access
          live.step
          (NEList.singleton live.state)
        |> Ply.toTask
      match stepped with
      | Ok(DTuple(next, DEnum(_, _, _, "Ok", [ DApplicable handler ]), [])) ->
        live.state <- next
        let root = rootOf handler
        let guest =
          match live.guest, root with
          | Some(h, guest), [ hash ] when h = hash -> guest
          | _, _ ->
            let guest = guestStateFor serverState invokerAccess handler
            live.guest <- (root |> List.tryHead |> Option.map (fun h -> (h, guest)))
            guest
        return Ok(guest, handler)
      | Ok(DTuple(next, DEnum(_, _, _, "Error", [ DString msg ]), [])) ->
        live.state <- next
        return Error msg
      | Ok other ->
        return Error $"live routing step returned an unexpected shape: {other}"
      | Error(rte, _) ->
        let! errorStrResult = Execution.runtimeErrorToString serverState rte
        let errorStr =
          match errorStrResult with
          | Ok(DString s) -> s
          | Ok other -> string other
          | Error _ -> string rte
        return Error $"live routing step failed: {errorStr}"
  }


// ───────── per-request dispatch ─────────
// ───────── per-request dispatch ─────────

let private executeHandler
  (exeState : ExecutionState)
  (handler : Applicable)
  (arg : Dval)
  : Task<Dval> =
  task {
    // `executeApplicable` returns a `Ply` now, so that a lambda which does not await costs no
    // builder; this caller is a `task`, so it needs the conversion.
    // This detached handler has no invoking VM. Run it under the server's child
    // state, which already includes the access of the frame that called `serve`.
    let! result =
      Execution.executeApplicable
        exeState
        exeState.access
        handler
        (NEList.singleton arg)
      |> Ply.toTask
    match result with
    | Ok dval -> return dval
    | Error(rte, _callStack) ->
      let! errorStrResult = Execution.runtimeErrorToString exeState rte
      let errorStr =
        match errorStrResult with
        | Ok(DString s) -> s
        | Ok other -> string other
        | Error _ -> string rte
      return DString $"Handler error: {errorStr}"
  }


/// The per-request state, built outside `handleRequest`'s task: a record copy of
/// `ExecutionState` inside the resumable block keeps the Release compiler from
/// reducing the state machine (FS3511).
let private perRequestStateFor
  (exeState : ExecutionState)
  (tracer : Tracing.T)
  : ExecutionState =
  { exeState with tracing = tracer.executionTracing }


/// Process a single request: parse → dispatch → write response. Errors
/// surface as 500s; full detail goes to `logRequest` rather than the wire.
let private handleRequest
  (exeState : ExecutionState)
  (invokerAccess : LibExecution.Permissions.Access)
  (routing : Routing)
  (maxBodyBytes : int64)
  (injectStandardHeaders : bool)
  (canonicalizeFromForwardedProto : bool)
  (logRequests : bool)
  (ctx : HttpListenerContext)
  : Task<unit> =
  task {
    let started = if logRequests then Some System.DateTime.UtcNow else None
    // Ephemeral blobs carry their bytes inline (lifetime is GC), so there's no
    // shared blob store for concurrent requests to race over.
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
          // `Url.ToString()` decodes and `queryParams` decodes again, so a `%26` in a value became
          // a real `&` and split into a second parameter. `RawUrl` is path+query as sent; the
          // absolute form is rebuilt around it.
          let rawUrl =
            match ctx.Request.RawUrl with
            | null -> ctx.Request.Url.ToString()
            | raw -> $"{ctx.Request.Url.Scheme}://{ctx.Request.Url.Authority}{raw}"
          let url =
            if canonicalizeFromForwardedProto then
              canonicalizeUrlFromForwardedProto rawUrl reqHeaders
            else
              rawUrl

          let requestDval = Http.Request.fromRequest url reqHeaders reqBody

          // Per-request tracer — same shape as `eval`/`run` so HTTP traces
          // appear alongside CLI traces with no consumer-side changes.
          let traceID = AT.TraceID.create ()
          let traceDesc =
            try
              $"{ctx.Request.HttpMethod} {ctx.Request.Url.PathAndQuery}"
            with _ ->
              "(http request)"
          let tracer =
            Tracing.createCliTracer traceID traceDesc "request" requestDval

          // Resolved per request, not per server: this is what makes an edit show up on the next
          // request. For a fixed router it is a match on a constant.
          let! resolved = resolveRouting exeState invokerAccess routing
          let handlerState =
            match resolved with
            | Ok(handlerState, _) -> handlerState
            | Error _ -> exeState

          let! result =
            match resolved with
            | Ok(handlerState, handler) ->
              executeHandler
                (perRequestStateFor handlerState tracer)
                handler
                requestDval
            | Error msg ->
              // No usable version: say so, keep listening. The diagnostic is on stdout already
              // (Dark prints it when the verdict changes), so the wire gets a plain 503.
              Telemetry.event "httpserver.unroutable" [ "reason", msg ]
              Task.FromResult(DString $"Service Unavailable: {msg}")
          let perRequestState = perRequestStateFor handlerState tracer
          let! response = Http.Response.toHttpResponse perRequestState result
          do! tracer.storeTraceResults perRequestState |> Ply.toTask

          let respHeaders =
            maybeInjectStandardHeaders injectStandardHeaders response.headers

          ctx.Response.StatusCode <- response.statusCode
          for (key, value) in respHeaders do
            ctx.Response.Headers.Add(key, value)

          // Only when the client asked (`maybeCompress` has the ratios and floor).
          // Never on a body the handler already encoded (double-wrap), and always
          // with `Vary`, or a shared cache hands brotli to a client that didn't ask.
          let alreadyEncoded =
            respHeaders
            |> List.exists (fun (k, _) ->
              String.equalsCaseInsensitive k "Content-Encoding")

          let body, encoding =
            if alreadyEncoded then
              response.body, None
            else
              maybeCompress ctx.Request response.body

          match encoding with
          | Some enc ->
            ctx.Response.Headers.Add("Content-Encoding", enc)
            ctx.Response.Headers.Add("Vary", "Accept-Encoding")
          | None -> ()

          ctx.Response.ContentLength64 <- int64 body.Length
          do! ctx.Response.OutputStream.WriteAsync(body, 0, body.Length)
      with _ex ->
        // Don't leak ex.Message — can carry stack hints / sensitive
        // strings. Detail goes to `logRequest` (which sees the 500
        // status). 4xx + handler-set codes flow through
        // `Response.toHttpResponse`; this path is F#-side failures only.
        ctx.Response.StatusCode <- 500
        let errorBytes = UTF8.toBytes "Internal server error"
        ctx.Response.ContentLength64 <- int64 errorBytes.Length
        do! ctx.Response.OutputStream.WriteAsync(errorBytes, 0, errorBytes.Length)
    finally
      match started with
      | Some started ->
        try
          logRequest ctx ctx.Response.StatusCode started
        with _ ->
          ()
      | None -> ()
      try
        ctx.Response.OutputStream.Close()
        ctx.Response.Close()
      with _ ->
        ()
  }


// ───────── listener loop ─────────

// TODO: replace `HttpListener` with raw `TcpListener` + a hand-rolled HTTP/1.1
// parser. ~80 ms/connection on loopback today; PoC drops it to near-zero.
// HttpListener gives us free-but-not-cheap defenses (T-E + C-L smuggling,
// slow-loris timeouts, per-connection caps, malformed-input handling) — all
// of which the swap PR has to re-implement before going public-facing.
// See `notes/merge-readiness-report.md` for the dotnet-trace numbers.


/// Serve requests off an already-bound listener, until cancelled.
let private runListenerWith
  (exeState : ExecutionState)
  (invokerAccess : LibExecution.Permissions.Access)
  (listener : HttpListener)
  (port : int64)
  (routing : Routing)
  (maxBodyBytes : int64)
  (injectStandardHeaders : bool)
  (canonicalizeFromForwardedProto : bool)
  (logRequests : bool)
  (cancellationToken : CancellationToken)
  : Task<unit> =
  task {
    Telemetry.event
      "httpserver.listening"
      [ "port", string port
        "maxBodyBytes", string maxBodyBytes
        "injectStandardHeaders", string injectStandardHeaders
        "canonicalizeFromForwardedProto", string canonicalizeFromForwardedProto
        "logRequests", string logRequests ]

    // Cancellation → listener.Stop() unblocks pending GetContextAsync
    // by raising HttpListenerException / ObjectDisposedException, both
    // caught below as normal exit.
    use _registration =
      cancellationToken.Register(fun () ->
        try
          listener.Stop()
        with _ ->
          ())

    while not cancellationToken.IsCancellationRequested do
      try
        let! ctx = listener.GetContextAsync()
        Task.Run(fun () ->
          task {
            try
              do!
                handleRequest
                  exeState
                  invokerAccess
                  routing
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

/// `runListenerWith` for one fixed handler, under the state as given. The tests' entry point.
let runListener
  (exeState : ExecutionState)
  (listener : HttpListener)
  (port : int64)
  (handler : Applicable)
  (maxBodyBytes : int64)
  (injectStandardHeaders : bool)
  (canonicalizeFromForwardedProto : bool)
  (logRequests : bool)
  (cancellationToken : CancellationToken)
  : Task<unit> =
  runListenerWith
    exeState
    exeState.access
    listener
    port
    (Fixed handler)
    maxBodyBytes
    injectStandardHeaders
    canonicalizeFromForwardedProto
    logRequests
    cancellationToken

/// `runListenerWith` for a handler resolved per request by a Dark `step` (see `LiveRouting`),
/// under the state as given. For the tests; the builtin builds its guest state first.
let runListenerLive
  (exeState : ExecutionState)
  (listener : HttpListener)
  (port : int64)
  (init : Dval)
  (step : Applicable)
  (maxBodyBytes : int64)
  (injectStandardHeaders : bool)
  (canonicalizeFromForwardedProto : bool)
  (logRequests : bool)
  (cancellationToken : CancellationToken)
  : Task<unit> =
  runListenerWith
    exeState
    exeState.access
    listener
    port
    (Live { step = step; state = init; guest = None })
    maxBodyBytes
    injectStandardHeaders
    canonicalizeFromForwardedProto
    logRequests
    cancellationToken


/// Bind, announce, serve until SIGINT. Shared by the fixed and the live builtin; the only thing
/// they differ in is how a request finds its handler.
let private serve
  (exeState : ExecutionState)
  (vm : VMState)
  (portArg : DarkInt)
  (routing : Routing)
  (approvalRoot : Applicable)
  (maxBodyBytesArg : DarkInt)
  (injectStandardHeaders : bool)
  (canonicalizeFromForwardedProto : bool)
  (logRequests : bool)
  (onListening : Applicable)
  : Ply<Dval> =
  uply {
    // The router and its callbacks are guest code, so use a guest
    // state rather than the trusted CLI state. Use it for both the
    // bind check and handler calls; the instance policy remains the
    // hard maximum. A named package router is the approval root;
    // lambda handlers have no package root of their own.
    //
    // `guestState` replaces access, so intersect the child with the
    // invoking frame. Guest code can call `serve` directly; its ceiling,
    // package approval and resource restrictions must reach the bind,
    // logging and callback checks.
    //
    // A live server re-derives this per router hash (`resolveRouting`); the one built here is for
    // the bind, the announce, and the routing step itself.
    let invokerAccess = vm.activeAccess
    let exeState = guestStateFor exeState invokerAccess approvalRoot
    // maxBodyBytes is a comparison threshold; a negative limit would
    // reject every request (treated as over-limit), so reject it. 0 is
    // valid (allow no body).
    let maxBodyBytes = intToInt64 vm maxBodyBytesArg
    if maxBodyBytes < 0L then
      RuntimeError.Ints.OutOfRange |> RuntimeError.Int |> raiseRTE vm.threadID
    // A TCP port must be in [0, 65535]. intToInt64 alone would let
    // larger-but-int64-sized values reach HttpListener.Start and throw a
    // host exception, so validate the real port range up front.
    let port = intToInt64 vm portArg
    if port < int64 IPEndPoint.MinPort || port > int64 IPEndPoint.MaxPort then
      RuntimeError.Ints.OutOfRange |> RuntimeError.Int |> raiseRTE vm.threadID

    // These ambient effects are performed on behalf of the child
    // guest state, so check its access (already narrowed by the
    // invoker's, above) rather than only the ordinary builtin gate,
    // which from `dark serve` sees the broader outer VM. Clock and
    // stdout are only used when request logging is enabled.
    if logRequests then
      LibExecution.PermissionCheck.requireBuiltinEffectsWithAccess
        exeState
        vm
        exeState.access
        (set [ Effect.Clock; Effect.Stdout ])
        "httpServerServe"
    use _serveSpan = Telemetry.span "httpserver.serve" [ "port", string port ]

    // Bind through the checked host boundary using the guest access,
    // so the instance policy applies instead of the trusted CLI's.
    let! bound =
      LibExecution.PermissionCheck.performHostWithAccess
        exeState
        vm
        exeState.access
        (LibExecution.Host.Operation.HttpServerBind(int port))
    match bound with
    | Error failure ->
      return Dval.resultError KTUnit KTString (DString failure.message)
    | Ok response ->
      use listener =
        response
        |> LibExecution.Host.expectHttpServerHandle
        |> LibExecution.Host.takeHttpServerListener
      let! _ =
        Execution.executeApplicable
          exeState
          exeState.access
          onListening
          (NEList.singleton DUnit)

      // SIGINT → cancel; in-flight requests drain by virtue of being
      // fire-and-forget Tasks.
      let cts = new CancellationTokenSource()
      let cancelHandler =
        ConsoleCancelEventHandler(fun _ args ->
          args.Cancel <- true
          cts.Cancel())
      Console.CancelKeyPress.AddHandler cancelHandler

      let listenerTask =
        runListenerWith
          exeState
          invokerAccess
          listener
          port
          routing
          maxBodyBytes
          injectStandardHeaders
          canonicalizeFromForwardedProto
          logRequests
          cts.Token

      listenerTask.Wait()

      Console.CancelKeyPress.RemoveHandler cancelHandler

      return Dval.resultOk KTUnit KTString DUnit
  }


let private requestType =
  TCustomType(
    FQTypeName.fqPackage (LibExecution.PackageRefs.Type.Stdlib.Http.request ())
    |> NR.ok,
    []
  )

let private responseType =
  TCustomType(
    FQTypeName.fqPackage (LibExecution.PackageRefs.Type.Stdlib.Http.response ())
    |> NR.ok,
    []
  )

let private handlerType = TFn(NEList.singleton requestType, responseType)

let private commonParams =
  [ Param.make
      "maxBodyBytes"
      TInt
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
      "If true, emit a per-request stdout line and Telemetry.event 'httpserver.request' with method/path/status/duration_ms"
    Param.makeWithArgs
      "onListening"
      (TFn(NEList.singleton TUnit, TUnit))
      "Fired once the port is bound — announce here, so a banner is never printed before it's true"
      [ "unit" ] ]


let fns () : List<BuiltInFn> =
  [ { name = fn "httpServerServe" 0
      typeParams = []
      parameters =
        [ Param.make "port" TInt "TCP port to listen on"
          Param.makeWithArgs
            "handler"
            handlerType
            "Handler function: request -> response"
            [ "request" ] ]
        @ commonParams
      returnType = TypeReference.result TUnit TString
      description =
        "Start an HTTP server. Calls handler for each request. Runs onListening once the port is bound; "
        + "returns Error with a plain message if the port can't be bound. Blocks until SIGINT."
      fn =
        (function
        | exeState,
          vm,
          _,
          [| DInt portArg
             DApplicable handler
             DInt maxBodyBytesArg
             DBool injectStandardHeaders
             DBool canonicalizeFromForwardedProto
             DBool logRequests
             DApplicable onListening |] ->
          serve
            exeState
            vm
            portArg
            (Fixed handler)
            handler
            maxBodyBytesArg
            injectStandardHeaders
            canonicalizeFromForwardedProto
            logRequests
            onListening
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.HttpServer; Effect.Stdout; Effect.Clock ]
      deprecated = NotDeprecated }


    // `httpServerServe` for a server that follows edits. The handler is not fixed at start: per request,
    // Dark's `step` is run over a state the server keeps between requests, and answers with the handler
    // to use this time (`Stdlib.Live` decides: the newest version of the router that passes its checks).
    // The router's hash is the approval root, re-derived when it moves. See `LiveRouting`.
    { name = fn "httpServerServeLive" 0
      typeParams = []
      parameters =
        [ Param.make "port" TInt "TCP port to listen on"
          Param.make "init" (TVariable "s") "The routing state to start from"
          Param.makeWithArgs
            "step"
            (TFn(
              NEList.singleton (TVariable "s"),
              TTuple(TVariable "s", TypeReference.result handlerType TString, [])
            ))
            "Per request: the next routing state, and the handler for this request (or why there is none)"
            [ "state" ]
          Param.makeWithArgs
            "first"
            handlerType
            "The handler as resolved at start, for the bind-time approval root"
            [ "request" ] ]
        @ commonParams
      returnType = TypeReference.result TUnit TString
      description =
        "Start an HTTP server whose handler is resolved per request by `step` over a state kept "
        + "between requests. Otherwise as `httpServerServe`."
      fn =
        (function
        | exeState,
          vm,
          _,
          [| DInt portArg
             init
             DApplicable step
             DApplicable first
             DInt maxBodyBytesArg
             DBool injectStandardHeaders
             DBool canonicalizeFromForwardedProto
             DBool logRequests
             DApplicable onListening |] ->
          serve
            exeState
            vm
            portArg
            (Live { step = step; state = init; guest = None })
            first
            maxBodyBytesArg
            injectStandardHeaders
            canonicalizeFromForwardedProto
            logRequests
            onListening
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.HttpServer; Effect.Stdout; Effect.Clock ]
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
