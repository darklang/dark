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
/// Implementation is split across three files:
///   - HttpServerHelpers.fs : pure stateless transforms (body, headers, log)
///   - HttpServerHandler.fs : per-request dispatch + tracer wiring
///   - HttpServer.fs (this) : listener loop + the `httpServerServe` builtin
module Builtins.Http.Server.Libs.HttpServer

open System
open System.Net
open System.Threading
open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Helpers = Builtins.Http.Server.Libs.HttpServerHelpers
module Handler = Builtins.Http.Server.Libs.HttpServerHandler


/// Default request body cap (30 MB). Re-exported for callers (tests, the
/// `httpServerServe` builtin's `--max-body-bytes` default).
let defaultMaxBodyBytes = Helpers.defaultMaxBodyBytes


/// Run the HTTP listener loop until `cancellationToken` fires. Public so
/// tests can drive a per-test listener with their own CancellationToken.
/// Starts an HttpListener on `port`, dispatches every request to `handler`
/// via `Handler.handleRequest`, and exits when cancellation is requested.
/// On cancellation, calls `listener.Stop()` to unblock `GetContextAsync`.
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
    //  - SSRF guard for outbound: `Builtins.Http.Client.Libs.HttpClient.
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
        // Fire-and-forget: errors are caught inside Handler.handleRequest;
        // anything escaping that try/catch is also swallowed here.
        Task.Run(fun () ->
          task {
            try
              do!
                Handler.handleRequest
                  exeState
                  handler
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
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
