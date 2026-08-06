/// The two builtins sync moves ops over, against a real HTTP server.
///
/// `httpGetUnsafeBytes` and `httpPostUnsafeBytes` are the whole network surface of `dark sync`. They exist
/// because a relay sits behind loopback / LAN / tailnet, which the safe HttpClient bans -- so they are the
/// one place in the system where those guards are relaxed, and the one place where getting the result wrong
/// means the CLI reports success for work a server rejected. That happened, and it's why these exist.
///
/// The server here is a bare `HttpListener`, not the Dark relay: the thing under test is the CLIENT's
/// reading of a response, so the server should be as dumb and as predictable as possible.
module Tests.SyncTransport

open Expecto

open System.Net
open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module Exe = LibExecution.Execution
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

open TestUtils.TestUtils


/// A free loopback port. Same brief race as `HttpServer.Tests`: another process could take it between
/// Stop() and use, which in practice doesn't happen on loopback.
let private allocateFreePort () : int =
  let listener = new Sockets.TcpListener(IPAddress.Loopback, 0)
  listener.Start()
  let port = (listener.LocalEndpoint :?> IPEndPoint).Port
  listener.Stop()
  port


/// Serve until cancelled: `/ok` answers 200 with a body, everything else answers 400 with a reason.
///
/// Bound with the `*` prefix rather than `127.0.0.1`, because half of what this tests is that the client
/// reaches the server by the address a person would actually type.
let private runServer (port : int) (token : CancellationToken) : Task<unit> =
  task {
    let listener = new HttpListener()
    listener.Prefixes.Add($"http://*:{port}/")
    listener.Start()

    use _ = token.Register(fun () -> listener.Stop())

    try
      while not token.IsCancellationRequested do
        let! ctx = listener.GetContextAsync()
        let path = ctx.Request.Url.AbsolutePath

        let (status, body) =
          if path = "/ok" then (200, "pong") else (400, "no such thing here")

        ctx.Response.StatusCode <- status
        let bytes = System.Text.Encoding.UTF8.GetBytes body
        ctx.Response.ContentLength64 <- int64 bytes.Length
        do! ctx.Response.OutputStream.WriteAsync(bytes, 0, bytes.Length)
        ctx.Response.Close()
    with _ ->
      // Stopping the listener is how this loop ends; GetContextAsync throws on the way out.
      ()
  }


/// Evaluate a Dark expression with the CLI's builtin set and hand back the Dval.
let private eval (code : string) : Task<RT.Dval> =
  task {
    let! state = executionStateFor pmPT true Map.empty
    let! ptExpr = parsePTExpr code
    let rtInstrs = PT2RT.Expr.toRT Map.empty 0 None ptExpr

    match! Exe.executeExpr state rtInstrs with
    | Ok dval -> return dval
    | Error(rte, _) ->
      return Exception.raiseInternal "eval failed" [ "rte", string rte ]
  }

/// Is this `Result.Ok`? The shape is what the test is about, so it's read positionally rather than
/// reconstructed.
let private isOk (dval : RT.Dval) : bool =
  match dval with
  | RT.DEnum(_, _, _, "Ok", _) -> true
  | _ -> false

let private errorMessage (dval : RT.Dval) : string =
  match dval with
  | RT.DEnum(_, _, _, "Error", [ RT.DString s ]) -> s
  | other -> $"not an Error: {other}"


let private withServer (f : int -> Task<unit>) : Task<unit> =
  task {
    let port = allocateFreePort ()
    use cts = new CancellationTokenSource()
    let serving = runServer port cts.Token

    // Give the listener a moment to bind; a request that races the bind reads as a network failure, which
    // is exactly the thing under test and would make this lie.
    do! Task.Delay 250

    // The unguarded transport only reaches this instance's sync TARGETS, so a test server has to be named
    // as one. In the CLI that naming comes from argv or the stored relay; here it is this line. Without
    // it every request below is refused before it leaves, which is the correct behaviour and would make
    // these tests look like a network failure.
    LibExecution.SyncTargets.setFromArgv
      [ $"http://127.0.0.1:{port}"; $"http://localhost:{port}" ]

    try
      do! f port
    finally
      cts.Cancel()
      try
        serving.Wait 1000 |> ignore<bool>
      with _ ->
        ()
  }


let nonSuccessIsAnError =
  testTask "a non-2xx reaches the caller as an Error, with the status and the body" {
    do!
      withServer (fun port ->
        task {
          let! ok =
            eval $"Builtin.httpGetUnsafeBytes \"http://127.0.0.1:{port}/ok\""
          Expect.isTrue (isOk ok) "a 200 is Ok"

          // The bug this exists for: both builtins returned `Ok body` for ANY completed exchange, so a
          // relay answering 400 arrived as a successful fetch whose payload happened to be an error page.
          // `push`, `pull`, `connect` and branch push all printed success for requests the server rejected,
          // and the only tell was a listing coming back empty.
          let! bad =
            eval $"Builtin.httpGetUnsafeBytes \"http://127.0.0.1:{port}/nope\""
          Expect.isFalse (isOk bad) "a 400 is an Error"

          let msg = errorMessage bad
          Expect.stringContains msg "400" "the status is in the message"
          Expect.stringContains
            msg
            "no such thing here"
            "and so is what the server said"

          let! posted =
            eval
              $"Builtin.httpPostUnsafeBytes \"http://127.0.0.1:{port}/nope\" (Stdlib.String.toBlob \"x\")"

          Expect.isFalse (isOk posted) "and POST agrees with GET"
        })
  }

let localhostIsReachable =
  testTask "`localhost` reaches a server bound to IPv4" {
    do!
      withServer (fun port ->
        task {
          // `localhost` resolves to BOTH `::1` and `127.0.0.1`, and the connection filter used to dial only
          // the first address it got back. Every server this repo starts binds IPv4, so the most natural
          // address anyone types was the one address that couldn't work -- and it failed as a flat "network
          // error", which sends you looking at the server.
          //
          // Nothing else covers this: it needs a real socket, so no unit test would have caught it, and it
          // only showed up walking the two-box runbook by hand.
          let! ok =
            eval $"Builtin.httpGetUnsafeBytes \"http://localhost:{port}/ok\""
          Expect.isTrue (isOk ok) "localhost connects"
        })
  }


let tests =
  testSequencedGroup "SyncTransport"
  <| testList "SyncTransport" [ nonSuccessIsAnError; localhostIsReachable ]
