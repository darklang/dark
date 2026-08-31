/// Which requests `httpGetUnsafeBytes` will make, against a real HTTP server.
///
/// It is the one builtin that turns the SSRF guards off, so it is the one place
/// where getting the scope wrong means arbitrary Dark can read services on the
/// machine running it.
///
/// The server here is a bare `HttpListener`: the thing under test is the CLIENT's
/// decision, so the server should be as dumb and as predictable as possible.
module Tests.UnguardedOrigins

open Expecto

open System.Net
open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module Exe = LibExecution.Execution
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module UnguardedOrigins = LibExecution.UnguardedOrigins

open TestUtils.TestUtils


/// A free loopback port. Same brief race as `HttpServer.Tests`: another process
/// could take it between Stop() and use, which in practice doesn't happen on
/// loopback.
let private allocateFreePort () : int =
  let listener = new Sockets.TcpListener(IPAddress.Loopback, 0)
  listener.Start()
  let port = (listener.LocalEndpoint :?> IPEndPoint).Port
  listener.Stop()
  port


/// Serve until cancelled: every path answers 200 with a body.
let private runServer (port : int) (token : CancellationToken) : Task<unit> =
  task {
    let listener = new HttpListener()
    listener.Prefixes.Add($"http://127.0.0.1:{port}/")
    listener.Start()

    use _ = token.Register(fun () -> listener.Stop())

    try
      while not token.IsCancellationRequested do
        let! ctx = listener.GetContextAsync()
        let bytes = System.Text.Encoding.UTF8.GetBytes "pong"
        ctx.Response.StatusCode <- 200
        ctx.Response.ContentLength64 <- int64 bytes.Length
        do! ctx.Response.OutputStream.WriteAsync(bytes, 0, bytes.Length)
        ctx.Response.Close()
    with _ ->
      // Stopping the listener is how this loop ends; GetContextAsync throws on the
      // way out.
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

/// Is this `Result.Ok`? The shape is what the test is about, so it's read
/// positionally rather than reconstructed.
let private isOk (dval : RT.Dval) : bool =
  match dval with
  | RT.DEnum(_, _, _, "Ok", _) -> true
  | _ -> false

let private errorMessage (dval : RT.Dval) : string =
  match dval with
  | RT.DEnum(_, _, _, "Error", [ RT.DString s ]) -> s
  | other -> $"not an Error: {other}"


/// Run <param f> against a server this instance has been pointed at.
///
/// Naming it is the part that matters. In the CLI that comes from argv or the stored
/// peers; here it is this line, and without it every request below would be refused
/// before it left, which is correct behaviour that would read as a network failure.
let private withServer (f : int -> Task<unit>) : Task<unit> =
  task {
    let port = allocateFreePort ()
    use cts = new CancellationTokenSource()
    let serving = runServer port cts.Token

    // Give the listener a moment to bind; a request that races the bind reads as a
    // network failure, which is exactly the thing under test and would make this
    // lie.
    do! Task.Delay 250

    UnguardedOrigins.setFromArgv [ $"http://127.0.0.1:{port}" ]

    try
      do! f port
    finally
      UnguardedOrigins.setFromArgv []
      cts.Cancel()
      try
        serving.Wait 1000 |> ignore<bool>
      with _ ->
        ()
  }


let namedOriginIsReachable =
  testTask "the unguarded transport reaches an origin this instance named" {
    do!
      withServer (fun port ->
        task {
          let! ok =
            eval $"Builtin.httpGetUnsafeBytes \"http://127.0.0.1:{port}/ok\""
          Expect.isTrue (isOk ok) "a named origin is fetched"

          // Every path of a named origin, not just the one that was named: an
          // origin is the unit of trust here, and a peer serves several routes.
          let! other =
            eval
              $"Builtin.httpGetUnsafeBytes \"http://127.0.0.1:{port}/sync/health\""
          Expect.isTrue (isOk other) "and so is another path on it"
        })
  }

let unnamedOriginIsRefused =
  testTask "an origin nobody named is refused, and nothing is fetched" {
    do!
      withServer (fun port ->
        task {
          // A second listener on loopback, which is exactly what pulled code would
          // go looking for: reachable, and not this instance's business.
          let other = allocateFreePort ()
          use cts = new CancellationTokenSource()
          let serving = runServer other cts.Token
          do! Task.Delay 250

          try
            let! refused =
              eval $"Builtin.httpGetUnsafeBytes \"http://127.0.0.1:{other}/ok\""
            Expect.isFalse (isOk refused) "an unnamed origin is not fetched"

            let msg = errorMessage refused
            Expect.stringContains
              msg
              $"http://127.0.0.1:{other}"
              "the refusal names the origin asked for"
            Expect.stringContains
              msg
              "not a peer you sync with"
              "and says why it was refused"
            Expect.isFalse
              (msg.Contains "pong")
              "and the body never reached the caller"

            // The named one still works, so this is a scope, not an outage.
            let! ok =
              eval $"Builtin.httpGetUnsafeBytes \"http://127.0.0.1:{port}/ok\""
            Expect.isTrue (isOk ok) "the named origin is unaffected"
          finally
            cts.Cancel()
            try
              serving.Wait 1000 |> ignore<bool>
            with _ ->
              ()
        })
  }

let unparseableUrlIsRefused =
  testTask
    "a url that cannot be read as an origin is refused rather than passed through" {
    // Fails CLOSED. Anything that cannot be reduced to scheme://host:port cannot be
    // compared against the named ones, so there is no answer to "is this one of them"
    // other than no.
    UnguardedOrigins.setFromArgv [ "http://127.0.0.1:1" ]

    try
      for url in [ "not a url"; ""; "/sync/health"; "file:///etc/passwd" ] do
        let! refused = eval $"Builtin.httpGetUnsafeBytes \"{url}\""
        Expect.isFalse (isOk refused) $"`{url}` is refused"
        // Refused by the origin check, specifically. Several of these would also
        // fail further down as a bad url or a banned scheme, so a test that only
        // asked for "not Ok" would pass with the check gone.
        Expect.stringContains
          (errorMessage refused)
          "not a peer you sync with"
          $"`{url}` is refused for not being a peer, not for some later reason"
    finally
      UnguardedOrigins.setFromArgv []
  }

let defaultPortIsExplicit =
  testTask "an origin named without a port covers the same one written with a port" {
    // `http://host` and `http://host:80` are the same place, and a peer url gets
    // written both ways -- typed one way, stored the other. Comparing the strings
    // would make the guard depend on spelling.
    UnguardedOrigins.setFromArgv [ "http://sync.example.com" ]

    try
      Expect.isTrue
        (UnguardedOrigins.isAllowed "http://sync.example.com:80/sync/health")
        "the default port is explicit on both sides"
      Expect.isTrue
        (UnguardedOrigins.isAllowed "http://SYNC.EXAMPLE.COM/sync/health")
        "and the host is compared case-insensitively"
      Expect.isFalse
        (UnguardedOrigins.isAllowed "https://sync.example.com/sync/health")
        "but a different scheme is a different origin"
      Expect.isFalse
        (UnguardedOrigins.isAllowed "http://sync.example.com:9000/sync/health")
        "and so is a different port"
    finally
      UnguardedOrigins.setFromArgv []
  }

let storedOriginsAreReadPerRequest =
  testTask "an origin stored during this process counts immediately" {
    // `dark sync connect <url>` adds a peer and probes it in one process, so the
    // stored side is a lookup rather than a value read at startup.
    let mutable stored = []
    UnguardedOrigins.setStoredLookup (fun () -> stored)

    try
      Expect.isFalse
        (UnguardedOrigins.isAllowed "http://later.example.com/sync/health")
        "not allowed yet"

      stored <- [ "http://later.example.com" ]

      Expect.isTrue
        (UnguardedOrigins.isAllowed "http://later.example.com/sync/health")
        "and allowed the moment it is stored"
    finally
      UnguardedOrigins.setStoredLookup (fun () -> [])
  }


let tests =
  testSequencedGroup "UnguardedOrigins"
  <| testList
    "UnguardedOrigins"
    [ namedOriginIsReachable
      unnamedOriginIsRefused
      unparseableUrlIsRefused
      defaultPortIsExplicit
      storedOriginsAreReadPerRequest ]
