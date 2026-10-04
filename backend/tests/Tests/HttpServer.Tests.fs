/// Tests the CLI's `Http.serve` builtin against the fixtures in
/// `testfiles/http-server/` (byte-exact `.test` files).
///
/// Per-test handlers are assembled into an in-memory router Dval by
/// `buildRouterForTest`; a free port is allocated with
/// `TcpListener(IPAddress.Loopback, 0)` and the listener is stopped via
/// `cts.Cancel()` in teardown. The host is fixed to `"localhost"` —
/// single-app server, no multi-tenant routing.
module Tests.HttpServer

let basePath = "testfiles/http-server"
let dataBasePath = "testfiles/data"

open Expecto

open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Net
open System.Net.Sockets

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Execution = LibExecution.Execution
module HttpServer = Builtins.Http.Server.Libs.HttpServer

open Tests
open TestUtils.TestUtils

type HandlerVersion = | Http

type TestHandler =
  { version : HandlerVersion; route : string; method : string; code : string }

type Test =
  { handlers : List<TestHandler>
    request : byte array
    expectedResponse : byte array }


// Bind test listeners through the production host boundary; test setup is trusted.
let bindListener (port : int) : Task<System.Net.HttpListener> =
  task {
    let access =
      LibExecution.Permissions.Access.start LibExecution.Permissions.Policy.allowAll
    let! outcome =
      LibExecution.Host.perform
        LibExecution.Permissions.NoRelax
        access
        (LibExecution.Host.Operation.HttpServerBind port)
    match outcome with
    | LibExecution.Host.Outcome.Success response ->
      return
        response
        |> LibExecution.Host.expectHttpServerHandle
        |> LibExecution.Host.takeHttpServerListener
    | other ->
      return
        Exception.raiseInternal "could not bind test listener" [ "outcome", other ]
  }


let newline = byte '\n'

/// Take a byte array and split it by newline, returning a list of lists. The
/// arrays do NOT have the newlines in them.
let splitAtNewlines (bytes : byte array) : byte list list =
  bytes
  |> Array.fold
    (fun state b ->
      if b = newline then
        [] :: state
      else
        match state with
        | [] -> Exception.raiseInternal "can't have no entries" []
        | head :: rest -> (b :: head) :: rest)
    [ [] ]
  |> List.map List.reverse
  |> List.reverse

#nowarn "57" // Negative array index using ^idx

/// Used to parse a .test file
/// See details in `http-server/README.md`
module ParseTest =
  type private TestParsingState =
    | Limbo
    | InHttpHandler
    | InResponse
    | InRequest

  /// Parse the test line-by-line.
  /// We don't use regex here because we want to test more than strings
  let parse (bytes : byte array) : Test =
    let lines : List<List<byte>> = bytes |> splitAtNewlines

    let emptyTest = { handlers = []; request = [||]; expectedResponse = [||] }

    lines
    |> List.fold
      (fun (state : TestParsingState, result : Test) (line : List<byte>) ->
        let asString : string = line |> Array.ofList |> UTF8.ofBytesWithReplacement
        match asString with
        | "[request]" -> (InRequest, result)
        | "[response]" -> (InResponse, result)

        | Regex.Regex "\[http-handler (\S+) (\S+)\]" [ method; route ] ->
          (InHttpHandler,
           { result with
               handlers =
                 { version = Http; route = route; method = method; code = "" }
                 :: result.handlers })

        | Regex.Regex "\<IMPORT_DATA_FROM_FILE=(\S+)\>" [ dataFileToInject ] ->
          let injectedBytes =
            System.IO.File.ReadAllBytes $"{dataBasePath}/{dataFileToInject}"

          match state with
          | InRequest ->
            let updatedRequest =
              Array.concat [| result.request; injectedBytes; [| newline |] |]
            (InRequest, { result with request = updatedRequest })

          | InResponse ->
            let updatedResponse =
              Array.concat
                [| result.expectedResponse; injectedBytes; [| newline |] |]
            (InRequest, { result with expectedResponse = updatedResponse })

          | InHttpHandler
          | Limbo ->
            Exception.raiseInternal
              "Unexpected <IMPORT_DATA_FROM_FILE>"
              [ "line", line ]

        | _ ->
          match state with
          | InHttpHandler ->
            let handlersWithUpdate =
              match result.handlers with
              | [] ->
                Exception.raiseInternal
                  "There should be at least one handler already"
                  []
              | handler :: other ->
                let updatedHandler =
                  { handler with code = handler.code + asString + "\n" }

                updatedHandler :: other
            InHttpHandler, { result with handlers = handlersWithUpdate }
          | InResponse ->
            InResponse,
            { result with
                expectedResponse =
                  Array.concat
                    [| result.expectedResponse; Array.ofList line; [| newline |] |] }
          | InRequest ->
            InRequest,
            { result with
                request =
                  Array.concat
                    [| result.request; Array.ofList line; [| newline |] |] }
          | Limbo ->
            if line.Length = 0 then
              (Limbo, result)
            else
              Exception.raiseInternal
                $"Line received while not in any state"
                [ "line", line ])
      (Limbo, emptyTest)
    |> Tuple2.second
    |> fun test ->
        { test with
            // Remove the superfluously added newline on response
            expectedResponse =
              Array.take (test.expectedResponse.Length - 1) test.expectedResponse
            // Allow separation from the next section with a blank line
            request = Array.take (test.request.Length - 2) test.request }


/// Allocate a free TCP port on loopback. Brief race: another process could
/// grab the port between Stop() and the listener using it, but in practice
/// loopback ephemeral ports are fine for in-process tests.
let allocateFreePort () : int =
  let listener = new TcpListener(IPAddress.Loopback, 0)
  listener.Start()
  let port = (listener.LocalEndpoint :?> IPEndPoint).Port
  listener.Stop()
  port


/// Build a router DApplicable from the test's parsed handlers. Compiles
/// `fun request -> (<body>)` directly — no `routeRequest` wrapping, so a
/// handler that returns the wrong shape still flows through F#'s
/// `toHttpResponse` / `wrongTypeResponse`. All current fixtures have
/// exactly one handler.
let private buildRouterForTest
  (exeState : RT.ExecutionState)
  (test : Test)
  : Task<RT.Applicable> =
  task {
    let routerSource =
      match test.handlers |> List.reverse with
      | [] -> "fun request -> Darklang.Stdlib.Http.notFound ()"
      | [ h ] -> $"""fun request -> ({h.code})"""
      | many ->
        Exception.raiseInternal
          "buildRouterForTest: multi-handler fixtures aren't supported"
          [ "handlerCount", List.length many ]
    let! ptExpr = TestUtils.TestUtils.parsePTExpr routerSource
    let rtInstrs = PT2RT.Expr.toRT Map.empty 0 None ptExpr
    let! result = Execution.executeExpr exeState rtInstrs
    match result with
    | Ok(RT.DApplicable applicable) -> return applicable
    | Ok dval ->
      return
        Exception.raiseInternal
          "buildRouterForTest returned non-DApplicable"
          [ "dval", dval ]
    | Error(rte, _callStack) ->
      let! errStr = Execution.runtimeErrorToString exeState rte
      let asString =
        match errStr with
        | Ok(RT.DString s) -> s
        | _ -> string rte
      return
        Exception.raiseInternal
          "buildRouterForTest failed to evaluate router source"
          [ "source", routerSource; "rte", asString ]
  }


/// Executes a test
module Execution =
  let private normalizeActualHeaders
    (handlerVersion : HandlerVersion)
    (hs : (string * string) list)
    : (string * string) list =
    match handlerVersion with
    | Http ->
      hs
      |> List.filterMap (fun (k, v) ->
        match k, v with
        | "Date", _ -> Some(k, "xxx, xx xxx xxxx xx:xx:xx xxx")
        // HttpListener auto-adds `Connection: close` on error-path
        // responses; fixtures don't include it. Strip so the comparison
        // stays byte-meaningful.
        | "Connection", _ -> None
        | _other -> Some(k, v))
      |> List.sortBy Tuple2.first

  let private normalizeExpectedHeaders
    (handlerVersion : HandlerVersion)
    (headers : (string * string) list)
    (actualBody : byte array)
    : (string * string) list =
    match handlerVersion with
    | Http ->
      headers
      |> List.map (fun (k, v) ->
        match k, v with
        | "Content-Length", "LENGTH" -> (k, string actualBody.Length)
        | _ -> (k, v))
      |> List.sortBy Tuple2.first

  /// create a TCP client, used to make test HTTP requests
  let private createClient (port : int) : Task<TcpClient> =
    task {
      let client = new TcpClient()

      // Listener might not be loaded yet
      let mutable connected = false
      for i in 1..10 do
        try
          if not connected then
            do! client.ConnectAsync("127.0.0.1", port)
            connected <- true
        with _ when i <> 10 ->
          do! System.Threading.Tasks.Task.Delay 100
      return client
    }

  /// Replace `pattern` in the byte array with `replacement` - both are
  /// provided as strings for convenience, but obviously both will be
  /// converted to bytes
  let private replaceByteStrings
    (pattern : string)
    (replacement : string)
    (bytes : byte array)
    : byte array =
    let patternBytes = UTF8.toBytes pattern
    let replacementBytes = UTF8.toBytes replacement |> Array.toList |> List.reverse

    if pattern.Length = 0 || bytes.Length < pattern.Length then
      bytes
    else
      // For each element of bytes, try to match every element of pattern with
      // it. If it matches, add in the relacement and skip the rest of the
      // pattern, otherwise skip
      let mutable result = [] // Add in reverse
      let mutable i = 0
      while i < bytes.Length - pattern.Length do
        let mutable matches = true
        let mutable j = 0
        while j < pattern.Length do
          if bytes[i + j] <> patternBytes[j] then
            matches <- false
            j <- pattern.Length // stop early
          else
            j <- j + 1
        if matches then
          // matched: save replacement, skip rest of pattern
          result <- replacementBytes @ result
          i <- i + pattern.Length
        else
          // not matched, char is in result, look at next char
          result <- bytes[i] :: result
          i <- i + 1
      // Add the final ones we skipped above
      for i = i to bytes.Length - 1 do
        result <- bytes[i] :: result
      // bytes are added in reverse, so one more reverse needed
      result |> List.reverse |> List.toArray

  // VS Code trims trailing whitespace from lines of code; <SPACE> in the
  // fixture survives that round-trip and is replaced here.
  let insertSpaces = replaceByteStrings "<SPACE>" " "

  /// Makes the test request to the server, testing the response matches
  /// expectations.
  let runTestRequest
    (handlerVersion : HandlerVersion)
    (port : int)
    (domain : string)
    (testRequest : byte array)
    (testExpectedResponse : byte array)
    : Task<unit> =
    task {
      let host = $"{domain}:{port}"

      let request =
        testRequest
        |> insertSpaces
        |> replaceByteStrings "HOST" host
        |> replaceByteStrings "DOMAIN" domain
        |> Http.setHeadersToCRLF

      // Check body matches content-length
      let incorrectContentTypeAllowed =
        testRequest
        |> UTF8.ofBytesWithReplacement
        |> String.contains "ALLOW-INCORRECT-CONTENT-LENGTH"
      if not incorrectContentTypeAllowed then
        let parsedTestRequest = Http.split request
        let contentLength =
          parsedTestRequest.headers
          |> List.find (fun (k, _) -> String.toLowercase k = "content-length")
        match contentLength with
        | None -> ()
        | Some(_, v) ->
          if String.contains "ALLOW-INCORRECT-CONTENT-LENGTH" v then
            ()
          else
            Expect.equal parsedTestRequest.body.Length (int v) ""

      // Check input LENGTH not set
      if
        testRequest |> UTF8.ofBytesWithReplacement |> String.contains "LENGTH"
        && not incorrectContentTypeAllowed
      then
        Expect.isFalse true "LENGTH substitution not done on request"

      // Make the request
      use! client = createClient port
      use stream = client.GetStream()
      stream.ReadTimeout <- 1000

      do! stream.WriteAsync(request, 0, request.Length)
      do! stream.FlushAsync()

      // Read the response
      let length = 10000
      let responseBuffer = Array.zeroCreate length
      let! byteCount = stream.ReadAsync(responseBuffer, 0, length)
      stream.Close()
      client.Close()
      let response = Array.take byteCount responseBuffer

      // Prepare expected response
      let expectedResponse =
        testExpectedResponse
        |> splitAtNewlines
        |> List.map (fun l -> List.append l [ newline ])
        |> List.flatten
        |> List.initial // remove final newline which we don't want
        |> List.toArray
        |> insertSpaces
        |> replaceByteStrings "HOST" host
        |> replaceByteStrings "DOMAIN" domain
        |> Http.setHeadersToCRLF

      // Parse and normalize the response
      let actual = Http.split response
      let expected = Http.split expectedResponse
      let expectedHeaders =
        normalizeExpectedHeaders handlerVersion expected.headers actual.body
      let actualHeaders = normalizeActualHeaders handlerVersion actual.headers

      // Compare strings
      match UTF8.ofBytesOpt actual.body, UTF8.ofBytesOpt expected.body with
      | Some actualBody, Some expectedBody ->
        Expect.equal
          (actual.status, actualHeaders, actualBody)
          (expected.status, expectedHeaders, expectedBody)
          $"(string)"
      | _ ->
        Expect.equal
          (actual.status, actualHeaders, actual.body)
          (expected.status, expectedHeaders, expected.body)
          $"(bytes)"
    }


/// Run one test fixture: build a router, start a per-test listener, fire
/// the request, compare, stop the listener.
let private runFixture (test : Test) : Task<unit> =
  task {
    let! exeState = executionStateFor pmPT true Map.empty

    let! handler = buildRouterForTest exeState test

    let port = allocateFreePort ()
    let cts = new CancellationTokenSource()

    let! listener = bindListener port

    let listenerTask =
      HttpServer.runListener
        exeState
        listener
        (int64 port)
        handler
        HttpServer.defaultMaxBodyBytes
        true // injectStandardHeaders
        true // canonicalizeFromForwardedProto
        false // logRequests — keep tests quiet
        cts.Token

    try
      do!
        Execution.runTestRequest
          Http
          port
          "localhost"
          test.request
          test.expectedResponse
    finally
      cts.Cancel()
      // Give the listener a moment to clean up; don't wait forever.
      try
        let waitTask = listenerTask
        if not (waitTask.Wait 2000) then () else ()
      with _ ->
        ()
  }


/// One server, one router, driven by raw sockets so the requests really are concurrent:
/// `requests` are (path, body) pairs sent with `method`, all at once. Returns (status line,
/// body) per request, in request order.
let private runRequestsAgainst
  (routerCode : string)
  (method : string)
  (requests : (string * byte[]) list)
  : Task<(string * byte[]) array> =
  task {
    let! exeState = executionStateFor pmPT true Map.empty
    let test =
      { handlers =
          [ { version = Http; route = "/"; method = method; code = routerCode } ]
        request = [||]
        expectedResponse = [||] }
    let! handler = buildRouterForTest exeState test
    let port = allocateFreePort ()
    let cts = new CancellationTokenSource()
    let! listener = bindListener port
    let listenerTask =
      HttpServer.runListener
        exeState
        listener
        (int64 port)
        handler
        HttpServer.defaultMaxBodyBytes
        false // injectStandardHeaders
        false // canonicalizeFromForwardedProto
        false // logRequests
        cts.Token
    let oneRequest (path : string, body : byte[]) : Task<string * byte[]> =
      task {
        let header =
          UTF8.toBytes
            $"{method} {path} HTTP/1.1\r\nHost: localhost:{port}\r\nContent-Length: {body.Length}\r\nConnection: close\r\n\r\n"
        let reqBytes = Array.append header body
        use client = new TcpClient()
        do! client.ConnectAsync("127.0.0.1", port)
        use stream = client.GetStream()
        do! stream.WriteAsync(reqBytes, 0, reqBytes.Length)
        do! stream.FlushAsync()
        // Read until the server closes (Connection: close); the cancel guards against a hang
        // if a connection is ever kept alive.
        use ms = new System.IO.MemoryStream()
        let buf = Array.zeroCreate 8192
        use readCts = new CancellationTokenSource(20_000)
        let mutable reading = true
        try
          while reading do
            let! n = stream.ReadAsync(buf, 0, buf.Length, readCts.Token)
            if n = 0 then reading <- false else ms.Write(buf, 0, n)
        with :? System.OperationCanceledException ->
          ()
        let parsed = Http.split (ms.ToArray())
        return (parsed.status, parsed.body)
      }
    try
      // `task { }` is hot, so mapping starts all requests concurrently.
      let! results = requests |> List.map oneRequest |> Task.WhenAll
      return results
    finally
      cts.Cancel()
      try
        listenerTask.Wait 2000 |> ignore<bool>
      with _ ->
        ()
  }

let private textOf (status : string, body : byte[]) : string * string =
  status, UTF8.ofBytesUnsafe body

/// Regression test for the ephemeral-blob HTTP race. The request body becomes an ephemeral
/// blob (`Http.Request.fromRequest`), which the handler reads back and the tracer promotes.
/// Bytes are inline, so there is no shared state for concurrent requests to race over: this
/// fires many overlapping body-echo requests and asserts each one gets ITS OWN body back.
let private concurrentEphemeralBlobRequests =
  testTask "concurrent requests don't lose or cross ephemeral blob bodies" {
    // Distinct, non-trivial body per request so a lost/mis-tagged blob
    // shows up as a wrong or empty echo, not a coincidental match.
    let bodyFor (i : int) : byte[] = UTF8.toBytes (String.replicate 64 $"req{i:D4}-")
    let! results =
      runRequestsAgainst
        "Darklang.Stdlib.Http.response request.body 200"
        "POST"
        ([ 1..64 ] |> List.map (fun i -> "/", bodyFor i))
    results
    |> Array.iteri (fun idx (status, body) ->
      let i = idx + 1
      Expect.stringContains
        status
        "200"
        $"request {i}: status 200 (lost blob => 500)"
      Expect.equal
        body
        (bodyFor i)
        $"request {i}: body echoed intact (no cross-request blob)")
  }


/// The path decides: `/slow` sleeps, `/boom` raises, anything else answers at once.
let private pathRouter =
  """(match request.url with
      | url when Darklang.Stdlib.String.contains url "/slow" ->
        let _ = Darklang.Stdlib.Cli.Posix.sleep 400.0
        Darklang.Stdlib.Http.responseWithText "slow done" 200
      | url when Darklang.Stdlib.String.contains url "/boom" ->
        Darklang.Stdlib.Http.responseWithText (Darklang.Stdlib.Int.toString (1 / 0)) 200
      | _ -> Darklang.Stdlib.Http.responseWithText "fast" 200)"""

/// `pathRouter` with the answer stamped with the millisecond it was made: `slow <ms>` or
/// `fast <ms>`, so a test can tell which finished first.
let private stampedRouter =
  """(match request.url with
      | url when Darklang.Stdlib.String.contains url "/slow" ->
        let _ = Darklang.Stdlib.Cli.Posix.sleep 400.0
        Darklang.Stdlib.Http.responseWithText
          ("slow " ++ Darklang.Stdlib.Int.toString (Darklang.Stdlib.DateTime.toMilliseconds (Darklang.Stdlib.DateTime.now ())))
          200
      | _ ->
        Darklang.Stdlib.Http.responseWithText
          ("fast " ++ Darklang.Stdlib.Int.toString (Darklang.Stdlib.DateTime.toMilliseconds (Darklang.Stdlib.DateTime.now ())))
          200)"""

// Sequenced: the timeout test lowers the process-wide `requestTimeoutMs` for its own server.
let private requestsAreProcesses =
  testSequenced
  <| testList
    "requests as processes"
    [ testTask "a slow handler does not hold up a fast one" {
        // The router stamps each answer with the time it was made, so the order the
        // requests finished in is on the wire, not in a wall-clock guess.
        let! results =
          runRequestsAgainst
            stampedRouter
            "GET"
            [ "/slow", [||]; "/fast", [||]; "/fast", [||]; "/fast", [||] ]
        let results = Array.map textOf results
        Expect.stringContains (fst results[0]) "200" "the slow request completes"
        let stamp (body : string) : int64 = int64 (body.Split(' ')[1])
        Expect.stringStarts (snd results[0]) "slow" "the slow body"
        for i in 1..3 do
          Expect.stringStarts (snd results[i]) "fast" $"fast request {i} answered"
          Expect.isLessThan
            (stamp (snd results[i]))
            (stamp (snd results[0]))
            $"fast request {i} answered before the slow one finished"
      }
      testTask "a handler past the request timeout gets a 504" {
        let before = HttpServer.requestTimeoutMs
        HttpServer.requestTimeoutMs <- 200
        try
          let! results =
            runRequestsAgainst pathRouter "GET" [ "/slow", [||]; "/fast", [||] ]
          let results = Array.map textOf results
          Expect.stringContains (fst results[0]) "504" "the slow request timed out"
          Expect.stringContains
            (snd results[0])
            "ran for more than 200 ms"
            "the body says why"
          Expect.equal (snd results[1]) "fast" "the fast request was unaffected"
        finally
          HttpServer.requestTimeoutMs <- before
      }
      testTask
        "a handler that raises gets a 500 with the error, not a type complaint" {
        let! results = runRequestsAgainst pathRouter "GET" [ "/boom", [||] ]
        let results = Array.map textOf results
        Expect.stringContains (fst results[0]) "500" "a failed handler is a 500"
        Expect.stringContains
          (snd results[0])
          "The handler failed"
          "the body names the failure"
        Expect.stringContains (snd results[0]) "divide" "the body carries the error"
      } ]


/// The serve builtin narrows `port`/`maxBodyBytes` (arbitrary-precision `Int`)
/// to native int64 and range-checks them BEFORE binding, turning what would be
/// a host `HttpListener`/overflow crash into a Dark `OutOfRange` error. This
/// asserts that guard: the values are rejected without a socket ever opening.
let private serveRejectsOutOfRangeArgs =
  testTask "serve rejects out-of-range port / maxBodyBytes with a Dark error" {
    let! exeState = executionStateFor pmPT true Map.empty

    let runServe (source : string) : Task<Result<RT.Dval, string>> =
      task {
        let! ptExpr = TestUtils.TestUtils.parsePTExpr source
        let rtInstrs = PT2RT.Expr.toRT Map.empty 0 None ptExpr
        match! Execution.executeExpr exeState rtInstrs with
        | Ok dval -> return Ok dval
        | Error(rte, _) ->
          match! Execution.runtimeErrorToString exeState rte with
          | Ok(RT.DString s) -> return Error s
          | _ -> return Error(string rte)
      }

    let handler = "(fun request -> Darklang.Stdlib.Http.notFound ())"

    // onListening: a no-op — the out-of-range checks fire before it would ever run.
    let onListening = "(fun () -> ())"

    // port 99999 is past the valid [0, 65535] range
    let! tooLargePort =
      runServe
        $"Builtin.httpServerServe 99999 {handler} 1048576 false false false {onListening}"
    match tooLargePort with
    | Error msg ->
      Expect.stringContains msg "out-of-range" "port 99999 -> Int OutOfRange"
    | Ok dval -> failtest $"expected OutOfRange for port 99999, got Ok {dval}"

    // a negative body limit would reject every request
    let! negativeBodyLimit =
      runServe
        $"Builtin.httpServerServe 8000 {handler} -1 false false false {onListening}"
    match negativeBodyLimit with
    | Error msg ->
      Expect.stringContains msg "out-of-range" "maxBodyBytes -1 -> Int OutOfRange"
    | Ok dval -> failtest $"expected OutOfRange for maxBodyBytes -1, got Ok {dval}"
  }


let tests =
  let t rootDir (filename : string) =
    testTask $"Http files: {filename}" {
      let shouldSkip = String.startsWith "_" filename

      let filenameAbs = $"{rootDir}/{filename}"
      let! contents = System.IO.File.ReadAllBytesAsync filenameAbs

      let test = ParseTest.parse contents

      if shouldSkip then
        let displayName =
          (if shouldSkip then String.dropLeft 1 filename else filename)
          |> String.dropRight (".test".Length)
        skiptest $"underscore test - {displayName}"
      else
        do! runFixture test
    }

  let fileTestLists =
    [ ($"{basePath}", "http") ]
    |> List.map (fun (dir, testListName) ->
      let tests =
        System.IO.Directory.GetFiles(dir, "*.test")
        |> Array.map (System.IO.Path.GetFileName)
        |> Array.toList
        |> List.map (t dir)
      testList testListName tests)
  testList
    "HttpServer"
    (serveRejectsOutOfRangeArgs
     :: concurrentEphemeralBlobRequests
     :: requestsAreProcesses
     :: fileTestLists)
