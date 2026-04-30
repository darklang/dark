/// Tests the CLI's `Http.serve` builtin against the fixtures in
/// `testfiles/httphandler/` (the same byte-exact `.test` files the old
/// `BwdServer.Tests.fs` used).
///
/// Differences from the predecessor:
/// - No SQL canvas. Per-test handlers are assembled into an in-memory
///   router Dval via `TestUtils.HandlerGraph.buildRouter`.
/// - Per-test HttpListener. A free port is allocated with
///   `TcpListener(IPAddress.Loopback, 0)`; the listener spins for the
///   duration of the test and is stopped via `cts.Cancel()` in teardown.
/// - `domain` is fixed to `"localhost"` (single-canvas model). Fixtures
///   that use `[domain ...]` for multi-canvas dispatch are intrinsically
///   BwdServer-specific and live in `_disabled-by-cli-model/`.
module Tests.HttpServer

let basePath = "testfiles/httphandler"
let dataBasePath = "testfiles/data"

open Expecto

open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Net
open System.Net.Sockets

open Prelude

module RT = LibExecution.RuntimeTypes
module HttpServer = BuiltinHttpServer.Libs.HttpServer

open Tests
open TestUtils.TestUtils

type HandlerVersion = | Http

type TestHandler =
  { version : HandlerVersion; route : string; method : string; code : string }

type TestSecret = string * string * int

type Test =
  {
    handlers : List<TestHandler>
    secrets : List<TestSecret>
    /// Allow testing of a specific canvas name (no-op in single-canvas CLI mode).
    domain : Option<string>
    request : byte array
    expectedResponse : byte array
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
/// See details in `httphandler/README.md`
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

    let emptyTest =
      { handlers = []
        secrets = []
        domain = None
        request = [||]
        expectedResponse = [||] }

    lines
    |> List.fold
      (fun (state : TestParsingState, result : Test) (line : List<byte>) ->
        let asString : string = line |> Array.ofList |> UTF8.ofBytesWithReplacement
        match asString with
        | Regex.Regex "\[secrets (\S+)]" [ secrets ] ->
          let secrets =
            secrets
            |> String.split ","
            |> List.map (fun secret ->
              match secret |> String.split ":" with
              | [ key; value; version ] -> key, value, int version
              | _ ->
                Exception.raiseInternal
                  $"Could not parse secret"
                  [ "secret", secret ])

          (Limbo, { result with secrets = secrets @ result.secrets })
        | Regex.Regex "\[domain (\S+)]" [ domain ] ->
          (Limbo, { result with domain = Some domain })
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
let private allocateFreePort () : int =
  let listener = new TcpListener(IPAddress.Loopback, 0)
  listener.Start()
  let port = (listener.LocalEndpoint :?> IPEndPoint).Port
  listener.Stop()
  port


/// Build a router DApplicable from a parsed `Test`'s handlers.
let private buildRouterForTest
  (exeState : RT.ExecutionState)
  (test : Test)
  : Task<RT.Applicable> =
  task {
    let raw : List<TestUtils.HandlerGraph.RawHandler> =
      test.handlers
      |> List.reverse
      |> List.map (fun h ->
        { route = h.route; method = h.method; code = h.code })
    let! routerDval = TestUtils.HandlerGraph.buildRouter exeState raw
    match routerDval with
    | RT.DApplicable applicable -> return applicable
    | _ ->
      return
        Exception.raiseInternal
          "buildRouter returned non-DApplicable"
          [ "dval", routerDval ]
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
        // HttpListener auto-adds `Connection: close` on some responses
        // (notably error paths). The fixtures predate this — fixtures were
        // authored against Kestrel's default which omits it. Strip so the
        // comparison stays byte-meaningful.
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
      let mutable result = []
      let mutable i = 0
      while i < bytes.Length - pattern.Length do
        let mutable matches = true
        let mutable j = 0
        while j < pattern.Length do
          if bytes[i + j] <> patternBytes[j] then
            matches <- false
            j <- pattern.Length
          else
            j <- j + 1
        if matches then
          result <- replacementBytes @ result
          i <- i + pattern.Length
        else
          result <- bytes[i] :: result
          i <- i + 1
      for i = i to bytes.Length - 1 do
        result <- bytes[i] :: result
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
    let canvasID = System.Guid.NewGuid()
    let! exeState = executionStateFor pmPT canvasID false false Map.empty

    let! handler = buildRouterForTest exeState test

    let port = allocateFreePort ()
    let cts = new CancellationTokenSource()

    let listenerTask =
      HttpServer.runListener
        exeState
        (int64 port)
        handler
        HttpServer.defaultMaxBodyBytes
        true // injectStandardHeaders
        true // canonicalizeFromForwardedProto
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

  [ ($"{basePath}", "http") ]
  |> List.map (fun (dir, testListName) ->
    let tests =
      System.IO.Directory.GetFiles(dir, "*.test")
      |> Array.map (System.IO.Path.GetFileName)
      |> Array.toList
      |> List.map (t dir)
    testList testListName tests)
  |> testList "HttpServer"
