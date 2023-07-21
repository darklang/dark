/// Tests the darklang.io server (`BwdServer`), which is the server that
/// runs Dark users' HTTP handlers.
///
/// Test files are stored in the `testfiles/httphandler` directory, which
/// includes a relevant README.md.
module Tests.BwdServer

let basePath = "testfiles/httphandler"
let dataBasePath = "testfiles/data"

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Net.Sockets
open System.Text.Json

open Prelude
open Tablecloth
open Prelude.Tablecloth

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module Routing = LibBackend.Routing
module Canvas = LibBackend.Canvas
module Serialize = LibBackend.Serialize

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
    /// Allow testing of a specific canvas name
    domain : Option<string>
    request : byte array
    expectedResponse : byte array
  }


let newline = byte '\n'

/// Take a byte array and split it by newline, returning a list of lists. The
/// arrays do NOT have the newlines in them.
let splitAtNewlines (bytes : byte array) : byte list list =
  bytes
  |> Array.fold [ [] ] (fun state b ->
    if b = newline then
      [] :: state
    else
      match state with
      | [] -> Exception.raiseInternal "can't have no entries" []
      | head :: rest -> (b :: head) :: rest)
  |> List.map List.reverse
  |> List.reverse


/// Used to parse a .test file
/// See details in httptestfiles/README.md
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
      (Limbo, emptyTest)
      (fun (state : TestParsingState, result : Test) (line : List<byte>) ->
        let asString : string = line |> Array.ofList |> UTF8.ofBytesWithReplacement
        match asString with
        | Regex "\[secrets (\S+)]" [ secrets ] ->
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
        | Regex "\[domain (\S+)]" [ domain ] ->
          (Limbo, { result with domain = Some domain })
        | "[request]" -> (InRequest, result)
        | "[response]" -> (InResponse, result)

        | Regex "\[http-handler (\S+) (\S+)\]" [ method; route ] ->
          (InHttpHandler,
           { result with
               handlers =
                 { version = Http; route = route; method = method; code = "" }
                 :: result.handlers })

        | Regex "\<IMPORT_DATA_FROM_FILE=(\S+)\>" [ dataFileToInject ] ->
          // TODO do this as part of run-time, not during parse-time
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
    |> Tuple2.second
    |> fun test ->
        { test with
            // Remove the superfluously added newline on response
            expectedResponse = Array.slice 0 -1 test.expectedResponse
            // Allow separation from the next section with a blank line
            request = Array.slice 0 -2 test.request }


/// Initializes and sets up a test canvas (handlers, secrets, etc.)
let setupTestCanvas (testName : string) (test : Test) : Task<CanvasID * string> =
  task {
    let! (canvasID, domain) = initializeTestCanvas' $"bwdserver-{testName}"
    let resolver =
      Parser.NameResolver.fromBuiltins LibRealExecution.RealExecution.builtins

    // Handlers
    let oplists =
      test.handlers
      |> List.map (fun handler ->
        let source =
          Parser.Parser.parsePTExpr resolver "BwdServer.Tests.fs" handler.code

        let spec =
          match handler.version with
          | Http -> PT.Handler.HTTP(route = handler.route, method = handler.method)

        let h : PT.Handler.T = { tlid = gid (); ast = source; spec = spec }

        (PT.Toplevel.TLHandler h, Serialize.NotDeleted))

    do! Canvas.saveTLIDs canvasID oplists

    // Custom domains
    match test.domain with
    | Some domain -> do! Canvas.addDomain canvasID domain
    | None -> ()

    // Secrets
    do!
      test.secrets
      |> List.map (fun (name, value, version) ->
        LibBackend.Secret.insert canvasID name value version)
      |> Task.WhenAll
      |> Task.map (fun _ -> ())

    return canvasID, domain
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
        | "x-darklang-execution-id", _ -> Some(k, "0123456789")
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

      // Web server might not be loaded yet
      let mutable connected = false
      for i in 1..10 do
        try
          if not connected then
            do! client.ConnectAsync("127.0.0.1", port)
            connected <- true
        with _ when i <> 10 ->
          print $"Server not ready on port {port}, maybe retry"
          do! System.Threading.Tasks.Task.Delay 1000
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

  // This is to handle code editors (vs code) that trim whitespace at the end
  // of a line of code, which can be annoying
  let insertSpaces = replaceByteStrings "<SPACE>" " "

  /// Makes the test request to one of the servers,
  /// testing the response matches expectations
  let runTestRequest
    (handlerVersion : HandlerVersion)
    (domain : string)
    (testRequest : byte array)
    (testExpectedResponse : byte array)
    : Task<unit> =
    task {
      let port = TestConfig.bwdServerBackendPort

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
        |> String.includes "ALLOW-INCORRECT-CONTENT-LENGTH"
      if not incorrectContentTypeAllowed then
        let parsedTestRequest = Http.split request
        let contentLength =
          parsedTestRequest.headers
          |> List.find (fun (k, _) -> String.toLowercase k = "content-length")
        match contentLength with
        | None -> ()
        | Some(_, v) ->
          if String.includes "ALLOW-INCORRECT-CONTENT-LENGTH" v then
            ()
          else
            Expect.equal parsedTestRequest.body.Length (int v) ""

      // Check input LENGTH not set
      if
        testRequest |> UTF8.ofBytesWithReplacement |> String.includes "LENGTH"
        && not incorrectContentTypeAllowed
      then // false alarm as also have LENGTH in it
        Expect.isFalse true "LENGTH substitution not done on request"

      // Make the request
      use! client = createClient (port)
      use stream = client.GetStream()
      stream.ReadTimeout <- 1000 // responses should be instant, right?

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
        |> Exception.unwrapOptionInternal "cannot find newline" []
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

let tests =
  /// Makes a test to be run
  let t rootDir handlerType (filename : string) =
    testTask $"Http files: {filename}" {
      let shouldSkip = String.startsWith "_" filename

      // read and parse the test
      let filename = $"{rootDir}/{filename}"
      let! contents = System.IO.File.ReadAllBytesAsync filename

      let test = ParseTest.parse contents
      let testName =
        let withoutPrefix =
          if shouldSkip then String.dropLeft 1 filename else filename
        withoutPrefix |> String.dropRight (".test".Length)

      // set up a test canvas
      let! (canvasID, domain) = setupTestCanvas testName test

      // execute the test
      if shouldSkip then
        skiptest $"underscore test - {testName}"
      else
        do!
          Execution.runTestRequest
            handlerType
            domain
            test.request
            test.expectedResponse
    }

  [ ($"{basePath}", "http", Http) ]
  |> List.map (fun (dir, testListName, handlerType) ->
    let tests =
      System.IO.Directory.GetFiles(dir, "*.test")
      |> Array.map (System.IO.Path.GetFileName)
      |> Array.toList
      |> List.map (t dir handlerType)
    testList testListName tests)
  |> testList "BwdServer"

open Microsoft.Extensions.Hosting

let init (token : System.Threading.CancellationToken) : Task =
  // run our own webserver instead of relying on the dev webserver
  let port = TestConfig.bwdServerBackendPort
  let k8sPort = TestConfig.bwdServerKubernetesPort
  let logger = configureLogging "test-bwdserver"
  (BwdServer.Server.webserver logger port k8sPort).RunAsync(token)
