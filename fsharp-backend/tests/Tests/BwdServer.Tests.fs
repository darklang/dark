/// Tests the builtwithdark.com server (`BwdServer`),
/// which is the server that runs Dark users' HTTP handlers.
///
/// Test files are stored in the `tests/httptestfiles` directory,
/// which includes a README.md of how these tests work.
module Tests.BwdServer

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
module DvalRepr = LibExecution.DvalReprExternal

open TestUtils.TestUtils
open System.Text.Json

type Server =
  | OCaml
  | FSharp

type Test =
  { handlers : List<string * string * string>
    cors : Option<string>
    secrets : List<string * string>
    /// Allow testing of a specific canvas name
    canvasName : Option<string>
    customDomain : Option<string>
    request : byte array
    response : byte array }

type TestParsingState =
  | Limbo
  | InHttpHandler
  | InResponse
  | InRequest

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

let findIndex (pattern : 'a list) (list : 'a list) : int option =
  if pattern.Length = 0 || list.Length < pattern.Length then
    None
  else
    let mutable i = 0
    let mutable result = None
    while result = None && i <= list.Length - pattern.Length do
      let mutable matches = true
      let mutable j = 0
      while matches && j < pattern.Length do
        if list[i + j] <> pattern[j] then
          matches <- false
          j <- pattern.Length // stop early
        else
          j <- j + 1
      if matches then result <- Some i else i <- i + 1
    result

let findLastIndex (pattern : 'a list) (list : 'a list) : int option =
  findIndex (List.reverse pattern) (List.reverse list)
  // The index we get back is the index of the LAST item
  |> Option.map (fun i -> list.Length - i - pattern.Length)


/// Parse the test line-by-line. We don't use regex here because we want to test more than strings
let parseTest (bytes : byte array) : Test =
  let lines : List<List<byte>> = bytes |> splitAtNewlines
  let emptyTest =
    { handlers = []
      cors = None
      secrets = []
      customDomain = None
      canvasName = None
      request = [||]
      response = [||] }
  lines
  |> List.fold
    (Limbo, emptyTest)
    (fun (state : TestParsingState, result : Test) (line : List<byte>) ->
      let asString : string = line |> Array.ofList |> UTF8.ofBytesWithReplacement
      match asString with
      | Regex "\[cors (\S+)]" [ cors ] -> (Limbo, { result with cors = Some cors })
      | Regex "\[secrets (\S+)]" [ secrets ] ->
        let secrets =
          secrets
          |> String.split ","
          |> List.map (fun secret ->
            match secret |> String.split ":" with
            | [ key; value ] -> key, value
            | _ ->
              Exception.raiseInternal $"Could not parse secret" [ "secret", secret ])

        (Limbo, { result with secrets = secrets @ result.secrets })
      | Regex "\[custom-domain (\S+)]" [ customDomain ] ->
        (Limbo, { result with customDomain = Some customDomain })
      | Regex "\[canvas-name (\S+)]" [ canvasName ] ->
        (Limbo, { result with canvasName = Some canvasName })
      | "[request]" -> (InRequest, result)
      | "[response]" -> (InResponse, result)
      | Regex "\[http-handler (\S+) (\S+)\]" [ method; route ] ->
        (InHttpHandler,
         { result with handlers = result.handlers @ [ (method, route, "") ] })
      | _ ->
        match state with
        | InHttpHandler ->
          let newHandlers =
            match List.reverse result.handlers with
            | [] -> Exception.raiseInternal "There should be handlers already" []
            | (method, route, text) :: other ->
              List.reverse ((method, route, text + asString + "\n") :: other)
          InHttpHandler, { result with handlers = newHandlers }
        | InResponse ->
          InResponse,
          { result with
              response =
                Array.concat [| result.response; Array.ofList line; [| newline |] |] }
        | InRequest ->
          InRequest,
          { result with
              request =
                Array.concat [| result.request; Array.ofList line; [| newline |] |] }
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
           response = Array.slice 0 -1 test.response
           // Allow separation from the next section with a blank line
           request = Array.slice 0 -2 test.request }

/// Replace `pattern` in the byte array with `replacement` - both are provided as strings
/// for convenience, but obviously both will be converted to bytes
let replaceByteStrings
  (pattern : string)
  (replacement : string)
  (bytes : byte array)
  : byte array =
  let patternBytes = UTF8.toBytes pattern
  let replacementBytes = UTF8.toBytes replacement |> Array.toList |> List.reverse

  if pattern.Length = 0 || bytes.Length < pattern.Length then
    bytes
  else
    // For each element of bytes, try to match every element of pattern with it. If
    // it matches, add in the relacement and skip the rest of the pattern, otherwise
    // skip
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

/// Initializes and sets up a test canvas (handlers, secrets, etc.)
let setupTestCanvas (testName : string) (test : Test) : Task<Canvas.Meta> =
  task {
    let! (meta : Canvas.Meta) =
      let canvasName =
        match test.canvasName with
        | Some name -> Exact name
        | None -> Randomized $"bwdserver-{testName}"
      initializeTestCanvas canvasName

    // Handlers
    let oplists =
      test.handlers
      |> List.map (fun (httpMethod, httpRoute, progString) ->
        let (source : PT.Expr) =
          progString |> FSharpToExpr.parse |> FSharpToExpr.convertToExpr

        let gid = Prelude.gid

        let ids : PT.Handler.ids =
          { moduleID = gid (); nameID = gid (); modifierID = gid () }

        let h : PT.Handler.T =
          { tlid = gid ()
            pos = { x = 0; y = 0 }
            ast = source
            spec = PT.Handler.HTTP(route = httpRoute, method = httpMethod, ids = ids) }

        (h.tlid,
         [ PT.SetHandler(h.tlid, h.pos, h) ],
         PT.Toplevel.TLHandler h,
         Canvas.NotDeleted))

    do! Canvas.saveTLIDs meta oplists

    // CORS
    match test.cors with
    | None -> ()
    | Some "" -> do! Canvas.updateCorsSetting meta.id None
    | Some "*" -> do! Canvas.updateCorsSetting meta.id (Some Canvas.AllOrigins)
    | Some domains ->
      let domains = (Canvas.Origins(String.split "," domains))
      do! Canvas.updateCorsSetting meta.id (Some domains)

    // Custom domains
    match test.customDomain with
    | Some cd -> do! Routing.addCustomDomain cd meta.name
    | None -> ()

    // Secrets
    do!
      test.secrets
      |> List.map (fun (name, value) -> LibBackend.Secret.insert meta.id name value)
      |> Task.WhenAll
      |> Task.map (fun _ -> ())

    return meta
  }

let normalizeActualHeaders (hs : (string * string) list) : (string * string) list =
  hs
  |> List.filterMap (fun (k, v) ->
    match k, v with
    | "Date", _ -> Some(k, "xxx, xx xxx xxxx xx:xx:xx xxx")
    | "expires", _
    | "Expires", _ -> Some(k, "xxx, xx xxx xxxx xx:xx:xx xxx")
    | "x-darklang-execution-id", _ -> Some(k, "0123456789")
    | "age", _
    | "Age", _ -> None
    | "X-GUploader-UploadID", _
    | "x-guploader-uploadid", _ -> Some(k, "xxxx")
    | "x-goog-generation", _ -> Some(k, "xxxx")
    | _other -> Some(k, v))
  |> List.sortBy Tuple2.first // CLEANUP ocaml headers are sorted, inexplicably

let normalizeExpectedHeaders
  (headers : (string * string) list)
  (actualBody : byte array)
  : (string * string) list =
  headers
  |> List.map (fun (k, v) ->
    match k, v with
    // JSON can be different lengths, this plugs in the expected length
    | "Content-Length", "LENGTH" -> (k, string actualBody.Length)
    | _ -> (k, v))
  |> List.sortBy Tuple2.first

/// create a TCP client, used to make test HTTP requests
let createClient (port : int) : Task<TcpClient> =
  task {
    let client = new TcpClient()

    // Web server might not be loaded yet
    let mutable connected = false
    for i in 1..10 do
      try
        if not connected then
          do! client.ConnectAsync("127.0.0.1", port)
          connected <- true
      with
      | _ when i <> 10 ->
        print $"Server not ready on port {port}, maybe retry"
        do! System.Threading.Tasks.Task.Delay 1000
    return client
  }

let prepareRequest
  (request : byte array)
  (host : string)
  (canvasName : string)
  : byte array =
  let request =
    request
    |> replaceByteStrings "HOST" host
    |> replaceByteStrings "CANVAS" canvasName
    |> Http.setHeadersToCRLF

  // Check body matches content-length
  let incorrectContentTypeAllowed =
    request
    |> UTF8.ofBytesWithReplacement
    |> String.includes "ALLOW-INCORRECT-CONTENT-LENGTH"

  if not incorrectContentTypeAllowed then
    let parsedTestRequest = Http.split request
    let contentLength =
      parsedTestRequest.headers
      |> List.find (fun (k, v) -> String.toLowercase k = "content-length")
    match contentLength with
    | None -> ()
    | Some (_, v) ->
      if String.includes "ALLOW-INCORRECT-CONTENT-LENGTH" v then
        ()
      else
        Expect.equal parsedTestRequest.body.Length (int v) ""

  // Check input LENGTH not set
  if request |> UTF8.ofBytesWithReplacement |> String.includes "LENGTH"
     && not incorrectContentTypeAllowed then // false alarm as also have LENGTH in it
    Expect.isFalse true "LENGTH substitution not done on request"

  request

let makeRequest (request : byte array) (port : int) : Task<Http.T> =
  task {
    // Make the request
    use! client = createClient port
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
    return Http.split response
  }



/// Makes the test request to one of the servers,
/// testing the response matches expectations
let runTestRequest
  (canvasName : string)
  (testRequest : byte array)
  (testResponse : byte array)
  (server : Server)
  : Task<unit> =
  task {
    let port =
      match server with
      | OCaml -> TestConfig.ocamlServerNginxPort
      | FSharp -> TestConfig.bwdServerBackendPort

    let host = $"{canvasName}.builtwithdark.localhost:{port}"

    let request = prepareRequest testRequest host canvasName
    let! actual = makeRequest request port

    // Prepare expected response
    let expected =
      testResponse
      |> splitAtNewlines
      |> List.filterMap (fun line ->
        let asString = line |> List.toArray |> UTF8.ofBytesWithReplacement
        if String.includes "// " asString then
          if String.includes "OCAMLONLY" asString && server = FSharp then
            None
          else if String.includes "FSHARPONLY" asString && server = OCaml then
            None
          else if String.includes "KEEP" asString then
            Some line
          else
            // Remove final comment only
            let index =
              findLastIndex [ byte ' '; byte '/'; byte '/' ] line
              |> Option.orElse (findLastIndex [ byte '/'; byte '/' ] line)
              |> Exception.unwrapOptionInternal "cannot find comment" []
            line |> List.splitAt index |> Tuple2.first |> Some
        else
          Some line)
      |> List.map (fun l -> List.append l [ newline ])
      |> List.flatten
      |> List.initial // remove final newline which we don't want
      |> Exception.unwrapOptionInternal "cannot find newline" []
      |> List.toArray
      |> replaceByteStrings "HOST" host
      |> replaceByteStrings "CANVAS" canvasName
      |> Http.setHeadersToCRLF
      |> Http.split

    // Normalize the responses
    let expectedHeaders = normalizeExpectedHeaders expected.headers actual.body
    let actualHeaders = normalizeActualHeaders actual.headers

    // Decompress the body if returned with a content-encoding. Throws an exception
    // if content-encoding is set and the body is not compressed. This lets us test
    // that the server returns compressed content
    let actual =
      { actual with body = Http.decompressIfNeeded actual.headers actual.body }

    // Test as json or strings
    let asJson =
      try
        Some(
          DvalRepr.parseJson (UTF8.ofBytesUnsafe actual.body),
          DvalRepr.parseJson (UTF8.ofBytesUnsafe expected.body)
        )
      with
      | e -> None

    match asJson with
    | Some (aJson, eJson) ->
      let serialize (json : JsonDocument) = DvalRepr.writePrettyJson json.WriteTo
      Expect.equal
        (actual.status, actualHeaders, serialize aJson)
        (expected.status, expectedHeaders, serialize eJson)
        $"({server} as json)"
    | None ->
      match UTF8.ofBytesOpt actual.body, UTF8.ofBytesOpt expected.body with
      | Some actualBody, Some expectedBody ->
        Expect.equal
          (actual.status, actualHeaders, actualBody)
          (expected.status, expectedHeaders, expectedBody)
          $"({server} as string)"
      | _ ->
        Expect.equal
          (actual.status, actualHeaders, actual.body)
          (expected.status, expectedHeaders, expected.body)
          $"({server} as bytes)"
  }

/// Makes a test to be run
let t (filename : string) =
  testTask $"Httpfiles: {filename}" {
    let shouldSkip = String.startsWith "_" filename

    // read and parse the test
    let filename = $"tests/httptestfiles/{filename}"
    let! contents = System.IO.File.ReadAllBytesAsync filename

    let test = parseTest contents
    let testName =
      let withoutPrefix = if shouldSkip then String.dropLeft 1 filename else filename
      withoutPrefix |> String.dropRight (".test".Length)

    // set up a test canvas
    let! (meta : Canvas.Meta) = setupTestCanvas testName test

    // run the test (by calling the server)
    let callServer = runTestRequest (string meta.name) test.request test.response

    if shouldSkip then
      skiptest $"underscore test - {testName}"

    else
      do! callServer OCaml // check OCaml to see if we got the right answer
      do! callServer FSharp // test F# impl
  }

let testsFromFiles =
  let dir = "tests/httptestfiles/"

  System.IO.Directory.GetFiles(dir, "*.test")
  |> Array.map (System.IO.Path.GetFileName)
  |> Array.toList
  |> List.map t


let tests = testList "BwdServer" [ testList "httptestfiles" testsFromFiles ]

open Microsoft.Extensions.Hosting

// run our own webserver instead of relying on the dev webserver
let init (token : System.Threading.CancellationToken) : Task =
  let port = TestConfig.bwdServerBackendPort
  let k8sPort = TestConfig.bwdServerKubernetesPort
  let logger = configureLogging "test-bwdserver"
  (BwdServer.Server.webserver logger port k8sPort).RunAsync(token)
