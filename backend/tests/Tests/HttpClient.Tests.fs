/// This module coordinates testing of our `HttpClient` builtin functions.
///
/// There are a variety of http client tests located in `../testfiles/httpclient`.
/// They all follow a standard format; this module loads and runs the tests.
/// See the `README.md` in that directory for how the tests are formatted and parsed.
///
/// These work almost exactly as our `LibExecution` tests do, which you can find tests
/// for in `LibExecution.Tests.fs`. It may make sense to merge these 2 test modules.
module Tests.HttpClient

let baseDirectory = "testfiles/httpclient"
let versions = [ "v0" ]

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.IO
open System.IO.Compression
open System.Text.RegularExpressions

type ConcurrentDictionary<'a, 'b> =
  System.Collections.Concurrent.ConcurrentDictionary<'a, 'b>

open Prelude

module RT = LibExecution.RuntimeTypes
module Stream = LibExecution.Stream
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module Exe = LibExecution.Execution
module PackageRefs = LibExecution.PackageRefs
module C2DT = LibExecution.CommonToDarkTypes

open TestUtils.TestUtils

type TestCase =
  {
    /// What we expect the webserver to receive when the HttpClient is used
    expectedRequest : Http.T

    /// The request that our webserver actually receives
    actualRequest : Option<Http.T>

    /// This is the result that our mock HTTP is configured to return for the test,
    /// as set by the test file.
    responseToReturn : Http.T
  }

let testCases : ConcurrentDictionary<string, TestCase> = ConcurrentDictionary()


let host = $"test.dlio.localhost:{TestConfig.httpClientPort}"

let normalizeHeaders
  (body : byte array)
  (headers : (string * string) list)
  : (string * string) list =
  headers
  |> List.map (fun (key, value) ->
    match value with
    // make writing tests easier
    | "HOST" when String.equalsCaseInsensitive "Host" key -> (key, host)
    // optionally change content length for writing responses more easily
    | "LENGTH" when String.equalsCaseInsensitive "Content-length" key ->
      key, string body.Length
    | other -> key, other)

let randomBytes =
  [ 0x2euy; 0x0Auy; 0xE8uy; 0xE6uy; 0xF1uy; 0xE0uy; 0x9Buy; 0xA6uy; 0xEuy ]

let updateBody (body : byte array) : byte array =
  // Test cases are parsed as strings, so we can't "add bytes" to the tests directly.
  // To get around this, we include the string "RANDOM_BYTES" in our test file,
  // and replace that during the test at run-time.
  let randomBytes = List.range 0 255 |> List.map (fun i -> byte i)

  let rec find (bytes : byte list) : byte list =
    match bytes with
    // Note: "51 41 ..." is equivalent to "RANDOM_BYTES"
    | 0x52uy :: 0x41uy :: 0x4Euy :: 0x44uy :: 0x4Fuy :: 0x4Duy :: 0x5Fuy :: 0x42uy :: 0x59uy :: 0x54uy :: 0x45uy :: 0x53uy :: tail ->
      randomBytes @ find tail
    | [] -> []
    | head :: tail -> head :: find tail

  body |> List.fromArray |> find |> List.toArray



module Internal =
  module D = LibExecution.DvalDecoder
  module PT2DT = LibExecution.ProgramTypesToDarkTypes

  module Test =
    type PTTest =
      { name : string; lineNumber : int64; actual : PT.Expr; expected : PT.Expr }

    let fromDT (d : RT.Dval) : PTTest =
      match d with
      | RT.DRecord(_, _, _, fields) ->
        { name = fields |> D.field "name" |> D.string
          lineNumber = fields |> D.field "lineNumber" |> D.int64
          actual = fields |> D.field "actual" |> PT2DT.Expr.fromDT
          expected = fields |> D.field "expected" |> PT2DT.Expr.fromDT }
      | _ -> Exception.raiseInternal "Invalid Test" []


let parseSingleTestFromFile
  (filename : string)
  (test : string)
  : Ply<Internal.Test.PTTest> =
  uply {
    let! (state : RT.ExecutionState) =
      let canvasID = System.Guid.NewGuid()
      executionStateFor pmPT canvasID false false Map.empty

    let name =
      RT.FQFnName.fqPackage (PackageRefs.Fn.Internal.Test.parseSingleTestFromFile ())

    let args = NEList.ofList (RT.DString filename) [ RT.DString test ]
    let! execResult = LibExecution.Execution.executeFunction state name [] args

    match execResult with
    | Ok dval ->
      match C2DT.Result.fromDT identity dval identity with
      | Ok testVal -> return Internal.Test.fromDT testVal
      | Error(RT.DString errMsg) ->
        return
          Exception.raiseInternal
            $"Error parsing test: {errMsg}"
            [ "filename", filename; "test", test ]
      | Error _ ->
        return
          Exception.raiseInternal
            "Invalid error format from parseSingleTestFromFile"
            [ "dval", dval ]
    | Error(rte, _) ->
      let! rteString = Exe.rteToString RT2DT.RuntimeError.toDT state rte
      return
        Exception.raiseInternal
          "Error executing parseSingleTestFromFile function"
          [ "error", rteString ]
  }

let makeTest versionName filename =
  // Parse the file contents now, rather than later, so that tests that refer to
  // other tests (that is, tests for redirects) will work.
  let shouldSkipTest = String.startsWith "_" filename
  let testName =
    let withoutPrefix =
      if shouldSkipTest then String.dropLeft 1 filename else filename
    withoutPrefix |> String.dropRight 5 // ".test"

  let filename = $"{baseDirectory}/{versionName}/{filename}"
  let content = System.IO.File.ReadAllBytes filename |> UTF8.ofBytesUnsafe

  // Parse Handler code, expected HTTP request, and (static) HTTP response to return
  let expectedRequest, response, darkCode =
    let m =
      Regex.Match(
        content,
        "^(\[expected-request\]\n(.*)\n)\[response\]\n(.*)\n\n\[test\]\n(.*)$",
        RegexOptions.Singleline
      )

    if not m.Success then
      Exception.raiseInternal $"incorrect format" [ "name", testName ]
    let g = m.Groups

    (g[2].Value, g[3].Value, g[4].Value)

  let expected =
    expectedRequest |> UTF8.toBytes |> Http.setHeadersToCRLF |> Http.split
  let newExpectedBody = updateBody expected.body
  let expected =
    { expected with
        headers = normalizeHeaders newExpectedBody expected.headers
        body = newExpectedBody }

  let response = response |> UTF8.toBytes |> Http.setHeadersToCRLF |> Http.split
  let newResponseBody = updateBody response.body
  let response =
    { response with
        headers = normalizeHeaders newResponseBody response.headers
        body = newResponseBody }

  let dictKey = $"{versionName}/{testName}"

  testCases[dictKey] <-
    { expectedRequest = expected; actualRequest = None; responseToReturn = response }


  // Load the testcases first so that redirection works
  testTask testName {
    // debuG "expectedRequest" (toStr expectedRequest)
    // debuG "response" (toStr response)
    // debuG "darkCode" darkCode

    if shouldSkipTest then
      skiptest $"underscore test - {testName}"
    else
      // Set up the canvas
      let canvasID = System.Guid.NewGuid()
      let! exeState = executionStateFor pmPT canvasID false true Map.empty

      // Parse the Dark code
      let! (test : Internal.Test.PTTest) =
        darkCode
        |> String.replace "URL" $"{host}/{versionName}/{testName}"
        // CLEANUP: this doesn't use the correct length, as it might be latin1 or
        // compressed
        |> String.replace "LENGTH" (string response.body.Length)
        |> parseSingleTestFromFile "httpclient.tests.fs"
        |> Ply.toTask

      // Run the handler (call the HTTP client)
      // Note: this will update the corresponding value in `testCases` with the
      // actual request received
      let! actual =
        let instrs = test.actual |> PT2RT.Expr.toRT Map.empty 0 None
        Exe.executeExpr exeState instrs

      // First check: expected HTTP request matches actual HTTP request
      let tc = testCases[dictKey]
      match tc.actualRequest with
      | None ->
        // We failed to make a request - almost undoubtedly, the result will be some
        // sort of error
        () //Expect.equal 1 2 "Unexpected - no actual request has been saved"
      | Some actualRequest ->
        Expect.equal actualRequest tc.expectedRequest "requests don't match"

      // Second check: expected result (Dval) matches actual result (Dval)
      let actual = Result.map normalizeDvalResult actual

      let! expected =
        let instrs = test.expected |> PT2RT.Expr.toRT Map.empty 0 None
        Exe.executeExpr exeState instrs

      // Promote ephemeral blobs on both sides so two independently-built
      // Blobs with identical bytes (different UUIDs) compare equal.
      let noopInsert _ _ = uply { return () }
      let promoteIfOk r =
        task {
          match r with
          | Ok dv ->
            let! p = LibExecution.Blob.promote exeState noopInsert dv |> Ply.toTask
            return Ok p
          | Error _ -> return r
        }
      let! actual = promoteIfOk actual
      let! expected = promoteIfOk expected

      match actual, expected with
      | Ok actual, Ok expected ->
        return Expect.RT.equalDval actual expected $"Responses don't match"
      | _ -> Expect.equal actual expected $"Responses don't match"
  }


// ---------------
// This is the webserver that we will be testing against.
// It records the received request, and returns the test case output.
// ---------------
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions
open Microsoft.Extensions.Hosting

type Compression =
  | Deflate
  | Brotli
  | Gzip

let runTestHandler (ctx : HttpContext) : Task<HttpContext> =
  task {
    try
      let versionName, testName =
        let segments = System.Uri(ctx.Request.Path.Value).Segments

        let versionName = segments[1]
        let versionName =
          if String.endsWith "/" versionName then
            String.dropRight 1 versionName
          else
            versionName

        let testName = segments[2]
        let testName =
          if String.endsWith "/" testName then
            String.dropRight 1 testName
          else
            testName

        versionName, testName

      let dictKey = $"{versionName}/{testName}"

      let testCase =
        try
          Some testCases[dictKey]
        with _ ->
          None

      match testCase with
      | None ->
        ctx.Response.StatusCode <- 404
        let body = "intentionally not found" |> UTF8.toBytes
        ctx.Response.ContentLength <- int64 body.Length
        do! ctx.Response.Body.WriteAsync(body, 0, body.Length)
        return ctx

      | Some testCase ->
        // Gather status, headers, and body from the actual request
        let actualStatus =
          $"{ctx.Request.Method} {ctx.Request.GetEncodedPathAndQuery()} {ctx.Request.Protocol}"
        let actualHeaders = BwdServer.Server.getHeadersWithoutMergingKeys ctx
        let! actualBody = BwdServer.Server.getBody ctx
        let actualRequest : Http.T =
          { status = actualStatus; headers = actualHeaders; body = actualBody }


        // Update the TestCase with the actual request;
        // also, update the PATH in the status line of the expected request
        let updatedTestCase =
          { testCase with
              actualRequest = Some actualRequest
              expectedRequest =
                { testCase.expectedRequest with
                    status =
                      testCase.expectedRequest.status
                      |> String.replace "PATH" ctx.Request.Path.Value } }
        testCases[dictKey] <- updatedTestCase


        // Return the response
        let mutable compression = None
        let mutable transcodeToLatin1 = false

        ctx.Response.StatusCode <-
          testCase.responseToReturn.status
          |> String.split " "
          |> List.getAt 1
          |> Exception.unwrapOptionInternal
            "invalid status code"
            [ "status", testCase.responseToReturn.status ]
          |> int

        testCase.responseToReturn.headers
        |> List.iter (fun (k, v) ->
          if String.equalsCaseInsensitive k "Content-Encoding" then
            if v = "deflate" then compression <- Some Deflate
            else if v = "br" then compression <- Some Brotli
            else if v = "gzip" then compression <- Some Gzip
            else ()
          elif String.equalsCaseInsensitive k "Content-Type" then
            if
              v.Contains "charset=iso-8859-1"
              || v.Contains "charset=latin1"
              || v.Contains "us-ascii"
            then
              transcodeToLatin1 <- true

          BwdServer.Server.setResponseHeader ctx k v)

        let data =
          if transcodeToLatin1 then
            System.Text.Encoding.Convert(
              System.Text.Encoding.UTF8,
              System.Text.Encoding.Latin1,
              testCase.responseToReturn.body
            )
          else
            testCase.responseToReturn.body

        match compression with
        | Some algo ->
          let stream : Stream =
            let body = ctx.Response.Body
            match algo with
            | Gzip -> new GZipStream(body, CompressionMode.Compress)
            | Brotli -> new BrotliStream(body, CompressionMode.Compress)
            | Deflate -> new DeflateStream(body, CompressionMode.Compress)
          do! stream.WriteAsync(data, 0, data.Length)
          do! stream.FlushAsync()
          do! stream.DisposeAsync()
        | None ->
          if ctx.Response.StatusCode <> 304 then
            do! ctx.Response.Body.WriteAsync(data, 0, data.Length)

        return ctx
    with e ->
      // It might already have started, in which case let's just get the exception in
      // the body and hope that helps
      if not ctx.Response.HasStarted then ctx.Response.StatusCode <- 500

      let body = $"{e.Message}\n\n{e.StackTrace}"
      print $"{body}-{ctx.Request.Path}"
      let body = UTF8.toBytes body

      do! ctx.Response.Body.WriteAsync(body, 0, body.Length)
      return ctx
  }



let configureApp (app : IApplicationBuilder) =
  let handler (ctx : HttpContext) : Task = runTestHandler ctx
  app.Run(RequestDelegate handler)

let webserver () =
  Host.CreateDefaultBuilder()
  |> fun h -> h.ConfigureLogging(configureLogging "test-httpclient-server")
  |> fun h ->
      h.ConfigureWebHost(fun wh ->
        wh
        |> fun wh -> wh.UseKestrel()
        |> fun wh -> wh.UseUrls($"http://*:{TestConfig.httpClientPort}")
        |> fun wh -> wh.Configure(configureApp)
        |> ignore<IWebHostBuilder>)
  |> fun h -> h.Build()

// run a webserver to read test input
let init (token : System.Threading.CancellationToken) : Task =
  (webserver ()).RunAsync(token)

let testsFromFiles version =
  System.IO.Directory.GetFiles($"{baseDirectory}/{version}", "*.test")
  |> Array.map (System.IO.Path.GetFileName)
  |> Array.toList
  |> List.map (makeTest version)


// ---------------
// Shared mock-server helpers — register a canned response by URL path,
// then hit `http://{host}/v0/{name}` to retrieve it. Used by both the
// DStream HTTP tests and the file-driven httpclient.tests corpus.
// ---------------
module MockHelpers =
  module HC = BuiltinExecution.Libs.HttpClient

  let httpConfig : HC.Configuration = { HC.defaultConfig with timeoutInMs = 5000 }

  let httpClient = HC.BaseClient.create httpConfig

  /// Register a test case in the mock server and return the URL to hit.
  let registerTestCase
    (name : string)
    (responseStatus : int)
    (contentType : string)
    (body : string)
    : string =
    let bodyBytes = UTF8.toBytes body
    let responseHeaders =
      [ ("Date", "xxx")
        ("Content-Type", contentType)
        ("Content-Length", string bodyBytes.Length) ]
    testCases[$"v0/{name}"] <-
      { expectedRequest = { status = ""; headers = []; body = [||] }
        actualRequest = None
        responseToReturn =
          { status = $"HTTP/1.1 {responseStatus} OK"
            headers = responseHeaders
            body = bodyBytes } }
    $"http://{host}/v0/{name}"

  let getRequest (url : string) : HC.Request =
    { url = url; method = System.Net.Http.HttpMethod.Get; headers = []; body = [||] }


// ————————————————————————————————————————————————————————————
// Tests for the DStream-returning `HttpClient.stream` builtin. The
// mock-server infrastructure at the top of this file is reused so
// we exercise the real Kestrel path end-to-end. The tests call
// `openStreamingRequest` + construct a FromIO directly to avoid
// standing up a full ExecutionState — the DStream drain path is
// the whole point of the test.
// ————————————————————————————————————————————————————————————
module StreamDvalTests =
  module HC = BuiltinExecution.Libs.HttpClient
  module Dval = LibExecution.Dval
  module RT = LibExecution.RuntimeTypes
  module VT = LibExecution.ValueType

  let httpConfig : HC.Configuration = { HC.defaultConfig with timeoutInMs = 5000 }
  let httpClient = HC.BaseClient.create httpConfig

  let private getRequest (url : string) : HC.Request =
    { url = url; method = System.Net.Http.HttpMethod.Get; headers = []; body = [||] }

  /// Build a DStream wrapping an open HttpResponseMessage's body —
  /// same closure shape as the builtin. Returns the DStream and a
  /// ref<bool> that flips when the disposer runs, so tests can
  /// assert cleanup.
  let private buildBodyStream
    (response : System.Net.Http.HttpResponseMessage)
    : Task<RT.Dval * bool ref> =
    task {
      let! responseStream = response.Content.ReadAsStreamAsync()
      let buffer = Array.zeroCreate<byte> 8192
      let mutable bufferLen = 0
      let mutable bufferPos = 0
      let next () : Ply<Option<RT.Dval>> =
        uply {
          if bufferPos >= bufferLen then
            let! n = responseStream.ReadAsync(buffer, 0, buffer.Length)
            if n = 0 then
              return None
            else
              bufferLen <- n
              bufferPos <- 0
              let b = buffer[bufferPos]
              bufferPos <- bufferPos + 1
              return Some(RT.DUInt8 b)
          else
            let b = buffer[bufferPos]
            bufferPos <- bufferPos + 1
            return Some(RT.DUInt8 b)
        }
      let disposerRan = ref false
      let disposer () =
        disposerRan.Value <- true
        responseStream.Dispose()
        response.Dispose()
      return Stream.newFromIO VT.uint8 next (Some disposer), disposerRan
    }


  let drainToBytes (s : RT.Dval) : Task<byte[]> =
    task {
      use ms = new System.IO.MemoryStream()
      let mutable keepGoing = true
      while keepGoing do
        let! pulled = Stream.readNext s |> Ply.toTask
        match pulled with
        | Some(RT.DUInt8 b) -> ms.WriteByte b
        | Some _ -> Exception.raiseInternal "expected DUInt8" []
        | None -> keepGoing <- false
      return ms.ToArray()
    }


  let tests =
    testList
      "HttpClient.stream (DStream)"
      [ testTask "drain preserves every byte; response length matches" {
          let body = "hello streams"
          let url =
            MockHelpers.registerTestCase "stream-dval-basic" 200 "text/plain" body
          let! setup = HC.openStreamingRequest httpConfig httpClient (getRequest url)
          match setup with
          | Error e -> failtest $"expected Ok, got Error: {e}"
          | Ok(response, _headers) ->
            let! (s, (disposerRan : bool ref)) = buildBodyStream response
            let! (bytes : byte[]) = drainToBytes s
            Expect.equal
              (UTF8.ofBytesUnsafe bytes)
              body
              "drained bytes round-trip through the stream"
            Expect.equal
              bytes.Length
              (UTF8.toBytes body).Length
              "drained length matches source"
            Expect.isTrue
              disposerRan.Value
              "drain-to-EOF runs the disposer (frees the response)"
        }

        testTask "large body drains cleanly across buffer refills" {
          // 8 KB buffer * 3.5 -> forces multiple refills.
          let body = String.replicate 28000 "x"
          let url =
            MockHelpers.registerTestCase "stream-dval-large" 200 "text/plain" body
          let! setup = HC.openStreamingRequest httpConfig httpClient (getRequest url)
          match setup with
          | Error e -> failtest $"expected Ok, got Error: {e}"
          | Ok(response, _headers) ->
            let! (s, _) = buildBodyStream response
            let! (bytes : byte[]) = drainToBytes s
            Expect.equal
              bytes.Length
              body.Length
              "all 28k bytes pulled through multiple buffer refills"
        }

        testTask "empty body drains to zero bytes; disposer still runs" {
          let url =
            MockHelpers.registerTestCase "stream-dval-empty" 200 "text/plain" ""
          let! setup = HC.openStreamingRequest httpConfig httpClient (getRequest url)
          match setup with
          | Error e -> failtest $"expected Ok, got Error: {e}"
          | Ok(response, _headers) ->
            let! (s, (disposerRan : bool ref)) = buildBodyStream response
            let! (bytes : byte[]) = drainToBytes s
            Expect.equal bytes.Length 0 "empty body = zero bytes"
            Expect.isTrue disposerRan.Value "disposer runs even on empty drain"
        }

        testTask "streamClose before drain disposes the response" {
          let body = "unused"
          let url =
            MockHelpers.registerTestCase "stream-dval-close" 200 "text/plain" body
          let! setup = HC.openStreamingRequest httpConfig httpClient (getRequest url)
          match setup with
          | Error e -> failtest $"expected Ok, got Error: {e}"
          | Ok(response, _headers) ->
            let! (s, (disposerRan : bool ref)) = buildBodyStream response
            // Replicate streamClose: flip disposed, walk impl chain.
            match s with
            | RT.DStream(impl, disposed, _) ->
              disposed.Value <- true
              Stream.disposeImpl impl
            | _ -> failtest "expected DStream"
            Expect.isTrue disposerRan.Value "disposer runs on explicit close"
            // Subsequent pulls yield None.
            let! after = Stream.readNext s |> Ply.toTask
            Expect.equal after None "closed stream yields None"
        } ]


let tests =
  [ versions |> List.map (fun v -> testList v (testsFromFiles v))
    [ StreamDvalTests.tests ] ]
  |> List.concat
  |> testList "HttpClient"
