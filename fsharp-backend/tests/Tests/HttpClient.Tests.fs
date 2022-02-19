module Tests.HttpClient

// There are a variety of http client tests located in
// "./tests/httpclienttestfiles".
// They all follow a standard format, and this file provides
// a framework for loading, running, and assessing those tests

let baseDirectory = "tests/httpclienttestfiles"

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.IO
open System.IO.Compression
open System.Text.RegularExpressions

type ConcurrentDictionary<'a, 'b> =
  System.Collections.Concurrent.ConcurrentDictionary<'a, 'b>

open NReco.Logging.File

open Prelude
open Tablecloth

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module Exe = LibExecution.Execution

open TestUtils.TestUtils

type TestCase = { expected : Http.T; result : Http.T }

let testCases : ConcurrentDictionary<string, TestCase> = ConcurrentDictionary()

let host = $"test.builtwithdark.localhost:{TestConfig.httpClientPort}"

let normalizeHeaders
  (body : byte array)
  (headers : (string * string) list)
  : (string * string) list =
  headers
  |> List.map (function
    // make writing tests easier
    | (key, "HOST") when String.equalsCaseInsensitive "Host" key -> (key, host)
    // optionally change content length for writing responses more easily
    | (key, "LENGTH") when String.equalsCaseInsensitive "Content-length" key ->
      (key, string body.Length)
    | other -> other)

let randomBytes =
  [ 0x2euy; 0x0Auy; 0xE8uy; 0xE6uy; 0xF1uy; 0xE0uy; 0x9Buy; 0xA6uy; 0xEuy ]

let updateBody (body : byte array) : byte array =
  // Test cases are parsed as strings, so we can't "add bytes" to the tests directly.
  // To get around this, we include the string "RANDOM_BYTES" in our test file,
  // and replace that during the test at run-time.
  let randomBytes =
    [ 0x2euy; 0x0Auy; 0xE8uy; 0xE6uy; 0xF1uy; 0xE0uy; 0x9Buy; 0xA6uy; 0xEuy ]

  let rec find (bytes : byte list) : byte list =
    match bytes with
    // Note: "51 41 ..." is equivalent to "RANDOM_BYTES"
    | 0x52uy :: 0x41uy :: 0x4Euy :: 0x44uy :: 0x4Fuy :: 0x4Duy :: 0x5Fuy :: 0x42uy :: 0x59uy :: 0x54uy :: 0x45uy :: 0x53uy :: tail ->
      randomBytes @ find tail
    | [] -> []
    | head :: tail -> head :: find tail

  body |> List.fromArray |> find |> List.toArray






let makeTest versionName filename =
  // Parse the file contents now, rather than later, so that tests that refer to
  // other tests (that is, tests for redirects) will work.
  let shouldSkipTest = String.startsWith "_" filename
  let testName =
    let withoutPrefix =
      if shouldSkipTest then String.dropLeft 1 filename else filename
    withoutPrefix |> String.dropRight 5 // ".test"

  let filename = $"{baseDirectory}/{versionName}/{filename}"
  let contents = System.IO.File.ReadAllBytes filename
  let content = UTF8.ofBytesUnsafe contents

  // Parse the Dark code and expected HTTP request,
  // as well as the (static) HTTP response to return
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

  testCases[$"{versionName}/{testName}"] <- { expected = expected
                                              result = response }


  // Load the testcases first so that redirection works
  testTask $"HttpClient files: {filename}" {
    // debuG "expectedRequest" (toStr expectedRequest)
    // debuG "response" (toStr response)
    // debuG "darkCode" darkCode

    if shouldSkipTest then
      skiptest $"underscore test - {testName}"
    else
      let! meta = createTestCanvas $"http-client-{testName}"

      // Parse the code
      let shouldEqual, actualDarkProg, expectedResult =
        darkCode
        |> String.replace "URL" $"{host}/{versionName}/{testName}"
        // CLEANUP: this doesn't use the correct length, as it might be latin1 or
        // compressed
        |> String.replace "LENGTH" (string response.body.Length)
        |> FSharpToExpr.parse
        |> FSharpToExpr.convertToTest

      let! state = executionStateFor meta Map.empty Map.empty

      // The Dark program is an expression - what's the expected result of that expression?
      // note: in many cases, the expression is some sort of equality-checker,
      //   in which case this 'expected' is an eBool (a known runtime type)
      let! expected =
        Exe.executeExpr state Map.empty (expectedResult.toRuntimeType ())

      let debugMsg = $"\n\n{actualDarkProg}\n=\n{expectedResult}\n\n"

      // Which tests to run
      let testOCaml, testFSharp =
        if String.includes "FSHARPONLY" darkCode then (false, true)
        else if String.includes "OCAMLONLY" darkCode then (true, false)
        else (true, true)

      // Test OCaml
      if testOCaml then
        let! ocamlActual =
          try
            LibBackend.OCamlInterop.execute
              state.program.accountID
              state.program.canvasID
              actualDarkProg
              Map.empty
              []
              []

          with
          | e ->
            Exception.raiseInternal
              $"When calling OCaml code, OCaml server failed"
              [ "msg", debugMsg ]
              [ "exception", e ]

        if shouldEqual then
          Expect.equalDval
            (normalizeDvalResult ocamlActual)
            expected
            $"{debugMsg} -> OCaml"
        else
          Expect.notEqual
            (normalizeDvalResult ocamlActual)
            expected
            $"{debugMsg} -> OCaml"

      // Test F#
      if testFSharp then
        let! fsharpActual =
          Exe.executeExpr state Map.empty (actualDarkProg.toRuntimeType ())

        let fsharpActual = normalizeDvalResult fsharpActual

        if shouldEqual then
          Expect.equalDval fsharpActual expected $"{debugMsg} -> FSharp"
        else
          Expect.notEqual fsharpActual expected $"{debugMsg} -> FSharp"
  }


// ---------------
// This is the webserver that we will be testing against. It reads the testCases,
// checks that the request is expected, then returns the test case output.
// ---------------
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection.Extensions

type ErrorResponse =
  { expectedStatus : string
    actualStatus : string
    expectedHeaders : List<string * string>
    actualHeaders : List<string * string>
    expectedBody : string
    actualBody : string
    message : string }

type Compression =
  | Deflate
  | Brotli
  | Gzip

let runTestHandler (ctx : HttpContext) : Task<HttpContext> =
  task {
    try
      let versionName, testName =
        let segments = System.Uri(ctx.Request.Path.Value).Segments

        let versionName = segments.[1]
        let versionName =
          if String.endsWith "/" versionName then
            String.dropRight 1 versionName
          else
            versionName

        let testName = segments.[2]
        let testName =
          if String.endsWith "/" testName then
            String.dropRight 1 testName
          else
            testName

        versionName, testName

      let testCase = testCases.[$"{versionName}/{testName}"]

      let actualHeaders =
        BwdServer.Server.getHeaders ctx
        |> Map
        // .NET always adds a Content-Length header, but OCaml doesn't
        |> Map.remove "Content-Length"
      let! actualBody = BwdServer.Server.getBody ctx

      let actualStatus =
        $"{ctx.Request.Method} {ctx.Request.GetEncodedPathAndQuery()} {ctx.Request.Protocol}"

      let expectedHeaders = Map testCase.expected.headers
      let expectedBody = testCase.expected.body
      let expectedStatus =
        testCase.expected.status |> String.replace "PATH" ctx.Request.Path.Value

      if (actualStatus, actualHeaders, actualBody) = (expectedStatus,
                                                      expectedHeaders,
                                                      expectedBody) then
        let mutable compression = None
        let mutable transcodeToLatin1 = false

        ctx.Response.StatusCode <-
          testCase.result.status
          |> String.split " "
          |> List.getAt 1
          |> Option.unwrapUnsafe
          |> int
        List.iter
          (fun (k, v) ->
            if String.equalsCaseInsensitive k "Content-Encoding" then
              if v = "deflate" then compression <- Some Deflate
              else if v = "br" then compression <- Some Brotli
              else if v = "gzip" then compression <- Some Gzip
              else ()
            elif String.equalsCaseInsensitive k "Content-Type" then
              if v.Contains "charset=iso-8859-1"
                 || v.Contains "charset=latin1"
                 || v.Contains "us-ascii" then
                transcodeToLatin1 <- true

            BwdServer.Server.setHeader ctx k v)
          testCase.result.headers

        let data =
          if transcodeToLatin1 then
            System.Text.Encoding.Convert(
              System.Text.Encoding.UTF8,
              System.Text.Encoding.Latin1,
              testCase.result.body
            )
          else
            testCase.result.body

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
        | None -> do! ctx.Response.Body.WriteAsync(data, 0, data.Length)
      else
        let expectedHeaders =
          expectedHeaders |> Map.toList |> List.sortBy Tuple2.first
        let actualHeaders = actualHeaders |> Map.toList |> List.sortBy Tuple2.first
        let message =
          [ (if actualStatus <> expectedStatus then "status" else "")
            (if actualHeaders <> expectedHeaders then "headers" else "")
            (if actualBody <> expectedBody then "body" else "") ]
          |> List.filter ((<>) "")
          |> String.concat ", "
          |> fun s -> $"The request to the server differs in {s}"

        let body =
          { message = message
            expectedStatus = expectedStatus
            actualStatus = actualStatus
            expectedHeaders = expectedHeaders
            expectedBody = UTF8.ofBytesUnsafe expectedBody
            actualHeaders = actualHeaders
            actualBody = UTF8.ofBytesUnsafe actualBody }
          |> Json.Vanilla.prettySerialize
          |> UTF8.toBytes

        ctx.Response.StatusCode <- 400
        ctx.Response.ContentLength <- int64 body.Length
        do! ctx.Response.Body.WriteAsync(body, 0, body.Length)

      return ctx
    with
    | e ->
      Exception.raiseInternal $"Exception raised in test handler" [ "e", e ]
      return ctx
  }




let configureApp (app : IApplicationBuilder) =
  let handler (ctx : HttpContext) : Task = runTestHandler ctx
  app.Run(RequestDelegate handler)

let configureServices (services : IServiceCollection) : unit = ()


let webserver () =
  Host.CreateDefaultBuilder()
  |> fun h -> h.ConfigureLogging(configureLogging "test-httpclient-server")
  |> fun h ->
       h.ConfigureWebHost (fun wh ->
         wh
         |> fun wh -> wh.UseKestrel()
         |> fun wh -> wh.UseUrls($"http://*:{TestConfig.httpClientPort}")
         |> fun wh -> wh.ConfigureServices(configureServices)
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

let tests =
  [ "v0"; "v1"; "v2"; "v3"; "v4"; "v5" ]
  |> List.map (fun versionName ->
    let tests = testsFromFiles versionName
    testList $"From files, {versionName}" tests)
  |> testList "HttpClient"
