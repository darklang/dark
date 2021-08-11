module Tests.HttpClient

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Net.Sockets
open System.Text.RegularExpressions
open FSharpPlus

type ConcurrentDictionary<'a, 'b> =
  System.Collections.Concurrent.ConcurrentDictionary<'a, 'b>

open Prelude
open Tablecloth
open Prelude.Tablecloth

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module Exe = LibExecution.Execution

open TestUtils

type TestCase = { expected : Http.T; result : Http.T }

let testCases : ConcurrentDictionary<string, TestCase> = ConcurrentDictionary()

let toBytes (str : string) = System.Text.Encoding.ASCII.GetBytes str
let toStr (bytes : byte array) = System.Text.Encoding.ASCII.GetString bytes

let t filename =
  testTask $"HttpClient files: {filename}" {
    let skip = String.startsWith "_" filename
    let name = if skip then String.dropLeft 1 filename else filename

    let filename = $"tests/httpclienttestfiles/{filename}"
    let! contents = System.IO.File.ReadAllBytesAsync filename
    let content = toStr contents

    let expectedRequest, response, code =
      let m =
        Regex.Match(
          content,
          "^(\[expected-request\]\n(.*)\n)\[response\]\n(.*)\n\n\[test\]\n(.*)$",
          RegexOptions.Singleline
        )

      if not m.Success then failwith $"incorrect format in {name}"
      let g = m.Groups

      (g.[2].Value, g.[3].Value, g.[4].Value)

    // debuG "expectedRequest" (toStr expectedRequest)
    // debuG "response" (toStr response)
    // debuG "code" code

    let host = $"test.builtwithdark.localhost:10005"

    let normalizeHeaders
      (body : byte array)
      (headers : (string * string) list)
      : (string * string) list =
      headers
      |> List.map
           (function
           // make writing tests easier
           | (key, "HOST") when String.equalsCaseInsensitive "Host" key ->
             (key, host)
           // optionally change content length for writing responses more easily
           | (key, "LENGTH") when String.equalsCaseInsensitive "Content-length" key ->
             (key, string body.Length)
           | other -> other)

    // Add the expectedRequest and response to the server
    let expected = expectedRequest |> toBytes |> Http.setHeadersToCRLF |> Http.split
    let expected =
      { expected with headers = normalizeHeaders expected.body expected.headers }
    let response = response |> toBytes |> Http.setHeadersToCRLF |> Http.split
    let response =
      { response with headers = normalizeHeaders response.body response.headers }

    let testCase = { expected = expected; result = response }

    testCases.[name] <- testCase

    if skip then
      skiptest $"underscore test - {name}"
    else
      // Parse the code
      let shouldEqual, actualProg, expectedResult =
        code
        |> String.replace "URL" $"{host}/{name}"
        |> String.replace "LENGTH" (string response.body.Length)
        |> FSharpToExpr.parse
        |> FSharpToExpr.convertToTest

      let! state = executionStateFor name Map.empty Map.empty

      let! expected =
        Exe.executeExpr state Map.empty (expectedResult.toRuntimeType ())

      let msg = $"\n\n{actualProg}\n=\n{expectedResult}\n\n"

      // Test OCaml
      let! ocamlActual =
        try
          LibBackend.OCamlInterop.execute
            state.program.accountID
            state.program.canvasID
            actualProg
            Map.empty
            []
            []

        with
        | e -> failwith $"When calling OCaml code, OCaml server failed: {msg}, {e}"

      if shouldEqual then
        Expect.equalDval (normalizeDvalResult ocamlActual) expected $"{msg} -> OCaml"
      else
        Expect.notEqual (normalizeDvalResult ocamlActual) expected $"{msg} -> OCaml"

      // Test F#
      let! fsharpActual =
        Exe.executeExpr state Map.empty (actualProg.toRuntimeType ())

      let fsharpActual = normalizeDvalResult fsharpActual

      if shouldEqual then
        Expect.equalDval fsharpActual expected $"{msg} -> FSharp"
      else
        Expect.notEqual fsharpActual expected $"{msg} -> FSharp"
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
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Server.Kestrel.Core

type ErrorResponse =
  { expectedStatus : string
    actualStatus : string
    expectedHeaders : List<string * string>
    actualHeaders : List<string * string>
    expectedBody : string
    actualBody : string
    message : string }

let runTestHandler (ctx : HttpContext) : Task<HttpContext> =
  task {
    let testName = ctx.Request.Path.Value |> String.dropLeft 1 |> String.dropRight 2

    let testCase = testCases.[testName]

    let actualHeaders =
      BwdServer.getHeaders ctx
      // .NET always adds a Content-Length header, but OCaml doesn't
      |> Map.remove "Content-Length"
    let! actualBody = BwdServer.getBody ctx

    let actualStatus =
      $"{ctx.Request.Method} {ctx.Request.GetEncodedPathAndQuery()} {ctx.Request.Protocol}"

    let expectedHeaders = Map testCase.expected.headers
    let expectedBody = testCase.expected.body
    let expectedStatus =
      testCase.expected.status |> String.replace "PATH" ctx.Request.Path.Value

    if (actualStatus, actualHeaders, actualBody) = (expectedStatus,
                                                    expectedHeaders,
                                                    expectedBody) then
      ctx.Response.StatusCode <-
        testCase.result.status
        |> String.split " "
        |> List.getAt 1
        |> Option.unwrapUnsafe
        |> int
      List.iter (fun (k, v) -> BwdServer.setHeader ctx k v) testCase.result.headers
      let body = testCase.result.body
      do! ctx.Response.Body.WriteAsync(body, 0, body.Length)
    else
      let expectedHeaders = expectedHeaders |> Map.toList |> List.sortBy Tuple2.first
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
          expectedBody = ofBytes expectedBody
          actualHeaders = actualHeaders
          actualBody = ofBytes actualBody }
        |> Json.Vanilla.prettySerialize
        |> toBytes

      ctx.Response.StatusCode <- 400
      ctx.Response.ContentLength <- int64 body.Length
      do! ctx.Response.Body.WriteAsync(body, 0, body.Length)

    return ctx
  }

let configureApp (app : IApplicationBuilder) =
  let handler (ctx : HttpContext) = runTestHandler ctx :> Task
  app.Run(RequestDelegate handler)

let configureServices (services : IServiceCollection) : unit = ()

let configureLogging (logging : ILoggingBuilder) : unit =
  // This removes the default ConsoleLogger. Having two console loggers (this one and
  // also the one in Main), caused a deadlock (possibly from having two different
  // console logging threads)
  logging.ClearProviders() |> ignore<ILoggingBuilder>
  logging.AddDebug() |> ignore<ILoggingBuilder>

let webserver () =
  Host.CreateDefaultBuilder()
  |> fun h -> h.ConfigureLogging(configureLogging)
  |> fun h ->
       h.ConfigureWebHost
         (fun wh ->
           wh
           |> fun wh -> wh.UseKestrel()
           |> fun wh -> wh.UseUrls("http://*:10005")
           |> fun wh -> wh.ConfigureServices(configureServices)
           |> fun wh -> wh.Configure(configureApp)
           |> ignore<IWebHostBuilder>)
  |> fun h -> h.Build()

// run a webserver to read test input
let init () : Task = (webserver ()).RunAsync()


let testsFromFiles =
  let dir = "tests/httpclienttestfiles/"

  System.IO.Directory.GetFiles(dir, "*")
  |> Array.map (System.IO.Path.GetFileName)
  |> Array.toList
  |> List.filter ((<>) "README.md")
  |> List.map t

let tests = testList "HttpClient" [ testList "From files" testsFromFiles ]
