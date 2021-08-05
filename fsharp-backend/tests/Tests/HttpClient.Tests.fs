module Tests.HttpClient

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Net.Sockets
open System.Text.RegularExpressions
open FSharpPlus

open Prelude
open Tablecloth
open Prelude.Tablecloth

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module Exe = LibExecution.Execution

open TestUtils

type TestCase = { expected : Http.T; result : Http.T }

let testCases : Dictionary.T<string, TestCase> = Dictionary.empty ()

let toBytes (str : string) = System.Text.Encoding.ASCII.GetBytes str
let toStr (bytes : byte array) = System.Text.Encoding.ASCII.GetString bytes

let t filename =
  testTask $"HttpClient files: {filename}" {
    let skip = String.startsWith "_" filename
    let name = if skip then String.dropLeft 1 filename else filename
    let testName = $"test-{name}"

    let filename = $"tests/httpclienttestfiles/{filename}"
    let! contents = System.IO.File.ReadAllBytesAsync filename
    let contents = toStr contents

    let expectedRequest, response, code =
      let m =
        Regex.Match(
          contents,
          "^(\[expected-request\]\n(.*)\n)\[response\]\n(.*)\[test\]\n(.*)$",
          RegexOptions.Singleline
        )

      if not m.Success then failwith $"incorrect format in {name}"
      let g = m.Groups

      (toBytes g.[2].Value, toBytes g.[3].Value, g.[4].Value)

    // debuG "expectedRequest" (toStr expectedRequest)
    // debuG "response" (toStr response)
    // debuG "code" code

    let host = $"test.builtwithdark.localhost:10005"

    let normalizeHeaders
      (headers : (string * string) list)
      : (string * string) list =
      List.map
        (function
        | ("Host", _) -> ("Host", host)
        | other -> other)
        headers

    // Add the expectedRequest and response to the server
    let expected = expectedRequest |> Http.setHeadersToCRLF |> Http.split
    let expected = { expected with headers = normalizeHeaders expected.headers }
    let response = response |> Http.setHeadersToCRLF |> Http.split
    let response = { response with headers = normalizeHeaders response.headers }

    let testCase = { expected = expected; result = response }

    testCases.Add(name, testCase)

    if skip then
      skiptest $"underscore test - {name}"
    else
      // Parse the code
      let shouldEqual, actualProg, expectedResult =
        code
        |> String.replace "URL" $"{host}/{name}"
        |> FSharpToExpr.parse
        |> FSharpToExpr.convertToTest

      let! state = executionStateFor name Map.empty Map.empty

      let! expected =
        Exe.executeExpr state Map.empty (expectedResult.toRuntimeType ())

      let msg = $"\n\n{actualProg}\n=\n{expectedResult} ->"

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
        | e -> failwith "When calling OCaml code, OCaml server failed: {msg}, {e}"

      if shouldEqual then
        Expect.equal (normalizeDvalResult ocamlActual) expected $"OCaml: {msg}"
      else
        Expect.notEqual (normalizeDvalResult ocamlActual) expected $"OCaml: {msg}"

      // Test F#
      let! fsharpActual =
        Exe.executeExpr state Map.empty (actualProg.toRuntimeType ())

      let fsharpActual = normalizeDvalResult fsharpActual

      if shouldEqual then
        Expect.equalDval fsharpActual expected $"FSharp: {msg}"
      else
        Expect.notEqual fsharpActual expected $"FSharp: {msg}"
  }


// ---------------
// Configure Kestrel/ASP.NET
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

let runTestHandler (ctx : HttpContext) : Task<HttpContext> =
  task {
    let testName =
      ctx.Request.Path.Value
      |> String.dropLeft 1
      |> String.dropRight 2
      |> debug "testName"

    debuG "testCases" (Dictionary.toList testCases)
    let testCase = Dictionary.tryGetValue testName testCases |> Option.unwrapUnsafe

    let headers = BwdServer.getHeaders ctx
    let! requestBody = BwdServer.getBody ctx
    debuG "requestBody" (toStr requestBody)

    let expectedHeaders = Map testCase.expected.headers
    let expectedBody = testCase.expected.body

    if (headers, requestBody) = (expectedHeaders, testCase.expected.body) then
      printfn "It matches, returning prepared response"
      ctx.Response.StatusCode <- 200
      Map.iter (fun k v -> BwdServer.setHeader ctx k v) expectedHeaders
      do! ctx.Response.Body.WriteAsync(expectedBody, 0, expectedBody.Length)
    else
      let expectedHeaders =
        expectedHeaders
        |> Map.toList
        |> List.map (fun (k, v) -> k + ": " + v)
        |> String.concat "\n"

      let headers =
        headers
        |> Map.toList
        |> List.map (fun (k, v) -> k + ": " + v)
        |> String.concat "\n"

      let body =
        $"Expected\n{expectedHeaders}\n{toStr expectedBody}\n"
        + $"Got:\n{headers}\n{toStr requestBody}"

      printfn "%s" body

      ctx.Response.StatusCode <- 400
      ctx.Response.ContentLength <- int64 body.Length
      do! ctx.Response.Body.WriteAsync(toBytes body, 0, body.Length)

    return ctx
  }

let configureApp (app : IApplicationBuilder) =
  let handler (ctx : HttpContext) = runTestHandler ctx :> Task
  app.Run(RequestDelegate handler)

let configureServices (services : IServiceCollection) : unit = ()

let webserver () =
  WebHost.CreateDefaultBuilder()
  |> fun wh -> wh.UseKestrel()
  |> fun wh -> wh.UseUrls("http://*:10005")
  |> fun wh -> wh.ConfigureServices(configureServices)
  |> fun wh -> wh.Configure(configureApp)
  |> fun wh -> wh.Build()

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
