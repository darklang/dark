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
module PT = LibBackend.ProgramTypes
module Exe = LibExecution.Execution

open TestUtils

type TestCase = { expected : byte array; result : byte array }

let testCases : Dictionary.T<string, TestCase> = Dictionary.empty ()

let t filename =
  testTask $"HttpClient files: {filename}" {
    let skip = String.startsWith "_" filename
    let name = if skip then String.dropLeft 1 filename else filename
    let testName = $"test-{name}"
    let toBytes (str : string) = System.Text.Encoding.ASCII.GetBytes str
    let toStr (bytes : byte array) = System.Text.Encoding.ASCII.GetString bytes

    let filename = $"tests/httpclienttestfiles/{filename}"
    let! contents = System.IO.File.ReadAllBytesAsync filename
    let contents = toStr contents

    let expectedRequest, response, code =
      let m =
        Regex.Match(
          contents,
          "^((\[expected-request\]\n.*\n)+)\[response\]\n(.*)\[test\]\n(.*)$",
          RegexOptions.Singleline
        )

      if not m.Success then failwith $"incorrect format in {name}"
      let g = m.Groups

      (g.[2].Value, g.[3].Value, g.[4].Value)


    // Add the expectedRequest and response to the server
    let host = $"test.builtwithdark.localhost:10005/{testName}"

    let testCase = { expected = toBytes expectedRequest; result = toBytes response }

    testCases.Add(testName, testCase)

    if skip then
      skiptest $"underscore test - {name}"
    else
      // Parse the code
      let shouldEqual, actualProg, expectedResult =
        code
        |> String.replace "HOST" host
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

        with e -> failwith "When calling OCaml code, OCaml server failed: {msg}, {e}"

      if shouldEqual then
        Expect.equalDval (normalizeDvalResult ocamlActual) expected $"OCaml: {msg}"
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
    // let request =
    //   request |> String.replace "HOST" host |> toBytes |> Http.setHeadersToCRLF


    // get host name
    // get the response
    // check if the request is as expected
    // return the response (headers, body, statuscode)
    // let actual = Http.split response
    // let expected = Http.split expectedResponse
    // let eHeaders = normalizeExpectedHeaders expected.headers actual.body.Length
    // let aHeaders = normalizeHeaders server actual.headers expected.body

    // let normalizeHeaders
    //   (server : Server)
    //   (hs : string list)
    //   (body : byte array)
    //   : string list =
    //   List.map
    //     (FsRegEx.replace
    //       "Date: ..., .. ... .... ..:..:.. ..."
    //       "Date: xxx, xx xxx xxxx xx:xx:xx xxx")
    //     hs
    //
    // let response =
    //   response
    //   |> String.splitOnNewline
    //   |> List.filterMap
    //        (fun line ->
    //          if String.includes "// " line then
    //            line
    //            |> String.split "// "
    //            |> List.head
    //            |> Option.map String.trim_right
    //          else
    //            Some line)
    //   |> String.concat "\n"
    //   |> String.replace "HOST" host
    //   |> toBytes
    //   |> Http.setHeadersToCRLF
    printfn "Getting a http request"
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
