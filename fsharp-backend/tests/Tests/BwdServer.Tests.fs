module Tests.BwdServer

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Net.Sockets
open System.Text.RegularExpressions

open Prelude
open Tablecloth
open Prelude.Tablecloth

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module Routing = LibBackend.Routing
module Canvas = LibBackend.Canvas

open TestUtils.TestUtils

type Server =
  | OCaml
  | FSharp

type Test =
  { handlers : List<string * string * string>
    cors : Option<string>
    customDomain : Option<string>
    request : byte array
    response : byte array }

type TestParsingState =
  | Limbo
  | InHttpHandler
  | InResponse
  | InRequest

let nl = byte '\n'

// Take a byte array and split it by newline, returning a list of lists. The
// arrays do NOT have the newlines in them.

let splitAtNewlines (bytes : byte array) : byte list list =
  bytes
  |> Array.fold [ [] ] (fun state b ->
    if b = nl then
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
    while result = None && i < list.Length - pattern.Length do
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


// Parse the test line-by-line. We don't use regex here because we want to test more than strings
let parseTest (bytes : byte array) : Test =
  let lines : List<List<byte>> = bytes |> splitAtNewlines
  let emptyTest =
    { handlers = []
      cors = None
      customDomain = None
      request = [||]
      response = [||] }
  lines
  |> List.fold
    (Limbo, emptyTest)
    (fun (state : TestParsingState, result : Test) (line : List<byte>) ->
      let asString : string = line |> Array.ofList |> UTF8.ofBytesWithReplacement
      match asString with
      | Regex "\[cors (\S+)]" [ cors ] -> (Limbo, { result with cors = Some cors })
      | Regex "\[custom-domain (\S+)]" [ customDomain ] ->
        (Limbo, { result with customDomain = Some customDomain })
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
                Array.concat [| result.response; Array.ofList line; [| nl |] |] }
        | InRequest ->
          InRequest,
          { result with
              request =
                Array.concat [| result.request; Array.ofList line; [| nl |] |] }
        | Limbo ->
          if line.Length = 0 then
            (Limbo, result)
          else
            Exception.raiseInternal
              $"Line received while not in any state"
              [ "line", line ])
  |> Tuple2.second
  |> fun test ->
       // Remove the superfluously added newline on response (keep it on the request though)
       { test with response = Array.slice 0 -1 test.response }

// Replace `pattern` in the byte array with `replacement` - both are provided as strings
// for convenience, but obviously both will be converted to bytes
let replaceByteStrings
  (pattern : string)
  (replacement : string)
  (bytes : byte array)
  : byte array =
  let patternBytes = UTF8.toBytes pattern
  let replacementBytes = UTF8.toBytes replacement |> Array.toList |> List.reverse
  let list = Array.toList bytes
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






let t filename =
  testTask $"Httpfiles: {filename}" {
    let skip = String.startsWith "_" filename
    let name = if skip then String.dropLeft 1 filename else filename
    let! (meta : Canvas.Meta) = initializeTestCanvas $"bwdserver-{name}"

    let filename = $"tests/httptestfiles/{filename}"
    let! contents = System.IO.File.ReadAllBytesAsync filename

    let test = parseTest contents

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
         PT.TLHandler h,
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

    let normalizeActualHeaders
      (hs : (string * string) list)
      : (string * string) list =
      hs
      |> List.map (fun (k, v) ->
        match k, v with
        | "Date", _ -> k, "xxx, xx xxx xxxx xx:xx:xx xxx"
        | "x-darklang-execution-id", _ -> k, "0123456789"
        | other -> (k, v))
      |> List.sortBy Tuple2.first // CLEANUP ocaml headers are sorted, inexplicably

    let normalizeExpectedHeaders
      (hs : (string * string) list)
      (actualBody : byte array)
      : (string * string) list =
      hs
      |> List.map (fun (k, v) ->
        match k, v with
        // Json can be different lengths, this plugs in the expected length
        | "Content-Length", "LENGTH" -> (k, string actualBody.Length)
        | _ -> (k, v))
      |> List.sortBy Tuple2.first


    let callServer (server : Server) : Task<unit> =
      task {
        // Web server might not be loaded yet
        use client = new TcpClient()

        let port =
          match server with
          | OCaml -> TestConfig.ocamlServerNginxPort
          | FSharp -> TestConfig.bwdServerNginxPort

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

        use stream = client.GetStream()
        stream.ReadTimeout <- 1000 // responses should be instant, right?

        if test.request |> UTF8.ofBytesWithReplacement |> String.includes "LENGTH" then
          Expect.isFalse true "LENGTH substitution not done on request"

        let host = $"{meta.name}.builtwithdark.localhost:{port}"
        let request =
          test.request |> replaceByteStrings "HOST" host |> Http.setHeadersToCRLF


        do! stream.WriteAsync(request, 0, request.Length)

        // Read the response
        let length = 10000
        let response = Array.zeroCreate length
        let! byteCount = stream.ReadAsync(response, 0, length)
        let response = Array.take byteCount response

        // Prepare expected response
        let expectedResponse =
          test.response
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
                  |> Option.unwrapUnsafe
                line |> List.splitAt index |> Tuple2.first |> Some
            else
              Some line)
          |> List.map (fun l -> List.append l [ nl ])
          |> List.flatten
          |> List.initial // remove final newline which we don't want
          |> Option.unwrapUnsafe
          |> List.toArray
          |> replaceByteStrings "HOST" host
          |> Http.setHeadersToCRLF

        // Parse and normalize the response
        let actual = Http.split response
        let expected = Http.split expectedResponse
        let eHeaders = normalizeExpectedHeaders expected.headers actual.body
        let aHeaders = normalizeActualHeaders actual.headers

        // Test as json or strings
        let asJson =
          try
            Some(
              LibExecution.DvalReprExternal.parseJson (
                UTF8.ofBytesUnsafe actual.body
              ),
              LibExecution.DvalReprExternal.parseJson (
                UTF8.ofBytesUnsafe expected.body
              )
            )
          with
          | e -> None

        match asJson with
        | Some (aJson, eJson) ->
          Expect.equal
            (actual.status, aHeaders, string aJson)
            (expected.status, eHeaders, string eJson)
            $"({server} as json)"
        | None ->
          match UTF8.ofBytesOpt actual.body, UTF8.ofBytesOpt expected.body with
          | Some actualBody, Some expectedBody ->
            Expect.equal
              (actual.status, aHeaders, actualBody)
              (expected.status, eHeaders, expectedBody)
              $"({server} as string)"
          | _ ->
            Expect.equal
              (actual.status, aHeaders, actual.body)
              (expected.status, eHeaders, expected.body)
              $"({server} as bytes)"
      }

    if skip then
      skiptest $"underscore test - {name}"
    else
      do! callServer OCaml // check OCaml to see if we got the right answer
      do! callServer FSharp // test F# impl
  }


let testsFromFiles =
  // get all files
  let dir = "tests/httptestfiles/"

  System.IO.Directory.GetFiles(dir, "*")
  |> Array.filter ((<>) "tests/httptestfiles/README.md")
  |> Array.map (System.IO.Path.GetFileName)
  |> Array.toList
  |> List.filter ((<>) ".gitattributes")
  |> List.filter ((<>) "README.md")
  |> List.map t



let tests = testList "BwdServer" [ testList "httptestfiles" testsFromFiles ]

open Microsoft.Extensions.Hosting

// run our own webserver instead of relying on the dev webserver
let init (token : System.Threading.CancellationToken) : Task =
  let port = TestConfig.bwdServerBackendPort
  let k8sPort = TestConfig.bwdServerKubernetesPort
  let logger = configureLogging "test-bwdserver"
  (BwdServer.Server.webserver logger port k8sPort).RunAsync(token)
