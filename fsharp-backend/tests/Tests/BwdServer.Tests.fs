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

open TestUtils

type Server =
  | OCaml
  | FSharp

let t filename =
  testTask $"Httpfiles: {filename}" {
    let skip = String.startsWith "_" filename
    let name = if skip then String.dropLeft 1 filename else filename
    let name = $"bwdserver-{name}"
    let testName = $"test-{name}"
    do! TestUtils.clearCanvasData (CanvasName.create testName)
    let toBytes (str : string) = System.Text.Encoding.ASCII.GetBytes str
    let toStr (bytes : byte array) = System.Text.Encoding.ASCII.GetString bytes

    let filename = $"tests/httptestfiles/{filename}"
    let! contents = System.IO.File.ReadAllBytesAsync filename
    let contents = toStr contents

    let request, expectedResponse, httpDefs, cors, customDomain =
      // TODO: use FsRegex instead
      let m =
        Regex.Match(
          contents,
          ("^((\[http-handler \S+ \S+\]\n.*?\n)+)"
           + "(\[cors .*\]\n+)?"
           + "(\[custom-domain .*\]\n+)?"
           + "\[request\]\n(.*)"
           + "\[response\]\n(.*)$"),
          RegexOptions.Singleline
        )

      if not m.Success then failwith $"incorrect format in {name}"
      let g = m.Groups

      (g.[5].Value, g.[6].Value, g.[1].Value, g.[3].Value, g.[4].Value)

    let oplists =
      Regex.Matches(
        httpDefs,
        "^\[http-handler (\S+) (\S+)\]\n(.*?)\n$",
        RegexOptions.Multiline ||| RegexOptions.Singleline
      )
      |> Seq.toList
      |> List.map
           (fun m ->
             let progString = m.Groups.[3].Value
             let httpRoute = m.Groups.[2].Value
             let httpMethod = m.Groups.[1].Value

             let (source : PT.Expr) =
               progString |> FSharpToExpr.parse |> FSharpToExpr.convertToExpr

             let gid = Prelude.gid

             let ids : PT.Handler.ids =
               { moduleID = gid (); nameID = gid (); modifierID = gid () }

             let h : PT.Handler.T =
               { tlid = gid ()
                 pos = { x = 0; y = 0 }
                 ast = source
                 spec =
                   PT.Handler.HTTP(route = httpRoute, method = httpMethod, ids = ids) }

             (h.tlid,
              [ PT.SetHandler(h.tlid, h.pos, h) ],
              PT.TLHandler h,
              Canvas.NotDeleted))

    let! (meta : Canvas.Meta) = testCanvasInfo testName
    do! Canvas.saveTLIDs meta oplists

    // CORS
    do!
      task {
        let m = Regex.Match(cors, "^\[cors (.*)\]\n*$", RegexOptions.Singleline)
        if m.Success then
          match m.Groups.[1].Value with
          | "" -> do! Canvas.updateCorsSetting meta.id None
          | "*" -> do! Canvas.updateCorsSetting meta.id (Some Canvas.AllOrigins)
          | domains ->
            let domains = (Canvas.Origins(String.split "," domains))
            do! Canvas.updateCorsSetting meta.id (Some domains)
      }

    // Custom domains
    do!
      task {
        let m =
          Regex.Match(
            customDomain,
            "^\[custom-domain (.*)\]\n*$",
            RegexOptions.Singleline
          )
        if m.Success then do! Routing.addCustomDomain m.Groups.[1].Value meta.name
      }

    let normalizeHeaders
      (server : Server)
      (hs : (string * string) list)
      (body : byte array)
      : (string * string) list =
      hs
      |> List.map
           (fun (k, v) ->
             match k with
             | "Date" -> k, "xxx, xx xxx xxxx xx:xx:xx xxx"
             | "x-darklang-execution-id" -> k, "0123456789"
             | other -> (k, v))
      |> List.sortBy Tuple2.first // FSTODO ocaml headers are sorted, inexplicably

    let normalizeExpectedHeaders
      (hs : (string * string) list)
      (bodyLength : int)
      : (string * string) list =
      hs
      |> List.map
           (fun (k, v) ->
             match String.toLowercase k, v with
             // Json can be different lengths, this plugs in the expected length
             | "Content-Length", "LENGTH" -> (k, string bodyLength)
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

        for i in 1 .. 10 do
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
        let host = $"test-{name}.builtwithdark.localhost:{port}"

        if request.Contains("LENGTH") then
          Expect.isFalse true "LENGTH substitution not done on request"

        let request =
          request |> String.replace "HOST" host |> toBytes |> Http.setHeadersToCRLF

        do! stream.WriteAsync(request, 0, request.Length)

        // Read the response
        let length = 10000
        let response = Array.zeroCreate length
        let! byteCount = stream.ReadAsync(response, 0, length)
        let response = Array.take byteCount response

        // Prepare expected response
        let expectedResponse =
          expectedResponse
          |> String.splitOnNewline
          |> List.filterMap
               (fun line ->
                 if String.includes "// " line then
                   if String.includes "OCAMLONLY" line && server = FSharp then
                     None
                   else if String.includes "FSHARPONLY" line && server = OCaml then
                     None
                   else if String.includes "KEEP" line then
                     Some line
                   else
                     // Remove OCAMLONLY and FSHARPONLY
                     line
                     |> String.split "// "
                     |> List.head
                     |> Option.map String.trim_right
                 else
                   Some line)
          |> String.concat "\n"
          |> String.replace "HOST" host
          |> toBytes
          |> Http.setHeadersToCRLF

        // Parse and normalize the response
        let actual = Http.split response
        let expected = Http.split expectedResponse
        let eHeaders = normalizeExpectedHeaders expected.headers actual.body.Length
        let aHeaders = normalizeHeaders server actual.headers expected.body

        // Test as bytes, json, or strings
        if expected.body
           |> Array.any
                (fun b -> b <> 10uy && b <> 13uy && not (Char.isPrintable (char b))) then
          // print as bytes for better readability
          Expect.equal
            (actual.status, aHeaders, actual.body)
            (expected.status, eHeaders, expected.body)
            $"({server} as bytes)"
        else
          // Json can be different shapes but equally valid
          let asJson =
            try
              Some(
                LibExecution.DvalRepr.parseJson (UTF8.ofBytesUnsafe actual.body),
                LibExecution.DvalRepr.parseJson (UTF8.ofBytesUnsafe expected.body)
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
            Expect.equal
              (actual.status, aHeaders, UTF8.ofBytesUnsafe actual.body)
              (expected.status, eHeaders, UTF8.ofBytesUnsafe expected.body)
              $"({server} as string)"

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
  |> List.map t



let tests = testList "BwdServer" [ testList "httptestfiles" testsFromFiles ]

open Microsoft.Extensions.Hosting

// run our own webserver instead of relying on the dev webserver
let init (token : System.Threading.CancellationToken) : Task =
  let port = TestConfig.bwdServerBackendPort
  let k8sPort = TestConfig.bwdServerKubernetesPort
  (BwdServer.webserver false port k8sPort).RunAsync(token)


// FSTODO
// let t_result_to_response_works () =
//   let req =
//     Req.make
//       ~headers:(Header.init ())
//       (Uri.of_string "http://test.builtwithdark.com/")
//   in
//   let req_example_com =
//     Req.make
//       ~headers:(Header.of_list [("Origin", "https://example.com")])
//       (Uri.of_string "http://test.builtwithdark.com/")
//   in
//   let req_google_com =
//     Req.make
//       ~headers:(Header.of_list [("Origin", "https://google.com")])
//       (Uri.of_string "http://test.builtwithdark.com/")
//   in
//   let c = ops2c_exn "test" [] in
//   ignore
//     (List.map
//        ~f:(fun (dval, req, cors_setting, check) ->
//          Canvas.update_cors_setting c cors_setting ;
//          dval
//          |> Webserver.result_to_response ~c ~execution_id ~req
//          |> Libcommon.Telemetry.with_root "test" (fun span ->
//                 Webserver.respond_or_redirect span)
//          |> Lwt_main.run
//          |> fst
//          |> check)
//        [ ( exec_ast (record [])
//          , req
//          , None
//          , fun r ->
//              AT.check
//                (AT.option AT.string)
//                "objects get application/json content-type"
//                (Some "application/json; charset=utf-8")
//                (Header.get (Resp.headers r) "Content-Type") )
//        ; ( exec_ast (list [int 1; int 2])
//          , req
//          , None
//          , fun r ->
//              AT.check
//                (AT.option AT.string)
//                "lists get application/json content-type"
//                (Some "application/json; charset=utf-8")
//                (Header.get (Resp.headers r) "Content-Type") )
//        ; ( exec_ast (int 2)
//          , req
//          , None
//          , fun r ->
//              AT.check
//                (AT.option AT.string)
//                "other things get text/plain content-type"
//                (Some "text/plain; charset=utf-8")
//                (Header.get (Resp.headers r) "Content-Type") )
//        ; ( exec_ast (fn "Http::success" [record []])
//          , req
//          , None
//          , fun r ->
//              AT.check
//                (AT.option AT.string)
//                "Http::success gets application/json"
//                (Some "application/json; charset=utf-8")
//                (Header.get (Resp.headers r) "Content-Type") )
//        ; ( exec_ast (int 1)
//          , req
//          , None
//          , fun r ->
//              AT.check
//                (AT.option AT.string)
//                "without any other settings, we get Access-Control-Allow-Origin: *."
//                (Some "*")
//                (Header.get (Resp.headers r) "Access-Control-Allow-Origin") )
//        ; ( DError (SourceNone, "oh no :(")
//          , req
//          , None
//          , fun r ->
//              AT.check
//                (AT.option AT.string)
//                "we get Access-Control-Allow-Origin: * even for errors."
//                (Some "*")
//                (Header.get (Resp.headers r) "Access-Control-Allow-Origin") )
//        ; ( DIncomplete SourceNone
//          , req
//          , None
//          , fun r ->
//              AT.check
//                (AT.option AT.string)
//                "we get Access-Control-Allow-Origin: * even for incompletes."
//                (Some "*")
//                (Header.get (Resp.headers r) "Access-Control-Allow-Origin") )
//        ; ( exec_ast (int 1)
//          , req
//          , Some Canvas.AllOrigins
//          , fun r ->
//              AT.check
//                (AT.option AT.string)
//                "with explicit wildcard setting, we get Access-Control-Allow-Origin: *."
//                (Some "*")
//                (Header.get (Resp.headers r) "Access-Control-Allow-Origin") )
//        ; ( exec_ast (int 1)
//          , req
//          , Some (Canvas.Origins ["https://example.com"])
//          , fun r ->
//              AT.check
//                (AT.option AT.string)
//                "with allowlist setting and no Origin, we get no Access-Control-Allow-Origin"
//                None
//                (Header.get (Resp.headers r) "Access-Control-Allow-Origin") )
//        ; ( exec_ast (int 1)
//          , req_example_com
//          , Some (Canvas.Origins ["https://example.com"])
//          , fun r ->
//              AT.check
//                (AT.option AT.string)
//                "with allowlist setting and matching Origin, we get good Access-Control-Allow-Origin"
//                (Some "https://example.com")
//                (Header.get (Resp.headers r) "Access-Control-Allow-Origin") )
//        ; ( exec_ast (int 1)
//          , req_google_com
//          , Some (Canvas.Origins ["https://example.com"])
//          , fun r ->
//              AT.check
//                (AT.option AT.string)
//                "with allowlist setting and mismatched Origin, we get null Access-Control-Allow-Origin"
//                (Some "null")
//                (Header.get (Resp.headers r) "Access-Control-Allow-Origin") ) ]) ;
//   ()
//
