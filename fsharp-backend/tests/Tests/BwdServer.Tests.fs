module Tests.BwdServer

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
module Routing = LibBackend.Routing
module Canvas = LibBackend.Canvas

open TestUtils

type Server =
  | OCaml
  | FSharp

let t name =
  testTask $"Httpfiles: {name}" {
    let testName = $"test-{name}"
    do! TestUtils.clearCanvasData (CanvasName.create testName)
    let toBytes (str : string) = System.Text.Encoding.ASCII.GetBytes str
    let toStr (bytes : byte array) = System.Text.Encoding.ASCII.GetString bytes

    let setHeadersToCRLF (text : byte array) : byte array =
      // We keep our test files with an LF line ending, but the HTTP spec
      // requires headers (but not the body, nor the first line) to have CRLF
      // line endings
      let mutable justSawNewline = false
      let mutable inBody = false

      text
      |> Array.toList
      |> List.collect
           (fun b ->
             if not inBody && b = byte '\n' then
               if justSawNewline then inBody <- true
               justSawNewline <- true
               [ byte '\r'; b ]
             else
               justSawNewline <- false
               [ b ])
      |> List.toArray

    let filename = $"tests/httptestfiles/{name}"
    let! contents = System.IO.File.ReadAllBytesAsync filename
    let contents = toStr contents

    let request, expectedResponse, httpDefs =
      // TODO: use FsRegex instead
      let options = RegexOptions.Singleline

      let m =
        Regex.Match(
          contents,
          "^((\[http-handler \S+ \S+\]\n.*)+)\[request\]\n(.*)\[response\]\n(.*)$",
          options
        )

      if not m.Success then failwith $"incorrect format in {name}"
      let g = m.Groups

      (g.[3].Value, g.[4].Value, g.[2].Value)

    let oplists =
      Regex.Matches(
        httpDefs,
        "\[http-handler (\S+) (\S+)\]\n(.*)\n",
        RegexOptions.Singleline
      )
      |> Seq.toList
      |> List.map
           (fun m ->
             let progString = m.Groups.[3].Value
             let httpRoute = m.Groups.[2].Value
             let httpMethod = m.Groups.[1].Value

             let (source : PT.Expr) =
               $"do ({progString})"
               |> FSharpToExpr.parse
               |> FSharpToExpr.convertToExpr

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

    let split (response : byte array) : (string * string list * byte array) =
      // read a single line of bytes (a line ends with \r\n)
      let rec consume
        (existing : byte list)
        (l : byte list)
        : byte list * byte list =
        match l with
        | [] -> [], []
        | 13uy :: 10uy :: tail -> existing, tail
        | head :: tail -> consume (existing @ [ head ]) tail

      // read all headers (ends when we get two \r\n in a row), return headers
      // and remaining byte string (the body). Assumes the status line is not
      // present. Headers are returned reversed
      let rec consumeHeaders
        (headers : string list)
        (l : byte list)
        : string list * byte list =
        let (line, remaining) = consume [] l

        if line = [] then
          (headers, remaining)
        else
          let str = line |> Array.ofList |> ofBytes
          consumeHeaders (str :: headers) remaining

      let bytes = Array.toList response

      // read the status like (eg HTTP 200 OK)
      let status, bytes = consume [] bytes

      let headers, body = consumeHeaders [] bytes

      (status |> List.toArray |> ofBytes, List.reverse headers, List.toArray body)

    let normalizeHeaders
      (server : Server)
      (hs : string list)
      (body : byte array)
      : string list =
      let headerMap =
        hs
        |> List.map (fun s -> s |> String.toLowercase |> String.split ":")
        |> List.filterMap List.head
        |> Set.ofList

      let ct =
        if (server = OCaml
            && (not (Set.contains "content-type" headerMap) && body.Length <> 0)) then
          [ "content-type: text/plain" ]
        else
          []

      let serverHeader = if server = OCaml then [ "server: darklang" ] else []

      let date =
        if server = OCaml then [ "Date: xxx, xx xxx xxxx xx:xx:xx xxx" ] else []

      // All kestrel responses have
      //  - a content-type if they have content
      //  - a server-darklang
      // So add these to the ocaml responses
      // Meanwhile all ocaml responses are sorted and lowercase
      ct @ date @ serverHeader @ hs
      |> List.map
           (fun h ->
             h
             |> String.toLowercase // FSTODO ocaml responses are lowercase
             |> FsRegEx.replace
                  "date: ..., .. ... .... ..:..:.. ..."
                  "date: xxx, xx xxx xxxx xx:xx:xx xxx"
             |> FsRegEx.replace
                  "x-darklang-execution-id: \\d+"
                  "x-darklang-execution-id: 0123456789")
      |> List.sort // FSTODO ocaml headers are sorted, inexplicably

    let normalizeExpectedHeaders
      (hs : string list)
      (bodyLength : int)
      : string list =
      hs
      |> List.map (fun h -> FsRegEx.replace "LENGTH" (toString bodyLength) h)
      |> List.map String.toLowercase
      // Json can be different lengths, this plugs in the expected length
      |> List.sort


    let callServer (server : Server) : Task<unit> =
      task {
        // Web server might not be loaded yet
        use client = new TcpClient()

        let mutable connected = false

        let port =
          match server with
          | OCaml -> 8001
          | FSharp -> 10001

        for i in 1 .. 10 do
          try
            if not connected then
              do! client.ConnectAsync("127.0.0.1", port)
              connected <- true
          with _ when i <> 10 ->
            printfn $"Server not ready on port {port}, maybe retry"
            do! System.Threading.Tasks.Task.Delay 1000

        use stream = client.GetStream()
        stream.ReadTimeout <- 1000 // responses should be instant, right?
        let host = $"test-{name}.builtwithdark.localhost:{port}"
        let request = request.Replace("HOST", host) |> toBytes |> setHeadersToCRLF
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
                   else
                     line
                     |> String.split "// "
                     |> List.head
                     |> Option.map String.trim_right
                 else
                   Some line)
          |> String.concat "\n"
          |> toBytes
          |> setHeadersToCRLF

        // Parse and normalize the response
        let (aStatus, aHeaders, aBody) = split response
        let (eStatus, eHeaders, eBody) = split expectedResponse
        let eHeaders = normalizeExpectedHeaders eHeaders aBody.Length
        let aHeaders = normalizeHeaders server aHeaders eBody

        // Test as bytes, json, or strings
        if eBody
           |> Array.any
                (fun b -> b <> 10uy && b <> 13uy && not (Char.isPrintable (char b))) then
          // print as bytes for better readability
          Expect.equal
            (aStatus, aHeaders, aBody)
            (eStatus, eHeaders, eBody)
            $"({server} as bytes)"
        else
          // Json can be different shapes but equally valid
          let asJson =
            try
              Some(
                LibExecution.DvalRepr.parseJson (ofBytes aBody),
                LibExecution.DvalRepr.parseJson (ofBytes eBody)
              )
            with e -> None

          match asJson with
          | Some (aJson, eJson) ->
              Expect.equal
                (aStatus, aHeaders, toString aJson)
                (eStatus, eHeaders, toString eJson)
                $"({server} as json)"
          | None ->
              Expect.equal
                (aStatus, aHeaders, ofBytes aBody)
                (eStatus, eHeaders, ofBytes eBody)
                $"({server} as string)"

      }

    if String.startsWith "_" name then
      skiptest $"underscore test - {name}"
    else
      do! callServer OCaml // check OCaml to see if we got the right answer
      do! callServer FSharp // test F# impl

  }

let testsFromFiles =
  // get all files
  let dir = "tests/httptestfiles/"

  System.IO.Directory.GetFiles(dir, "*")
  |> Array.map (System.IO.Path.GetFileName)
  |> Array.toList
  |> List.map t

let unitTests =
  [ testMany
      "sanitizeUrlPath"
      BwdServer.sanitizeUrlPath
      [ ("//", "/")
        ("/foo//bar", "/foo/bar")
        ("/abc//", "/abc")
        ("/abc/", "/abc")
        ("/abc", "/abc")
        ("/", "/")
        ("/abcabc//xyz///", "/abcabc/xyz")
        ("", "/") ]
    testMany
      "ownerNameFromHost"
      (fun cn ->
        cn
        |> CanvasName.create
        |> LibBackend.Account.ownerNameFromCanvasName
        |> fun (on : OwnerName.T) -> on.ToString())
      [ ("test-something", "test"); ("test", "test"); ("test-many-hyphens", "test") ]
    testMany
      "routeVariables"
      Routing.routeVariables
      [ ("/user/:userid/card/:cardid", [ "userid"; "cardid" ]) ]
    testMany2
      "routeInputVars"
      Routing.routeInputVars
      [ ("/hello/:name", "/hello/alice-bob", Some [ "name", RT.DStr "alice-bob" ])
        ("/hello/alice-bob", "/hello/", None)
        ("/user/:userid/card/:cardid",
         "/user/myid/card/0",
         Some [ "userid", RT.DStr "myid"; "cardid", RT.DStr "0" ])
        ("/a/:b/c/d", "/a/b/c/d", Some [ "b", RT.DStr "b" ])
        ("/a/:b/c/d", "/a/b/c", None)
        ("/a/:b", "/a/b/c/d", Some [ "b", RT.DStr "b/c/d" ])
        ("/:a/:b/:c",
         "/a/b/c/d/e",
         Some [ "a", RT.DStr "a"; "b", RT.DStr "b"; "c", RT.DStr "c/d/e" ])
        ("/a/:b/c/d", "/a/b/c/e", None)
        ("/letters:var", "lettersextra", None) ]
    testManyTask
      "canvasNameFromHost"
      (fun h ->
        h
        |> BwdServer.canvasNameFromHost
        |> Task.map (Option.map (fun cn -> cn.ToString())))
      [ ("test-something.builtwithdark.com", Some "test-something")
        ("my-canvas.builtwithdark.localhost", Some "my-canvas")
        ("builtwithdark.localhost", Some "builtwithdark")
        ("my-canvas.darkcustomdomain.com", Some "my-canvas")
        ("www.microsoft.com", None) ] ]

let tests =
  testList
    "BwdServer"
    [ testList "From files" testsFromFiles; testList "unit tests" unitTests ]

open Microsoft.AspNetCore.Hosting
// run our own webserver instead of relying on the dev webserver
let init () : Task = (BwdServer.webserver false 10001).RunAsync()


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
