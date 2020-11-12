module Tests.BwdServer

open Expecto

open LibExecution
open BwdServer

open System.IO
open System.Threading
open System.Net
open System.Net.Sockets
open System.Text.RegularExpressions


let t name =
  testTask "connect to server and make request" {
    // TODO: This test relies on the server running already. Run the server
    // instead as part of the test suite.
    let toBytes (str : string) = System.Text.Encoding.ASCII.GetBytes str
    let toStr (bytes : byte array) = System.Text.Encoding.ASCII.GetString bytes

    let setHeadersToCRLF (text : byte array) : byte array =
      // We keep our test files with an LF line ending, but the HTTP spec
      // requires headers (but not the body) to have CRLF line endings
      let mutable justSawNewline = false
      let mutable inHeaderSection = true
      text
      |> Array.toList
      |> List.collect (fun b ->
           justSawNewline <- false
           if inHeaderSection && b = byte '\n' then
             if justSawNewline then inHeaderSection <- false
             justSawNewline <- true
             [ byte '\r'; b ]
           else
             [ b ])
      |> List.toArray

    let request, expectedResponse =
      let filename = $"tests/httptestfiles/{name}"
      let contents = filename |> System.IO.File.ReadAllBytes |> toStr

      // TODO: use FsRegex instead
      let options = System.Text.RegularExpressions.RegexOptions.Singleline
      let m = Regex.Match(contents, "^\[request\](.*)\[response\](.*)$", options)
      if not m.Success then failwith $"incorrect format in {name}"
      toBytes m.Groups.[1].Value, m.Groups.[2].Value


    // Web server might not be loaded yet
    let client = new TcpClient()
    let mutable connected = false
    for i in 1 .. 10 do
      try
        if not connected then
          do! client.ConnectAsync("127.0.0.1", 9001)
          connected <- true
      with _ -> do! System.Threading.Tasks.Task.Delay 1000

    let stream = client.GetStream()
    stream.ReadTimeout <- 1000 // responses should be instant, right?
    stream.Write(request, 0, request.Length)

    let length = 10000
    let response = Array.zeroCreate length
    let byteCount = stream.Read(response, 0, length)
    let response = Array.take byteCount response

    stream.Close()
    client.Close()

    let response =
      FsRegEx.replace
        "Date: ..., .. ... .... ..:..:.. ..."
        "Date: XXX, XX XXX XXXX XX:XX:XX XXX"
        (toStr response)

    Expect.equal expectedResponse response "Result should be ok"
  }

let testsFromFiles =
  // get all files
  let dir = "tests/httptestfiles/"
  System.IO.Directory.GetFiles(dir, "*")
  |> Array.map (System.IO.Path.GetFileName)
  |> Array.toList
  |> List.map t


let unitTests =
  List.mapi (fun i (input, expected) ->
    test $"sanitizeUrlPath - {i}" {
      Expect.equal (BwdServer.sanitizeUrlPath input) expected "" })
    [ ("//", "/")
      ("/abc//", "/abc")
      ("/", "/")
      ("/abcabc//xyz///", "/abcabc/xyz")
      ("", "/") ]


let tests =
  testList
    "BwdServer"
    [ testList "From files" testsFromFiles; testList "unit tests" unitTests ]
