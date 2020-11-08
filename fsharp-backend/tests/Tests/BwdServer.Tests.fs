module Tests.BwdServer

open Expecto

open LibExecution
open BwdServer

open System.IO
open System.Threading
open System.Net
open System.Net.Sockets


let t name =
  testTask "connect to server and make request" {
    // TODO: This test relies on the server running already. Run the server
    // instead as part of the test suite.
    let read file = System.IO.File.ReadAllBytes $"tests/httptestfiles/{file}"
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

    let request = read $"{name}.request" |> setHeadersToCRLF
    let expectedResponse = read $"{name}.expected" |> setHeadersToCRLF

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

    Expect.equal (toStr expectedResponse) response "Result should be ok"
  }

let testsFromFiles =
  // get all files
  let dir = "tests/httptestfiles/"
  System.IO.Directory.GetFiles(dir, "*.expected")
  |> Array.map (System.IO.Path.GetFileName)
  |> Array.map (fun filename -> filename.Split(".").[0])
  |> Array.toList
  |> List.map t


let tests = testList "BwdServer" testsFromFiles
