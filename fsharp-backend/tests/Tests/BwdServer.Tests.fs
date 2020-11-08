module Tests.BwdServer

open Expecto

open LibExecution
open BwdServer

open System.IO
open System.Threading
open System.Net
open System.Net.Sockets

let server =
  testList
    "Server"
    [ testTask "connect to server and make request" {
        // TODO: This test relies on the server running already. Run the server
        // instead as part of the test suite.
        let read file = System.IO.File.ReadAllBytes $"tests/httptestfiles/{file}"
        let toBytes (str : string) = System.Text.Encoding.ASCII.GetBytes str
        let toStr (bytes : byte array) = System.Text.Encoding.ASCII.GetString bytes

        let body = read "basic.body"
        let headers = read "basic.headers"
        let expectedResponse = read "basic.expected"

        let client = new TcpClient()
        let mutable connected = false
        for i in 1 .. 10 do
          try
            if not connected then
              do! client.ConnectAsync("127.0.0.1", 9001)
              connected <- true
          with _ -> do! System.Threading.Tasks.Task.Delay 1000

        let stream = client.GetStream()
        stream.ReadTimeout <- 10000

        let start = "GET / HTTP/1.1\nHost: 127.0.0.1:9001\n"B
        let newline = "\n"B

        stream.Write(start, 0, start.Length)
        stream.Write(headers, 0, headers.Length)
        stream.Write(newline, 0, newline.Length)
        stream.Write(body, 0, body.Length)
        stream.Write(newline, 0, newline.Length)
        stream.Write(newline, 0, newline.Length)

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

        let expected = FsRegEx.replace "\n" "\r\n" (toStr expectedResponse)

        Expect.equal expected response "Result should be ok"
      } ]

let tests = testList "BwdServer" [ server ]
