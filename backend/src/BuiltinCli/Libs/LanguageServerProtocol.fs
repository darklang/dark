module BuiltinCli.Libs.LanguageServerProtocol

open System.IO
open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin
open Builtin.Shortcuts

// TODO reimplement this in Darklang, without any custom built-in function. I had
// some difficulties around this - I forget what - but this got the job done for now
let fns : List<BuiltInFn> =
  [ { name = fn "dangerLanguageServerProtocolReadNextMessage" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Reads a single incoming request from an LSP client, over stdin."
      fn =
        (function
        | _, _, [ DUnit ] ->
          let tryReadHeader (input : StreamReader) =
            let mutable contentLength = None
            let mutable complete = false

            // Continue reading lines until a blank line is encountered
            while not complete do
              let line = input.ReadLine()

              if System.String.IsNullOrEmpty line then
                // End of headers
                complete <- true
              else if line.StartsWith("Content-Length: ") then
                let lengthStr = line.Substring 16 // Get substring after 'Content-Length: '
                contentLength <- Some(System.Int32.Parse lengthStr)
            // Note: it's rarely expected for one additional header to be included -
            // a Content-Type one - but this impl. skips over that header

            contentLength


          let readBody (input : StreamReader) (length : int) =
            let buffer = Array.create length ' '
            input.ReadBlock(buffer, 0, length) |> ignore<int>
            System.String buffer

          let reader = new StreamReader(System.Console.OpenStandardInput())

          match tryReadHeader reader with
          | Some length when length > 0 ->
            let body = readBody reader length
            Ply(DString body)
          | _ -> Exception.raiseInternal "Can't read incoming LSP message" []

        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

(*
  A Darklang draft of replacing this function


  let tryReadContentLengthHeader
    (acc: Stdlib.Option.Option<Int64>)
    : Stdlib.Result.Result<Int64, Unit> =
    log "a"

    match Builtin.Stdin.readLine () with
    | "" ->
      log "b"

      match acc with
      | None -> Stdlib.Result.Result.Error()
      | Some contentLength -> Stdlib.Result.Result.Ok contentLength

    | line ->
      log $"c: {line}"

      if Stdlib.String.startsWith line "Content-Length: " then
        log "d"

        let lengthStr =
          Stdlib.String.dropFirst line (Stdlib.String.length "Content-Length: ")

        log lengthStr

        match Stdlib.Int64.parse lengthStr with
        | Ok length ->
          tryReadContentLengthHeader (Stdlib.Option.Option.Some length)
        | Error _ -> Stdlib.Result.Result.Error()

      else
        log "e"
        // ignore other headers
        tryReadContentLengthHeader acc


  let readBody (input : StreamReader) (length : int) =
      let buffer = Array.create length ' '
      input.ReadBlock(buffer, 0, length) |> ignore
      System.String buffer


  match tryReadContentLengthHeader Stdlib.Option.Option.None with
  | Ok contentLength -> "yay"
  | Error() -> "boo"
*)


let builtins : Builtins = Builtin.make [] fns
