module BuiltinCli.Libs.LanguageServerProtocol

open System.IO
open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin
open Builtin.Shortcuts

let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

// TODO reimplement this in Darklang, without any custom built-in function. I had
// some difficulties around this - I forget what - but this got the job done for now
let fns : List<BuiltInFn> =
  [ { name = fn [ "LanguageServerProtocol" ] "readNextMessage" 0
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


let contents : Builtin.Contents = (fns, types, constants)
