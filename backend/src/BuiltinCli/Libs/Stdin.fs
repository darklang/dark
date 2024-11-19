/// Standard libraries for reading data from the user via the CLI
module BuiltinCli.Libs.Stdin

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin
open Builtin.Shortcuts

let fns : List<BuiltInFn> =
  [ { name = fn "stdinReadLine" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Reads a single line from the standard input."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          let input = System.Console.ReadLine()
          Ply(DString input)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "stdinReadExactly" 0
      typeParams = []
      parameters = [ Param.make "length" TInt64 "The number of characters to read." ]
      returnType = TString
      description = "Reads a specified number of characters from the standard input."
      fn =
        (function
        | _, _, _, [ DInt64 length ] ->
          if length < 0 then
            Exception.raiseInternal "Length must be non-negative" []
          else
            let buffer = Array.zeroCreate (int length)
            let bytesRead = System.Console.In.Read(buffer, 0, (int length))
            let input = System.String(buffer, 0, bytesRead)
            Ply(DString input)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    ]


let builtins : Builtins = Builtin.make [] fns
