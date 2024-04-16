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
        | _, _, [ DUnit ] ->
          let input = System.Console.ReadLine()
          Ply(DString input)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = Builtin.make [] fns
