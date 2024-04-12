/// Standard libraries for reading data from the user via the CLI
module BuiltinCli.Libs.Stdin

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin
open Builtin.Shortcuts

let constants : List<BuiltInConstant> = []

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


let contents : Builtins = Builtin.fromContents constants fns
