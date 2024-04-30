/// Standard libraries for printing and output
module BuiltinCli.Libs.Output

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin
open Builtin.Shortcuts


let fns : List<BuiltInFn> =
  [ { name = fn "printLine" 0
      typeParams = []
      parameters = [ Param.make "value" TString "The value to be printed." ]
      returnType = TUnit
      description =
        "Prints the given <param value> to the standard output, followed by a newline."
      fn =
        (function
        | _, _, [ DString str ] ->
          print str
          Ply DUnit
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "print" 0
      typeParams = []
      parameters = [ Param.make "value" TString "The value to be printed." ]
      returnType = TUnit
      description = "Prints the given <param value> to the standard output."
      fn =
        (function
        | _, _, [ DString str ] ->
          printInline str
          Ply DUnit
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
