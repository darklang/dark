/// Standard libraries for printing and output
module BuiltinCli.Libs.Output

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin
open Builtin.Shortcuts

let constants : List<BuiltInConstant> = []

let fns : List<BuiltInFn> =
  [ { name = fn [] "printLine" 0
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


    { name = fn [] "print" 0
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
      deprecated = NotDeprecated }


    { name = fn [] "debug" 0
      typeParams = []
      parameters =
        [ Param.make "value" (TVariable "a") "The value to be printed."
          Param.make "label" TString "The label to be printed." ]
      returnType = TVariable "a"
      description = "Prints the given <param value> to the standard output"
      fn =
        (function
        | _, _, [ value; DString label ] ->
          print $"DEBUG: {label} - {value}"
          Ply value
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    ]


let contents : Builtin.Contents = (fns,  constants)
