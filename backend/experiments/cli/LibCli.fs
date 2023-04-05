module Cli.LibCli

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = Errors.incorrectArgs

let varA = TVariable "a"


let fns : List<BuiltInFn> =
  [ { name = fn "IO" "print" 0 // CLEANUP make this not use IO prefix
      typeParams = []
      parameters = [ Param.make "value" varA "The value to be printed." ]
      returnType = TUnit
      description = "Prints the given <param value> to the standard output."
      fn =
        (function
        | state, _, [ value ] ->
          let str = LibExecution.DvalReprDeveloper.toRepr value
          print str
          Ply(DUnit)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "IO" "readFile" 0
      typeParams = []
      parameters = [ Param.make "path" TStr "" ]
      returnType = TResult(TBytes, TStr)
      description =
        "Reads the contents of a file specified by <param path> asynchronously and returns its contents as Bytes wrapped in a Result"
      fn =
        (function
        | state, _, [ DStr path ] ->
          uply {
            try
              let! contents = System.IO.File.ReadAllBytesAsync path
              return DResult(Ok(DBytes contents))
            with
            | e -> return DResult(Error(DStr($"Error reading file: {e.Message}")))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }





    ]
