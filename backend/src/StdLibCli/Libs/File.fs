/// Standard libraries for Files
module StdLibCli.Libs.File

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module StdLib = LibExecution.StdLib
open StdLib.Shortcuts

let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn "File" "read" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TResult(TBytes, TString)
      description =
        "Reads the contents of a file specified by <param path> asynchronously and returns its contents as Bytes wrapped in a Result"
      fn =
        (function
        | _, _, [ DString path ] ->
          uply {
            try
              let! contents = System.IO.File.ReadAllBytesAsync path
              return DResult(Ok(DBytes contents))
            with
            | e -> return DResult(Error(DString($"Error reading file: {e.Message}")))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "File" "write" 0
      typeParams = []
      parameters = [ Param.make "contents" TBytes ""; Param.make "path" TString "" ]
      returnType = TResult(TUnit, TString)
      description =
        "Writes the specified byte array <param contents> to the file specified by <param path> asynchronously"
      fn =
        (function
        | _, _, [ DString path; DBytes contents ] ->
          uply {
            try
              do! System.IO.File.WriteAllBytesAsync(path, contents)
              return DResult(Ok(DUnit))
            with
            | e -> return DResult(Error(DString($"Error writing file: {e.Message}")))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let contents : StdLib.Contents = (fns, types)
