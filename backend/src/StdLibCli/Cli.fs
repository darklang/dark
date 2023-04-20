/// Standard libraries for Files, Directories, and other OS/file system stuff
module StdLibCli.Cli

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

open LibExecution.StdLib.Shortcuts

let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fnNoMod "print" 0
      typeParams = []
      parameters = [ Param.make "value" (TVariable "a") "The value to be printed." ]
      returnType = TUnit
      description = "Prints the given <param value> to the standard output."
      fn =
        (function
        | _, _, [ value ] ->
          let str = LibExecution.DvalReprDeveloper.toRepr value
          print str
          Ply(DUnit)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "File" "read" 0
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


    { name = fn "EnvVar" "get" 0
      typeParams = []
      parameters = [ Param.make "varName" TString "" ]
      returnType = TOption TString
      description =
        "Gets the value of the environment variable with the given <param varName> if it exists."
      fn =
        (function
        | _, _, [ DString varName ] ->
          let envValue = System.Environment.GetEnvironmentVariable(varName)

          if isNull envValue then
            Ply(DOption None)
          else
            Ply(DOption(Some(DString envValue)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "EnvVar" "getAll" 0
      typeParams = []
      parameters = []
      returnType = TDict TString
      description =
        "Returns a list of tuples containing all the environment variables and their values."
      fn =
        (function
        | _, _, [] ->
          let envVars = System.Environment.GetEnvironmentVariables()

          let envMap =
            envVars
            |> Seq.cast<System.Collections.DictionaryEntry>
            |> Seq.map (fun kv -> (string kv.Key, DString(string kv.Value)))
            |> Map.ofSeq
            |> DDict

          Ply(envMap)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "File" "write" 0
      typeParams = []
      parameters = [ Param.make "path" TString ""; Param.make "contents" TBytes "" ]
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
      deprecated = NotDeprecated }


    { name = fn "Directory" "current" 0
      typeParams = []
      parameters = [ Param.make "" TUnit "" ]
      returnType = TString
      description = "Returns the current working directory"
      fn =
        (function
        | _, _, [] ->
          uply {
            let contents = System.IO.Directory.GetCurrentDirectory()
            return DString contents
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "Directory" "list" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TList TString
      description = "Returns the directory at <param path>"
      fn =
        (function
        | _, _, [ DString path ] ->
          uply {
            // TODO make async
            let contents = System.IO.Directory.EnumerateFiles path |> Seq.toList
            return List.map DString contents |> DList
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]
