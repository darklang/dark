/// Standard libraries for Environment Variables
module StdLibCli.Libs.Environment

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module StdLib = LibExecution.StdLib
open StdLib.Shortcuts

let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn "Environment" "get" 0
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


    { name = fn "Environment" "getAll" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TDict TString
      description =
        "Returns a list of tuples containing all the environment variables and their values."
      fn =
        (function
        | _, _, [ DUnit ] ->
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
      deprecated = NotDeprecated } ]


let contents : StdLib.Contents = (fns, types)
