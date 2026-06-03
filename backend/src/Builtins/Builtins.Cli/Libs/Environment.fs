/// Standard libraries for Environment Variables
module Builtins.Cli.Libs.Environment

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
open Builtin.Shortcuts


let fns () : List<BuiltInFn> =
  [ { name = fn "environmentGet" 0
      typeParams = []
      parameters = [ Param.make "varName" TString "" ]
      returnType = TypeReference.option TString
      description =
        "Gets the value of the environment variable with the given <param varName> if it exists."
      fn =
        (function
        | state, _, _, [ DString varName ] ->
          // precise check: reading this exact env var must be covered (gate checked env presence).
          LibExecution.CapabilityCheck.requireEnvRead state.grantedCaps varName
          let envValue = System.Environment.GetEnvironmentVariable(varName)

          if isNull envValue then
            Dval.optionNone KTString |> Ply
          else
            Dval.optionSome KTString (DString envValue) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.envRead
      deprecated = NotDeprecated }


    { name = fn "environmentGetAll" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TDict TString
      description =
        "Returns a list of tuples containing all the environment variables and their values."
      fn =
        (function
        | state, _, _, [ DUnit ] ->
          // precise check: reading the WHOLE environment needs an unscoped env-read grant.
          LibExecution.CapabilityCheck.requireEnvReadAll state.grantedCaps
          let envVars = System.Environment.GetEnvironmentVariables()

          let envMap =
            envVars
            |> Seq.cast<System.Collections.DictionaryEntry>
            |> Seq.map (fun kv -> (string kv.Key, DString(string kv.Value)))
            |> Seq.toList
            |> Dval.dict KTString

          Ply(envMap)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.envRead
      deprecated = NotDeprecated }


    { name = fn "getBuildHash" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Returns the git hash of the current CLI build"
      fn =
        function
        | _, _, [], [ DUnit ] -> uply { return DString LibConfig.Config.buildHash }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
