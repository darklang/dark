/// Standard libraries for Environment Variables
module Builtins.Cli.Libs.Environment

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
module Host = LibExecution.Host
module PermissionCheck = LibExecution.PermissionCheck
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
        | state, vm, _, [| DString varName |] ->
          uply {
            let op = Host.Operation.EnvGet varName
            match! PermissionCheck.performHost state vm op with
            | Ok response ->
              match Host.expectEnvValue response with
              | Some value -> return Dval.optionSome KTString (DString value)
              | None -> return Dval.optionNone KTString
            | Error _ -> return Dval.optionNone KTString
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.EnvRead ]
      deprecated = NotDeprecated }


    { name = fn "environmentGetAll" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TDict(TString, TString)
      description =
        "Returns a list of tuples containing all the environment variables and their values."
      fn =
        (function
        | state, vm, _, [| DUnit |] ->
          uply {
            match! PermissionCheck.performHost state vm Host.Operation.EnvList with
            | Ok response ->
              return
                Host.expectEnvEntries response
                |> List.map (fun (k, v) -> (k, DString v))
                |> Dval.stringDict KTString
            | Error _ -> return Dval.stringDict KTString []
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.EnvRead ]
      deprecated = NotDeprecated }


    { name = fn "getBuildHash" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Returns the git hash of the current CLI build"
      fn =
        function
        | _, _, [], [| DUnit |] -> uply { return DString LibConfig.Config.buildHash }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
