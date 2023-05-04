/// StdLib for handling JS-WASM interactions via WASM'd Darklang code
module Wasm.LibWASM

open System

open Prelude
open Tablecloth

open LibExecution.RuntimeTypes

open LibExecution.StdLib.Shortcuts

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module CAT = LibAnalysis.ClientAnalysisTypes
module Exe = LibExecution.Execution

let debug args = WasmHelpers.callJSFunction "console.log" args


let justStrings args =
  args
  |> List.fold (Ok []) (fun agg item ->
    match agg, item with
    | (Error err, _) -> Error err
    | (Ok l, DString arg) -> Ok(arg :: l)
    | (_, notAString) ->
      // this should be a DError, not a "normal" error
      $"Expected args to be a `List<String>`, but got: {LibExecution.DvalReprDeveloper.toRepr notAString}"
      |> Error)
  |> Result.map (fun pairs -> List.rev pairs)

type Program =
  { UserTypes : List<PT.UserType.T>
    UserFunctions : List<PT.UserFunction.T>
    StateType : RT.TypeReference
    CurrentState : Dval }

let mutable program : Program =
  { UserTypes = []; UserFunctions = []; StateType = TUnit; CurrentState = DUnit }

let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn "WASM" "callJSFunction" 0
      typeParams = []
      parameters =
        [ Param.make "functionName" TString ""
          Param.make "args" (TList TString) "" ]
      returnType = TResult(TUnit, TString)
      description = "Calls a globally-accessible JS function with the given args"
      fn =
        (function
        | _, _, [ DString functionName; DList args ] ->
          match justStrings args with
          | Ok args ->
            uply {
              try
                do WasmHelpers.callJSFunction functionName args
                return DResult(Ok DUnit)
              with
              | e ->
                return
                  $"Error calling {functionName} with provided args: {e.Message}"
                  |> DString
                  |> Error
                  |> DResult
            }
          | Error err -> Ply(DResult(Error(DString err)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "WASM" "getState" 0
      typeParams = [ "state" ]
      parameters = []
      returnType = TResult(TVariable "a", TString)
      description = "TODO"
      fn =
        (function
        | _, [ _typeParam ], [] ->
          uply {
            let state = program.CurrentState
            // TODO: assert that the type matches the given typeParam
            return DResult(Ok state)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "WASM" "setState" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "state" (TVariable "a") "" ]
      returnType = TResult(TUnit, TString)
      description = "TODO"
      fn =
        (function
        | _, [ _typeParam ], [ v ] ->
          uply {
            // TODO: verify that the type matches the given typeParam
            program <- { program with CurrentState = v }
            return DResult(Ok DUnit)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "WASM" "exportProgram" 0
      typeParams = []
      parameters = []
      returnType = TString
      description = "TODO"
      fn =
        (function
        | _, [], [] -> Ply(DString(Json.Vanilla.serialize program))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "WASM" "callUserFunction" 0
      typeParams = []
      parameters =
        [ Param.make "functionName" TString ""

          // todo: actual typeArgs, too?

          Param.make "typesOfArgs" (TList(TVariable "")) ""
          Param.make "args" (TList(TVariable "")) "" ]
      returnType = TResult(TUnit, TString)
      description = ""
      fn =
        (function
        | state, [], [ DString fnName; DList typesOfArgs; DList args ] ->
          let state =
            { state with
                program =
                  { state.program with
                      userTypes =
                        program.UserTypes
                        |> List.map PT2RT.UserType.toRT
                        |> List.map (fun ut -> ut.name, ut)
                        |> Map
                      userFns =
                        program.UserFunctions
                        |> List.map PT2RT.UserFunction.toRT
                        |> List.map (fun uf -> uf.name, uf)
                        |> Map } }

          let availableTypes = ExecutionState.availableTypes state

          match justStrings typesOfArgs, justStrings args with
          | Ok typesOfArgs, Ok args ->
            let typesOfArgs =
              typesOfArgs |> List.map Json.Vanilla.deserialize<RT.TypeReference>

            let args =
              List.zip typesOfArgs args
              |> List.map (fun (t, a) ->
                (t, StdLibExecution.Libs.Json.parse availableTypes t a))
              |> List.map snd
              |> List.map Result.unwrap_unsafe

            uply {
              let fnName =
                FQFnName.User { modules = []; function_ = fnName; version = 0 }

              let! result =
                LibExecution.Interpreter.callFn state (gid ()) fnName [] args

              return result
            }
          | _ -> Ply(DResult(Error(DString "TODO")))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
