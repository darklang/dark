/// StdLib for handling JS-WASM interactions via WASM'd Darklang code
module Wasm.LibWASM

open System

open Prelude
open Tablecloth

open LibExecution.RuntimeTypes

open LibExecution.StdLib.Shortcuts

module PT = LibExecution.ProgramTypes
module Exe = LibExecution.Execution


let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn "WASM" "callJSFunction" 0
      typeParams = []
      parameters =
        [ Param.make "functionName" TString ""

          // TODO: maybe instead of a `TList TString`,
          // we require a `TList(TCustomType SimpleJSON)`,
          // where `type SimpleJSON = | JNull | JBool | JList ...`
          Param.make "serializedArgs" (TList TString) "" ]
      returnType = TResult(TUnit, TString)
      description =
        "Calls a function exposed in JS host, i.e. `console.log`, or a user-defined function globally available"
      fn =
        (function
        | _, _, [ DString functionName; DList args ] ->
          let args =
            args
            |> List.fold (Ok []) (fun agg item ->
              match agg, item with
              | (Error err, _) -> Error err
              | (Ok l, DString arg) ->
                // Should empty args be allowed?
                if arg = "" then
                  "Empty request header key provided" |> Error
                else
                  Ok(arg :: l)

              | (_, notAString) ->
                // this should be a DError, not a "normal" error
                $"Expected args to be a `List<String>`, but got: {LibExecution.DvalReprDeveloper.toRepr notAString}"
                |> Error)
            |> Result.map (fun pairs -> List.rev pairs)

          match args with
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
          | Error argsError -> Ply(DResult(Error(DString argsError)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
