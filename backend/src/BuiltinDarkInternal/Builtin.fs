/// Builtin functions that can only be run on the backend
///
/// Aggregates functions in other modules
module BuiltinDarkInternal.Builtin

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin


let fnRenames : Builtin.FnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

// only accessible to the LibCloud.Config.allowedDarkInternalCanvasID canvas
let internalFn (f : BuiltInFnSig) : BuiltInFnSig =
  (fun (state, typeArgs, args) ->
    uply {
      if state.program.internalFnsAllowed then
        return! f (state, typeArgs, args)
      else
        return
          Exception.raiseInternal
            "internal function attempted to be used in another canvas"
            [ "canavasId", state.program.canvasID ]
    })


let contents =
  Builtin.combine
    [ Libs.Canvases.contents
      Libs.DBs.contents
      Libs.Documentation.contents
      Libs.Domains.contents
      Libs.F404.contents
      Libs.Infra.contents
      Libs.Secrets.contents
      Libs.Users.contents
      Libs.Workers.contents ]
    fnRenames
  |> (fun (fns, types, constants) ->
    (fns |> List.map (fun f -> { f with fn = internalFn f.fn }), types, constants))
