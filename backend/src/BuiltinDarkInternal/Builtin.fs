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
  (fun (exeState, vmState, typeArgs, args) ->
    uply {
      if exeState.program.internalFnsAllowed then
        return! f (exeState, vmState, typeArgs, args)
      else
        return
          Exception.raiseInternal
            "internal function attempted to be used in another canvas"
            [ "canavasId", exeState.program.canvasID ]
    })


let builtins : Builtins =
  let builtins =
    Builtin.combine
      [
      // Libs.Canvases.builtins
      // Libs.DBs.builtins
      // Libs.Domains.builtins
      // Libs.F404.builtins
      // Libs.Infra.builtins
      // Libs.Secrets.builtins
      // Libs.Users.builtins
      // Libs.Workers.builtins
      ]
      fnRenames

  { builtins with
      fns = builtins.fns |> Map.map (fun f -> { f with fn = internalFn f.fn }) }
