/// StdLib functions that can only be run on the backend
///
/// Aggregates functions in other modules
module StdLibDarkInternal.StdLib

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes

module StdLib = LibExecution.StdLib


let fnRenames : StdLib.FnRenames =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let typeRenames : StdLib.TypeRenames =
  // old names, new names
  // eg: typ "Http" "Response" 0, typ "Http" "Response" 1
  []

// only accessible to the LibBackend.Config.allowedDarkInternalCanvasID canvas
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
  StdLib.combine
    [ Libs.Canvases.contents
      Libs.DBs.contents
      Libs.Documentation.contents
      Libs.Domains.contents
      Libs.F404.contents
      Libs.Infra.contents
      Libs.Packages.contents
      Libs.Secrets.contents
      Libs.Users.contents
      Libs.Workers.contents ]
    fnRenames
    typeRenames
  |> (fun (fns, types) ->
    (fns |> List.map (fun f -> { f with fn = internalFn f.fn }), types))
