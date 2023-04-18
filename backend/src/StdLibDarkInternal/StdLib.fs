/// StdLib functions that can only be run on the backend
///
/// Aggregates functions in other modules
module StdLibDarkInternal.StdLib

open System.Threading.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes

let renames = []

// only accessible to the LibBackend.Config.allowedDarkInternalCanvasID canvas
let internalFn (f : RT.BuiltInFnSig) : RT.BuiltInFnSig =
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


let types : List<RT.BuiltInType> =
  [ Libs.Canvases.types
    Libs.DBs.types
    Libs.Documentation.types
    Libs.Domains.types
    Libs.Infra.types
    Libs.Secrets.types
    Libs.Users.types
    Libs.Workers.types ]
  |> List.concat

let fns : List<RT.BuiltInFn> =
  [ Libs.Canvases.fns
    Libs.DBs.fns
    Libs.Documentation.fns
    Libs.Domains.fns
    Libs.Infra.fns
    Libs.Secrets.fns
    Libs.Users.fns
    Libs.Workers.fns ]
  |> List.concat
  |> List.map (fun f -> { f with fn = internalFn f.fn })
  |> RT.renameFunctions renames
