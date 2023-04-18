/// StdLib functions for user management. Note that user management is intended to be
/// built in Darklang itself, so this functionality is deliberately sparse.
module StdLibDarkInternal.Libs.UserManagement

open System.Threading.Tasks

open Npgsql.FSharp
open LibBackend.Db

open Prelude

open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module DvalReprDeveloper = LibExecution.DvalReprDeveloper
module Errors = LibExecution.Errors
module Telemetry = LibService.Telemetry

open LibBackend
module SchedulingRules = LibBackend.QueueSchedulingRules

let fn = FQFnName.stdlibFnName
let typ = FQTypeName.stdlibTypeName

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"


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

let modifySchedule (fn : CanvasID -> string -> Task<unit>) =
  internalFn (function
    | _, _, [ DUuid canvasID; DString handlerName ] ->
      uply {
        do! fn canvasID handlerName
        let! s = SchedulingRules.getWorkerSchedules canvasID
        Pusher.push
          ClientTypes2BackendTypes.Pusher.eventSerializer
          canvasID
          (Pusher.UpdateWorkerStates s)
          None
        return DUnit
      }
    | _ -> incorrectArgs ())

let types : List<BuiltInType> =
  [ { name = typ "Canvas" "Meta" 0
      typeParams = []
      definition = CustomType.Record({ id = 1UL; name = "id"; typ = TUuid }, [])
      description = "Metadata about a canvas" }
    { name = typ "Canvas" "DB" 0
      typeParams = []
      definition =
        CustomType.Record(
          { id = 2UL; name = "name"; typ = TString },
          [ { id = 3UL; name = "tlid"; typ = TString } ]
        )
      description = "A database on a canvas" }
    { name = typ "Canvas" "HttpHandler" 0
      typeParams = []
      definition =
        CustomType.Record(
          { id = 2UL; name = "method"; typ = TString },
          [ { id = 3UL; name = "route"; typ = TString }
            { id = 4UL; name = "tlid"; typ = TString } ]
        )
      description = "An HTTP handler on a canvas" }
    { name = typ "Canvas" "Program" 0
      typeParams = []
      definition =
        CustomType.Record(
          { id = 1UL; name = "id"; typ = TUuid },
          [ { id = 2UL
              name = "dbs"
              typ = TList(TCustomType(FQTypeName.Stdlib(typ "Canvas" "DB" 0), [])) }
            { id = 3UL
              name = "httpHandlers"
              typ =
                TList(
                  TCustomType(FQTypeName.Stdlib(typ "Canvas" "HttpHandler" 0), [])
                ) } ]
        )
      description = "A program on a canvas" }
    { name = typ "Canvas" "F404" 0
      typeParams = []
      definition =
        CustomType.Record(
          { id = 1UL; name = "space"; typ = TString },
          [ { id = 3UL; name = "path"; typ = TString }
            { id = 4UL; name = "modifier"; typ = TString }
            { id = 5UL; name = "timestamp"; typ = TDateTime }
            { id = 6UL; name = "traceID"; typ = TUuid } ]
        )
      description = "A 404 trace" } ]


let fns : List<BuiltInFn> =
  [ { name = fn "DarkInternal" "createUser" 0
      typeParams = []
      parameters = []
      returnType = TUuid
      description = "Creates a user, and returns their userID."
      fn =
        internalFn (function
          | _, _, [] ->
            uply {
              let! canvasID = Account.createUser ()
              return DUuid canvasID
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]
