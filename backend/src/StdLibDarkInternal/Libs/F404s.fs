/// StdLib functions for building Dark functionality via Dark canvases
module StdLibDarkInternal.Libs.F404

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

module Telemetry = LibService.Telemetry

let modul = [ "DarkInternal"; "Canvas"; "F404" ]

let typ (name : string) (version : int) : FQTypeName.StdlibTypeName =
  FQTypeName.stdlibTypeName' modul name version

let fn (name : string) (version : int) : FQFnName.StdlibFnName =
  FQFnName.stdlibFnName' modul name version


let types : List<BuiltInType> =
  [ { name = typ "F404" 0
      typeParams = []
      definition =
        CustomType.Record(
          { name = "space"
            typ = TString
            description = "Name of the space (eg WORKER, HTTP, REPL)" },
          [ { name = "path"; typ = TString; description = "" }
            { name = "modifier"; typ = TString; description = "" }
            { name = "timestamp"; typ = TDateTime; description = "" }
            { name = "traceID"; typ = TUuid; description = "" } ]
        )
      deprecated = NotDeprecated
      description = "404 record" } ]


let fns : List<BuiltInFn> = []
// [ { name = fn "delete" 0
//     typeParams = []
//     parameters =
//       [ Param.make "canvasID" TUuid ""
//         Param.make "space" TString ""
//         Param.make "path" TString ""
//         Param.make "modifier" TString "" ]
//     returnType = TUnit
//     description = "Deletes a specific 404 for a canvas"
//     fn =
//       (function
//       | _, _, [ DUuid canvasID; DString space; DString path; DString modifier ] ->
//         uply {
//           Telemetry.addTags [ "space", space; "path", path; "modifier", modifier ]
//           do! TraceInputs.delete404s canvasID space path modifier
//           return DUnit
//         }
//       | _ -> incorrectArgs ())
//     sqlSpec = NotQueryable
//     previewable = Impure
//     deprecated = NotDeprecated }


//   { name = fn "recent" 0
//     typeParams = []
//     parameters = [ Param.make "canvasID" TUuid "" ]
//     returnType = TList(TCustomType(FQTypeName.Stdlib(typ "F404" 0), []))
//     description = "Fetch a list of recent 404s"
//     fn =
//       (function
//       | _, _, [ DUuid canvasID ] ->
//         uply {
//           let! f404s = TraceInputs.getRecent404s canvasID
//           let typeName = FQTypeName.Stdlib(typ "F404" 0)
//           return
//             f404s
//             |> List.map (fun (space, path, modifier, instant, traceID) ->
//               [ "space", DString space
//                 "path", DString path
//                 "modifier", DString modifier
//                 "timestamp", DDateTime(DarkDateTime.fromInstant instant)
//                 "traceID",
//                 DUuid(LibExecution.AnalysisTypes.TraceID.toUUID traceID) ]
//               |> Dval.record typeName)
//             |> DList
//         }
//       | _ -> incorrectArgs ())
//     sqlSpec = NotQueryable
//     previewable = Impure
//     deprecated = NotDeprecated } ]

let contents = (fns, types)
