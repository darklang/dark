/// StdLib functions for building Dark functionality via Dark canvases
module BuiltinDarkInternal.Libs.F404

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Telemetry = LibService.Telemetry

let modules = [ "DarkInternal"; "Canvas"; "F404" ]

let typ = typ modules
let fn = fn modules

let types : List<BuiltInType> =
  [ { name = typ "F404" 0
      declaration =
        { typeParams = []
          definition =
            TypeDeclaration.Record(
              NEList.ofList
                { name = "space"; typ = TString }
                [ { name = "path"; typ = TString }
                  { name = "modifier"; typ = TString }
                  { name = "timestamp"; typ = TDateTime }
                  { name = "traceID"; typ = TUuid } ]
            ) }
      deprecated = NotDeprecated
      description = "404 record" } ]

let constants : List<BuiltInConstant> = []
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
//     returnType = TList(TCustomType(FQName.BuiltIn(typ "F404" 0), []))
//     description = "Fetch a list of recent 404s"
//     fn =
//       (function
//       | _, _, [ DUuid canvasID ] ->
//         uply {
//           let! f404s = TraceInputs.getRecent404s canvasID
//           let typeName = FQName.BuiltIn(typ "F404" 0)
//           return
//             f404s
//             |> List.map (fun (space, path, modifier, instant, traceID) ->
//               [ "space", DString space
//                 "path", DString path
//                 "modifier", DString modifier
//                 "timestamp", DDateTime(DarkDateTime.fromInstant instant)
//                 "traceID",
//                 DUuid(LibExecution.AnalysisTypes.TraceID.toUUID traceID) ]
//               |> DvalUtils.record typeName)
//             |> Dval.list valueTypeTODO
//         }
//       | _ -> incorrectArgs ())
//     sqlSpec = NotQueryable
//     previewable = Impure
//     deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
