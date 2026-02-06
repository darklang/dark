/// Builtin functions for building Dark functionality via Dark canvases
module BuiltinDarkInternal.Libs.Canvases

open System.Threading.Tasks

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module PT = LibExecution.ProgramTypes
module Canvas = LibCloud.Canvas
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module PackageIDs = LibExecution.PackageIDs


let fns : List<BuiltInFn> =
  [ { name = fn "darkInternalCanvasList" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList TUuid
      description = "Get a list of all canvas IDs"
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let! hosts = Canvas.allCanvasIDs ()
            return DList(VT.uuid, List.map DUuid hosts)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "darkInternalCanvasCreate" 0
      typeParams = []
      parameters = [ Param.make "name" TString "" ]
      returnType = TUuid
      description = "Creates a new canvas"
      fn =
        (function
        | _, _, _, [ DString name ] ->
          uply {
            let! canvasID = Canvas.create name
            return DUuid canvasID
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // ---------------------
    // Toplevels
    // ---------------------
    { name = fn "darkInternalCanvasDeleteToplevelForever" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid ""; Param.make "tlid" TUInt64 "" ]
      returnType = TBool
      description =
        "Delete a toplevel forever. Requires that the toplevel already by deleted. If so, deletes and returns true. Otherwise returns false"
      fn =
        (function
        | _, _, _, [ DUuid canvasID; DUInt64 tlid ] ->
          uply {
            let tlid = uint64 tlid
            let! c = Canvas.loadFrom canvasID [ tlid ]
            if
              Map.containsKey tlid c.deletedHandlers
              || Map.containsKey tlid c.deletedDBs
            then
              do! Canvas.deleteToplevelForever canvasID tlid
              return DBool true
            else
              return DBool false
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "darkInternalCanvasFullProgram" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType =
        TypeReference.result
          (TCustomType(
            Ok(FQTypeName.Package PackageIDs.Type.Internal.Canvas.program),
            []
          ))
          TString
      description =
        "Returns a list of toplevel ids of http handlers in canvas <param canvasId>"
      fn =
        (function
        | _, _, _, [ DUuid canvasID ] ->
          uply {
            let! _canvas = Canvas.loadAll canvasID

            // let dbs =
            //   Map.values canvas.dbs
            //   |> Seq.toList
            //   |> List.map (fun db ->
            //     [ "tlid", DString(db.tlid.ToString()); "name", DString db.name ]
            //     |> Dval.dict)
            //   |> Dval.list VT.unknownTODO

            // let httpHandlers =
            //   Map.values canvas.handlers
            //   |> Seq.toList
            //   |> List.choose (fun handler ->
            //     match handler.spec with
            //     | PT.Handler.Worker _
            //     | PT.Handler.Cron _
            //     | PT.Handler.REPL _ -> None
            //     | PT.Handler.HTTP (route, method) ->
            //       [ "tlid", DString(handler.tlid.ToString())
            //         "method", DString method
            //         "route", DString route ]
            //       |> Dval.dict
            //       |> Some)
            //   |> Dval.list VT.unknownTODO

            let typeName = FQTypeName.Package PackageIDs.Type.Internal.Canvas.program
            return
              DRecord(typeName, typeName, [], Map [])
              |> Dval.resultOk (KTCustomType(typeName, [])) KTString
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
