/// StdLib functions for building Dark functionality via Dark canvases
module StdLibDarkInternal.Libs.Canvases

open System.Threading.Tasks

open Prelude

open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module Canvas = LibBackend.Canvas
module Serialize = LibBackend.Serialize


let fn = FQFnName.stdlibFnName
let typ = FQTypeName.stdlibTypeName

let incorrectArgs = LibExecution.Errors.incorrectArgs


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
      description = "A program on a canvas" } ]


let fns : List<BuiltInFn> =
  [ { name = fn "DarkInternal" "getAllCanvasIDs" 0
      typeParams = []
      parameters = []
      returnType = TList TUuid
      description = "Get a list of all canvas IDs"
      fn =
        (function
        | _, _, [] ->
          uply {
            let! hosts = Canvas.allCanvasIDs ()
            return hosts |> List.map DUuid |> DList
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "createCanvas" 0
      typeParams = []
      parameters = [ Param.make "owner" TUuid ""; Param.make "name" TString "" ]
      returnType = TUuid
      description = "Creates a new canvas"
      fn =
        (function
        | _, _, [ DUuid owner; DString name ] ->
          uply {
            let! canvasID = Canvas.create owner name
            return DUuid canvasID
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "getOwner" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType = TString
      description = "Get the owner of a canvas"
      fn =
        (function
        | _, _, [ DUuid canvasID ] ->
          uply {
            let! owner = Canvas.getOwner canvasID
            return DUuid owner
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // ---------------------
    // Toplevels
    // ---------------------
    { name = fn "DarkInternal" "deleteToplevelForever" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid ""; Param.make "tlid" TInt "" ]
      returnType = TBool
      description =
        "Delete a toplevel forever. Requires that the toplevel already by deleted. If so, deletes and returns true. Otherwise returns false"
      fn =
        (function
        | _, _, [ DUuid canvasID; DInt tlid ] ->
          uply {
            let tlid = uint64 tlid
            let! c =
              Canvas.loadFrom Serialize.IncludeDeletedToplevels canvasID [ tlid ]
            if Map.containsKey tlid c.deletedHandlers
               || Map.containsKey tlid c.deletedDBs
               || Map.containsKey tlid c.deletedUserTypes
               || Map.containsKey tlid c.deletedUserFunctions then
              do! Canvas.deleteToplevelForever canvasID tlid
              return DBool true
            else
              return DBool false
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    // ---------------------
    // Programs
    // ---------------------
    { name = fn "DarkInternal" "getOpsForToplevel" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid ""; Param.make "tlid" TInt "" ]
      returnType = TList TString
      description = "Returns all ops for a tlid in the given canvas"
      fn =
        (function
        | _, _, [ DUuid canvasID; DInt tlid ] ->
          uply {
            let tlid = uint64 tlid
            let! ops =
              let loadAmount = Serialize.LoadAmount.IncludeDeletedToplevels
              Serialize.loadOplists loadAmount canvasID [ tlid ]

            match ops with
            | [ (_tlid, ops) ] -> return ops |> List.map (string >> DString) |> DList
            | _ -> return DList []
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "darkEditorCanvas" 0
      typeParams = []
      parameters = []
      returnType = TCustomType(FQTypeName.Stdlib(typ "Canvas" "Meta" 0), [])
      description = "Returns basic details of the dark-editor canvas"
      fn =
        (function
        | state, _, [] ->
          uply {
            return [ "id", DUuid state.program.canvasID ] |> Map |> DDict // TODO: DRecord
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // TODO: this name is bad?
    { name = fn "DarkInternal" "canvasProgram" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType =
        TResult(
          TCustomType(FQTypeName.Stdlib(typ "Canvas" "Program" 0), []),
          TString
        )
      description =
        "Returns a list of toplevel ids of http handlers in canvas <param canvasId>"
      fn =
        (function
        | _, _, [ DUuid canvasID ] ->
          uply {
            let! canvas = Canvas.loadAll canvasID

            let dbs =
              Map.values canvas.dbs
              |> Seq.toList
              |> List.map (fun db ->
                [ "tlid", DString(db.tlid.ToString()); "name", DString db.name ]
                |> Map
                |> DDict)
              |> DList

            let httpHandlers =
              Map.values canvas.handlers
              |> Seq.toList
              |> List.choose (fun handler ->
                match handler.spec with
                | PT.Handler.Worker _
                | PT.Handler.Cron _
                | PT.Handler.REPL _ -> None
                | PT.Handler.HTTP (route, method, _ids) ->
                  [ "tlid", DString(handler.tlid.ToString())
                    "method", DString method
                    "route", DString route ]
                  |> Map
                  |> DDict
                  |> Some)
              |> DList

            return
              DResult(Ok(DDict(Map [ "dbs", dbs; "httpHandlers", httpHandlers ])))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]
