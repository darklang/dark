/// StdLib functions for building Dark functionality via Dark canvases
module StdLibDarkInternal.Libs.Canvases

open System.Threading.Tasks

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

module PT = LibExecution.ProgramTypes
module Canvas = LibBackend.Canvas
module Serialize = LibBackend.Serialize
module PT2DT = ProgramTypes2DarkTypes

let modul = [ "DarkInternal"; "Canvas" ]

let typ (name : string) (version : int) : FQTypeName.StdlibTypeName =
  FQTypeName.stdlibTypeName' modul name version

let ptTyp
  (submodules : List<string>)
  (name : string)
  (version : int)
  : FQTypeName.T =
  FQTypeName.packageTypeName'
    "Darklang"
    (NonEmptyList.ofList ([ "Stdlib"; "ProgramTypes" ] @ submodules))
    name
    version
  |> FQTypeName.Package

let fn (name : string) (version : int) : FQFnName.StdlibFnName =
  FQFnName.stdlibFnName' modul name version


let types : List<BuiltInType> =
  [ { name = typ "Meta" 0
      typeParams = []
      definition =
        CustomType.Record({ name = "id"; typ = TUuid; description = "" }, [])
      description = "Metadata about a canvas"
      deprecated = NotDeprecated }


    { name = typ "DB" 0
      typeParams = []
      definition =
        CustomType.Record(
          { name = "name"; typ = TString; description = "" },
          [ { name = "tlid"; typ = TString; description = "" } ]
        )
      deprecated = NotDeprecated
      description = "A database on a canvas" }


    { name = typ "HttpHandler" 0
      typeParams = []
      definition =
        CustomType.Record(
          { name = "method"; typ = TString; description = "" },
          [ { name = "route"; typ = TString; description = "" }
            { name = "tlid"; typ = TString; description = "" } ]
        )
      deprecated = NotDeprecated
      description = "An HTTP handler on a canvas" }


    { name = typ "Program" 0
      typeParams = []
      definition =
        CustomType.Record(
          { name = "id"; typ = TUuid; description = "" },
          [ { name = "types"
              typ = TList(TCustomType(ptTyp [] "UserType" 0, []))
              description = "All typed defined within this canvas" }

            // { name = "dbs"
            //   typ = TList(TCustomType(FQTypeName.Stdlib(typ "DB" 0), []))
            //   description = "" }

            // { name = "httpHandlers"
            //   typ = TList(TCustomType(FQTypeName.Stdlib(typ "HttpHandler" 0), []))
            //   description = "" }
            ]
        )
      deprecated = NotDeprecated
      description = "A program on a canvas" } ]


let fns : List<BuiltInFn> =
  [ { name = fn "list" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList TUuid
      description = "Get a list of all canvas IDs"
      fn =
        (function
        | _, _, [ DUnit ] ->
          uply {
            let! hosts = Canvas.allCanvasIDs ()
            return hosts |> List.map DUuid |> DList
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "create" 0
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


    { name = fn "owner" 0
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
    { name = fn "deleteToplevelForever" 0
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
            if
              Map.containsKey tlid c.deletedHandlers
              || Map.containsKey tlid c.deletedDBs
              || Map.containsKey tlid c.deletedUserTypes
              || Map.containsKey tlid c.deletedUserFunctions
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

    // ---------------------
    // Programs
    // ---------------------
    { name = fn "getOpsForToplevel" 0
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


    { name = fn "darkEditorCanvasID" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TUuid
      description = "Returns the ID of the special dark-editor canvas"
      fn =
        (function
        | state, _, [ DUnit ] -> uply { return DUuid state.program.canvasID }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "fullProgram" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType =
        TResult(TCustomType(FQTypeName.Stdlib(typ "Program" 0), []), TString)
      description =
        "Returns a list of toplevel ids of http handlers in canvas <param canvasId>"
      fn =
        (function
        | _, _, [ DUuid canvasID ] ->
          uply {
            let! canvas = Canvas.loadAll canvasID

            let types =
              canvas.userTypes
              |> Map.toList
              |> List.map (fun (tlid, userType) -> PT2DT.UserType.toDT tlid userType)
              |> DList

            // let dbs =
            //   Map.values canvas.dbs
            //   |> Seq.toList
            //   |> List.map (fun db ->
            //     [ "tlid", DString(db.tlid.ToString()); "name", DString db.name ]
            //     |> Map
            //     |> DDict)
            //   |> DList

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
            //       |> Map
            //       |> DDict
            //       |> Some)
            //   |> DList

            return
              DRecord(FQTypeName.Stdlib(typ "Program" 0), Map [ "types", types ])
              |> Ok
              |> DResult
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
