/// StdLib functions for building Dark functionality via Dark canvases
module StdLibDarkInternal.Libs.DBs

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

module UserDB = LibBackend.UserDB

let typ (name : string) (version : int) : FQTypeName.StdlibTypeName =
  FQTypeName.stdlibTypeName' [ "DarkInternal"; "Canvas"; "DB" ] name version

let fn (name : string) (version : int) : FQFnName.StdlibFnName =
  FQFnName.stdlibFnName' [ "DarkInternal"; "Canvas"; "DB" ] name version

let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn "list" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType = TList TInt
      description = "Returns a list of toplevel ids of dbs in <param canvasName>"
      fn =
        (function
        | _, _, [ DUuid canvasID ] ->
          uply {
            let! tlids = UserDB.all canvasID
            return tlids |> List.map int64 |> List.map DInt |> DList
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "unlocked" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType = TList TInt
      description = "Get a list of unlocked DBs"
      fn =
        (function
        | _, _, [ DUuid canvasID ] ->
          uply {
            let! unlocked = UserDB.unlocked canvasID
            return unlocked |> List.map int64 |> List.map DInt |> DList
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
