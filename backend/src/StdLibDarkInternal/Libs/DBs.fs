/// StdLib functions for building Dark functionality via Dark canvases
module StdLibDarkInternal.Libs.DBs

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes

module UserDB = LibBackend.UserDB


let fn = FQFnName.stdlibFnName
let typ = FQTypeName.stdlibTypeName

let incorrectArgs = LibExecution.Errors.incorrectArgs

let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn "DarkInternal" "dbs" 0
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


    { name = fn "DarkInternal" "unlockedDBs" 0
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
