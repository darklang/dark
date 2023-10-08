/// Builtin functions for building Dark functionality via Dark canvases
module BuiltinDarkInternal.Libs.DBs

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module UserDB = LibCloud.UserDB

let modules = [ "DarkInternal"; "Canvas"; "DB" ]

let typ = typ modules
let fn = fn modules


let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

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
            return tlids |> List.map int64 |> List.map DInt |> Dval.list KTInt
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
            return unlocked |> List.map int64 |> List.map DInt |> Dval.list KTInt
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
