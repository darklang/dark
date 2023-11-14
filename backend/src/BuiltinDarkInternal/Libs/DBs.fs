/// Builtin functions for building Dark functionality via Dark canvases
module BuiltinDarkInternal.Libs.DBs

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module UserDB = LibCloud.UserDB

let fn = fn [ "DarkInternal"; "Canvas"; "DB" ]


let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let fns : List<BuiltInFn> =
  [ { name = fn "list" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType = TList TInt64
      description = "Returns a list of toplevel ids of dbs in <param canvasName>"
      fn =
        (function
        | _, _, [ DUuid canvasID ] ->
          uply {
            let! tlids = UserDB.all canvasID
            return tlids |> List.map int64 |> List.map DInt64 |> Dval.list KTInt64
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "unlocked" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType = TList TInt64
      description = "Get a list of unlocked DBs"
      fn =
        (function
        | _, _, [ DUuid canvasID ] ->
          uply {
            let! unlocked = UserDB.unlocked canvasID
            return unlocked |> List.map int64 |> List.map DInt64 |> Dval.list KTInt64
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
