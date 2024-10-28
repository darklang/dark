/// Builtin functions for building Dark functionality via Dark canvases
module BuiltinDarkInternal.Libs.DBs

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
//module UserDB = LibCloud.UserDB


let fns : List<BuiltInFn> =
  [
  // { name = fn "darkInternalCanvasDBList" 0
  //   typeParams = []
  //   parameters = [ Param.make "canvasID" TUuid "" ]
  //   returnType = TList TInt64
  //   description = "Returns a list of toplevel ids of dbs in <param canvasName>"
  //   fn =
  //     (function
  //     | _, _, [ DUuid canvasID ] ->
  //       uply {
  //         let! tlids = UserDB.all canvasID
  //         return tlids |> List.map uint64 |> List.map DUInt64 |> Dval.list KTUInt64
  //       }
  //     | _ -> incorrectArgs ())
  //   sqlSpec = NotQueryable
  //   previewable = Impure
  //   deprecated = NotDeprecated }


  // { name = fn "darkInternalCanvasDBUnlocked" 0
  //   typeParams = []
  //   parameters = [ Param.make "canvasID" TUuid "" ]
  //   returnType = TList TInt64
  //   description = "Get a list of unlocked DBs"
  //   fn =
  //     (function
  //     | _, _, [ DUuid canvasID ] ->
  //       uply {
  //         let! unlocked = UserDB.unlocked canvasID
  //         return unlocked |> List.map int64 |> List.map DInt64 |> Dval.list KTInt64
  //       }
  //     | _ -> incorrectArgs ())
  //   sqlSpec = NotQueryable
  //   previewable = Impure
  //   deprecated = NotDeprecated }
  ]

let builtins = LibExecution.Builtin.make [] fns
