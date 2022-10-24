/// API endpoint called when client initially loads a Canvas
module ApiServer.InitialLoad

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Http

open Prelude
open Tablecloth
open Http

open Npgsql.FSharp
open LibBackend.Db

module PT = LibExecution.ProgramTypes

module Account = LibBackend.Account
module Auth = LibBackend.Authorization
module Canvas = LibBackend.Canvas
module SA = LibBackend.StaticAssets
module SchedulingRules = LibBackend.QueueSchedulingRules
module CT2Program = ClientTypes2ExecutionTypes.ProgramTypes
module CT2StaticDeploy = ClientTypes2BackendTypes.StaticDeploy
module CT2Auth = ClientTypes2BackendTypes.Authorization

module V1 =
  /// API endpoint called when client initially loads a Canvas
  ///
  /// Returns high-level references of the Canvas' components,
  /// along with data beyond the specific canvas (e.g. account info)
  let initialLoad
    (ctx : HttpContext)
    : Task<ClientTypes.Api.InitialLoad.V1.Response> =
    task {
      use t = startTimer "read-api" ctx
      let user = loadUserInfo ctx
      let canvasInfo = loadCanvasInfo ctx
      let permission = loadPermission ctx

      t.next "load-canvas"
      let! canvas = Canvas.loadAll canvasInfo

      t.next "load-canvas-creation-date"
      let! creationDate = Canvas.canvasCreationDate canvasInfo.id

      t.next "load-op-ctrs"
      let! opCtrs =
        Sql.query "SELECT browser_id, ctr FROM op_ctrs WHERE canvas_id = @canvasID"
        |> Sql.parameters [ "canvasID", Sql.uuid canvasInfo.id ]
        |> Sql.executeAsync (fun read -> (read.uuid "browser_id", read.int "ctr"))
        |> Task.map Map


      t.next "get-unlocked-dbs"
      let! unlocked = LibBackend.UserDB.unlocked canvasInfo.owner canvasInfo.id

      t.next "get-static-assets"
      let! staticAssets = SA.allDeploysInCanvas canvasInfo.name canvasInfo.id

      t.next "get-canvas-list"
      let! canvasList = Account.ownedCanvases user.id

      let! orgCanvasList = Account.accessibleCanvases user.id
      t.next "get-org-canvas-list"

      t.next "get-org-list"
      let! orgList = Account.orgs user.id

      t.next "get-worker-schedules"
      let! workerSchedules = SchedulingRules.getWorkerSchedules canvas.meta.id

      t.next "get-secrets"
      let! secrets = LibBackend.Secret.getCanvasSecrets canvas.meta.id

      t.next "write-api"

      let result : ClientTypes.Api.InitialLoad.V1.Response =
        { handlers = Map.values canvas.handlers |> List.map CT2Program.Handler.toCT
          deletedHandlers =
            Map.values canvas.deletedHandlers |> List.map CT2Program.Handler.toCT
          dbs = Map.values canvas.dbs |> List.map CT2Program.DB.toCT
          deletedDBs = Map.values canvas.deletedDBs |> List.map CT2Program.DB.toCT
          userFunctions =
            Map.values canvas.userFunctions |> List.map CT2Program.UserFunction.toCT
          deletedUserFunctions =
            Map.values canvas.deletedUserFunctions
            |> List.map CT2Program.UserFunction.toCT
          userTypes =
            Map.values canvas.userTypes |> List.map CT2Program.UserType.toCT
          deletedUserTypes =
            Map.values canvas.deletedUserTypes |> List.map CT2Program.UserType.toCT
          unlockedDBs = unlocked
          staticDeploys = List.map CT2StaticDeploy.toCT staticAssets
          opCtrs = opCtrs
          canvasList = List.map string canvasList
          orgCanvasList = List.map string orgCanvasList
          permission = Option.map CT2Auth.Permission.toCT permission
          orgs = List.map string orgList
          workerSchedules =
            workerSchedules |> ClientTypes2BackendTypes.Worker.WorkerStates.toCT
          account =
            { username = string user.username; name = user.name; email = user.email }
          creationDate = creationDate
          secrets = secrets |> List.map (fun s -> { name = s.name; value = s.value }) }

      return result
    }
