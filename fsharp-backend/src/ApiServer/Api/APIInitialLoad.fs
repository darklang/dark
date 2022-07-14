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
module ORT = LibExecution.OCamlTypes.RuntimeT
module Convert = LibExecution.OCamlTypes.Convert

module Account = LibBackend.Account
module Auth = LibBackend.Authorization
module Canvas = LibBackend.Canvas
module SA = LibBackend.StaticAssets
module SchedulingRules = LibBackend.QueueSchedulingRules

module V0 =
  type ApiUserInfo =
    { username : string // as opposed to UserName.T
      name : string
      admin : bool
      email : string
      id : UserID }

  type ApiStaticDeploy =
    { deploy_hash : string
      url : string
      last_update : NodaTime.Instant
      status : SA.DeployStatus }

  let toApiStaticDeploys (d : SA.StaticDeploy) : ApiStaticDeploy =
    { deploy_hash = d.deployHash
      url = d.url
      last_update = d.lastUpdate
      status = d.status }

  type ApiSecret = { secret_name : string; secret_value : string }

  type T =
    { toplevels : ORT.toplevels
      deleted_toplevels : ORT.toplevels
      user_functions : ORT.user_fn list
      deleted_user_functions : ORT.user_fn list
      unlocked_dbs : tlid list
      user_tipes : ORT.user_tipe list
      deleted_user_tipes : ORT.user_tipe list
      assets : List<ApiStaticDeploy>
      op_ctrs : (System.Guid * int) list
      canvas_list : string list
      org_canvas_list : string list
      permission : Auth.Permission option
      orgs : string list
      account : ApiUserInfo
      creation_date : NodaTime.Instant
      worker_schedules : SchedulingRules.WorkerStates.T
      secrets : List<ApiSecret> }

  /// API endpoint called when client initially loads a Canvas
  ///
  /// Returns high-level references of the Canvas' components,
  /// along with data beyond the specific canvas (e.g. account info)
  let initialLoad (ctx : HttpContext) : Task<T> =
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
      let ocamlToplevels = canvas |> Canvas.toplevels |> Convert.pt2ocamlToplevels

      let ocamlDeletedToplevels =
        canvas |> Canvas.deletedToplevels |> Convert.pt2ocamlToplevels

      let result =
        { toplevels = Tuple3.first ocamlToplevels
          deleted_toplevels = Tuple3.first ocamlDeletedToplevels
          user_functions = Tuple3.second ocamlToplevels
          deleted_user_functions = Tuple3.second ocamlDeletedToplevels
          user_tipes = Tuple3.third ocamlToplevels
          deleted_user_tipes = Tuple3.third ocamlDeletedToplevels
          unlocked_dbs = unlocked
          assets = List.map toApiStaticDeploys staticAssets
          op_ctrs = opCtrs
          canvas_list = List.map string canvasList
          org_canvas_list = List.map string orgCanvasList
          permission = permission
          orgs = List.map string orgList
          worker_schedules = workerSchedules
          account =
            { username = string user.username
              name = user.name
              email = user.email
              admin = user.admin
              id = user.id }
          creation_date = creationDate
          secrets =
            List.map
              (fun (s : PT.Secret.T) ->
                { secret_name = s.name; secret_value = s.value })
              secrets }

      return result
    }

module V1 =
  type ApiUserInfo =
    { username : string // as opposed to UserName.T
      name : string
      admin : bool
      email : string
      id : UserID }

  type ApiStaticDeploy =
    { deploy_hash : string
      url : string
      last_update : NodaTime.Instant
      status : SA.DeployStatus }

  let toApiStaticDeploys (d : SA.StaticDeploy) : ApiStaticDeploy =
    { deploy_hash = d.deployHash
      url = d.url
      last_update = d.lastUpdate
      status = d.status }

  type ApiSecret = { secret_name : string; secret_value : string }

  type T =
    { toplevels : ORT.toplevels
      deleted_toplevels : ORT.toplevels
      user_functions : ORT.user_fn list
      deleted_user_functions : ORT.user_fn list
      unlocked_dbs : tlid list
      user_tipes : ORT.user_tipe list
      deleted_user_tipes : ORT.user_tipe list
      assets : List<ApiStaticDeploy>
      op_ctrs : (System.Guid * int) list
      canvas_list : string list
      org_canvas_list : string list
      permission : Auth.Permission option
      orgs : string list
      account : ApiUserInfo
      creation_date : NodaTime.Instant
      worker_schedules : SchedulingRules.WorkerStates.T
      secrets : List<ApiSecret> }

  /// API endpoint called when client initially loads a Canvas
  ///
  /// Returns high-level references of the Canvas' components,
  /// along with data beyond the specific canvas (e.g. account info)
  let initialLoad (ctx : HttpContext) : Task<T> =
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
      let ocamlToplevels = canvas |> Canvas.toplevels |> Convert.pt2ocamlToplevels

      let ocamlDeletedToplevels =
        canvas |> Canvas.deletedToplevels |> Convert.pt2ocamlToplevels

      let result =
        { toplevels = Tuple3.first ocamlToplevels
          deleted_toplevels = Tuple3.first ocamlDeletedToplevels
          user_functions = Tuple3.second ocamlToplevels
          deleted_user_functions = Tuple3.second ocamlDeletedToplevels
          user_tipes = Tuple3.third ocamlToplevels
          deleted_user_tipes = Tuple3.third ocamlDeletedToplevels
          unlocked_dbs = unlocked
          assets = List.map toApiStaticDeploys staticAssets
          op_ctrs = opCtrs
          canvas_list = List.map string canvasList
          org_canvas_list = List.map string orgCanvasList
          permission = permission
          orgs = List.map string orgList
          worker_schedules = workerSchedules
          account =
            { username = string user.username
              name = user.name
              email = user.email
              admin = user.admin
              id = user.id }
          creation_date = creationDate
          secrets =
            List.map
              (fun (s : PT.Secret.T) ->
                { secret_name = s.name; secret_value = s.value })
              secrets }

      return result
    }
