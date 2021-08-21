module ApiServer.InitialLoad

// InitialLoad API endpoint

open Microsoft.AspNetCore.Http
open Giraffe
open Giraffe.EndpointRouting

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open Prelude
open Tablecloth

open Npgsql.FSharp
open LibBackend.Db

module PT = LibExecution.ProgramTypes
module ORT = LibExecution.OCamlTypes.RuntimeT
module Convert = LibExecution.OCamlTypes.Convert

module Account = LibBackend.Account
module Auth = LibBackend.Authorization
module Canvas = LibBackend.Canvas
module SA = LibBackend.StaticAssets

type ApiUserInfo =
  { username : string // as opposed to UserName.T
    name : string
    admin : bool
    email : string
    id : UserID }

type ApiStaticDeploy =
  { deploy_hash : string
    url : string
    last_update : System.DateTime
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
    user_functions : ORT.user_fn<ORT.fluidExpr> list
    deleted_user_functions : ORT.user_fn<ORT.fluidExpr> list
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
    creation_date : System.DateTime
    worker_schedules : LibBackend.EventQueue.WorkerStates.T
    secrets : List<ApiSecret> }

let initialLoad (ctx : HttpContext) : Task<T> =
  task {
    let t = Middleware.startTimer ctx
    let user = Middleware.loadUserInfo ctx
    let canvasInfo = Middleware.loadCanvasInfo ctx
    let permission = Middleware.loadPermission ctx
    t "read-api"

    let! canvas = Canvas.loadAll canvasInfo |> Task.map Result.unwrapUnsafe
    t "load-canvas"

    let! creationDate = Canvas.canvasCreationDate canvasInfo.id
    t "load-canvas-creation-data"

    let! opCtrs =
      Sql.query "SELECT browser_id, ctr FROM op_ctrs WHERE canvas_id = @canvasID"
      |> Sql.parameters [ "canvasID", Sql.uuid canvasInfo.id ]
      |> Sql.executeAsync (fun read -> (read.uuid "browser_id", read.int "ctr"))

    t "load-op-ctrs"

    let! unlocked = LibBackend.UserDB.unlocked canvasInfo.owner canvasInfo.id
    t "get-unlocked-dbs"

    let! staticAssets = SA.allDeploysInCanvas canvasInfo.name canvasInfo.id
    t "get-static-assets"

    let! canvasList = Account.ownedCanvases user.id
    t "get-canvas-list"

    let! orgCanvasList = Account.accessibleCanvases user.id
    t "get-org-canvas-list"

    let! orgList = Account.orgs user.id
    t "get-org-list"

    let! workerSchedules = LibBackend.EventQueue.getWorkerSchedules canvas.meta.id
    t "get-worker-schedules"

    let! secrets = LibBackend.Secret.getCanvasSecrets canvas.meta.id
    t "get-secrets"

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
        canvas_list = List.map toString canvasList
        org_canvas_list = List.map toString orgCanvasList
        permission = permission
        orgs = List.map toString orgList
        worker_schedules = workerSchedules
        account =
          { username = user.username.ToString()
            name = user.name
            email = user.email
            admin = user.admin
            id = user.id }
        creation_date = creationDate
        secrets =
          List.map
            (fun (s : LibBackend.Secret.Secret) ->
              { secret_name = s.name; secret_value = s.value })
            secrets }

    t "write-api"
    return result
  }
