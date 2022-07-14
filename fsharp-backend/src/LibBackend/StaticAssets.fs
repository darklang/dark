/// <summary>
/// Supports users' ability to upload (with an external CLI tool)
/// and fetch/serve static assets, such as their frontend via
/// several html, css, etc. files.
/// </summary>
/// <remarks>
/// Upload is currently only supported for to-be-deprecated OCaml backend, which is
/// running specifically for this purpose.  and is not yet supported here. There are
/// plans to replace such with Dark functions rather than implementing it here.
/// </remarks>
module LibBackend.StaticAssets

module Account = LibBackend.Account

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open Db

open Prelude
open Prelude.Tablecloth
open Tablecloth

type DeployStatus =
  | Deploying
  | Deployed

type StaticAssetError =
  | GcloudAuthError of string
  | FailureUploadingStaticAsset of string
  | FailureDeletingStaticAsset of string

type StaticDeploy =
  { deployHash : string
    url : string
    lastUpdate : NodaTime.Instant
    status : DeployStatus }

// let static_deploy_to_yojson (sd : static_deploy) : Yojson.Safe.t =
//   `Assoc
//     [ ("deploy_hash", `String sd.deploy_hash)
//     ; ("url", `String sd.url)
//     ; ( "last_update"
//       , `String
//           (* Js.Date.parse expects ISO-8601 formatted string *)
//           (Core.Time.to_string_iso8601_basic
//              sd.last_update
//              ~zone:Core.Time.Zone.utc) )
//     ; ("status", deploy_status_to_yojson sd.status) ]
//
//
// let oauth2_token () : (string, [> static_asset_error]) Lwt_result.t =
//   let scopes = ["https://www.googleapis.com/auth/devstorage.read_write"] in
//   let r = Gcloud.Auth.get_access_token ~scopes () in
//   match%lwt r with
//   | Ok token_info ->
//       Lwt_result.return token_info.token.access_token
//   | Error x ->
//       Caml.print_endline ("Gcloud oauth error: " ^ pp_gcloud_err x) ;
//       Lwt_result.fail (`GcloudAuthError (pp_gcloud_err x))

let appHash (canvasName : CanvasName.T) : string =
  // enough of a hash to make this not easily discoverable
  $"{canvasName}SOME SALT HERE{LibBackend.Config.staticAssetsSaltSuffix}"
  |> sha1digest
  |> Base64.urlEncodeToString
  |> String.removeSuffix "="
  |> String.toLowercase
  |> String.take 63

type UrlType =
  | Short
  | Long

let url (canvasName : CanvasName.T) (deployHash : string) (t : UrlType) : string =
  let domain =
    match t with
    | Short -> ".darksa.com"
    | Long -> ".darkstaticassets.com"

  let apphash = appHash canvasName

  $"https://{canvasName}{domain}/{apphash}/{deployHash}"

// TODO [polish] could instrument this to error on bad deploy hash, maybe also
// unknown file
let urlFor
  (canvasName : CanvasName.T)
  (deployHash : string)
  (variant : UrlType)
  (file : string)
  : string =
  url canvasName deployHash variant + "/" + file

let getLatestDeployHash (canvasID : CanvasID) : Task<Option<string>> =
  let branch = "main"

  Sql.query
    "SELECT deploy_hash FROM static_asset_deploys
       WHERE canvas_id=@canvasID AND branch=@branch AND live_at IS NOT NULL
       ORDER BY created_at desc
       LIMIT 1"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "branch", Sql.string branch ]
  |> Sql.executeRowOptionAsync (fun read -> read.string "deploy_hash")

let allDeploysInCanvas
  (canvasName : CanvasName.T)
  (canvasID : CanvasID)
  : Task<List<StaticDeploy>> =
  Sql.query
    "SELECT deploy_hash, created_at, live_at
     FROM static_asset_deploys
     WHERE canvas_id=@canvasID
     ORDER BY created_at
     DESC LIMIT 25"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read ->
    let deployHash = read.string "deploy_hash"

    let status, lastUpdate =
      match read.instantOrNone "live_at" with
      | Some datetime -> Deployed, datetime
      | None -> Deploying, read.instant "created_at"

    { deployHash = deployHash
      url = url canvasName deployHash Short
      status = status
      lastUpdate = lastUpdate })

let startStaticAssetDeploy
  (user : Account.UserInfo)
  (canvasID : CanvasID)
  (canvasName : CanvasName.T)
  : Task<StaticDeploy> =

  let branch = "main"

  let deployHash =
    $"{canvasID}{System.DateTime.Now.ToString()}"
    |> sha1digest
    |> Base64.urlEncodeToString
    |> String.removeSuffix "="
    |> String.toLowercase
    |> String.take 10

  Sql.query
    "INSERT INTO static_asset_deploys
      (canvas_id, branch, deploy_hash, uploaded_by_account_id)
    VALUES (@canvasID, @branch, @deployHash, @uploadedBy)
    RETURNING created_at"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                      "branch", Sql.string branch
                      "deployHash", Sql.string deployHash
                      "uploadedBy", Sql.uuid user.id ]
  |> Sql.executeRowAsync (fun reader -> reader.instant "created_at")
  |> Task.map (fun lastUpdate ->
    { deployHash = deployHash
      url = url canvasName deployHash Short
      lastUpdate = lastUpdate
      status = Deploying })


// (* since postgres doesn't have named transactions, we just delete the db
//  * record in question. For now, we're leaving files where they are; the right
//  * thing to do here would be to shell out to `gsutil -m rm -r`, but shelling out
//  * from ocaml causes ECHILD errors, so leaving this for a later round of
//  * 'garbage collection' work, in which we can query for files/dirs not known to
//  * the db and delete them *)
// let delete_static_asset_deploy
//     ~(user : Account.user_info)
//     (canvas_id : Uuidm.t)
//     (branch : string)
//     (deploy_hash : string) : unit =
//   Db.run
//     ~name:"delete static_asset_deploy record"
//     ~subject:deploy_hash
//     "DELETE FROM static_asset_deploys
//     WHERE canvas_id=$1 AND branch=$2 AND deploy_hash=$3 AND uploaded_by_account_id=$4"
//     ~params:[Uuid canvas_id; String branch; String deploy_hash; Uuid user.id]

// TODO: what should happen if the deploy hash doesn't exist?
// TODO: what if the deploy is already finished?
let finishStaticAssetDeploy
  (canvasID : CanvasID)
  (canvasName : CanvasName.T)
  (deployHash : string)
  : Task<StaticDeploy> =
  Sql.query
    "UPDATE static_asset_deploys
      SET live_at = NOW()
      WHERE canvas_id = @canvasID AND deploy_hash = @deployHash
      RETURNING live_at"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                      "deployHash", Sql.string deployHash ]
  |> Sql.executeRowAsync (fun reader -> reader.instant "live_at")
  |> Task.map (fun lastUpdate ->
    { deployHash = deployHash
      url = url canvasName deployHash Short
      lastUpdate = lastUpdate
      status = Deployed })

// TODO: this code is to be ported to Dark code, and is here only for reference
// let upload_to_bucket
//     (filename : string)
//     (body : string)
//     (canvas_id : Uuidm.t)
//     (deploy_hash : string) : (unit, [> static_asset_error]) Lwt_result.t =
//   let uri =
//     Uri.make
//       ()
//       ~scheme:"https"
//       ~host:"www.googleapis.com"
//       ~path:
//         ( "upload/storage/v1/b/"
//         ^ (Config.static_assets_bucket |> Option.value_exn)
//         ^ "/o" )
//       ~query:
//         [ ("uploadType", ["multipart"])
//         ; ("contentEncoding", ["gzip"])
//         ; ("name", [app_hash canvas_id ^ "/" ^ deploy_hash ^ "/" ^ filename]) ]
//   in
//   let ct = Magic_mime.lookup filename in
//   let cl = String.length body |> string_of_int in
//   (*
//    * Correctly send object metadata using a multi-part upload with both the raw asset and the metadata in JSON.
//    * Multipart uploads: https://cloud.google.com/storage/docs/json_api/v1/how-tos/multipart-upload
//    * Metadata schema:   https://cloud.google.com/storage/docs/json_api/v1/objects#resource
//    * *)
//   let body_string = body |> Ezgzip.compress in
//   let boundary = "metadata_boundary" in
//   let body =
//     Printf.sprintf
//       {|--%s
// Content-type: application/json; charset=UTF-8
//
// {
//   "cacheControl": "public, max-age=604800, immutable",
//   "contentType": "%s",
//   "size": %s
// }
//
// --%s
// Content-type: %s
//
// %s
// --%s--|}
//       boundary
//       ct
//       cl
//       boundary
//       ct
//       body_string
//       boundary
//   in
//   let headers =
//     oauth2_token ()
//     >|= fun token ->
//     Cohttp.Header.of_list
//       [ ("Authorization", "Bearer " ^ token)
//       ; ("Content-type", "multipart/related; boundary=" ^ boundary)
//       ; ("Content-length", body |> String.length |> string_of_int) ]
//   in
//   headers
//   >|= (fun headers ->
//         Cohttp_lwt_unix.Client.post
//           uri
//           ~headers
//           ~body:(body |> Cohttp_lwt.Body.of_string))
//   >>= fun x ->
//   Lwt.bind x (fun (resp, _) ->
//       match resp.status with
//       | `OK | `Created ->
//           Lwt_result.return ()
//       | _ as s ->
//           Lwt_result.fail
//             (`FailureUploadingStaticAsset
//               ( "Failure uploading static asset: "
//               ^ Cohttp.Code.string_of_status s )))
