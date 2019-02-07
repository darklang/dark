open Core_kernel
open Libexecution
open Libcommon
open Types
open Lwt
open Lwt_result.Infix

let pp_gcloud_err (err : Gcloud.Auth.error) : string =
  Gcloud.Auth.pp_error Format.str_formatter err ;
  Format.flush_str_formatter ()


type static_asset_error =
  [ `GcloudAuthError of string
  | `FailureUploadingStaticAsset of string
  | `FailureDeletingStaticAsset of string ]

let bucket = "dark-static-assets"

let oauth2_token () : (string, [> static_asset_error]) Lwt_result.t =
  let _ =
    match Config.gcloud_application_credentials with
    | Some s ->
        Unix.putenv
          Gcloud.Auth.Environment_vars.google_application_credentials
          s
    | None ->
        ()
  in
  let scopes = ["https://www.googleapis.com/auth/devstorage.read_write"] in
  let r = Gcloud.Auth.get_access_token ~scopes () in
  match%lwt r with
  | Ok token_info ->
      Lwt_result.return token_info.token.access_token
  | Error x ->
      Caml.print_endline ("Gcloud oauth error: " ^ pp_gcloud_err x) ;
      Lwt_result.fail (`GcloudAuthError (pp_gcloud_err x))


let app_hash (canvas_id : Uuidm.t) =
  Nocrypto.Hash.SHA1.digest
    (Cstruct.of_string
       ( Canvas.name_for_id canvas_id
       ^ "SOME SALT HERE"
       ^ Config.env_display_name ))
  |> Cstruct.to_string
  |> B64.encode ~alphabet:B64.uri_safe_alphabet
  |> Util.maybe_chop_suffix ~suffix:"="
  |> String.lowercase
  |> fun s -> String.prefix s 63


let url (canvas_id : Uuidm.t) (deploy_hash : string) variant : string =
  let domain =
    match variant with
    | `Short ->
        ".darksa.com"
    | `Long ->
        ".darkstaticassets.com"
  in
  String.concat
    ~sep:"/"
    [ "https:/"
    ; Canvas.name_for_id canvas_id ^ domain
    ; app_hash canvas_id
    ; deploy_hash ]


let upload_to_bucket
    (filename : string)
    (obj : string)
    (canvas_id : Uuidm.t)
    (deploy_hash : string) : (unit, [> static_asset_error]) Lwt_result.t =
  let uri =
    Uri.make
      ()
      ~scheme:"https"
      ~host:"www.googleapis.com"
      ~path:("upload/storage/v1/b/" ^ bucket ^ "/o")
      ~query:
        [ ("uploadType", ["media"])
        ; ("name", [app_hash canvas_id ^ "/" ^ deploy_hash ^ "/" ^ filename])
        ]
  in
  let ct = Magic_mime.lookup filename in
  let body = Cohttp_lwt.Body.of_string obj in
  let headers =
    oauth2_token ()
    >|= fun token ->
    Cohttp.Header.of_list
      [("Authorization", "Bearer " ^ token); ("Content-type", ct)]
  in
  headers
  >|= (fun headers -> Cohttp_lwt_unix.Client.post uri ~headers ~body)
  >>= fun x ->
  Lwt.bind x (fun (resp, _) ->
      match resp.status with
      | `OK | `Created ->
          Lwt_result.return ()
      | _ as s ->
          Lwt_result.fail
            (`FailureUploadingStaticAsset
              ( "Failure uploading static asset: "
              ^ Cohttp.Code.string_of_status s )) )


let delete_from_bucket
    (filename : string)
    (bucket : string)
    (canvas : Uuidm.t)
    (deploy_hash : string) : (unit, [> Gcloud.Auth.error]) Lwt_result.t =
  let uri =
    Uri.make
      ()
      ~scheme:"https"
      ~host:"www.googleapis.com"
      ~path:
        ( "upload/storage/v1/b/"
        ^ bucket
        ^ "/o/"
        ^ app_hash canvas
        ^ "/"
        ^ deploy_hash
        ^ "/"
        ^ filename )
  in
  let headers =
    oauth2_token ()
    >|= fun token ->
    Cohttp.Header.of_list [("Authorization", "Bearer " ^ token)]
  in
  headers
  >|= (fun headers -> Cohttp_lwt_unix.Client.delete uri ~headers)
  >>= fun x ->
  Lwt.bind x (fun (resp, _) ->
      match resp.status with
      | `OK ->
          Lwt_result.return ()
      | _ as s ->
          Lwt_result.fail
            (`FailureDeletingStaticAsset
              ( "Failure deleting static asset: "
              ^ Cohttp.Code.string_of_status s )) )


let start_static_asset_deploy
    (canvas_id : Uuidm.t) (branch : string) (username : string) : string =
  let account_id = Account.id_of_username username |> Option.value_exn in
  let deploy_hash =
    Nocrypto.Hash.SHA1.digest
      (Cstruct.of_string
         (Uuidm.to_string canvas_id ^ Time.to_string (Time.now ())))
    |> Cstruct.to_string
    |> B64.encode ~alphabet:B64.uri_safe_alphabet
    |> Util.maybe_chop_suffix ~suffix:"="
    |> String.lowercase
    |> fun s -> String.prefix s 10
  in
  Db.run
    ~name:"add static_asset_deploy record"
    ~subject:deploy_hash
    "INSERT INTO static_asset_deploys
      (canvas_id, branch, deploy_hash, uploaded_by_account_id)
      VALUES ($1, $2, $3, $4)"
    ~params:[Uuid canvas_id; String branch; String deploy_hash; Uuid account_id] ;
  deploy_hash


let finish_static_asset_deploy (canvas_id : Uuidm.t) (deploy_hash : string) =
  Db.run
    ~name:"finish static_asset_deploy record"
    ~subject:deploy_hash
    "UPDATE static_asset_deploys
    SET live_at = NOW()
    WHERE canvas_id = $1 AND deploy_hash = $2"
    ~params:[Uuid canvas_id; String deploy_hash]
