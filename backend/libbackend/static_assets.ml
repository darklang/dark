open Core_kernel
open Libexecution
open Libcommon
open Types
open Lwt
open Lwt_result.Infix

let pp_gcloud_err (err : Gcloud.Auth.error) : string =
  Gcloud.Auth.pp_error Format.str_formatter err ;
  Format.flush_str_formatter ()


let oauth2_token () : (string, [> Gcloud.Auth.error]) Lwt_result.t =
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
  | Error x as e ->
      Caml.print_endline ("Gcloud oauth error: " ^ pp_gcloud_err x) ;
      Lwt_result.lift e


let create_gcloud_bucket (canvas_id : Uuidm.t) :
    (string, [> Gcloud.Auth.error]) Lwt_result.t =
  let name =
    "darksa-"
    ^ ( Nocrypto.Hash.SHA1.digest
          (Cstruct.of_string (Canvas.name_for_id canvas_id ^ "SOME SALT HERE"))
      |> Cstruct.to_string
      |> B64.encode ~alphabet:B64.uri_safe_alphabet
      |> Util.maybe_chop_suffix ~suffix:"="
      |> String.lowercase )
    |> fun s -> String.prefix s 63
    (* max bucket name length *)
  in
  let body_pairs =
    `Assoc
      [ ("name", `String name)
      ; ("location", `String "us")
      ; ("storageClass", `String "multi_regional") ]
  in
  let uri =
    Uri.make
      ()
      ~scheme:"https"
      ~host:"www.googleapis.com"
      ~path:"storage/v1/b" (* TODO config this *)
      ~query:[("project", ["balmy-ground-195100"])]
  in
  let body =
    body_pairs |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string
  in
  let headers =
    oauth2_token ()
    >|= fun token ->
    Cohttp.Header.of_list
      [ ("Authorization", "Bearer " ^ token)
      ; ("Content-type", "application/json") ]
  in
  headers
  >|= (fun headers -> Cohttp_lwt_unix.Client.post uri ~headers ~body)
  >>= fun x ->
  Lwt.bind x (fun (resp, _) ->
      match resp.status with
      | `OK | `Conflict ->
          Lwt_result.return name
      | _ as s ->
          Lwt_result.return
            (Exception.internal
               ("Failure creating bucket: " ^ Cohttp.Code.string_of_status s))
  )


let upload_to_bucket
    (filename : string) (obj : string) (bucket : string) (deploy_hash : string)
    : (unit, [> Gcloud.Auth.error]) Lwt_result.t =
  let uri =
    Uri.make
      ()
      ~scheme:"https"
      ~host:"www.googleapis.com"
      ~path:("upload/storage/v1/b/" ^ bucket ^ "/o")
      ~query:
        [("uploadType", ["media"]); ("name", [deploy_hash ^ "/" ^ filename])]
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
          Lwt_result.return
            (Exception.internal
               ( "Failure uploading static asset: "
               ^ Cohttp.Code.string_of_status s )) )


let delete_from_bucket
    (filename : string) (bucket : string) (deploy_hash : string) :
    (unit, [> Gcloud.Auth.error]) Lwt_result.t =
  let uri =
    Uri.make
      ()
      ~scheme:"https"
      ~host:"www.googleapis.com"
      ~path:
        ("upload/storage/v1/b/" ^ bucket ^ "/o/" ^ deploy_hash ^ "/" ^ filename)
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
          Lwt_result.return
            (Exception.internal
               ( "Failure uploading static asset: "
               ^ Cohttp.Code.string_of_status s )) )


let bucket_for_canvas_option (canvas_id : Uuidm.t) : string option =
  let retval =
    Db.fetch_one_option
      ~name:"get_bucket_for_canvas"
      ~subject:(Uuidm.to_string canvas_id)
      "SELECT gcloud_bucket_name
    FROM canvases
    WHERE id = $1"
      ~params:[Uuid canvas_id]
  in
  retval |> Option.map ~f:List.hd_exn


let bucket_for_canvas (canvas_id : Uuidm.t) : string =
  bucket_for_canvas_option canvas_id
  |> fun x -> match x with None | Some "" -> "unknown" | Some name -> name


(* will no-op if this canvas already has a bucket *)
let add_bucket_to_canvas (canvas_id : Uuidm.t) :
    (string, [> Gcloud.Auth.error]) Lwt_result.t =
  create_gcloud_bucket canvas_id
  >|= fun gcloud_bucket_name ->
  Db.run
    ~name:"add_bucket_to_canvas"
    ~subject:(Uuidm.to_string canvas_id)
    "UPDATE canvases
    SET gcloud_bucket_name = $1
    WHERE id = $2
    AND gcloud_bucket_name IS NULL"
    ~params:[String gcloud_bucket_name; Uuid canvas_id] ;
  gcloud_bucket_name


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
  (* should this be a Uuid account_id? *)
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
