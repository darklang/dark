open Core
open Libservice.Config_dsl

(* ------------------------- *)
(* Note: if you add an env-var in development, you'll probably need to
 * restart the dev container. *)
(* ------------------------- *)

(* ------------------------- *)
(* Root directories *)
(* ------------------------- *)

let run_dir = absolute_dir "DARK_CONFIG_RUNDIR"

let root_dir = absolute_dir "DARK_CONFIG_ROOT_DIR"

let backend_dir = root_dir ^ "backend/"

let testdata_dir = backend_dir ^ "test_appdata/"

let testresult_dir = run_dir ^ "test_results/"

let log_dir = run_dir ^ "logs/"

let serialization_dir = backend_dir ^ "serialization/"

let completed_test_dir = run_dir ^ "completed_tests/"

(* ------------------------- *)
(* Configurable dirs *)
(* ------------------------- *)
let webroot_dir = absolute_dir "DARK_CONFIG_WEBROOT_DIR"

let swagger_dir = absolute_dir "DARK_CONFIG_SWAGGER_DIR"

let migrations_dir = absolute_dir "DARK_CONFIG_MIGRATIONS_DIR"

let bin_root_dir = absolute_dir "DARK_CONFIG_BIN_ROOT_DIR"

let __unused_bin_scripts_dir = absolute_dir "DARK_CONFIG_SCRIPTS_DIR"

(* ------------------------- *)
(* Web configuration *)
(* ------------------------- *)
let static_host = string "DARK_CONFIG_STATIC_HOST"

let cookie_domain = string "DARK_CONFIG_COOKIE_DOMAIN"

let user_content_host = string "DARK_CONFIG_USER_CONTENT_HOST"

let env_display_name = Libservice.Config.env_display_name

(* ------------------------- *)
(* Kubernetes *)
(* ------------------------- *)

let curl_tunnel_url = string "DARK_CONFIG_CURL_TUNNEL_URL"

(* -------------------- *)
(* For use in Util *)
(* -------------------- *)
type root =
  | Log
  | Serialization
  | Webroot
  | Completed_test
  | Testdata
  | Testresults
  | Bin_root
  | Swagger
  | Migrations
  | No_check

let dir root =
  match root with
  | Log ->
      log_dir
  | Serialization ->
      serialization_dir
  | Webroot ->
      webroot_dir
  | Completed_test ->
      completed_test_dir
  | Bin_root ->
      bin_root_dir
  | Swagger ->
      swagger_dir
  | Testdata ->
      testdata_dir
  | Testresults ->
      testresult_dir
  | Migrations ->
      migrations_dir
  | No_check ->
      ""


(* ------------------------- *)
(* Running the server *)
(* ------------------------- *)
let port = int "DARK_CONFIG_HTTP_PORT"

let allow_test_routes = bool "DARK_CONFIG_ALLOW_TEST_ROUTES"

let __unused_trigger_queue_workers = bool "DARK_CONFIG_TRIGGER_QUEUE_WORKERS"

let create_accounts = bool "DARK_CONFIG_CREATE_ACCOUNTS"

let show_stacktrace = bool "DARK_CONFIG_SHOW_STACKTRACE"

(* ------------------------- *)
(* Rollbar *)
(* ------------------------- *)

let rollbar_client_access_token =
  (* This is what the rollbar UI calls it *)
  match string "DARK_CONFIG_ROLLBAR_POST_CLIENT_ITEM" with
  | "none" ->
      None
  | item ->
      Some item


let rollbar_enabled = Libservice.Config.rollbar_enabled

let rollbar_environment = Libservice.Config.rollbar_environment

let rollbar_js =
  match rollbar_client_access_token with
  | Some token ->
      Printf.sprintf
        "{captureUncaught:true,verbose:true,enabled:%s,accessToken:'%s',payload:{environment: '%s'}}"
        (string_of_bool rollbar_enabled)
        token
        rollbar_environment
  | _ ->
      "{enabled:false}"


let stroller_port = None

let pusher_key = string_option "DARK_CONFIG_PUSHER_KEY"

let pusher_cluster = string "DARK_CONFIG_PUSHER_CLUSTER"

let pusher_js =
  match pusher_key with
  | Some key ->
      Printf.sprintf
        "{enabled: true, key: '%s', cluster: '%s'}"
        key
        pusher_cluster
  | _ ->
      "{enabled: false}"


let heapio_id = string "DARK_CONFIG_HEAPIO_ID"

let public_domain = string "DARK_CONFIG_PUBLIC_DOMAIN"

let browser_reload_enabled = bool "DARK_CONFIG_BROWSER_RELOAD_ENABLED"

let hash_static_filenames = bool "DARK_CONFIG_HASH_STATIC_FILENAMES"

let check_tier_one_hosts = bool "DARK_CONFIG_CHECK_TIER_ONE_HOSTS"

let static_assets_bucket = string_option "DARK_CONFIG_STATIC_ASSETS_BUCKET"

let static_assets_salt_suffix = string "DARK_CONFIG_STATIC_ASSETS_SALT_SUFFIX"

(* If the GIT_COMMIT is in the environment, use that as the build hash.
 * Otherwise, set it to the env name so that it's constant.
 *
 * We intentionally bypass our DSL here as `GIT_COMMIT` is not set by the
 * config _files_ but as part of the production container build process.
 *
 * *)
let build_hash =
  match Sys.getenv "GIT_COMMIT" with Some s -> s | None -> env_display_name


let use_login_darklang_com_for_login =
  bool "DARK_CONFIG_USE_LOGIN_DARKLANG_COM_FOR_LOGIN"
