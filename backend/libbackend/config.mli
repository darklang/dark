open Core_kernel

type root =
  | Log
  | Serialization
  | Templates
  | Webroot
  | Completed_test
  | Testdata
  | Testresults
  | Bin_root
  | Swagger
  | Migrations
  | No_check

val dir : root -> string

val port : int

val should_write_shape_data : bool

val allow_test_routes : bool

val create_accounts : bool

(* DO NOT DISPATCH OFF THIS VALUE UNDER ANY CIRCUMSTANCES *)
val env_display_name : string

val static_host : string

val cookie_domain : string

val user_content_host : string

val show_stacktrace : bool

val rollbar_enabled : bool

val rollbar_environment : string

val rollbar_client_access_token : string option

val rollbar_js : string

val stroller_port : int option

val pusher_js : string

val heapio_id : string

val public_domain : string

val browser_reload_enabled : bool

val hash_static_filenames : bool

val curl_tunnel_url : string

val check_tier_one_hosts : bool

val static_assets_bucket : string option

val static_assets_salt_suffix : string

val build_hash : string

(* Do we use the new login service - login.darklang.com - or depend on local
 * sessions in postgres only *)
val use_login_darklang_com_for_login : bool
