open Core
open Config_dsl

let rollbar_server_access_token =
  (* This is what the rollbar UI calls it *)
  string "DARK_CONFIG_ROLLBAR_POST_SERVER_ITEM"

let rollbar_url =
  "https://api.rollbar.com/api/1/item/"

let rollbar_enabled = bool "DARK_CONFIG_ROLLBAR_ENABLED"
let rollbar_environment = string "DARK_CONFIG_ROLLBAR_ENVIRONMENT"
