open Core

type root = Events
          | Function_results
          | Log
          | Serialization
          | Templates
          | Webroot
          | Completed_test
          | Testdata
          | Bin_root
          | Appdata
          | Swagger
          | No_check

type postgres_config = { host: string
                       ; dbname: string
                       ; user: string
                       ; password: string
                       }

val postgres_settings : postgres_config

val dir : root -> string

val port : int

val log_level : Log.level

val should_write_shape_data : bool

val allow_server_shutdown : bool

val rollbar_url : string
val rollbar_enabled : bool
val rollbar_environment : string
val rollbar_client_access_token : string option
val rollbar_server_access_token : string
val rollbar_js : string

