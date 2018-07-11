open Core_kernel

type root = Log
          | Serialization
          | Templates
          | Webroot
          | Completed_test
          | Testdata
          | Testresults
          | Bin_root
          | Appdata
          | Swagger
          | Migrations
          | No_check

type postgres_config = { host: string
                       ; dbname: string
                       ; user: string
                       ; password: string
                       }

val postgres_settings : postgres_config

val dir : root -> string

val port : int

val log_format : [`Stackdriver | `Regular | `Decorated ]
val log_level : Libcommon.Log.level

val should_write_shape_data : bool

val allow_server_shutdown : bool

val rollbar_enabled : bool
val rollbar_environment : string
val rollbar_client_access_token : string option
val rollbar_js : string

