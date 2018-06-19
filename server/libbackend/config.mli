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

val log_format : [`Stackdriver | `Regular ]
val log_level : [`Off | `Fatal | `Error | `Warn | `Info | `Debug | `All ]
val log_decorate : bool

val should_write_shape_data : bool

val allow_server_shutdown : bool

val rollbar_url : string
val rollbar_enabled : bool
val rollbar_environment : string
val rollbar_client_access_token : string option
val rollbar_server_access_token : string
val rollbar_js : string

