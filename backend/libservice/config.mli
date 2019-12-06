val env_display_name : string

val rollbar_server_access_token : string

val rollbar_url : string

val rollbar_enabled : bool

val rollbar_environment : string

val health_check_port : int

type postgres_config =
  { host : string
  ; dbname : string
  ; user : string
  ; password : string
  }

val postgres_settings : postgres_config
