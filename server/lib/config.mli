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


val dir : root -> string

val port : int

