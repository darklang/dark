open Core_kernel
open Libcommon
module PG = Postgresql

(* globals *)
let rec rec_con depth =
  try
    let db = Config.postgres_settings in
    Log.infO
      "Connecting to postgres"
      ~params:[("host", db.host); ("dbname", db.dbname); ("user", db.user)] ;
    let c =
      new PG.connection
        ~host:db.host
        ~dbname:db.dbname
        ~user:db.user
        ~password:db.password
        ()
    in
    c#set_notice_processor (fun notice ->
        Log.warN "postgres_notice" ~data:notice ) ;
    c
  with e ->
    Log.infO
      "Couldn't connect to postgres"
      ~params:[("attempt", string_of_int depth)] ;
    if depth < 10
    then (
      Unix.sleep 1 ;
      rec_con (depth + 1) )
    else raise e


let conn = rec_con 0

let status () =
  try
    match conn#status with
    | Ok ->
        `Healthy
    | Connection_started ->
        `Healthy
    | Connection_made ->
        `Healthy
    | Connection_awaiting_response ->
        `Healthy
    | Connection_auth_ok ->
        `Healthy
    | Connection_setenv ->
        `Healthy
    | Connection_ssl_startup ->
        `Healthy
    | Bad ->
        `Disconnected
  with e -> `Disconnected
