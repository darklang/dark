open Core_kernel
open Libexecution

module PG = Postgresql

(* globals *)
let rec rec_con depth =
  try
    let db = Config.postgres_settings in
    Log.infO "Connecting to postgres" (db.host, db.dbname, db.user);
    let c = new PG.connection ~host:db.host ~dbname:db.dbname
      ~user:db.user ~password:db.password ()  in
    c#set_notice_processor (Log.infO "postgres");
    c
  with
  | e ->
      Log.infO "Couldn't connect to postgres, attempt " depth;
      if depth < 10
      then
        (Unix.sleep 1;
         rec_con (depth+1))
      else
        raise e

let conn = rec_con 0

