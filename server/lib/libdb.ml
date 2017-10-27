open Core
open Lib

module RT = Runtime
module PG = Postgresql

let fns : Lib.shortfn list = [

  { n = "DB::insert"
  ; o = []
  ; p = [par "table" TOpaque; par "key" TStr; par "val" TAny]
  ; r = TAny
  ; d = "Insert `val` into `table` indexed by `key`"
  ; f = InProcess
        (function
          | [DOpaque o; DStr key; value] ->
              let c = new PG.connection ~host:"localhost" ~dbname:"proddb" ~user:"dark" ~password:"eapnsdc" () in
              DStr "done"
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


]

