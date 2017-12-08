open Core
open Lib
open Runtime


let fns : Lib.shortfn list = [

  { n = "DB::insert"
  ; o = []
  ; p = [par "table" TOpaque; par "val" TObj]
  ; r = TAny
  ; d = "Insert `val` into `table`"
  ; f = InProcess
        (function
          | [DOpaque o; DObj value] ->
            Db.with_postgres o
              (fun table ->
                 Db.insert table value);
            DNull
          | args -> fail args)
  ; pr = None
  ; pu = false
  }
  ;

(*  *)
(*   { n = "DB::delete" *)
(*   ; o = [] *)
(*   ; p = [par "table" TOpaque; par "key" TStr] *)
(*   ; r = TAny *)
(*   ; d = "Delete the value in `table` keyed by `key`" *)
(*   ; f = InProcess *)
(*         (function *)
(*           | [DOpaque o; DStr key] -> *)
(*             Db.with_postgres o (fun table -> Db.kv_delete table key) *)
(*           | args -> fail args) *)
(*   ; pr = None *)
(*   ; pu = false *)
(*   } *)
(*   ; *)
(*  *)
(*  *)
(*   { n = "DB::fetch" *)
(*   ; o = [] *)
(*   ; p = [par "table" TOpaque; par "key" TStr] *)
(*   ; r = TAny *)
(*   ; d = "Fetch the value in `table` keyed by `key`" *)
(*   ; f = InProcess *)
(*         (function *)
(*           | [DOpaque o; DStr key] -> *)
(*             Db.with_postgres o (fun table -> Db.kv_fetch table key) *)
(*           | args -> fail args) *)
(*   ; pr = None *)
(*   ; pu = true *)
(*   } *)
(*   ; *)
(*  *)
(*  *)
(*   { n = "DB::keys" *)
(*   ; o = [] *)
(*   ; p = [par "table" TOpaque] *)
(*   ; r = TAny *)
(*   ; d = "Fetch all the keys in `table`" *)
(*   ; f = InProcess *)
(*         (function *)
(*           | [DOpaque o] -> *)
(*             Db.with_postgres o (fun table -> Db.kv_keys table) *)
(*           | args -> fail args) *)
(*   ; pr = None *)
(*   ; pu = true *)
(*   } *)
(*   ; *)
(*  *)
(*  *)
  { n = "DB::fetch_all"
  ; o = []
  ; p = [par "table" TOpaque]
  ; r = TList
  ; d = "Fetch all the values in `table`"
  ; f = InProcess
        (function
          | [DOpaque o] ->
            Db.with_postgres o (fun table -> Db.fetch_all table)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


]

