open Core
open Lib
open Runtime
open Types.RuntimeT


let fns : Lib.shortfn list = [

  { pns = ["DB::insert"]
  ; ins = []
  ; p = [par "val" TObj; par "table" TDB]
  ; r = TObj
  ; d = "Insert `val` into `table`"
  ; f = InProcess
        (function
          | (_, [DObj value; DDB db]) ->
            let id = Db.with_postgres (fun _ -> Db.insert db value) in
            DObj (Map.set value "id" (DID id))
          | (_, args) -> fail args)
  ; pr = None
  ; ps = false
  }
  ;

  { pns = ["DB::delete"]
  ; ins = []
  ; p = [par "value" TObj; par "table" TDB]
  ; r = TNull
  ; d = "Delete `value` from `table`"
  ; f = InProcess
        (function
          | (_, [DObj vals; DDB db]) ->
            Db.with_postgres (fun _ -> Db.delete db vals);
            DNull
          | (_, args) -> fail args)
  ; pr = None
  ; ps = false
  }
  ;

  { pns = ["DB::update"]
  ; ins = []
  ; p = [par "value" TObj; par "table" TDB]
  ; r = TNull
  ; d = "Update `table` value which has the same ID as `value`"
  ; f = InProcess
        (function
          | (_, [DObj vals; DDB db] ) ->
            Db.with_postgres (fun _ -> Db.update db vals);
            DObj vals
          | (_, args) -> fail args)
  ; pr = None
  ; ps = false
  }
  ;

  { pns = ["DB::fetchBy"]
  ; ins = []
  ; p = [par "value" TAny; par "field" TStr; par "table" TDB]
  ; r = TList
  ; d = "Fetch the value in `table` whose field `field` is `value`"
  ; f = InProcess
        (function
          | (_, [value; DStr field; DDB db] ) ->
            Db.with_postgres (fun _ -> Db.fetch_by db field value)
          | (_, args) -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["DB::fetchOneBy"]
  ; ins = []
  ; p = [par "value" TAny; par "field" TStr; par "table" TDB]
  ; r = TAny
  ; d = "Fetch exactly one value in `table` whose field `field` is `value`"
  ; f = InProcess
        (function
          | (_, [value; DStr field; DDB db] ) ->
            let result = Db.with_postgres (fun _ -> Db.fetch_by db field value) in
            (match result with
             | DList (x :: xs) -> x
               (* TODO(ian): Maybe/Option *)
             | _ -> DNull)
          | (_, args) -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["DB::fetchAll"]
  ; ins = []
  ; p = [par "table" TDB]
  ; r = TList
  ; d = "Fetch all the values in `table`"
  ; f = InProcess
        (function
          | (_, [DDB db]) ->
            Db.with_postgres (fun _ -> Db.fetch_all db)
          | (_, args) -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["DB::keys"]
  ; ins = []
  ; p = [par "table" TDB]
  ; r = TList
  ; d = "Fetch all the keys in `table`"
  ; f = InProcess
        (function
          | (_, [DDB db]) ->
            Db.cols_for db
            |> List.map ~f:(fun (k,v) -> DStr k)
            |> DList
          | (_, args) -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["DB::schema"]
  ; ins = []
  ; p = [par "table" TDB]
  ; r = TObj
  ; d = "Fetch all the values in `table`"
  ; f = InProcess
        (function
          | (_, [DDB db]) ->
            Db.cols_for db
            |> List.map ~f:(fun (k,v) -> (k, DStr (Dval.tipe_to_string v)))
            |> Dval.to_dobj
          | (_, args) -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

]

