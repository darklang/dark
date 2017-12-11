open Core
open Lib
open Runtime


let fns : Lib.shortfn list = [

  { n = "DB::insert"
  ; o = []
  ; p = [par "table" TDB; par "val" TObj]
  ; r = TNull
  ; d = "Insert `val` into `table`"
  ; f = InProcess
        (function
          | [DDB db; DObj value] ->
            Db.with_postgres (fun _ -> Db.insert db value);
            DNull
          | args -> fail args)
  ; pr = None
  ; pu = false
  }
  ;

  { n = "DB::delete"
  ; o = []
  ; p = [par "table" TDB; par "value" TObj]
  ; r = TNull
  ; d = "Delete `value` from `table`"
  ; f = InProcess
        (function
          | [DDB db; DObj value ] ->
            Db.with_postgres (fun _ -> Db.delete db (DObj value));
            DNull
          | args -> fail args)
  ; pr = None
  ; pu = false
  }
  ;

  { n = "DB::update"
  ; o = []
  ; p = [par "table" TDB; par "value" TObj]
  ; r = TNull
  ; d = "Update `table` value which has the same ID as `value`"
  ; f = InProcess
        (function
          | [DDB db; DObj value ] ->
            Db.with_postgres (fun _ -> Db.update db (DObj value));
            DNull
          | args -> fail args)
  ; pr = None
  ; pu = false
  }
  ;


  { n = "DB::fetchBy"
  ; o = []
  ; p = [par "table" TDB; par "field" TStr; par "value" TAny]
  ; r = TAny
  ; d = "Fetch the value in `table` whose field `field` is `value`"
  ; f = InProcess
        (function
          | [DDB db; DStr field; value] ->
            Db.with_postgres (fun _ -> Db.fetch_by db field value)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "DB::keys"
  ; o = []
  ; p = [par "table" TDB]
  ; r = TAny
  ; d = "Fetch all the keys in `table`"
  ; f = InProcess
        (function
          | [DDB db] ->
            Db.with_postgres (fun _ -> Db.keys db)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "DB::fetchAll"
  ; o = []
  ; p = [par "table" TDB]
  ; r = TList
  ; d = "Fetch all the values in `table`"
  ; f = InProcess
        (function
          | [DDB db] ->
            Db.with_postgres (fun _ -> Db.fetch_all db)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


]

