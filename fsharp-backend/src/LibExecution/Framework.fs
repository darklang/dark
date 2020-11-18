module LibExecution.Framework

// fsharplint:disable FL0039

open LibExecution.Runtime

type pos = { x : int; y : int }

module Handler =
  type CronInterval =
    | EveryDay
    | EveryWeek
    | EveryFortnight
    | EveryHour
    | Every12Hours
    | EveryMinute

  // We need to keep the IDs around until we get rid of them on the client
  type ids = { moduleID : id; nameID : id; modifierID : id }

  type Spec =
    | HTTP of path : string * method : string * ids : ids
    | Worker of name : string * ids : ids
    // Deprecated but still supported form
    | OldWorker of modulename : string * name : string * ids : ids
    | Cron of name : string * interval : string * ids : ids
    | REPL of name : string * ids : ids


  type T = { tlid : tlid; ast : Expr; spec : Spec }

module DB =
  type Col = string * DType
  type T = { tlid : tlid; name : string; cols : List<Col> }

module UserType =
  type RecordField = { name : string; typ : DType }
  type Definition = UTRecord of List<RecordField>

  type T = { tlid : tlid; name : string; version : int; definition : Definition }

module UserFunction =
  type Parameter = { name : string; typ : DType; description : string }

  type T =
    { tlid : tlid
      name : string
      parameters : List<Parameter>
      returnType : DType
      description : string
      infix : bool
      ast : Expr }

type Toplevel =
  | TLHandler of Handler.T
  | TLDB of DB.T
  | TLFunction of UserFunction.T
  | TLType of UserType.T

  member this.toTLID() : tlid =
    match this with
    | TLHandler h -> h.tlid
    | TLDB db -> db.tlid
    | TLFunction f -> f.tlid
    | TLType t -> t.tlid
