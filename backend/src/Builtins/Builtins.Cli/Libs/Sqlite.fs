/// SQL on SQLite database files, through the checked host boundary (`HostSqlite`).
/// `Darklang.Sqlite` builds on these.
module Builtins.Cli.Libs.Sqlite

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
module Bytes = LibExecution.Bytes
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution
module HostTypes = LibExecution.HostTypes
module Interpreter = LibExecution.Interpreter
open Builtin.Shortcuts

type private SV = HostTypes.SqliteValue

let private valueName () = FQTypeName.fqPackage (PackageRefs.Type.Sqlite.value ())
let private errorName () = FQTypeName.fqPackage (PackageRefs.Type.Sqlite.error ())
let private changesName () =
  FQTypeName.fqPackage (PackageRefs.Type.Sqlite.changes ())

let private valueKT () = KTCustomType(valueName (), [])
let private errorKT () = KTCustomType(errorName (), [])
let private changesKT () = KTCustomType(changesName (), [])
let private rowKT () = KTDict(VT.string, VT.known (valueKT ()))
let private rowsKT () = KTList(VT.known (rowKT ()))

let private valueTypeRef () = TCustomType(NR.ok (valueName ()), [])
let private errorTypeRef () = TCustomType(NR.ok (errorName ()), [])
let private changesTypeRef () = TCustomType(NR.ok (changesName ()), [])

let private toHost (state : ExecutionState) (dv : Dval) : SV =
  match dv with
  | DEnum(_, _, _, "Null", []) -> SV.Null
  | DEnum(_, _, _, "Integer", [ DInt64 n ]) -> SV.Integer n
  | DEnum(_, _, _, "Real", [ DFloat64 f ]) -> SV.Real f
  | DEnum(_, _, _, "Text", [ DString s ]) -> SV.Text s
  | DEnum(_, _, _, "Blob", [ DBytes b ]) -> SV.Blob(Bytes.readBytes state b)
  | _ -> incorrectArgs ()

let private ofHost (value : SV) : Dval =
  let case name fields =
    let tn = valueName ()
    DEnum(tn, tn, [], name, fields)
  match value with
  | SV.Null -> case "Null" []
  | SV.Integer n -> case "Integer" [ Dval.dint64 n ]
  | SV.Real f -> case "Real" [ DFloat64 f ]
  | SV.Text s -> case "Text" [ DString s ]
  | SV.Blob b -> case "Blob" [ Bytes.newEphemeral b ]

let private record (name : FQTypeName.FQTypeName) (fields : List<string * Dval>) =
  DRecord(name, name, [], RecordFields.ofList fields)

let private error (code : int) (message : string) : Dval =
  record
    (errorName ())
    [ "code", Dval.dint64 (int64 code); "message", DString message ]

let private rows (answer : HostTypes.SqliteAnswer) : Dval =
  answer.rows
  |> List.map (fun row ->
    row
    |> List.map (fun (name, value) -> name, ofHost value)
    |> Dval.stringDict (valueKT ()))
  |> Dval.list (rowKT ())

let private changes (answer : HostTypes.SqliteAnswer) : Dval =
  record
    (changesName ())
    [ "rowsAffected", Dval.dint64 answer.rowsAffected
      "lastInsertRowid", Dval.dint64 answer.lastInsertRowid ]

let private call
  (write : bool)
  (okKT : unit -> KnownType)
  (ok : HostTypes.SqliteAnswer -> Dval)
  : BuiltInFnSig =
  function
  | state, vm, _, [| DString path; DString sql; DList(_, Items parameters) |] ->
    let parameters = List.map (toHost state) parameters
    let op = HostTypes.Operation.Sqlite(path, write, sql, parameters)
    Interpreter.requestHost vm op (fun outcome ->
      let okKT = okKT ()
      match outcome with
      | Ok(HostTypes.Response.Sqlite(Ok answer)) ->
        Dval.resultOk okKT (errorKT ()) (ok answer)
      | Ok(HostTypes.Response.Sqlite(Error e)) ->
        Dval.resultError okKT (errorKT ()) (error e.code e.message)
      | Ok _ -> incorrectArgs ()
      | Error failure ->
        Dval.resultError okKT (errorKT ()) (error -1 failure.message))
  | _ -> incorrectArgs ()

let private parameters () =
  [ Param.make "path" TString "The database file"
    Param.make "sql" TString "One or more SQL statements"
    Param.make "params" (TList(valueTypeRef ())) "Bound to ?1, ?2, ... in order" ]

let fns () : List<BuiltInFn> =
  [ { name = fn "sqliteQuery" 0
      typeParams = []
      parameters = parameters ()
      returnType =
        TypeReference.result
          (TList(TDict(TString, valueTypeRef ())))
          (errorTypeRef ())
      description =
        "Runs <param sql> on the SQLite database at <param path>, opened read-only, and returns the rows of every statement, each keyed by column name."
      fn = call false rowsKT rows
      previewable = Impure
      callEffects = set [ Effect.FileRead ]
      deprecated = NotDeprecated }

    { name = fn "sqliteExecute" 0
      typeParams = []
      parameters = parameters ()
      returnType = TypeReference.result (changesTypeRef ()) (errorTypeRef ())
      description =
        "Runs the statements in <param sql> on the SQLite database at <param path> in one transaction, creating the file if needed, and returns the rows they changed and the last inserted row id."
      fn = call true changesKT changes
      previewable = Impure
      callEffects = set [ Effect.FileWrite ]
      deprecated = NotDeprecated } ]

let builtins () : Builtins = Builtin.make [] (fns ())
