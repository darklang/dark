/// Builtin functions for querying traces from the CLI
module BuiltinCliHost.Libs.Traces

open System.Text.Json

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open Fumble
open LibDB.Db

module Dval = LibExecution.Dval
module DvalReprInternalRoundtrippable = LibExecution.DvalReprInternalRoundtrippable
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module TracesRefs = LibExecution.PackageRefs.Type.Cli.Commands.Traces

let dvalTypeName () =
  FQTypeName.fqPackage (
    LibExecution.PackageRefs.Type.LanguageTools.RuntimeTypes.dval ()
  )
let inputVarTypeName () = FQTypeName.fqPackage (TracesRefs.inputVar ())
let fnCallTypeName () = FQTypeName.fqPackage (TracesRefs.fnCall ())
let traceDataTypeName () = FQTypeName.fqPackage (TracesRefs.traceData ())


let private parseDvalJson (json : string) : Dval =
  json |> DvalReprInternalRoundtrippable.parseJsonV0 |> RT2DT.Dval.toDT

let private parseArgsJson (argsJson : string) : List<Dval> =
  use doc = JsonDocument.Parse(argsJson)
  doc.RootElement.EnumerateArray()
  |> Seq.toList
  |> List.map (fun el -> el.GetRawText() |> parseDvalJson)

/// Load inputs for a trace from the trace_inputs_v0 table
let private loadInputs (traceId : string) : Ply<Dval> =
  let typeName = inputVarTypeName ()
  uply {
    let! rows =
      Sql.query
        "SELECT name, value_json FROM trace_inputs_v0 WHERE trace_id = @traceId"
      |> Sql.parameters [ "traceId", Sql.string traceId ]
      |> Sql.executeAsync (fun read ->
        (read.string "name", read.string "value_json"))

    return
      rows
      |> List.map (fun (name, valueJson) ->
        let fields = Map [ "name", DString name; "value", parseDvalJson valueJson ]
        DRecord(typeName, typeName, [], fields))
      |> Dval.list (KTCustomType(typeName, []))
  }

/// Load function calls for a trace, ordered by rowid. SQLite assigns rowids
/// monotonically as the writer INSERTs rows in execution order within a
/// single transaction, so `ORDER BY rowid` recovers execution order.
let private loadFnCalls (traceId : string) : Ply<Dval> =
  let typeName = fnCallTypeName ()
  let dvalKT = KTCustomType(dvalTypeName (), [])
  uply {
    let! rows =
      Sql.query
        "SELECT caller_kind, caller, fn_kind, fn_name, args_json, result_json
         FROM trace_fn_calls_v0
         WHERE trace_id = @traceId
         ORDER BY rowid"
      |> Sql.parameters [ "traceId", Sql.string traceId ]
      |> Sql.executeAsync (fun read ->
        (read.string "caller_kind",
         read.string "caller",
         read.string "fn_kind",
         read.string "fn_name",
         read.string "args_json",
         read.string "result_json"))

    return
      rows
      |> List.map (fun (callerKind, caller, fnKind, fnName, argsJson, resultJson) ->
        let args = parseArgsJson argsJson
        let fields =
          Map
            [ "callerKind", DString callerKind
              "caller", DString caller
              "fnKind", DString fnKind
              "fnName", DString fnName
              "args", Dval.list dvalKT args
              "result", parseDvalJson resultJson ]
        DRecord(typeName, typeName, [], fields))
      |> Dval.list (KTCustomType(typeName, []))
  }


let fns () : List<BuiltInFn> =
  [ { name = fn "cliTracesList" 0
      typeParams = []
      parameters = [ Param.make "limit" TInt64 "Max number of traces to return" ]
      returnType = TList(TList TString)
      description = "List recent traces"
      fn =
        (function
        | _, _, _, [ DInt64 limit ] ->
          uply {
            let! rows =
              Sql.query
                "SELECT trace_id, handler_desc, timestamp
                 FROM traces_v0
                 ORDER BY rowid DESC
                 LIMIT @limit"
              |> Sql.parameters [ "limit", Sql.int64 limit ]
              |> Sql.executeAsync (fun read ->
                [ read.string "trace_id"
                  read.string "handler_desc"
                  read.string "timestamp" ])

            return
              rows
              |> List.map (fun fields ->
                fields |> List.map DString |> Dval.list KTString)
              |> Dval.list (KTList(ValueType.Known KTString))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "cliTracesView" 0
      typeParams = []
      parameters = [ Param.make "traceID" TString "The trace ID to view" ]
      returnType = TypeReference.option (TVariable "a")
      description =
        "View trace details by trace ID. Returns a TraceData record with structured inputs and function calls."
      fn =
        (function
        | _, _, _, [ DString traceID ] ->
          uply {
            let! row =
              Sql.query
                "SELECT trace_id, handler_desc, timestamp
                 FROM traces_v0
                 WHERE trace_id = @traceId"
              |> Sql.parameters [ "traceId", Sql.string traceID ]
              |> Sql.executeRowOptionAsync (fun read ->
                (read.string "trace_id",
                 read.string "handler_desc",
                 read.string "timestamp"))

            let typeName = traceDataTypeName ()
            match row with
            | Some(traceId, handlerDesc, timestamp) ->
              let! inputs = loadInputs traceId
              let! fnCalls = loadFnCalls traceId
              let fields =
                Map
                  [ "traceId", DString traceId
                    "handler", DString handlerDesc
                    "timestamp", DString timestamp
                    "inputs", inputs
                    "functionCalls", fnCalls ]
              return
                DRecord(typeName, typeName, [], fields)
                |> Dval.optionSome (KTCustomType(typeName, []))
            | None -> return Dval.optionNone (KTCustomType(typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "cliTracesListByFn" 0
      typeParams = []
      parameters =
        [ Param.make "fnName" TString "Function name to search for"
          Param.make "limit" TInt64 "Max number of traces to return" ]
      returnType = TList(TList TString)
      description = "List traces that called a specific function"
      fn =
        (function
        | _, _, _, [ DString fnName; DInt64 limit ] ->
          uply {
            let pattern = $"%%{fnName}%%"
            let! rows =
              Sql.query
                "SELECT DISTINCT t.trace_id, t.handler_desc, t.timestamp
                 FROM traces_v0 t
                 JOIN trace_fn_calls_v0 c ON t.trace_id = c.trace_id
                 WHERE c.fn_name LIKE @pattern
                 ORDER BY t.rowid DESC
                 LIMIT @limit"
              |> Sql.parameters
                [ "pattern", Sql.string pattern; "limit", Sql.int64 limit ]
              |> Sql.executeAsync (fun read ->
                [ read.string "trace_id"
                  read.string "handler_desc"
                  read.string "timestamp" ])

            return
              rows
              |> List.map (fun fields ->
                fields |> List.map DString |> Dval.list KTString)
              |> Dval.list (KTList(ValueType.Known KTString))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "cliTracesGetInput" 0
      typeParams = []
      parameters = [ Param.make "traceID" TString "The trace ID to get input from" ]
      returnType = TypeReference.option TString
      description = "Get the stored input code for a trace"
      fn =
        (function
        | _, _, _, [ DString traceID ] ->
          uply {
            let! row =
              Sql.query
                "SELECT value_json FROM trace_inputs_v0
                 WHERE trace_id = @traceId LIMIT 1"
              |> Sql.parameters [ "traceId", Sql.string traceID ]
              |> Sql.executeRowOptionAsync (fun read -> read.string "value_json")

            match row with
            | None -> return Dval.optionNone KTString
            | Some valueJson ->
              try
                let dval = DvalReprInternalRoundtrippable.parseJsonV0 valueJson
                match dval with
                | DString code -> return Dval.optionSome KTString (DString code)
                | _ -> return Dval.optionNone KTString
              with ex ->
                print $"[traces] Failed to parse input for replay: {ex.Message}"
                return Dval.optionNone KTString
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "cliTracesClear" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "Ignored" ]
      returnType = TInt64
      description = "Delete all traces, returns count deleted"
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let! count =
              Sql.query "SELECT COUNT(*) as c FROM traces_v0"
              |> Sql.executeRowAsync (fun read -> read.int64 "c")
            do! Sql.query "DELETE FROM trace_inputs_v0" |> Sql.executeStatementAsync
            do!
              Sql.query "DELETE FROM trace_fn_calls_v0" |> Sql.executeStatementAsync
            do! Sql.query "DELETE FROM traces_v0" |> Sql.executeStatementAsync
            return DInt64 count
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
