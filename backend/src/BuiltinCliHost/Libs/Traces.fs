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
module NR = LibExecution.RuntimeTypes.NameResolution
module TracesRefs = LibExecution.PackageRefs.Type.Cli.Commands.Traces

let dvalTypeName () =
  FQTypeName.fqPackage (
    LibExecution.PackageRefs.Type.LanguageTools.RuntimeTypes.dval ()
  )
let traceSummaryTypeName () = FQTypeName.fqPackage (TracesRefs.traceSummary ())
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


/// Load call events for a trace, ordered by rowid (= execution order).
/// Args/result are stored as JSON inline; parse them per row. The display
/// name was resolved at write time and lives in fn_hash, so reads are a
/// flat SELECT — lambdas have NULL fn_hash and render as "(lambda)".
let private loadFnCalls (traceId : string) : Ply<Dval> =
  let typeName = fnCallTypeName ()
  let dvalKT = KTCustomType(dvalTypeName (), [])
  uply {
    let! events =
      Sql.query
        "SELECT call_id, parent_call_id, kind, fn_hash, lambda_expr_id,
                args_json, result_json
         FROM trace_fn_calls
         WHERE trace_id = @traceId
         ORDER BY rowid"
      |> Sql.parameters [ "traceId", Sql.string traceId ]
      |> Sql.executeAsync (fun read ->
        {| callId = read.string "call_id"
           parentCallId = read.stringOrNone "parent_call_id"
           kind = read.string "kind"
           fnHash = read.stringOrNone "fn_hash"
           lambdaExprId = read.stringOrNone "lambda_expr_id"
           argsJson = read.string "args_json"
           resultJson = read.string "result_json" |})

    let enriched =
      events
      |> List.map (fun ev ->
        let displayName =
          match ev.kind, ev.fnHash with
          | "lambda", _ -> "(lambda)"
          | _, Some name -> name
          | _, None -> "(unknown)"
        let args = parseArgsJson ev.argsJson
        let result = parseDvalJson ev.resultJson
        let parentCallIdDval =
          match ev.parentCallId with
          | Some p -> Dval.optionSome KTString (DString p)
          | None -> Dval.optionNone KTString
        let lambdaExprIdDval =
          match ev.lambdaExprId with
          | Some i -> Dval.optionSome KTString (DString i)
          | None -> Dval.optionNone KTString
        let fields =
          Map
            [ "callId", DString ev.callId
              "parentCallId", parentCallIdDval
              "kind", DString ev.kind
              "fnName", DString displayName
              "lambdaExprId", lambdaExprIdDval
              "args", Dval.list dvalKT args
              "result", result ]
        DRecord(typeName, typeName, [], fields))

    return enriched |> Dval.list (KTCustomType(typeName, []))
  }


let fns () : List<BuiltInFn> =
  [ { name = fn "cliTracesList" 0
      typeParams = []
      parameters = [ Param.make "limit" TInt64 "Max number of traces to return" ]
      returnType = TList(TVariable "a")
      description = "List recent traces"
      fn =
        (function
        | _, _, _, [ DInt64 limit ] ->
          uply {
            let typeName = traceSummaryTypeName ()
            let! rows =
              Sql.query
                "SELECT id, handler_desc, timestamp
                 FROM traces
                 ORDER BY rowid DESC
                 LIMIT @limit"
              |> Sql.parameters [ "limit", Sql.int64 limit ]
              |> Sql.executeAsync (fun read ->
                {| id = read.string "id"
                   handler = read.string "handler_desc"
                   timestamp = read.string "timestamp" |})

            return
              rows
              |> List.map (fun r ->
                let fields =
                  Map
                    [ "traceId", DString r.id
                      "handler", DString r.handler
                      "timestamp", DString r.timestamp ]
                DRecord(typeName, typeName, [], fields))
              |> Dval.list (KTCustomType(typeName, []))
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
            // One SELECT covers metadata + input — both live on the trace row.
            let! row =
              Sql.query
                "SELECT id, handler_desc, timestamp, input_name, input_value_json
                 FROM traces
                 WHERE id = @traceId"
              |> Sql.parameters [ "traceId", Sql.string traceID ]
              |> Sql.executeRowOptionAsync (fun read ->
                {| id = read.string "id"
                   handlerDesc = read.string "handler_desc"
                   timestamp = read.string "timestamp"
                   inputName = read.string "input_name"
                   inputValueJson = read.string "input_value_json" |})

            let typeName = traceDataTypeName ()
            match row with
            | Some r ->
              let! fnCalls = loadFnCalls r.id
              let inputVarType = inputVarTypeName ()
              let inputFields =
                Map
                  [ "name", DString r.inputName
                    "value", parseDvalJson r.inputValueJson ]
              let inputs =
                [ DRecord(inputVarType, inputVarType, [], inputFields) ]
                |> Dval.list (KTCustomType(inputVarType, []))
              let fields =
                Map
                  [ "traceId", DString r.id
                    "handler", DString r.handlerDesc
                    "timestamp", DString r.timestamp
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
      returnType = TList(TVariable "a")
      description = "List traces that called a specific function"
      fn =
        (function
        | _, _, _, [ DString fnName; DInt64 limit ] ->
          uply {
            // Both builtins and package fns store their display name in
            // fn_hash (resolved at write time), so one LIKE matches either.
            let typeName = traceSummaryTypeName ()
            let pattern = $"%%{fnName}%%"
            let! rows =
              Sql.query
                "SELECT DISTINCT t.id, t.handler_desc, t.timestamp
                 FROM traces t
                 JOIN trace_fn_calls c ON t.id = c.trace_id
                 WHERE c.fn_hash LIKE @pattern
                 ORDER BY t.rowid DESC
                 LIMIT @limit"
              |> Sql.parameters
                [ "pattern", Sql.string pattern; "limit", Sql.int64 limit ]
              |> Sql.executeAsync (fun read ->
                {| id = read.string "id"
                   handler = read.string "handler_desc"
                   timestamp = read.string "timestamp" |})

            return
              rows
              |> List.map (fun r ->
                let fields =
                  Map
                    [ "traceId", DString r.id
                      "handler", DString r.handler
                      "timestamp", DString r.timestamp ]
                DRecord(typeName, typeName, [], fields))
              |> Dval.list (KTCustomType(typeName, []))
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
              Sql.query "SELECT input_value_json FROM traces WHERE id = @traceId"
              |> Sql.parameters [ "traceId", Sql.string traceID ]
              |> Sql.executeRowOptionAsync (fun read ->
                read.string "input_value_json")

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
              Sql.query "SELECT COUNT(*) as c FROM traces"
              |> Sql.executeRowAsync (fun read -> read.int64 "c")
            do! Sql.query "DELETE FROM trace_fn_calls" |> Sql.executeStatementAsync
            do! Sql.query "DELETE FROM traces" |> Sql.executeStatementAsync
            return DInt64 count
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
