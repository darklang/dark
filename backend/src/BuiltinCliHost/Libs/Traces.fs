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


/// Parse the stored input_data JSON and return list of InputVar records
let private parseInputData (inputDataJson : string) : Dval =
  let typeName = inputVarTypeName ()
  try
    use doc = JsonDocument.Parse(inputDataJson)
    doc.RootElement.EnumerateArray()
    |> Seq.toList
    |> List.map (fun item ->
      let name = item.GetProperty("name").GetString()
      let value =
        item.GetProperty("value").GetRawText()
        |> DvalReprInternalRoundtrippable.parseJsonV0
        |> RT2DT.Dval.toDT
      let fields = Map [ "name", DString name; "value", value ]
      DRecord(typeName, typeName, [], fields))
    |> Dval.list (KTCustomType(typeName, []))
  with ex ->
    print $"[traces] Failed to parse input_data JSON: {ex.Message}"
    Dval.list (KTCustomType(typeName, [])) []


/// Parse the stored function_results JSON and return list of FnCall records
let private parseFnCalls (fnResultsJson : string) : Dval =
  let typeName = fnCallTypeName ()
  let dvalKT = KTCustomType(dvalTypeName (), [])
  try
    use doc = JsonDocument.Parse(fnResultsJson)
    doc.RootElement.EnumerateArray()
    |> Seq.toList
    |> List.map (fun item ->
      let fnName = item.GetProperty("fn").GetString()
      let result =
        item.GetProperty("result").GetRawText()
        |> DvalReprInternalRoundtrippable.parseJsonV0
        |> RT2DT.Dval.toDT
      let args =
        match item.TryGetProperty("args") with
        | true, argsEl ->
          argsEl.EnumerateArray()
          |> Seq.toList
          |> List.map (fun argEl ->
            argEl.GetRawText()
            |> DvalReprInternalRoundtrippable.parseJsonV0
            |> RT2DT.Dval.toDT)
        | _ -> []
      let fields =
        Map
          [ "fnName", DString fnName
            "args", Dval.list dvalKT args
            "result", result ]
      DRecord(typeName, typeName, [], fields))
    |> Dval.list (KTCustomType(typeName, []))
  with ex ->
    print $"[traces] Failed to parse function_results JSON: {ex.Message}"
    Dval.list (KTCustomType(typeName, [])) []


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
                "SELECT trace_id, handler_desc, timestamp, input_data, function_results
                 FROM traces_v0
                 WHERE trace_id = @traceId"
              |> Sql.parameters [ "traceId", Sql.string traceID ]
              |> Sql.executeRowOptionAsync (fun read ->
                (read.string "trace_id",
                 read.string "handler_desc",
                 read.string "timestamp",
                 read.string "input_data",
                 read.string "function_results"))

            let typeName = traceDataTypeName ()
            match row with
            | Some(traceId, handlerDesc, timestamp, inputDataJson, fnResultsJson) ->
              let fields =
                Map
                  [ "traceId", DString traceId
                    "handler", DString handlerDesc
                    "timestamp", DString timestamp
                    "inputs", parseInputData inputDataJson
                    "functionCalls", parseFnCalls fnResultsJson ]
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
                "SELECT trace_id, handler_desc, timestamp
                 FROM traces_v0
                 WHERE function_results LIKE @pattern
                 ORDER BY rowid DESC
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
              Sql.query "SELECT input_data FROM traces_v0 WHERE trace_id = @traceId"
              |> Sql.parameters [ "traceId", Sql.string traceID ]
              |> Sql.executeRowOptionAsync (fun read -> read.string "input_data")

            match row with
            | None -> return Dval.optionNone KTString
            | Some inputDataJson ->
              try
                use doc = JsonDocument.Parse(inputDataJson)
                let items = doc.RootElement.EnumerateArray() |> Seq.toList
                match items with
                | item :: _ ->
                  let valueJson = item.GetProperty("value").GetRawText()
                  let dval = DvalReprInternalRoundtrippable.parseJsonV0 valueJson
                  match dval with
                  | DString code -> return Dval.optionSome KTString (DString code)
                  | _ -> return Dval.optionNone KTString
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
            do! Sql.query "DELETE FROM traces_v0" |> Sql.executeStatementAsync
            return DInt64 count
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
