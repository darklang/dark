/// Builtin functions for querying traces from the CLI
module BuiltinCliHost.Libs.Traces

open System.Text.Json

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open Fumble
open LibDB.Db

module Dval = LibExecution.Dval
module VT = LibExecution.ValueType
module Exe = LibExecution.Execution
module DvalReprInternalRoundtrippable = LibExecution.DvalReprInternalRoundtrippable


/// Parse the stored input_data JSON and format each input var with dvalToRepr
let formatInputData
  (exeState : ExecutionState)
  (inputDataJson : string)
  : Ply<string> =
  uply {
    try
      use doc = JsonDocument.Parse(inputDataJson)
      let items = doc.RootElement.EnumerateArray() |> Seq.toList

      let! parts =
        items
        |> Ply.List.mapSequentially (fun item ->
          uply {
            let name = item.GetProperty("name").GetString()
            let valueJson = item.GetProperty("value").GetRawText()
            try
              let dval = DvalReprInternalRoundtrippable.parseJsonV0 valueJson
              let! repr = Exe.dvalToRepr exeState dval
              return $"  {name} = {repr}"
            with ex ->
              print $"[traces] Failed to format input {name}: {ex.Message}"
              return $"  {name} = <unparseable>"
          })

      return parts |> String.concat "\n"
    with ex ->
      print $"[traces] Failed to parse input_data JSON: {ex.Message}"
      return $"  {inputDataJson}"
  }


/// Parse the stored function_results JSON and format each call with dvalToRepr
let formatFnCalls
  (exeState : ExecutionState)
  (fnResultsJson : string)
  : Ply<string> =
  uply {
    try
      use doc = JsonDocument.Parse(fnResultsJson)
      let items = doc.RootElement.EnumerateArray() |> Seq.toList

      if items.IsEmpty then
        return "  (none)"
      else
        let! parts =
          items
          |> Ply.List.mapSequentially (fun item ->
            uply {
              let fnName = item.GetProperty("fn").GetString()
              let resultJson = item.GetProperty("result").GetRawText()
              try
                let resultDval =
                  DvalReprInternalRoundtrippable.parseJsonV0 resultJson

                // Format args if present
                let! argsRepr =
                  uply {
                    match item.TryGetProperty("args") with
                    | true, argsEl ->
                      let! parts =
                        argsEl.EnumerateArray()
                        |> Seq.toList
                        |> Ply.List.mapSequentially (fun argEl ->
                          uply {
                            try
                              let argDval =
                                DvalReprInternalRoundtrippable.parseJsonV0 (
                                  argEl.GetRawText()
                                )
                              return! Exe.dvalToRepr exeState argDval
                            with ex ->
                              print
                                $"[traces] Failed to format arg for {fnName}: {ex.Message}"
                              return "<unparseable>"
                          })
                      return parts |> String.concat ", "
                    | _ -> return ""
                  }

                let! resultRepr = Exe.dvalToRepr exeState resultDval
                if argsRepr = "" then
                  return $"  {fnName} → {resultRepr}"
                else
                  return $"  {fnName}({argsRepr}) → {resultRepr}"
              with ex ->
                print $"[traces] Failed to format fn call {fnName}: {ex.Message}"
                return $"  {fnName} → <unparseable>"
            })

        return parts |> String.concat "\n"
    with ex ->
      print $"[traces] Failed to parse function_results JSON: {ex.Message}"
      return $"  {fnResultsJson}"
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
      returnType = TypeReference.option (TDict TString)
      description = "View trace details by trace ID, with pretty-printed values"
      fn =
        (function
        | exeState, _, _, [ DString traceID ] ->
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

            match row with
            | None -> return Dval.optionNone (KTDict(ValueType.Known KTString))
            | Some(traceId, handlerDesc, timestamp, inputDataJson, fnResultsJson) ->
              let! inputFormatted = formatInputData exeState inputDataJson
              let! fnCallsFormatted = formatFnCalls exeState fnResultsJson

              return
                [ "traceId", DString traceId
                  "handler", DString handlerDesc
                  "timestamp", DString timestamp
                  "input", DString inputFormatted
                  "functionCalls", DString fnCallsFormatted ]
                |> Map.ofList
                |> fun m -> DDict(ValueType.Known KTString, m)
                |> Dval.optionSome (KTDict(ValueType.Known KTString))
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
