/// Types used in Analysis of Dark code in the client.
///
/// These should match directly with AnalysisTypes.res in the client, and are
/// a simple translation layer between those types and AnalysisTypes.fs in the
/// backend.
module ClientTypes2ExecutionTypes.Analysis

open Prelude
open Tablecloth

module AT = LibExecution.AnalysisTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

module CTRuntime = ClientTypes.Runtime
module CTAnalysis = ClientTypes.Analysis
module CT2Runtime = Runtime
module CT2Program = ProgramTypes


module TraceData =
  let fromCT (td : CTAnalysis.TraceData) : AT.TraceData =
    { input = List.map (fun (k, v) -> (k, CT2Runtime.Dval.fromCT v)) td.input
      functionResults =
        List.map
          (fun (name, id, hash, version, dval) ->
            (name, id, hash, version, CT2Runtime.Dval.fromCT dval))
          td.functionResults }


module ExecutionResult =
  let toCT (er : AT.ExecutionResult) : CTAnalysis.ExecutionResult =
    match er with
    | AT.ExecutedResult (dv) ->
      CTAnalysis.ExecutionResult.ExecutedResult(CT2Runtime.Dval.toCT dv)
    | AT.NonExecutedResult (dv) ->
      CTAnalysis.ExecutionResult.NonExecutedResult(CT2Runtime.Dval.toCT dv)


module AnalysisRequest =
  let fromCT (ar : CTAnalysis.PerformAnalysisParams) : AT.AnalysisRequest =
    let mapUserFn = CT2Program.UserFunction.fromCT >> PT2RT.UserFunction.toRT
    let mapUserType = CT2Program.UserType.fromCT >> PT2RT.UserType.toRT
    let mapUserDB = CT2Program.DB.fromCT >> PT2RT.DB.toRT
    let mapExpr = CT2Program.Expr.fromCT >> PT2RT.Expr.toRT
    let mapPackageFn = CT2Program.Package.Fn.fromCT >> PT2RT.Package.toRT
    let mapUserSecret = CT2Program.Secret.fromCT >> PT2RT.Secret.toRT

    match ar with
    | CTAnalysis.AnalyzeHandler ah ->
      { requestID = ah.requestID
        requestTime = ah.requestTime
        tlid = ah.handler.tlid
        traceData = TraceData.fromCT ah.traceData
        traceID = AT.TraceID.fromUUID ah.traceID
        userFns = List.map mapUserFn ah.userFns
        userTypes = List.map mapUserType ah.userTypes
        dbs = List.map mapUserDB ah.dbs
        expr = mapExpr ah.handler.ast
        packageFns = List.map mapPackageFn ah.packageFns
        secrets = List.map mapUserSecret ah.secrets }

    | CTAnalysis.AnalyzeFunction af ->
      { requestID = af.requestID
        requestTime = af.requestTime
        tlid = af.func.tlid
        traceData = TraceData.fromCT af.traceData
        traceID = AT.TraceID.fromUUID af.traceID
        userFns = List.map mapUserFn af.userFns
        userTypes = List.map mapUserType af.userTypes
        dbs = List.map mapUserDB af.dbs
        expr = mapExpr af.func.body
        packageFns = List.map mapPackageFn af.packageFns
        secrets = List.map mapUserSecret af.secrets }


module AnalysisResults =
  let toCT (ar : AT.AnalysisResults) : CTAnalysis.AnalysisResults =
    ar
    |> Dictionary.toList
    |> List.map (fun (k, v) -> (k, ExecutionResult.toCT v))
    |> Dictionary.fromList
