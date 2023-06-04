/// Types used in Analysis of Dark code in the client.
///
/// These should match directly with AnalysisTypes in the client, and are
/// a simple translation layer between those types and AnalysisTypes.fs in the
/// backend.
module LibAnalysis.ClientAnalysisTypes

open Prelude
open Tablecloth

module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

module CPT = ClientProgramTypes
module CRT = ClientRuntimeTypes

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module AT = LibExecution.AnalysisTypes


type ExecutionResult =
  | ExecutedResult of CRT.Dval.T
  | NonExecutedResult of CRT.Dval.T

type InputVars = List<string * CRT.Dval.T>

type FunctionArgHash = string
type HashVersion = int
type FnName = string
type FunctionResult = FnName * id * FunctionArgHash * HashVersion * CRT.Dval.T

type TraceID = System.Guid

type TraceData = { input : InputVars; functionResults : List<FunctionResult> }

type Trace = TraceID * TraceData

type AnalysisResults = System.Collections.Generic.Dictionary<id, ExecutionResult>

type HandlerAnalysisParam =
  { requestID : int
    requestTime : NodaTime.Instant
    handler : CPT.Handler.T
    traceID : TraceID
    traceData : TraceData
    dbs : List<CPT.DB.T>
    userFns : list<CPT.UserFunction.T>
    userTypes : list<CPT.UserType.T>
    packageFns : list<CPT.PackageFn.T>
    packageTypes : list<CPT.PackageType.T>
    secrets : list<CPT.Secret> }

type FunctionAnalysisParam =
  { requestID : int
    requestTime : NodaTime.Instant
    func : CPT.UserFunction.T
    traceID : TraceID
    traceData : TraceData
    dbs : List<CPT.DB.T>
    userFns : list<CPT.UserFunction.T>
    userTypes : list<CPT.UserType.T>
    packageFns : list<CPT.PackageFn.T>
    packageTypes : list<CPT.PackageType.T>
    secrets : list<CPT.Secret> }

type PerformAnalysisParams =
  | AnalyzeHandler of HandlerAnalysisParam
  | AnalyzeFunction of FunctionAnalysisParam

type AnalysisEnvelope = TraceID * AnalysisResults * int * NodaTime.Instant

type AnalysisResult = Result<AnalysisEnvelope, string>





module TraceData =
  let fromCT (td : TraceData) : AT.TraceData =
    { input = List.map (fun (k, v) -> (k, CRT.Dval.fromCT v)) td.input
      functionResults =
        List.map
          (fun (name, id, hash, version, dval) ->
            (name, id, hash, version, CRT.Dval.fromCT dval))
          td.functionResults }


module ExecutionResult =
  let toCT (er : AT.ExecutionResult) : ExecutionResult =
    match er with
    | AT.ExecutedResult dv -> ExecutedResult(CRT.Dval.toCT dv)
    | AT.NonExecutedResult(dv) -> NonExecutedResult(CRT.Dval.toCT dv)


module AnalysisRequest =
  let fromCT (ar : PerformAnalysisParams) : AT.AnalysisRequest =
    let mapUserFn = CPT.UserFunction.fromCT >> PT2RT.UserFunction.toRT
    let mapUserType = CPT.UserType.fromCT >> PT2RT.UserType.toRT
    let mapUserDB = CPT.DB.fromCT >> PT2RT.DB.toRT
    let mapExpr = CPT.Expr.fromCT >> PT2RT.Expr.toRT
    let mapPackageFn = CPT.PackageFn.fromCT >> PT2RT.PackageFn.toRT
    let mapPackageType = CPT.PackageType.fromCT >> PT2RT.PackageType.toRT
    let mapUserSecret = CPT.Secret.fromCT >> PT2RT.Secret.toRT

    match ar with
    | AnalyzeHandler ah ->
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
        packageTypes = List.map mapPackageType ah.packageTypes
        secrets = List.map mapUserSecret ah.secrets }

    | AnalyzeFunction af ->
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
        packageTypes = List.map mapPackageType af.packageTypes
        secrets = List.map mapUserSecret af.secrets }


module AnalysisResults =
  let toCT (ar : AT.AnalysisResults) : AnalysisResults =
    ar
    |> Dictionary.toList
    |> List.map (fun (k, v) -> (k, ExecutionResult.toCT v))
    |> Dictionary.fromList
