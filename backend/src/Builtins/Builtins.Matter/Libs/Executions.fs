/// Durable runs, as data: the rows `LibDB.Executions` keeps beside the trace store. `dark exec`
/// renders them (`cli/exec.dark`); `resume` arms a replay and then runs the execution's input
/// through the ordinary `eval`/`run` path, which is where the replay tracer is picked up.
module Builtins.Matter.Libs.Executions

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin
open Builtin.Shortcuts

module Dval = LibExecution.Dval
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution
module Executions = LibDB.Executions


let private typ (name : unit -> string) : FQTypeName.FQTypeName =
  FQTypeName.fqPackage (name ())

let private statusToDT (status : Executions.Status) : Dval =
  let tn = typ PackageRefs.Type.Stdlib.Exec.Execution.status
  let case =
    match status with
    | Executions.Running -> "Running"
    | Executions.Done -> "Done"
    | Executions.Failed -> "Failed"
    | Executions.Suspended -> "Suspended"
  DEnum(tn, tn, [], case, [])

let private executionToDT (e : Executions.Execution) : Dval =
  let tn = typ PackageRefs.Type.Stdlib.Exec.Execution.execution
  let parentKT = KTTuple(ValueType.Known KTUuid, ValueType.Known KTInt64, [])
  let parent =
    match e.parent with
    | Some(id, at) -> Dval.optionSome parentKT (DTuple(DUuid id, DInt64 at, []))
    | None -> Dval.optionNone parentKT
  let input =
    match e.input with
    | DString s -> s
    | other -> string other
  DRecord(
    tn,
    tn,
    [],
    Map
      [ "id", DUuid e.id
        "entry", DString e.handlerDesc
        "input", DString input
        "traceId", DUuid(LibExecution.AnalysisTypes.TraceID.toUUID e.traceId)
        "status", statusToDT e.status
        "parent", parent
        "created", DString e.created
        "updated", DString e.updated ]
  )

let private executionType () =
  TCustomType(NR.ok (typ PackageRefs.Type.Stdlib.Exec.Execution.execution), [])

let private executionKT () =
  KTCustomType(typ PackageRefs.Type.Stdlib.Exec.Execution.execution, [])


let fns () : List<BuiltInFn> =
  [ { name = fn "executionList" 0
      typeParams = []
      parameters = [ Param.make "limit" TInt64 "" ]
      returnType = TList(executionType ())
      description = "The most recent executions, newest first."
      fn =
        (function
        | _, _, _, [| DInt64 limit |] ->
          uply {
            let! rows = Executions.list (int limit)
            return
              DList(ValueType.Known(executionKT ()), rows |> List.map executionToDT)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ LibExecution.Effects.Effect.TraceRead ]
      deprecated = NotDeprecated }


    { name = fn "executionGet" 0
      typeParams = []
      parameters = [ Param.make "id" TUuid "" ]
      returnType = TypeReference.option (executionType ())
      description = "One execution, or None for an id nobody has."
      fn =
        (function
        | _, _, _, [| DUuid id |] ->
          uply {
            let! row = Executions.get id
            return Dval.option (executionKT ()) (row |> Option.map executionToDT)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ LibExecution.Effects.Effect.TraceRead ]
      deprecated = NotDeprecated }


    { name = fn "executionFork" 0
      typeParams = []
      parameters =
        [ Param.make "id" TUuid ""
          Param.make "at" (TypeReference.option TInt64) "" ]
      returnType = TypeReference.result TUuid TString
      description =
        "A new execution branched from this one: the same input, its log up to `at` (a position "
        + "in the trace; the whole log when None), suspended so `resume` takes it from there."
      fn =
        (function
        | _, _, _, [| DUuid id; at |] ->
          uply {
            let at =
              match at with
              | DEnum(_, _, _, "Some", [ DInt64 n ]) -> Some n
              | _ -> None
            match! Executions.fork id at with
            | Ok child -> return Dval.resultOk KTUuid KTString (DUuid child)
            | Error msg -> return Dval.resultError KTUuid KTString (DString msg)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ LibExecution.Effects.Effect.TraceWrite ]
      deprecated = NotDeprecated }


    { name = fn "executionArmResume" 0
      typeParams = []
      parameters = [ Param.make "id" TUuid "" ]
      returnType = TBool
      description =
        "Arm a resume of this execution: the next `eval` or `run` this instance starts replays "
        + "its log instead of performing the effects, then goes live. False for an id nobody has."
      fn =
        (function
        | _, _, _, [| DUuid id |] ->
          uply {
            let! armed = Executions.Replay.arm id
            return DBool armed
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ LibExecution.Effects.Effect.TraceRead ]
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
