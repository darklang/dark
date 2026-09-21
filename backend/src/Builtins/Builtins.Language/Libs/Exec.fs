/// The process table, for `dark ps`: what the scheduler in this instance is running, as data.
///
/// Beside Instrumentation for the same reason that sits beside Reflection: these report on the
/// system executing the code. Everything here is a copy taken on the scheduler thread through
/// `Scheduler.Snapshot`; nothing hands Dark a reference into a running VM. Rendering is Dark's
/// (`cli/ps.dark`).
module Builtins.Language.Libs.Exec

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin
open Builtin.Shortcuts

module Dval = LibExecution.Dval
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution
module Scheduler = LibExecution.Scheduler
module HE = LibExecution.HostEvents


let private typ (name : unit -> string) : FQTypeName.FQTypeName =
  FQTypeName.fqPackage (name ())

let private enumOf (name : unit -> string) (case : string) (fields : List<Dval>) =
  let tn = typ name
  DEnum(tn, tn, [], case, fields)

let private recordOf (name : unit -> string) (fields : List<string * Dval>) =
  let tn = typ name
  DRecord(tn, tn, [], Map fields)


let private eventSpecToDT (spec : HE.EventSpec) : Dval =
  let case = enumOf PackageRefs.Type.Stdlib.Host.eventSpec
  match spec with
  | HE.EventSpec.Key -> case "Key" []
  | HE.EventSpec.StoreChanged -> case "StoreChanged" []
  | HE.EventSpec.Timer ms -> case "Timer" [ DInt64 ms ]
  | HE.EventSpec.ExecDone id -> case "ExecDone" [ DUuid id ]

let private parkedToDT (parked : Scheduler.Parked) : Dval =
  let case = enumOf PackageRefs.Type.Stdlib.Exec.parkedOn
  match parked with
  | Scheduler.OnBuiltin b -> case "Builtin" [ RT2DT.FQFnName.Builtin.toDT b ]
  | Scheduler.OnPackageFn h -> case "PackageFn" [ RT2DT.FQFnName.Package.toDT h ]
  | Scheduler.OnLambda -> case "Lambda" []
  | Scheduler.OnRareOpcode -> case "RareOpcode" []
  | Scheduler.OnEvent specs ->
    let specs = specs |> List.map eventSpecToDT
    case
      "Events"
      [ DList(
          ValueType.Known(
            KTCustomType(typ PackageRefs.Type.Stdlib.Host.eventSpec, [])
          ),
          specs
        ) ]

let private statusToDT (status : Scheduler.Status) : Dval =
  let case = enumOf PackageRefs.Type.Stdlib.Exec.status
  match status with
  | Scheduler.Runnable -> case "Runnable" []
  | Scheduler.Parked parked -> case "Parked" [ parkedToDT parked ]
  | Scheduler.Done _ -> case "Done" []
  | Scheduler.Failed(rte, _) -> case "Failed" [ RT2DT.RuntimeError.toDT rte ]

let private entryToDT (entry : Scheduler.Entry) : Dval =
  let case = enumOf PackageRefs.Type.Stdlib.Exec.entry
  match entry with
  | Scheduler.EntryFunction name -> case "Function" [ RT2DT.FQFnName.toDT name ]
  | Scheduler.EntryExpr -> case "Expr" []

let rec private executionPointToDT (ep : ExecutionPoint) : Dval =
  let case = enumOf PackageRefs.Type.Stdlib.Exec.executionPoint
  match ep with
  | Source -> case "Source" []
  | Function name -> case "Function" [ RT2DT.FQFnName.toDT name ]
  | Lambda(parent, exprId) ->
    case "Lambda" [ executionPointToDT parent; DInt64(int64 exprId) ]

let private summaryToDT (p : Scheduler.ProcessSummary) : Dval =
  recordOf
    PackageRefs.Type.Stdlib.Exec.summary
    [ "id", DUuid p.id
      "entry", entryToDT p.entry
      "status", statusToDT p.status
      "parent", Dval.option KTUuid (p.parent |> Option.map DUuid)
      "started", DDateTime(LibExecution.DarkDateTime.fromDateTime p.started)
      "slices", DInt64 p.slices ]

let private summaryType () =
  KTCustomType(typ PackageRefs.Type.Stdlib.Exec.summary, [])

let private detailToDT (p : Scheduler.ProcessSummary) : Dval =
  let frames = p.frames |> List.map executionPointToDT
  recordOf
    PackageRefs.Type.Stdlib.Exec.detail
    [ "summary", summaryToDT p
      "frames",
      DList(
        ValueType.Known(
          KTCustomType(typ PackageRefs.Type.Stdlib.Exec.executionPoint, [])
        ),
        frames
      ) ]

/// The scheduler this call runs under, if any. Without one (a plain `execute`) the table is
/// empty rather than an error: there are no processes to list.
let private snapshot () : List<Scheduler.ProcessSummary> =
  match Scheduler.Scheduler.Current with
  | Some s -> s.Snapshot() |> List.sortBy (fun p -> p.started)
  | None -> []


let fns () : List<BuiltInFn> =
  [ { name = fn "execList" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TList(TCustomType(NR.ok (typ PackageRefs.Type.Stdlib.Exec.summary), []))
      description =
        "Every process the scheduler in this instance knows, oldest first, finished ones "
        + "included. Empty when nothing is running under a scheduler."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          let rows = snapshot () |> List.map summaryToDT
          DList(ValueType.Known(summaryType ()), rows) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ LibExecution.Effects.Effect.TraceRead ]
      deprecated = NotDeprecated }


    { name = fn "execInspect" 0
      typeParams = []
      parameters = [ Param.make "id" TUuid "" ]
      returnType =
        TypeReference.option (
          TCustomType(NR.ok (typ PackageRefs.Type.Stdlib.Exec.detail), [])
        )
      description =
        "One process with its call stack (outermost first), or None for an id nobody has."
      fn =
        (function
        | _, _, _, [| DUuid id |] ->
          let found = snapshot () |> List.tryFind (fun p -> p.id = id)
          let detailType = KTCustomType(typ PackageRefs.Type.Stdlib.Exec.detail, [])
          Dval.option detailType (found |> Option.map detailToDT) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ LibExecution.Effects.Effect.TraceRead ]
      deprecated = NotDeprecated }


    { name = fn "execKill" 0
      typeParams = []
      parameters = [ Param.make "id" TUuid "" ]
      returnType = TBool
      description =
        "Ask a process to stop; it fails with 'stopped by ps kill' at its next turn. False for an id "
        + "nobody has."
      fn =
        (function
        | _, _, _, [| DUuid id |] ->
          match Scheduler.Scheduler.Current with
          | Some s -> DBool(s.Kill id) |> Ply
          | None -> DBool false |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ LibExecution.Effects.Effect.TraceWrite ]
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
