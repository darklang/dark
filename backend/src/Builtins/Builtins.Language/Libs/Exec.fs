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
  | Scheduler.OnProcess pid -> case "Process" [ DUuid pid ]

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

let summaryToDT (p : Scheduler.ProcessSummary) : Dval =
  recordOf
    PackageRefs.Type.Stdlib.Exec.summary
    [ "id", DUuid p.id
      "entry", entryToDT p.entry
      "status", statusToDT p.status
      "parent", Dval.option KTUuid (p.parent |> Option.map DUuid)
      "started", DDateTime(LibExecution.DarkDateTime.fromDateTime p.started)
      "slices", DInt64 p.slices
      "inflight", DInt64(int64 p.inflight) ]

let summaryType () = KTCustomType(typ PackageRefs.Type.Stdlib.Exec.summary, [])

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

/// A scheduling policy written in Dark, as the scheduler's `Chooser`: `fn` takes the runnable
/// processes (`List<Stdlib.Exec.Summary>`, oldest first) and answers `Some id` to step next, or
/// `None` for no preference. It runs on the scheduler's own thread, unscheduled, between slices,
/// so it should be quick and should not wait; a failure or an answer of another shape counts as
/// no preference for that turn, and the first failure is said once on stderr.
let chooserFor
  (state : ExecutionState)
  (fn : FQFnName.FQFnName)
  : List<Scheduler.ProcessSummary> -> Option<Scheduler.ProcessId> =
  let mutable complained = false
  // The policy's own calls are nobody's business: they are not in the trace of whatever runs.
  let state = { state with tracing = LibExecution.Execution.noTracing }
  fun runnable ->
    let arg =
      DList(ValueType.Known(summaryType ()), runnable |> List.map summaryToDT)
    try
      match
        (LibExecution.Execution.executeFunction state fn [] (NEList.singleton arg))
          .Result
      with
      | Ok(DEnum(_, _, _, "Some", [ DUuid id ])) -> Some id
      | Ok(DEnum(_, _, _, "None", [])) -> None
      | Ok other ->
        if not complained then
          complained <- true
          System.Console.Error.WriteLine
            $"exec.policy: the policy answered {other} rather than a process id; round robin for now"
        None
      | Error(rte, _) ->
        if not complained then
          complained <- true
          System.Console.Error.WriteLine
            $"exec.policy: the policy failed ({rte}); round robin for now"
        None
    with ex ->
      if not complained then
        complained <- true
        System.Console.Error.WriteLine
          $"exec.policy: the policy failed ({ex.Message}); round robin for now"
      None


/// The scheduler this call runs under, if any. Without one (a plain `execute`) the table is
/// empty rather than an error: there are no processes to list.
let private snapshot () : List<Scheduler.ProcessSummary> =
  match Scheduler.Scheduler.Current with
  | Some s -> s.Snapshot() |> List.sortBy (fun p -> p.started)
  | None -> []


/// `Stdlib.Exec.Handle<'a>`: what `spawn` hands back, `{ id }`.
let private handleOf (pid : Scheduler.ProcessId) : Dval =
  let tn = typ PackageRefs.Type.Stdlib.Exec.handle
  DRecord(tn, tn, [ ValueType.Unknown ], Map [ "id", DUuid pid ])

let private handleType () : KnownType =
  KTCustomType(typ PackageRefs.Type.Stdlib.Exec.handle, [ ValueType.Unknown ])

let private pidOfHandle (vm : VMState) (h : Dval) : Scheduler.ProcessId =
  match h with
  | DRecord(_, _, _, fields) ->
    match Map.tryFind "id" fields with
    | Some(DUuid pid) -> pid
    | _ -> RuntimeError.UncaughtException("not a handle", []) |> raiseRTE vm.threadID
  | _ -> RuntimeError.UncaughtException("not a handle", []) |> raiseRTE vm.threadID

/// The result a finished process gives its awaiter: its value, or its error raised again here
/// with the frames it failed in kept below the caller's.
let private resultOf (vm : VMState) (result : ExecutionResult) : Dval =
  match result with
  | Ok dv -> dv
  | Error(rte, stack) ->
    vm.nestedCallStack <- stack
    raiseRTE vm.threadID rte

/// Wait for `p`, parking the calling process when there is one.
let private awaitProcess (vm : VMState) (p : Scheduler.Process) : Ply<Dval> =
  let task = p.completion.Task
  if task.IsCompletedSuccessfully then
    Ply(resultOf vm task.Result)
  else
    match Scheduler.Scheduler.CurrentProcess with
    | Some me -> me.parkHint <- ValueSome(Scheduler.OnProcess p.id)
    | None -> ()
    uply {
      let! result = task
      return resultOf vm result
    }


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


    { name = fn "execSpawn" 0
      typeParams = [ "a" ]
      parameters =
        [ Param.makeWithArgs
            "f"
            (TFn(NEList.singleton TUnit, TVariable "a"))
            ""
            [ "unit" ] ]
      returnType =
        TCustomType(
          NR.ok (typ PackageRefs.Type.Stdlib.Exec.handle),
          [ TVariable "a" ]
        )
      description =
        "Start `f ()` as a process of its own, on another core when one is free, and hand back "
        + "the handle `await` takes. It runs under the same access as the caller had here."
      fn =
        (function
        | state, vm, _, [| DApplicable f |] ->
          let s = Scheduler.Scheduler.CurrentOrShared
          let parent =
            Scheduler.Scheduler.CurrentProcess |> Option.map (fun p -> p.id)
          let p = s.SpawnApply(state, f, DUnit, parent, vm.activeAccess)
          handleOf p.id |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ LibExecution.Effects.Effect.Concurrency ]
      deprecated = NotDeprecated }


    { name = fn "execAwait" 0
      typeParams = [ "a" ]
      parameters =
        [ Param.make
            "handle"
            (TCustomType(
              NR.ok (typ PackageRefs.Type.Stdlib.Exec.handle),
              [ TVariable "a" ]
            ))
            "" ]
      returnType = TVariable "a"
      description =
        "The value the process behind `handle` finished with, waiting for it if it has not. Its "
        + "error, if it failed, is raised here."
      fn =
        (function
        | _, vm, _, [| handle |] ->
          let pid = pidOfHandle vm handle
          match Scheduler.Scheduler.CurrentOrShared.Find pid with
          | Some p -> awaitProcess vm p
          | None ->
            RuntimeError.UncaughtException("no process has this handle", [])
            |> raiseRTE vm.threadID
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "execSelect" 0
      typeParams = [ "a" ]
      parameters =
        [ Param.make
            "handles"
            (TList(
              TCustomType(
                NR.ok (typ PackageRefs.Type.Stdlib.Exec.handle),
                [ TVariable "a" ]
              )
            ))
            "" ]
      returnType =
        TTuple(
          TCustomType(
            NR.ok (typ PackageRefs.Type.Stdlib.Exec.handle),
            [ TVariable "a" ]
          ),
          TVariable "a",
          []
        )
      description =
        "The first of `handles` to finish, with its value; waits when none has. An empty list "
        + "is an error."
      fn =
        (function
        | _, vm, _, [| DList(_, handles) |] ->
          let s = Scheduler.Scheduler.CurrentOrShared
          let procs =
            handles
            |> List.map (fun h ->
              let pid = pidOfHandle vm h
              match s.Find pid with
              | Some p -> h, p
              | None ->
                RuntimeError.UncaughtException("no process has this handle", [])
                |> raiseRTE vm.threadID)
          match procs with
          | [] ->
            RuntimeError.UncaughtException("select needs at least one handle", [])
            |> raiseRTE vm.threadID
          | _ ->
            let answer (h : Dval, p : Scheduler.Process) : Dval =
              DTuple(h, resultOf vm p.completion.Task.Result, [])
            match
              procs |> List.tryFind (fun (_, p) -> p.completion.Task.IsCompleted)
            with
            | Some done' -> Ply(answer done')
            | None ->
              uply {
                let! first =
                  System.Threading.Tasks.Task.WhenAny(
                    procs
                    |> List.map (fun (_, p) ->
                      p.completion.Task :> System.Threading.Tasks.Task)
                  )
                match
                  procs
                  |> List.tryFind (fun (_, p) ->
                    obj.ReferenceEquals(p.completion.Task, first))
                with
                | Some won -> return answer won
                | None ->
                  return
                    Exception.raiseInternal "select: the winner is not a handle" []
              }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "execKill" 0
      typeParams = []
      parameters = [ Param.make "id" TUuid "" ]
      returnType = TBool
      description =
        "Ask a process to stop; it fails with 'cancelled' at its next turn. False for an id "
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
