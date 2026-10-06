/// Processes, as Dark sees them: the table `dark ps` renders, and spawn, await and cancel over
/// the scheduler.
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
module HostTypes = LibExecution.HostTypes


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
  | HE.EventSpec.StdinLine -> case "StdinLine" []
  | HE.EventSpec.StdinBytes n -> case "StdinBytes" [ DInt64(int64 n) ]

let private parkedToDT (parked : Scheduler.Parked) : Dval =
  let case = enumOf PackageRefs.Type.Stdlib.Exec.parkedOn
  match parked with
  | Scheduler.OnHost op -> case "Host" [ DString(HostTypes.describeOperation op) ]
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
      "instructionsTaken", DInt64 p.instructionsTaken
      "allocated", DInt64 p.allocated
      "inflight", DInt64(int64 p.inflight) ]

let summaryType () = KTCustomType(typ PackageRefs.Type.Stdlib.Exec.summary, [])

let private machineProcessToDT (e : LibExecution.HostRegistry.Entry) : Dval =
  recordOf
    PackageRefs.Type.Stdlib.Exec.machineProcess
    [ "pid", DInt64(int64 e.pid)
      "title", DString e.title
      "command", DString e.command
      "branch", DString e.branch
      "started", DDateTime(LibExecution.DarkDateTime.fromDateTime e.started) ]

let private machineProcessType () =
  KTCustomType(typ PackageRefs.Type.Stdlib.Exec.machineProcess, [])

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
/// What stderr says once when `exec.policy` cannot be used for a turn: the setting, what went
/// wrong, and how to put it back.
let policyComplaint (policy : string) (what : string) : string =
  $"exec.policy: {policy} {what}; round robin for this turn. "
  + "`dark config set exec.policy Darklang.Stdlib.Exec.Policy.roundRobin` resets it."

let chooserFor
  (state : ExecutionState)
  (fn : FQFnName.FQFnName)
  : List<Scheduler.ProcessSummary> -> Option<Scheduler.ProcessId> =
  let mutable complained = false
  let complain (what : string) =
    if not complained then
      complained <- true
      System.Console.Error.WriteLine(policyComplaint (string fn) what)
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
        complain $"answered {other} rather than a process id"
        None
      | Error(rte, _) ->
        complain $"failed ({rte})"
        None
    with ex ->
      complain $"failed ({ex.Message})"
      None


/// The table of the scheduler this call runs under; a run nobody scheduled sees the shared one,
/// where its own spawns live.
let private snapshot () : List<Scheduler.ProcessSummary> =
  Scheduler.Scheduler.CurrentOrShared.Snapshot()
  |> List.sortBy (fun p -> p.started)


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

/// A handle nothing answers to: never spawned on this instance, or finished so long ago that
/// even its answer has been dropped.
let private noSuchProcess () : RuntimeError.Error =
  RuntimeError.UncaughtException(
    "no process has this handle: it was not spawned here, or it finished long enough ago "
    + "that its answer was dropped",
    []
  )

/// Tell `ps` what the calling process is about to park on.
let private parkOn (p : Scheduler.Process) : unit =
  match Scheduler.Scheduler.CurrentProcess with
  | Some me -> me.parkHint <- ValueSome(Scheduler.OnProcess p.id)
  | None -> ()

/// Wait for `p`, parking the calling process when there is one.
let private awaitProcess (vm : VMState) (p : Scheduler.Process) : Ply<Dval> =
  let task = p.completion.Task
  if task.IsCompletedSuccessfully then
    Ply(resultOf vm task.Result)
  else
    parkOn p
    uply {
      let! result = task
      return resultOf vm result
    }


let fns () : List<BuiltInFn> =
  [ { name = fn "execMachineList" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TList(
          TCustomType(NR.ok (typ PackageRefs.Type.Stdlib.Exec.machineProcess), [])
        )
      description =
        "Every Dark process on this machine that is still running, oldest first: pid, what it "
        + "runs, its command line, its branch, when it started. From the registry each CLI "
        + "writes under the rundir at startup."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          let rows = LibExecution.HostRegistry.list () |> List.map machineProcessToDT
          DList(ValueType.Known(machineProcessType ()), rows) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      // The table is not a trace; `TraceRead`/`TraceWrite` are borrowed because they are
      // on by default and a read is a read: `ps` needs no permission of its own.
      callEffects = set [ LibExecution.Effects.Effect.TraceRead ] }
    { name = fn "execList" 0
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


    { name = fn "execSpawnDetached" 0
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
        "`execSpawn`, for a process that keeps running after the one that started it has "
        + "finished."
      fn =
        (function
        | state, vm, _, [| DApplicable f |] ->
          let s = Scheduler.Scheduler.CurrentOrShared
          let parent =
            Scheduler.Scheduler.CurrentProcess |> Option.map (fun p -> p.id)
          let p = s.SpawnApply(state, f, DUnit, parent, vm.activeAccess)
          // Set by the spawner, which is the only process that could finish it meanwhile and
          // is busy here; nothing races it.
          p.detached <- true
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
          let sched = Scheduler.Scheduler.CurrentOrShared
          match sched.Find pid with
          | Some p -> awaitProcess vm p
          | None ->
            // Finished and dropped from the display table, but its answer is kept until
            // somebody takes it, which is what makes a long `List.parallelMap` work.
            match sched.TakeResult pid with
            | Some result -> Ply(resultOf vm result)
            | None -> noSuchProcess () |> raiseRTE vm.threadID
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "execAwaitWithin" 0
      typeParams = [ "a" ]
      parameters =
        [ Param.make "ms" TInt64 ""
          Param.make
            "handle"
            (TCustomType(
              NR.ok (typ PackageRefs.Type.Stdlib.Exec.handle),
              [ TVariable "a" ]
            ))
            "" ]
      returnType = TypeReference.option (TVariable "a")
      description =
        "`execAwait`, giving up after `ms` milliseconds: None then, and the process keeps "
        + "running."
      fn =
        (function
        | _, vm, _, [| DInt64 ms; handle |] ->
          let pid = pidOfHandle vm handle
          let sched = Scheduler.Scheduler.CurrentOrShared
          match sched.Find pid with
          | Some p ->
            // The inner type is the handle's `'a`, which nothing here knows: an unknown.
            let optionOf (dv : Option<Dval>) : Dval =
              let tn = Dval.optionType ()
              match dv with
              | Some dv -> DEnum(tn, tn, [ ValueType.Unknown ], "Some", [ dv ])
              | None -> DEnum(tn, tn, [ ValueType.Unknown ], "None", [])
            let some (result : ExecutionResult) = optionOf (Some(resultOf vm result))
            let task = p.completion.Task
            if task.IsCompletedSuccessfully then
              Ply(some task.Result)
            elif ms <= 0L then
              Ply(optionOf None)
            else
              parkOn p
              uply {
                match!
                  Scheduler.Scheduler.CurrentOrShared.AwaitWithin(p, int ms)
                with
                | Some result -> return some result
                | None -> return optionOf None
              }
          | None ->
            let optionOf (dv : Option<Dval>) : Dval =
              let tn = Dval.optionType ()
              match dv with
              | Some dv -> DEnum(tn, tn, [ ValueType.Unknown ], "Some", [ dv ])
              | None -> DEnum(tn, tn, [ ValueType.Unknown ], "None", [])
            match sched.TakeResult pid with
            | Some result -> Ply(optionOf (Some(resultOf vm result)))
            | None -> noSuchProcess () |> raiseRTE vm.threadID
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
              | None -> noSuchProcess () |> raiseRTE vm.threadID)
          // `select` over handles whose processes have all been forgotten is the one case the
          // kept answers cannot serve, since it needs the tasks to race; it says so above.
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


    { name = fn "execCancel" 0
      typeParams = [ "a" ]
      parameters =
        [ Param.make
            "handle"
            (TCustomType(
              NR.ok (typ PackageRefs.Type.Stdlib.Exec.handle),
              [ TVariable "a" ]
            ))
            "" ]
      returnType = TBool
      description =
        "Ask the process behind `handle` to stop, and its undetached children; it fails with "
        + "'cancelled' at its next turn, after what it is doing on the host completes. False for "
        + "a handle nobody has."
      fn =
        (function
        | _, vm, _, [| handle |] ->
          let pid = pidOfHandle vm handle
          DBool(Scheduler.Scheduler.CurrentOrShared.Cancel pid) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ LibExecution.Effects.Effect.TraceWrite ]
      deprecated = NotDeprecated }


    { name = fn "execKill" 0
      typeParams = []
      parameters = [ Param.make "id" TUuid "" ]
      returnType = TBool
      description =
        "`dark ps kill`: stop a process without waiting for what it is doing on the host; it fails "
        + "with 'killed from ps' at its next turn, which a parked one is given now. Its "
        + "undetached children go the same way. False for an id nobody has."
      fn =
        (function
        | _, _, _, [| DUuid id |] ->
          DBool(Scheduler.Scheduler.CurrentOrShared.Kill id) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ LibExecution.Effects.Effect.TraceWrite ]
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
