/// A run as a durable thing (`docs/processes.md`, "Executions"): what was run, the trace that is
/// its log of effectful calls, where it stands, and, for a fork, where it branched from. The rows
/// behind `dark exec list/show/resume/fork`.
///
/// Resume and fork are record/replay: a resumed run re-runs the same input with a tracer that
/// answers every effectful call from the log by `(process, ordinal)` instead of performing it, and
/// goes live when the log runs out (`Tracing.createReplayTracer`). The log is thin because of the
/// classic rule: only calls with effects are in it. Nothing about frames is saved; the run is
/// recomputed, which is also what makes "replay after a package edit" run the new pure code
/// against the old I/O.
module LibDB.Executions

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Fumble
open LibDB.Sqlite

module RT = LibExecution.RuntimeTypes
module AT = LibExecution.AnalysisTypes
module BinarySer = LibSerialization.Binary.Serialization

type Status =
  | Running
  | Done
  | Failed
  | Suspended

module Status =
  let name (s : Status) : string =
    match s with
    | Running -> "running"
    | Done -> "done"
    | Failed -> "failed"
    | Suspended -> "suspended"

  let parse (s : string) : Status =
    match s with
    | "running" -> Running
    | "done" -> Done
    | "failed" -> Failed
    | "suspended" -> Suspended
    | other -> Exception.raiseInternal "unknown execution status" [ "status", other ]

type Execution =
  {
    id : System.Guid
    /// `eval`, or `run <file>`: the same description the trace row carries.
    handlerDesc : string
    inputName : string
    /// The expression or the script's source: what `resume` runs again.
    input : RT.Dval
    traceId : AT.TraceID.T
    status : Status
    /// The execution this was forked from, and the `seq` it branched at.
    parent : Option<System.Guid * int64>
    created : string
    updated : string
  }


/// Fractional seconds, so two rows made within a second still order.
let private now () : string =
  NodaTime.Text.InstantPattern.ExtendedIso.Format(NodaTime.Instant.now ())

let private readRow (read : RowReader) : Execution =
  { id = System.Guid.Parse(read.string "id")
    handlerDesc = read.string "handler_desc"
    inputName = read.string "input_name"
    input =
      BinarySer.RT.Dval.deserialize
        "executions.input_value"
        (read.bytes "input_value")
    traceId = AT.TraceID.fromUUID (System.Guid.Parse(read.string "trace_id"))
    status = Status.parse (read.string "status")
    parent =
      match read.uuidOrNone "parent_id", read.int64OrNone "parent_ord" with
      | Some p, Some ord -> Some(p, ord)
      | _ -> None
    created = read.string "created"
    updated = read.string "updated" }

let private columns =
  "id, handler_desc, input_name, input_value, trace_id, status, parent_id, parent_ord, created, updated"

/// Record a run that has started.
let create
  (id : System.Guid)
  (handlerDesc : string)
  (inputName : string)
  (input : RT.Dval)
  (traceId : AT.TraceID.T)
  (parent : Option<System.Guid * int64>)
  : unit =
  let stamp = now ()
  Sql.query
    "INSERT INTO executions
      (id, handler_desc, input_name, input_value, trace_id, status, parent_id, parent_ord, created, updated)
     VALUES
      (@id, @desc, @inputName, @input, @traceId, @status, @parentId, @parentOrd, @created, @updated)"
  |> Sql.parameters
    [ "id", Sql.uuid id
      "desc", Sql.string handlerDesc
      "inputName", Sql.string inputName
      "input", Sql.bytes (BinarySer.RT.Dval.serialize "executions.input_value" input)
      "traceId", Sql.string (string traceId)
      "status", Sql.string (Status.name Running)
      "parentId", (parent |> Option.map fst |> Sql.uuidOrNone)
      "parentOrd",
      (match parent with
       | Some(_, ord) -> Sql.int64 ord
       | None -> Sql.dbnull)
      "created", Sql.string stamp
      "updated", Sql.string stamp ]
  |> Sql.executeStatementSync

let setStatus (id : System.Guid) (status : Status) : unit =
  Sql.query
    "UPDATE executions SET status = @status, updated = @updated WHERE id = @id"
  |> Sql.parameters
    [ "id", Sql.uuid id
      "status", Sql.string (Status.name status)
      "updated", Sql.string (now ()) ]
  |> Sql.executeStatementSync

let get (id : System.Guid) : Task<Option<Execution>> =
  Sql.query $"SELECT {columns} FROM executions WHERE id = @id"
  |> Sql.parameters [ "id", Sql.uuid id ]
  |> Sql.executeRowOptionAsync readRow

/// The most recent `limit`, newest first.
let list (limit : int) : Task<List<Execution>> =
  Sql.query
    $"SELECT {columns} FROM executions ORDER BY created DESC, rowid DESC LIMIT @limit"
  |> Sql.parameters [ "limit", Sql.int limit ]
  |> Sql.executeAsync readRow

/// The effectful calls a trace recorded, as `(process, ordinal, result)`, in completion order.
/// What a replay tracer answers from.
let log (traceId : AT.TraceID.T) : Task<List<System.Guid * int64 * RT.Dval>> =
  task {
    let! rows =
      Sql.query
        "SELECT process_id, ord, result FROM trace_fn_calls
         WHERE trace_id = @t AND ord >= 0 ORDER BY seq"
      |> Sql.parameters [ "t", Sql.string (string traceId) ]
      |> Sql.executeAsync (fun read ->
        read.string "process_id", read.int64 "ord", read.bytes "result")
    return
      rows
      |> List.choose (fun (pid, ord, bytes) ->
        // '' is a run nobody scheduled (a plain `execute`): its process is `Guid.Empty`.
        let g =
          match System.Guid.TryParse pid with
          | true, g -> g
          | _ -> System.Guid.Empty
        try
          Some(g, ord, BinarySer.RT.Dval.deserialize "trace_fn_calls.result" bytes)
        with _ ->
          None)
  }

/// A new execution branched from `id`: the same input, a new trace holding the parent's log up to
/// `at` (a `seq`; the whole log when `None`), status suspended, so `resume` picks it up and it
/// diverges from there. The parent's trace row is copied too, so the new trace lists like any.
let fork
  (id : System.Guid)
  (at : Option<int64>)
  : Task<Result<System.Guid, string>> =
  task {
    match! get id with
    | None -> return Error "no execution has this id"
    | Some parent ->
      let childId = System.Guid.NewGuid()
      let childTrace = AT.TraceID.create ()
      let parentTrace = string parent.traceId
      let childTraceStr = string childTrace
      let cutoff = at |> Option.defaultValue System.Int64.MaxValue
      Sql.executeTransactionSync
        [ "INSERT INTO traces (id, root_tlid, handler_desc, timestamp, input_name, input_value, account_id)
           SELECT @child, root_tlid, handler_desc, @stamp, input_name, input_value, account_id
           FROM traces WHERE id = @parent",
          [ [ "child", Sql.string childTraceStr
              "parent", Sql.string parentTrace
              "stamp", Sql.string (now ()) ] ]
          "INSERT INTO trace_fn_calls
            (trace_id, call_id, parent_call_id, kind, fn_hash, lambda_expr_id, args, result,
             duration_ms, process_id, seq, ord)
           SELECT @child, call_id, parent_call_id, kind, fn_hash, lambda_expr_id, args, result,
                  duration_ms, process_id, seq, ord
           FROM trace_fn_calls WHERE trace_id = @parent AND seq < @cutoff",
          [ [ "child", Sql.string childTraceStr
              "parent", Sql.string parentTrace
              "cutoff", Sql.int64 cutoff ] ] ]
      |> ignore<List<int>>
      create
        childId
        parent.handlerDesc
        parent.inputName
        parent.input
        childTrace
        (Some(id, cutoff))
      setStatus childId Suspended
      return Ok childId
  }


/// The run in the foreground of this OS process, so Ctrl-C can suspend it: store what its
/// tracer has so far and mark it, then leave. Set by the CLI host around a traced run.
module Foreground =
  type T =
    {
      id : System.Guid
      /// Write the trace as it stands now.
      flush : unit -> Task<unit>
    }

  let mutable private current : Option<T> = None
  let private sync = obj ()

  let set (t : T) : unit = lock sync (fun () -> current <- Some t)

  /// The foreground run's id, if one is registered.
  let currentId () : Option<System.Guid> =
    lock sync (fun () -> current |> Option.map (fun t -> t.id))

  /// Unregister `id` at the end of its run. False when it was not registered any more: a
  /// suspend took it, and the run's own ending must then leave the row and the stored log as
  /// the suspend left them (the CLI has exited by then; a test's run goes on).
  let clear (id : System.Guid) : bool =
    lock sync (fun () ->
      match current with
      | Some t when t.id = id ->
        current <- None
        true
      | _ -> false)

  /// Suspend the foreground run, if there is one: its log so far is stored and it is marked
  /// suspended, so `dark exec resume <id>` can take it from there. Answers the id.
  let suspend () : Task<Option<System.Guid>> =
    task {
      match lock sync (fun () -> current) with
      | None -> return None
      | Some t ->
        do! t.flush ()
        setStatus t.id Suspended
        lock sync (fun () -> current <- None)
        return Some t.id
    }


/// A resume armed for the next run the CLI host starts: `dark exec resume <id>` arms it, then runs
/// the execution's input through the ordinary `eval` or `run` path, and the host's script runner
/// takes it in place of a fresh tracer (`Builtins.CliHost.Libs.Cli.execute`). One shot: taken by
/// the next run, whichever it is, so the CLI arms and runs back to back.
module Replay =
  type T = { execution : Execution; log : List<System.Guid * int64 * RT.Dval> }

  let mutable private armed : Option<T> = None
  let private sync = obj ()

  /// Arm a resume of `id`. False when no execution has the id.
  let arm (id : System.Guid) : Task<bool> =
    task {
      match! get id with
      | None -> return false
      | Some execution ->
        let! log = log execution.traceId
        lock sync (fun () -> armed <- Some { execution = execution; log = log })
        return true
    }

  /// The armed resume, if any, disarming it.
  let take () : Option<T> =
    lock sync (fun () ->
      let t = armed
      armed <- None
      t)
