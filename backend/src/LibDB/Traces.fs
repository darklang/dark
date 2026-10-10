/// A trace is a run: what was run, what it did to the world, where it stands, and, for a fork,
/// where it branched from. The rows behind `dark traces list/show/resume/fork`.
///
/// This file owns the ROW. `Tracing.fs` owns the RECORDER that fills it in, and the retention
/// that drops it. A run that is still going has a row with `status = running`; the recorder
/// upserts the input and the calls onto that same row when the run ends.
///
/// Resume and fork are record/replay: a resumed run re-runs the same input with a tracer that
/// answers every effectful call from the log by `(process, ordinal)` instead of performing it, and
/// goes live when the log runs out (`Tracing.createReplayTracer`). The log is thin because of the
/// classic rule: only calls with effects are in it. Nothing about frames is saved; the run is
/// recomputed, which is also what makes "replay after a package edit" run the new pure code
/// against the old I/O.
module LibDB.Traces

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
    | other -> Exception.raiseInternal "unknown run status" [ "status", other ]

type Trace =
  {
    id : System.Guid
    /// `eval`, `run <file>`, or `GET /path`: what was run.
    entry : string
    inputName : string
    /// The expression, the script's source, or the request: what `resume` runs again.
    input : RT.Dval
    status : Status
    /// The trace this was forked from, and the `seq` it branched at: the parent's log below
    /// that position is the child's too.
    parent : Option<System.Guid * int64>
    /// Retention never drops a pinned trace, whatever the caps say.
    pinned : bool
    /// What the run answered, once it has. `None` while it is still going, for a run that
    /// failed, and for a run recorded before this column existed.
    result : Option<RT.Dval>
    /// Wall clock for the whole run, from the recorder's own stopwatch. 0 for a run still going,
    /// and for one recorded before this column existed. NOT `updated - timestamp`: those are the
    /// same instant for a served request, and on a resumed run they span the suspension.
    durationMs : int64
    /// For a served request: the handler that served it. What a view applies to the
    /// recorded request, since a request's input is not source it can re-run.
    entryHash : Option<string>
    created : string
    updated : string
  }


/// Fractional seconds, so two rows made within a second still order.
let private now () : string =
  NodaTime.Text.InstantPattern.ExtendedIso.Format(NodaTime.Instant.now ())

let private readRow (read : RowReader) : Trace =
  { id = System.Guid.Parse(read.string "id")
    entry = read.string "handler_desc"
    inputName = read.string "input_name"
    input =
      BinarySer.RT.Dval.deserialize "traces.input_value" (read.bytes "input_value")
    status = Status.parse (read.string "status")
    parent =
      match read.uuidOrNone "parent_id", read.int64OrNone "parent_seq" with
      | Some p, Some seq -> Some(p, seq)
      | _ -> None
    pinned = read.int "pinned" = 1
    result =
      read.bytesOrNone "result_value"
      |> Option.map (BinarySer.RT.Dval.deserialize "traces.result_value")
    durationMs = read.int64 "duration_ms"
    entryHash = read.stringOrNone "entry_hash"
    created = read.string "timestamp"
    updated = read.string "updated" }

let private columns =
  "id, handler_desc, input_name, input_value, status, parent_id, parent_seq, pinned, "
  + "timestamp, updated, entry_hash, result_value, duration_ms"

/// A run that has started, before the recorder has anything to say about it. `root_tlid` is 0
/// on every path that reaches here; the recorder's upsert fills in the rest.
let private insertSql =
  "INSERT INTO traces
    (id, root_tlid, handler_desc, timestamp, input_name, input_value, account_id,
     status, parent_id, parent_seq, pinned, updated)
   VALUES
    (@id, 0, @desc, @created, @inputName, @input, NULL,
     @status, @parentId, @parentSeq, 0, @updated)"

let private insertParams
  (id : System.Guid)
  (entry : string)
  (inputName : string)
  (input : byte[])
  (status : Status)
  (parent : Option<System.Guid * int64>)
  (created : string)
  (updated : string)
  =
  [ "id", Sql.uuid id
    "desc", Sql.string entry
    "inputName", Sql.string inputName
    "input", Sql.bytes input
    "status", Sql.string (Status.name status)
    "parentId", (parent |> Option.map fst |> Sql.uuidOrNone)
    "parentSeq",
    (match parent with
     | Some(_, seq) -> Sql.int64 seq
     | None -> Sql.dbnull)
    "created", Sql.string created
    "updated", Sql.string updated ]

/// Record a run that has started. The id IS the trace id: the recorder fills the same row in
/// when the run ends.
let create
  (id : System.Guid)
  (entry : string)
  (inputName : string)
  (input : RT.Dval)
  (status : Status)
  (parent : Option<System.Guid * int64>)
  : unit =
  let stamp = now ()
  let bytes = BinarySer.RT.Dval.serialize "traces.input_value" input
  Sql.query insertSql
  |> Sql.parameters (insertParams id entry inputName bytes status parent stamp stamp)
  |> Sql.executeStatementSync

let setStatus (id : System.Guid) (status : Status) : unit =
  Sql.query "UPDATE traces SET status = @status, updated = @updated WHERE id = @id"
  |> Sql.parameters
    [ "id", Sql.uuid id
      "status", Sql.string (Status.name status)
      "updated", Sql.string (now ()) ]
  |> Sql.executeStatementSync

let get (id : System.Guid) : Task<Option<Trace>> =
  Sql.query $"SELECT {columns} FROM traces WHERE id = @id"
  |> Sql.parameters [ "id", Sql.uuid id ]
  |> Sql.executeRowOptionAsync readRow

/// The most recent `limit`, newest first.
let list (limit : int) : Task<List<Trace>> =
  Sql.query
    $"SELECT {columns} FROM traces ORDER BY timestamp DESC, rowid DESC LIMIT @limit"
  |> Sql.parameters [ "limit", Sql.int limit ]
  |> Sql.executeAsync readRow

/// Which handler served a request, recorded once the response is known.
/// What the run answered. Written once, when the run ends well; a run that failed or was
/// suspended leaves it alone, because there is no answer to record.
/// The run's answer, counted in what the trace weighs (`bytes`): replacing an earlier answer
/// takes the old one's length back out, so a run that answers twice is not counted twice.
let setResult (id : System.Guid) (result : RT.Dval) : unit =
  let bytes = BinarySer.RT.Dval.serialize "traces.result_value" result
  Sql.query
    "UPDATE traces SET
       bytes = bytes - COALESCE(LENGTH(result_value), 0) + LENGTH(@result),
       result_value = @result, updated = @updated
     WHERE id = @id"
  |> Sql.parameters
    [ "id", Sql.uuid id; "result", Sql.bytes bytes; "updated", Sql.string (now ()) ]
  |> Sql.executeStatementSync

let setEntryHash (id : System.Guid) (hash : string) : unit =
  Sql.query "UPDATE traces SET entry_hash = @hash WHERE id = @id"
  |> Sql.parameters [ "id", Sql.uuid id; "hash", Sql.string hash ]
  |> Sql.executeStatementSync

/// When the log a run replays was recorded: its own start, or for a fork, the start of the run
/// at the root of its lineage, since a fork's log is a copy of that run's.
let recordedAt (e : Trace) : Task<string> =
  task {
    let mutable current = e
    let mutable more = true
    while more do
      match current.parent with
      | Some(pid, _) ->
        match! get pid with
        | Some p -> current <- p
        | None -> more <- false
      | None -> more <- false
    return current.created
  }

/// The effectful calls a trace recorded, as `(process, ordinal, answer, what was called)`, in
/// completion order. What a replay tracer answers from, and what it checks each call against
/// before answering it. A row whose builtin is in `Tracing.Redact.performAgain` (an environment
/// read, whose result is the secret; a spawn, whose result is a handle to a process that no
/// longer exists) answers `PerformOnce`, so the replay performs that one call again for real
/// and goes on replaying.
let log
  (id : System.Guid)
  : Task<List<System.Guid * int64 * RT.Tracing.ReplayStep * Tracing.RecordedCall>> =
  task {
    let! rows =
      Sql.query
        "SELECT c.process_id, c.ord, c.fn_hash, c.args, r.bytes AS result
         FROM trace_fn_calls c
         JOIN trace_blobs r ON r.trace_id = c.trace_id AND r.hash = c.result
         WHERE c.trace_id = @t AND c.ord >= 0 ORDER BY c.seq"
      |> Sql.parameters [ "t", Sql.uuid id ]
      |> Sql.executeAsync (fun read ->
        read.string "process_id",
        read.int64 "ord",
        (read.stringOrNone "fn_hash" |> Option.defaultValue ""),
        read.bytes "args",
        read.bytes "result")
    return
      rows
      |> List.choose (fun (pid, ord, builtin, argBytes, bytes) ->
        // '' is a run nobody scheduled (a plain `execute`): its process is `Guid.Empty`.
        let g =
          match System.Guid.TryParse pid with
          | true, g -> g
          | _ -> System.Guid.Empty
        // Arguments that cannot be read back still leave the builtin's name to check; the
        // check is weaker there and nothing is refused for it.
        let args =
          try
            match BinarySer.RT.Dval.deserialize "trace_fn_calls.args" argBytes with
            | RT.DList(_, items) -> Some items
            | _ -> None
          with _ ->
            None
        let call : Tracing.RecordedCall = { builtin = builtin; args = args }
        if Set.contains builtin Tracing.Redact.performAgain then
          Some(g, ord, RT.Tracing.ReplayStep.PerformOnce, call)
        else
          try
            Some(
              g,
              ord,
              RT.Tracing.ReplayStep.Serve(
                BinarySer.RT.Dval.deserialize "trace_fn_calls.result" bytes
              ),
              call
            )
          with e ->
            // Dropping the row quietly is what makes this dangerous: the ordinal then has no
            // answer, which ends the replay for that process and takes the rest of the log
            // with it, so the run goes live and performs the remaining effects for real. Say
            // it once, with the position, rather than leaving that a mystery.
            NonBlockingConsole.writeErrLine (
              $"[traces] step {ord} of this run could not be read back ({e.Message}); "
              + "the resume goes live from there"
            )
            None)
  }

/// A new run branched from `id`: the same input, holding the parent's log up to `at` (a `seq`;
/// the whole log when `None`), suspended, so `resume` picks it up and it diverges from there.
/// One table now, so the child's row and the child's log are the same copy.
let fork
  (id : System.Guid)
  (at : Option<int64>)
  : Task<Result<System.Guid, string>> =
  task {
    match! get id with
    | None -> return Error "no run has this id"
    | Some _parent ->
      let childId = System.Guid.NewGuid()
      // The whole log is "past its last row", stored as a position so `show` can say it.
      let! cutoff =
        match at with
        | Some seq -> Task.FromResult seq
        | None ->
          Sql.query
            "SELECT COALESCE(MAX(seq), -1) + 1 AS n FROM trace_fn_calls WHERE trace_id = @t"
          |> Sql.parameters [ "t", Sql.uuid id ]
          |> Sql.executeRowAsync (fun read -> read.int64 "n")
      // `entry_hash` comes across too: for a served request it is the only thing that says
      // what to replay against, so a fork without it is a run nothing can look at.
      let counts =
        Sql.executeTransactionSync
          [ "INSERT INTO traces
              (id, root_tlid, handler_desc, timestamp, input_name, input_value, account_id,
               status, parent_id, parent_seq, pinned, updated, entry_hash, bytes)
             SELECT @child, root_tlid, handler_desc, @stamp, input_name, input_value,
                    account_id, 'suspended', @parent, @cutoff, 0, @stamp, entry_hash, bytes
             FROM traces WHERE id = @parent",
            [ [ "child", Sql.uuid childId
                "parent", Sql.uuid id
                "cutoff", Sql.int64 cutoff
                "stamp", Sql.string (now ()) ] ]
            "INSERT INTO trace_fn_calls
              (trace_id, call_id, parent_call_id, kind, fn_hash, lambda_expr_id, args, result,
               duration_ms, process_id, seq, ord)
             SELECT @child, call_id, parent_call_id, kind, fn_hash, lambda_expr_id, args,
                    result, duration_ms, process_id, seq, ord
             FROM trace_fn_calls WHERE trace_id = @parent AND seq < @cutoff",
            [ [ "child", Sql.uuid childId
                "parent", Sql.uuid id
                "cutoff", Sql.int64 cutoff ] ]
            // The child answers `traces calls <fn>` for the same functions the parent did, and
            // carries the HASHES too. Without `fn_hash` the column takes its `DEFAULT ''`, and
            // `resume` compares those against the store to decide what has been edited since, so
            // a forked run reported every function it went through as edited, immediately, with
            // nothing having changed. `trace_fn_calls` above copies its `fn_hash` for the same
            // reason.
            "INSERT OR IGNORE INTO trace_fns (trace_id, fn_name, fn_hash)
             SELECT @child, fn_name, fn_hash FROM trace_fns WHERE trace_id = @parent",
            [ [ "child", Sql.uuid childId; "parent", Sql.uuid id ] ]
            // A captured body belongs to the trace that holds it, so the child takes its own
            // copy: retention removing the parent must not take the child's request with it.
            "INSERT OR IGNORE INTO trace_blobs (trace_id, hash, bytes)
             SELECT @child, hash, bytes FROM trace_blobs WHERE trace_id = @parent",
            [ [ "child", Sql.uuid childId; "parent", Sql.uuid id ] ] ]
      // `INSERT ... SELECT` inserts nothing when the parent has gone -- retention runs every
      // ten seconds -- and reporting a child that does not exist sends the person to a resume
      // that cannot find it.
      match counts with
      | rowsInserted :: _ when rowsInserted = 0 ->
        return Error "the run was deleted while it was being forked"
      | _ -> return Ok childId
  }


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
  /// suspended, so `dark traces resume <id>` can take it from there. Answers the id.
  let suspend () : Task<Option<System.Guid>> =
    task {
      match lock sync (fun () -> current) with
      | None -> return None
      | Some t ->
        // Marked before the store, so retention sees a suspended run's trace as one it must
        // keep even on the pass the store itself triggers.
        setStatus t.id Suspended
        do! t.flush ()
        lock sync (fun () -> current <- None)
        return Some t.id
    }


/// The recorded log of a run, as a view answers from: the name of each effectful call, the
/// arguments as they were stored, and the result. Keyed by name and arguments rather than by
/// ordinal (`Tracing.createViewTracer` says why), so this is the whole of what a view needs.
///
/// No arming and no shared slot: a view is one call that loads this, runs, and hands back
/// what it collected. Two of them at once cannot take each other's log, which matters when the
/// thing driving `dark` is an agent rather than a person at a prompt.
let viewLog (id : System.Guid) : Task<List<string * byte[] * RT.Dval>> =
  task {
    let! rows =
      Sql.query
        "SELECT c.fn_hash, c.args, r.bytes AS result
         FROM trace_fn_calls c
         JOIN trace_blobs r ON r.trace_id = c.trace_id AND r.hash = c.result
         WHERE c.trace_id = @t AND c.ord >= 0 ORDER BY c.seq"
      |> Sql.parameters [ "t", Sql.uuid id ]
      |> Sql.executeAsync (fun read ->
        (read.stringOrNone "fn_hash" |> Option.defaultValue ""),
        read.bytes "args",
        read.bytes "result")
    return
      rows
      |> List.choose (fun (name, args, resultBytes) ->
        try
          Some(
            name,
            args,
            BinarySer.RT.Dval.deserialize "trace_fn_calls.result" resultBytes
          )
        with _ ->
          None)
  }


/// A resume armed for the next run the CLI host starts. `dark traces resume <id>` arms it, then
/// runs that run's input through the ordinary `eval` or `run` path, and the host's script runner
/// takes it in place of a fresh tracer (`Builtins.CliHost.Libs.Cli.execute`). One shot: taken by
/// the next run, whichever it is, so the CLI arms and runs back to back.
module Replay =
  type T =
    {
      run : Trace
      log : List<System.Guid * int64 * RT.Tracing.ReplayStep * Tracing.RecordedCall>
      /// When the log was recorded (`recordedAt`), for the stale-file warning.
      recordedAt : string
    }

  let mutable private armed : Option<T> = None
  let private sync = obj ()

  /// Arm a resume of `id`. False when no run has the id.
  let arm (id : System.Guid) : Task<bool> =
    task {
      match! get id with
      | None -> return false
      | Some run ->
        let! log = log run.id
        let! recorded = recordedAt run
        lock sync (fun () ->
          armed <- Some { run = run; log = log; recordedAt = recorded })
        return true
    }

  /// The armed resume, if any, disarming it.
  let take () : Option<T> =
    lock sync (fun () ->
      let t = armed
      armed <- None
      t)
