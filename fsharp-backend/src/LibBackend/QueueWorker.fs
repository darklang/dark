module LibBackend.QueueWorker

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth
open Db

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module EQ = EventQueue
module TI = TraceInputs
module Execution = LibExecution.Execution

open LibService.Telemetry

type Activity = System.Diagnostics.Activity

let dequeueAndProcess (executionID : id) : Task<Result<Option<RT.Dval>, exn>> =
  use root = Span.root "dequeue_and_process"
  root.AddTag("meta.process_id", toString executionID) |> ignore<Activity>

  Sql.withTransaction
    (fun () ->
      task {
        let! event =
          try
            EQ.dequeue root |> Task.map Ok
          with
          | e ->
            // exception occurred while dequeuing, no item to put back
            Span.addEvent "Exception while dequeuing" root
            Task.FromResult(Error e)

        match event with
        | Ok (None) ->
          root.AddTag("event_queue.no_events", true) |> ignore<Activity>
          return Ok None
        | Ok (Some event) ->
          let! canvas =
            task {
              // Span creation might belong inside
              // Canvas.loadForEvent, but then so would the
              // error handling ... this may want a refactor
              use span = Span.child "Canvas.load_for_event_from_cache" root

              try
                let! c = Canvas.loadForEvent event

                let c =
                  c |> Result.mapError (String.concat ", ") |> Result.unwrapUnsafe

                Span.addTagBool' "load_event_succeeded" true span
                return Ok c
              with
              | e ->
                // exception occurred when processing an item, so put it back as an error
                do! EQ.putBack root event EQ.Err
                // CLEANUP why have these attributes a different name
                Span.addTagBool' "event.load_success" false span
                return Error e
            }

          match canvas with
          | Ok c ->
            let host = c.meta.name
            let traceID = System.Guid.NewGuid()
            let canvasID = c.meta.id
            let desc = EQ.toEventDesc event

            root
            |> Span.addTag "canvas" (toString host)
            |> Span.addTagUUID "trace_id" traceID
            |> Span.addTagUUID "canvas_id" canvasID
            |> Span.addTag "module" event.space
            |> Span.addTag "handler_name" event.name
            |> Span.addTag "method" event.modifier
            |> Span.addTagInt' "retries" event.retries

            try
              let! eventTimestamp = TI.storeEvent canvasID traceID desc event.value

              let h =
                c.handlers
                |> Map.values
                |> List.filter (fun h -> Some desc = h.spec.toEventDesc ())
                |> List.head

              root
              |> Span.addTag "host" (toString host)
              |> Span.addTag "event" (toString desc)
              |> Span.addTagID' "event_id" event.id

              match h with
              | None ->
                // If an event gets put in the queue and there's no handler for
                // it, they're probably emiting to a handler they haven't created
                // yet. This creates a number of problems. Firstly, the event
                // will sit in the queue and rattle around forever, which is bad
                // operationally. However, it will also constantly run while the
                // user is editing code, until something finally works. This is
                // annoying, but also unnecessary - so long as they have the
                // trace they can use it to build. So just drop it immediately.
                Span.addTagFloat' "delay" event.delay root
                let space, name, modifier = desc
                let f404 = (space, name, modifier, eventTimestamp, traceID)
                Pusher.pushNew404 executionID canvasID f404
                do! EQ.putBack root event EQ.Missing
                return Ok None
              | Some h ->
                let! (state, touchedTLIDs) =
                  RealExecution.createState traceID h.tlid (Canvas.toProgram c)

                // FSTODO: add parent span to state
                // ~parent:(Some parent)
                Span.addTagID' "handler_id" h.tlid root
                let symtable = Map.ofList [ ("event", event.value) ]

                let! result =
                  Execution.executeHandler state symtable (h.ast.toRuntimeType ())

                Pusher.pushNewTraceID
                  executionID
                  canvasID
                  traceID
                  (h.tlid :: HashSet.toList touchedTLIDs)

                let resultType =
                  match result with
                  | RT.DResult (Ok _) -> "ResOk"
                  | RT.DResult (Error _) -> "ResError"
                  | RT.DOption (Some _) -> "OptJust"
                  | RT.DOption None -> "OptNothing"
                  | _ -> (RT.Dval.toType result).toOldString ()

                root
                |> Span.addTag "result_tipe" resultType
                |> Span.addTagBool' "event.execution_success" true

                do! EQ.finish root event
                return Ok(Some result)
            with
            | e ->
              // exception occurred when processing an item, so put it back as an error
              Span.addTagBool' "event.execution_success" false root

              try
                do! EQ.putBack root event EQ.Err
              with
              | e -> Span.addTag' "error.msg" (toString e) root

              return Error e
          | Error e -> return Error e
        | Error e -> return Error e
      })

let run (executionID : id) : Task<Result<Option<RT.Dval>, exn>> =
  if String.toLowercase LibService.Config.postgresSettings.dbname = "prodclone" then
    use (span : Span.T) = Span.root "Pointing at prodclone; will not dequeue"
    Task.FromResult(Ok None)
  else
    dequeueAndProcess executionID
