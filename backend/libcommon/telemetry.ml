open Core_kernel

(* If you are unfamiliar with terminology such as trace and span, you may want
 * to read some of the overview documentation at https://opentelemetry.io/docs/
 * before trying to understand this module. *)

(* An ID is used for trace and span IDs.
 * These are native (64-bit) ints for ease/speed of generating lots of random
 * ones, but will be logged as strings since that's what Honeycomb expects. *)
module ID = Int

(** gid returns a new randomly generated ID **)
let gid () : ID.t = Random.int (Int.max_value - 1) + 1

(** A Span is a segment of a program wrapped for timing and telemetry. *)
module Span = struct
  type t =
    { name : string
          (** the unique name of the span, commonly the function name *)
    ; service_name : string
          (** the service name, like "backend" or "scheduler-service" *)
    ; span_id : ID.t  (** this span's unique ID *)
    ; trace_id : ID.t  (** the span's trace's ID *)
    ; parent_id : ID.t
          (** ID of the span's parent, or 0 if it is a root span *)
    ; start_time : Time.t
    ; attributes : (string, Yojson.Safe.t) Hashtbl.t
          (* arbitrary KV data that will be serialized along with the span *) }

  (** root creates a new root span (that is, one without a parent),
   * generating a new trace ID. *)
  let root (name : string) : t =
    (* service_name is the name of the binary file that was executed - argv[0] minus
     * the path & the .exe *)
    let service_name =
      let exe_pattern = String.Search_pattern.create ".exe" in
      Sys.argv.(0)
      (* Remove paths - this is like bash's 'basename' *)
      |> String.split ~on:'/'
      |> List.last_exn
      (* Remove .exe *)
      |> fun in_ ->
      String.Search_pattern.replace_first exe_pattern ~in_ ~with_:""
    in
    { name
    ; service_name
    ; trace_id = gid ()
    ; span_id = gid ()
    ; parent_id = 0
    ; start_time = Time.now ()
    ; attributes = Hashtbl.create (module String) }


  (** from_parent creates a new span deriving from the passed [parent] *)
  let from_parent (name : string) (parent : t) : t =
    { name
    ; service_name = parent.service_name
    ; trace_id = parent.trace_id
    ; span_id = gid ()
    ; parent_id = parent.span_id
    ; start_time = Time.now ()
    ; attributes = Hashtbl.create (module String) }


  (** set_attr sets a single key-value attribute on the span (mutating).
   * Only the most recent value for an attribute is kept. *)
  let set_attr (span : t) (key : string) (value : Yojson.Safe.t) : unit =
    Hashtbl.set span.attributes key value


  (** set_attrs sets many key-value attributes on the span (mutating).
   * Only the most recent value for an attribute is kept. *)
  let set_attrs (span : t) (attrs : (string * Yojson.Safe.t) list) : unit =
    List.iter attrs ~f:(fun (k, v) -> Hashtbl.set span.attributes k v)


  (** log_params returns the span data to be logged
   *
   * See https://docs.honeycomb.io/working-with-your-data/tracing/send-trace-data/#manual-tracing *)
  let log_params (span : t) : (string * Yojson.Safe.t) list =
    let duration_ms =
      span.start_time |> Time.diff (Time.now ()) |> Time.Span.to_ms
    in
    let timestamp =
      Time.to_string_iso8601_basic ~zone:Time.Zone.utc span.start_time
    in
    let p =
      [ ("timestamp", `String timestamp)
      ; ("service_name", `String span.service_name)
      ; ("name", `String span.name)
      ; ("duration_ms", `Float duration_ms)
      ; ("trace.span_id", `String (ID.to_string span.span_id))
      ; ("trace.trace_id", `String (ID.to_string span.trace_id)) ]
      @ Hashtbl.to_alist span.attributes
    in
    if span.parent_id = 0
    then p
    else ("trace.parent_id", `String (ID.to_string span.parent_id)) :: p


  (** finish records the span end time and logs it. *)
  let finish (span : t) : unit =
    `Assoc (log_params span) |> Yojson.Safe.to_string |> Caml.print_endline


  (** event immediately logs a span event, ie, a timestamped log without a
    * duration, associated with the passed [span]
    *
    * See https://docs.honeycomb.io/working-with-your-data/tracing/send-trace-data/#span-events *)
  let event
      ?(attrs : (string * Yojson.Safe.t) list = []) (span : t) (name : string) :
      unit =
    let now = Time.now () |> Time.to_string_iso8601_basic ~zone:Time.Zone.utc in
    `Assoc
      ( [ ("timestamp", `String now)
        ; ("name", `String name)
        ; ("trace.parent_id", `String (ID.to_string span.span_id))
        ; ("trace.trace_id", `String (ID.to_string span.trace_id))
        ; ("meta.span_type", `String "span_event") ]
      @ attrs )
    |> Yojson.Safe.to_string
    |> Caml.print_endline
end

(** with_span is a helper for wrapping a function call in a span. It calls the
 * given function [f] with a newly created child span, ensuring a call to
 * [finish] after the function returns. *)
let with_span
    (parent : Span.t)
    (name : string)
    ?(attrs : (string * Yojson.Safe.t) list = [])
    (f : Span.t -> 'a) : 'a =
  let span = Span.from_parent name parent in
  Span.set_attrs span attrs ;
  protectx span ~f ~finally:Span.finish
