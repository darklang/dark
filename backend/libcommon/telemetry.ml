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
    ; span_id : ID.t  (** this span's unique ID *)
    ; trace_id : ID.t  (** the span's trace's ID *)
    ; parent_id : ID.t
          (** ID of the span's parent, or 0 if it is a root span *)
    ; start_time : float (* unix time at start *)
    ; end_time : float (* unix time at end *)
    ; attributes : (string, Yojson.Safe.t) Hashtbl.t
          (* arbitrary KV data that will be serialized along with the span *) }

  (** root creates a new root span (that is, one without a parent),
   * generating a new trace ID. *)
  let root (name : string) : t =
    { name
    ; trace_id = gid ()
    ; span_id = gid ()
    ; parent_id = 0
    ; start_time = Unix.gettimeofday ()
    ; end_time = 0.0
    ; attributes = Hashtbl.create (module String) }


  (** from_parent creates a new span deriving from the passed [parent] *)
  let from_parent (name : string) (parent : t) : t =
    { name
    ; trace_id = parent.trace_id
    ; span_id = gid ()
    ; parent_id = parent.span_id
    ; start_time = Unix.gettimeofday ()
    ; end_time = 0.0
    ; attributes = Hashtbl.create (module String) }


  (** set_attr sets a single key-value attribute on the span (mutating).
   * Only the most recent value for an attribute is kept. *)
  let set_attr (span : t) (key : string) (value : Yojson.Safe.t) : unit =
    Hashtbl.set span.attributes key value


  (** set_attrs sets many key-value attributes on the span (mutating).
   * Only the most recent value for an attribute is kept. *)
  let set_attrs (span : t) (attrs : (string * Yojson.Safe.t) list) : unit =
    List.iter attrs ~f:(fun (k, v) -> Hashtbl.set span.attributes k v)


  (** log_params returns the span data to be logged *)
  let log_params (span : t) : (string * Yojson.Safe.t) list =
    let p =
      ("trace.span_id", `String (ID.to_string span.span_id))
      :: ("trace.trace_id", `String (ID.to_string span.trace_id))
      :: Hashtbl.to_alist span.attributes
    in
    let p =
      if span.parent_id = 0
      then p
      else ("trace.parent_id", `String (ID.to_string span.parent_id)) :: p
    in
    if span.end_time = 0.0
    then p
    else
      let d = 1000.0 *. (span.end_time -. span.start_time) in
      ("duration_ms", `Float d) :: p


  (** finish records the span end time and logs it. *)
  let finish (span : t) : unit =
    (* From the OCaml Gc docs:
     * The total amount of memory allocated by the program since it was started
     * is (in words) minor_words + major_words - promoted_words. Multiply by
     * the word size (4 on a 32-bit machine, 8 on a 64-bit machine) to get the
     * number of bytes. *)
    let usage_kb =
      Gc.allocated_bytes () /. 1024.0 |> Float.iround |> Option.value ~default:0
    in
    set_attr span "meta.process_memory_kb" (`Int usage_kb) ;
    let span = {span with end_time = Unix.gettimeofday ()} in
    Log.infO span.name ~jsonparams:(log_params span)
end

(** with_span is a helper for wrapping a function call in a span. It calls the
 * given function [f] with a newly created span, ensuring a call to [finish]
 * after the function returns.
 * [~parent] should usually be passed to link the new span to an existing
 * trace. Omitting [~parent] will create a new root span. *)
let with_span
    (name : string)
    ?(parent : Span.t option)
    ?(attrs : (string * Yojson.Safe.t) list = [])
    (f : Span.t -> 'a) : 'a =
  let span =
    match parent with
    | Some p ->
        Span.from_parent name p
    | None ->
        Span.root name
  in
  Span.set_attrs span attrs ;
  protectx span ~f ~finally:Span.finish
