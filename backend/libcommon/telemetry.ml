open Core_kernel
module ID = Int

let gid () = Random.int (Int.max_value - 1) + 1

module Span = struct
  type t =
    { name : string
    ; span_id : ID.t
    ; trace_id : ID.t
    ; parent_id : ID.t
    ; start_time : float
    ; end_time : float
    ; attributes : (string, Yojson.Safe.t) Hashtbl.t }

  let root (name : string) : t =
    { name
    ; trace_id = gid ()
    ; span_id = gid ()
    ; parent_id = 0
    ; start_time = Unix.gettimeofday ()
    ; end_time = 0.0
    ; attributes = Hashtbl.create (module String) }


  let from_parent (name : string) (parent : t) : t =
    { name
    ; trace_id = parent.trace_id
    ; span_id = gid ()
    ; parent_id = parent.span_id
    ; start_time = Unix.gettimeofday ()
    ; end_time = 0.0
    ; attributes = Hashtbl.create (module String) }


  let set (span : t) (key : string) (value : Yojson.Safe.t) : unit =
    Hashtbl.set span.attributes key value


  let mset (span : t) (attrs : (string * Yojson.Safe.t) list) : unit =
    List.iter attrs ~f:(fun (k, v) -> Hashtbl.set span.attributes k v)


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
    else ("duration_ms", `Float (span.end_time -. span.start_time)) :: p


  let finish (span : t) : unit =
    let span = {span with end_time = Unix.gettimeofday ()} in
    Log.infO span.name ~jsonparams:(log_params span)
end

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
  Span.mset span attrs ;
  protectx span ~f ~finally:Span.finish
