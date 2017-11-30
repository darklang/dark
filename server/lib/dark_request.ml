open Core

module RT = Runtime
module Clu = Cohttp_lwt_unix

(* Internal invariant, _must_ be a DObj *)
type t = RT.dval

(* not exported *)
let body_parser content_type =
  let form_parser f =
    f |> Uri.query_of_encoded |> RT.query_to_dval
  in
  match content_type with
  | "application/json" -> RT.parse
  | "application/x-www-form-urlencoded" -> form_parser
  | _ -> RT.parse

let headers req =
  req |> Clu.Request.headers

let request_content_type req =
  match Cohttp.Header.get (headers req) "content-type" with
  | None -> "unknown"
  | Some v -> v

let parsed_body req reqbody =
  let bdval =
    if reqbody = ""
    then RT.DNull
    else body_parser (request_content_type req) reqbody
  in
  RT.to_dobj [("body", bdval)]

let parsed_query_string uri =
  let dval = RT.query_to_dval (Uri.query uri) in
  RT.to_dobj [("queryString", dval)]

(* exported *)
let from_request req rbody uri =
  let parts =
    [ parsed_body req rbody
    ; parsed_query_string uri
    ]
  in
  List.fold_left ~init:RT.empty_dobj ~f:(fun acc p -> RT.obj_merge acc p) parts

let to_dval self =
  self
