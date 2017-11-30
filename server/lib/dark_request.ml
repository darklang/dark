open Core

module RT = Runtime
module Clu = Cohttp_lwt_unix
module C = Cohttp

(* Internal invariant, _must_ be a DObj *)
type t = RT.dval

(* ------------------------- *)
(* Internal *)
(* ------------------------- *)

let body_parser content_type =
  let form_parser f =
    f |> Uri.query_of_encoded |> RT.query_to_dval
  in
  match content_type with
  | "application/json" -> RT.parse
  | "application/x-www-form-urlencoded" -> form_parser
  | _ -> RT.parse

let request_content_type req =
  match C.Header.get (Clu.Request.headers req) "content-type" with
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
  RT.to_dobj [("queryParams", dval)]

let parsed_headers req =
  req
  |> Clu.Request.headers
  |> C.Header.to_list
  |> List.map ~f:(fun (k, v) -> (k, RT.DStr v))
  |> RT.DvalMap.of_alist_exn
  |> fun dm -> RT.DObj dm
  |> fun dv -> RT.to_dobj [("headers", dv)]

(* ------------------------- *)
(* Exported *)
(* ------------------------- *)

let from_request req rbody uri =
  let parts =
    [ parsed_body req rbody
    ; parsed_query_string uri
    ; parsed_headers req
    ]
  in
  List.fold_left
    ~init:RT.empty_dobj
    ~f:(fun acc p -> RT.obj_merge acc p)
    parts

let to_dval self =
  self
