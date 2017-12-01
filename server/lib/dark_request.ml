open Core

module RT = Runtime
module Clu = Cohttp_lwt_unix
module C = Cohttp

(* Internal invariant, _must_ be a DObj *)
type t = RT.dval

(* ------------------------- *)
(* Internal *)
(* ------------------------- *)

type parser = Json
            | Form
            | Unknown

let form_parser f =
  f |> Uri.query_of_encoded |> RT.query_to_dval

let body_parser_type req =
  let content_type =
    match C.Header.get (Clu.Request.headers req) "content-type" with
    | None -> "unknown"
    | Some v -> v
  in
  match content_type with
  | "application/json" -> Json
  | "application/x-www-form-urlencoded" -> Form
  | _ -> Unknown

let parser_fn p =
  match p with
  | Json -> RT.parse
  | Form -> form_parser
  | Unknown -> RT.parse

let parsed_body req reqbody =
  let bdval =
    if reqbody = ""
    then RT.DNull
    else reqbody |> parser_fn (body_parser_type req)
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

let unparsed_body rb =
  let dval = RT.DStr rb in
  RT.to_dobj [("fullBody", dval)]

let body_of_fmt ~fmt ~key req rbody =
  let dval =
    match (body_parser_type req, rbody) with
    | (fmt, content) when String.length content > 0 ->
      parser_fn fmt content
    | _  -> RT.DNull
  in
  RT.to_dobj [(key, dval)]

let json_body =
  body_of_fmt ~fmt:Json ~key:"jsonBody"

let form_body =
  body_of_fmt ~fmt:Form ~key:"formBody"

(* ------------------------- *)
(* Exported *)
(* ------------------------- *)

let from_request req rbody uri =
  let parts =
    [ parsed_body req rbody
    ; json_body req rbody
    ; form_body req rbody
    ; parsed_query_string uri
    ; parsed_headers req
    ; unparsed_body rbody
    ]
  in
  List.fold_left
    ~init:RT.empty_dobj
    ~f:(fun acc p -> RT.obj_merge acc p)
    parts

let to_dval self =
  self

let sample =
  let open_record = RT.DObj (RT.DvalMap.empty) in
  let parts =
    [ RT.to_dobj [("body", open_record)]
    ; RT.to_dobj [("jsonBody", open_record)]
    ; RT.to_dobj [("formBody", open_record)]
    ; RT.to_dobj [("queryParams", open_record)]
    ; RT.to_dobj [("headers", open_record)]
    ; RT.to_dobj [("fullBody", RT.DStr "")]
    ]
  in
  List.fold_left
    ~init:RT.empty_dobj
    ~f:(fun acc p -> RT.obj_merge acc p)
    parts
