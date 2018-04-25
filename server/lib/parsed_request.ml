open Core

module RT = Runtime
module Clu = Cohttp_lwt_unix
module C = Cohttp
module CRequest = Clu.Request

open Types.RuntimeT

(* Internal invariant, _must_ be a DObj *)
type t = dval

(* ------------------------- *)
(* Internal *)
(* ------------------------- *)

type parser = Json
            | Form
            | Unknown

let body_parser_type req =
  let content_type =
    match C.Header.get (Clu.Request.headers req) "content-type" with
    | None -> "unknown"
    | Some v -> v
  in
  if String.is_substring
       ~substring:"application/json"
       content_type
  then
    Json
  else if String.is_substring
      ~substring:"application/x-www-form-urlencoded"
       content_type
  then
    Form
  else
    Unknown

let parser_fn p =
  match p with
  | Json -> Dval.parse
  | Form -> Dval.from_form_encoding
  | Unknown -> Dval.parse

let parsed_body req reqbody =
  let bdval =
    if reqbody = ""
    then DNull
    else reqbody |> parser_fn (body_parser_type req)
  in
  Dval.to_dobj [("body", bdval)]

let parsed_query_string uri =
  let dval = Dval.query_to_dval (Uri.query uri) in
  Dval.to_dobj [("queryParams", dval)]

let parsed_headers req =
  req
  |> Clu.Request.headers
  |> C.Header.to_list
  |> List.map ~f:(fun (k, v) -> (k, DStr v))
  |> DvalMap.of_alist_reduce
    ~f:(fun l r -> r)
  |> fun dm -> DObj dm
  |> fun dv -> Dval.to_dobj [("headers", dv)]

let unparsed_body rb =
  let dval = DStr rb in
  Dval.to_dobj [("fullBody", dval)]

let body_of_fmt ~fmt ~key req rbody =
  let dval =
    match (body_parser_type req, rbody) with
    | (fmt, content) when String.length content > 0 ->
      parser_fn fmt content
    | _  -> DNull
  in
  Dval.to_dobj [(key, dval)]

let json_body =
  body_of_fmt ~fmt:Json ~key:"jsonBody"

let form_body =
  body_of_fmt ~fmt:Form ~key:"formBody"

(* ------------------------- *)
(* Exported *)
(* ------------------------- *)

let from_request req rbody =
  let parts =
    [ parsed_body req rbody
    ; json_body req rbody
    ; form_body req rbody
    ; parsed_query_string (CRequest.uri req)
    ; parsed_headers req
    ; unparsed_body rbody
    ]
  in
  List.fold_left
    ~init:Dval.empty_dobj
    ~f:(fun acc p -> Dval.obj_merge acc p)
    parts

let to_dval self =
  self

let sample =
  let open_record = DObj (DvalMap.empty) in
  let parts =
    [ Dval.to_dobj [("body", open_record)]
    ; Dval.to_dobj [("jsonBody", open_record)]
    ; Dval.to_dobj [("formBody", open_record)]
    ; Dval.to_dobj [("queryParams", open_record)]
    ; Dval.to_dobj [("headers", open_record)]
    ; Dval.to_dobj [("fullBody", DStr "")]
    ]
  in
  List.fold_left
    ~init:Dval.empty_dobj
    ~f:(fun acc p -> Dval.obj_merge acc p)
    parts
