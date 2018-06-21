open Core_kernel

module RT = Runtime
open Types.RuntimeT

(* Internal invariant, _must_ be a DObj *)
type t = dval

(* ------------------------- *)
(* Internal *)
(* ------------------------- *)

type parser = Json
            | Form
            | Unknown

let body_parser_type headers =
  let ct_is ct =
      List.exists headers
      ~f:(fun (k, v) ->
            String.Caseless.equal k "content-type"
            && String.is_substring ~substring:ct v)
  in
  if ct_is "application/json"
  then Json
  else if ct_is "application/x-www-form-urlencoded"
  then Form
  else Unknown

let parser_fn p =
  match p with
  | Json -> Dval.parse
  | Form -> Dval.from_form_encoding
  | Unknown -> Dval.parse

let parsed_body headers reqbody =
  let bdval =
    if reqbody = ""
    then DNull
    else reqbody |> parser_fn (body_parser_type headers)
  in
  Dval.to_dobj [("body", bdval)]

let parsed_query_string (queryvals:(string * string list) list)  =
  let dval = Dval.query_to_dval queryvals in
  Dval.to_dobj [("queryParams", dval)]

let parsed_headers (headers: (string * string) list) =
  headers
  |> List.map ~f:(fun (k, v) -> (k, DStr v))
  |> DvalMap.of_alist_reduce
    ~f:(fun l r -> r)
  |> fun dm -> DObj dm
  |> fun dv -> Dval.to_dobj [("headers", dv)]

let unparsed_body rb =
  let dval = DStr rb in
  Dval.to_dobj [("fullBody", dval)]

let body_of_fmt ~fmt ~key headers rbody =
  let dval =
    match (body_parser_type headers, rbody) with
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
type header = string * string
type query_val = string * string list

let from_request (headers: header list) (query:query_val list) rbody =
  let parts =
    [ parsed_body headers rbody
    ; json_body headers rbody
    ; form_body headers rbody
    ; parsed_query_string query
    ; parsed_headers headers
    ; unparsed_body rbody
    ]
  in
  List.fold_left
    ~init:Dval.empty_dobj
    ~f:(fun acc p -> Dval.obj_merge acc p)
    parts

let to_dval self =
  self

let sample_request =
  let parts =
    [ Dval.to_dobj [("body", DIncomplete)]
    ; Dval.to_dobj [("jsonBody", DIncomplete)]
    ; Dval.to_dobj [("formBody", DIncomplete)]
    ; Dval.to_dobj [("queryParams", DIncomplete)]
    ; Dval.to_dobj [("headers", DIncomplete)]
    ; Dval.to_dobj [("fullBody", DIncomplete)]
    ]
  in
  List.fold_left
    ~init:Dval.empty_dobj
    ~f:(fun acc p -> Dval.obj_merge acc p)
    parts


