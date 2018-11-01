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

let parser_fn p str =
  match p with
  | Json ->
    (match Dval.parse_basic_json str with
     | Some dv -> dv
     | None ->
       Exception.user ~actual:str "Invalid json")
  | Form -> Dval.from_form_encoding str
  | Unknown ->
    (match Dval.parse_basic_json str with
     | Some dv -> dv
     | None ->
       Exception.enduser ~actual:str ("Unknown Content-type -- we assumed application/json but invalid JSON was sent"))

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

let parsed_cookies cookies =
  cookies
  |> String.split ~on:';'
  |> List.map ~f:String.strip
  |> List.map ~f:(String.lsplit2 ~on:'=')
  |> List.filter_opt
  |> List.map ~f:(fun (k, v) -> (k, DStr v))
  |> Dval.to_dobj

let cookies (headers : (string * string) list) =
  List.Assoc.find ~equal:(=) headers "cookie"
  |> Option.map ~f:parsed_cookies
  |> Option.value ~default:(Dval.to_dobj [])
  |> fun x ->  Dval.to_dobj [("cookies",  x)]

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
    ; cookies headers
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


