open Core_kernel
open Types.RuntimeT

(* Internal invariant, _must_ be a DObj *)
type t = dval

(* ------------------------- *)
(* Internal *)
(* ------------------------- *)

type parser =
  | Json
  | Form
  | Unknown

let body_parser_type headers =
  let ct_is ct =
    List.exists headers ~f:(fun (k, v) ->
        String.Caseless.equal k "content-type"
        && String.is_substring ~substring:ct v )
  in
  if ct_is "application/json"
  then Json
  else if ct_is "application/x-www-form-urlencoded"
  then Form
  else Unknown


let parser_fn p (str : string) : dval =
  match p with
  | Json ->
    ( try Dval.of_unknown_json_v0 str with e ->
        Exception.enduser ~actual:str ("Invalid json: " ^ str) )
  | Form ->
      Dval.of_form_encoding str
  | Unknown ->
    ( try Dval.of_unknown_json_v0 str with e ->
        Exception.enduser
          ~actual:str
          "Unknown Content-type -- we assumed application/json but invalid JSON was sent"
    )


let parsed_body headers reqbody =
  let bdval =
    if reqbody = ""
    then DNull
    else reqbody |> parser_fn (body_parser_type headers)
  in
  Dval.to_dobj_exn [("body", bdval)]


let parsed_query_string (queryvals : (string * string list) list) =
  let dval = Dval.query_to_dval queryvals in
  Dval.to_dobj_exn [("queryParams", dval)]


let parsed_headers (headers : (string * string) list) =
  headers
  |> List.map ~f:(fun (k, v) -> (k, Dval.dstr_of_string_exn v))
  |> DvalMap.from_list
  |> fun dm -> DObj dm |> fun dv -> Dval.to_dobj_exn [("headers", dv)]


let unparsed_body rb =
  let dval = Dval.dstr_of_string_exn rb in
  Dval.to_dobj_exn [("fullBody", dval)]


let body_of_fmt ~fmt ~key headers rbody =
  let dval =
    match (body_parser_type headers, rbody) with
    | fmt, content when String.length content > 0 ->
        parser_fn fmt content
    | _ ->
        DNull
  in
  Dval.to_dobj_exn [(key, dval)]


let json_body = body_of_fmt ~fmt:Json ~key:"jsonBody"

let form_body = body_of_fmt ~fmt:Form ~key:"formBody"

let parsed_cookies cookies =
  cookies
  |> String.split ~on:';'
  |> List.map ~f:String.strip
  |> List.map ~f:(String.lsplit2 ~on:'=')
  |> List.filter_opt
  |> List.map ~f:(fun (k, v) ->
         (Uri.pct_decode k, Dval.dstr_of_string_exn (Uri.pct_decode v)) )
  |> Dval.to_dobj_exn


let cookies (headers : (string * string) list) =
  List.Assoc.find ~equal:( = ) headers "cookie"
  |> Option.map ~f:parsed_cookies
  |> Option.value ~default:(Dval.to_dobj_exn [])
  |> fun x -> Dval.to_dobj_exn [("cookies", x)]

let url (uri : Uri.t) =
  uri
  |> Uri.to_string
  |> fun s -> Dval.to_dobj_exn [("url", Dval.dstr_of_string_exn s)]


(* ------------------------- *)
(* Exported *)
(* ------------------------- *)
type header = string * string

type query_val = string * string list

(* If allow_unparsed is true, we fall back to DNull; this allows us to create a
 * 404 for a request with an unparseable body *)
let from_request
    ?(allow_unparseable = false)
    (uri : Uri.t)
    (headers : header list)
    (query : query_val list)
    rbody =
  let parsed_body =
    try parsed_body headers rbody with e ->
      if allow_unparseable then DNull else raise e
  in
  let json_body =
    try json_body headers rbody with e ->
      if allow_unparseable then DNull else raise e
  in
  let form_body =
    try form_body headers rbody with e ->
      if allow_unparseable then DNull else raise e
  in
  let parts =
    [ parsed_body
    ; json_body
    ; form_body
    ; parsed_query_string query
    ; parsed_headers headers
    ; unparsed_body rbody
    ; cookies headers
    ; url uri
  ]
  in
  List.fold_left
    ~init:Dval.empty_dobj
    ~f:(fun acc p -> Dval.obj_merge acc p)
    parts


let to_dval self = self

let sample_request =
  let parts =
    [ Dval.to_dobj_exn [("body", DIncomplete)]
    ; Dval.to_dobj_exn [("jsonBody", DIncomplete)]
    ; Dval.to_dobj_exn [("formBody", DIncomplete)]
    ; Dval.to_dobj_exn [("queryParams", DIncomplete)]
    ; Dval.to_dobj_exn [("headers", DIncomplete)]
    ; Dval.to_dobj_exn [("fullBody", DIncomplete)]
    ; Dval.to_dobj_exn [("url", DIncomplete)] ]
  in
  List.fold_left
    ~init:Dval.empty_dobj
    ~f:(fun acc p -> Dval.obj_merge acc p)
    parts
