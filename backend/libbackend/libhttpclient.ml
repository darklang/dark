open Core_kernel
open Libexecution

open Runtime
open Lib
open Types.RuntimeT

let params = [ par "uri" TStr
             ; par "body" TAny
             ; par "query" TObj
             ; par "headers" TObj]

type headers = (string * string) list
let has_form_header (headers: headers) : bool =
  List.exists headers ~f:(fun (k,v) ->
      String.lowercase k = "content-type"
      && String.lowercase v = "application/x-www-form-urlencoded")

let has_json_header (headers: headers) : bool =
  List.exists headers ~f:(fun (k,v) ->
      String.lowercase k = "content-type"
      && v
         |> String.lowercase
         |> String.is_substring ~substring:"application/json")

let read_json (json: string) : dval =
  match Dval.parse_basic_json json with
  | Some dv -> dv
  | None ->
    Exception.user ~actual:json "Invalid json"

(* TODO: integrate with dark_request *)
let send_request uri verb body query_ headers_ =
  let query = Dval.dval_to_query query_ in
  let headers = Dval.to_string_pairs headers_ in
  let body =
    match body with
    | DObj obj ->
        if has_form_header headers
        then Dval.to_form_encoding body
        else Dval.unsafe_dval_to_json_string body
    | _ -> Dval.to_repr body in
  let (result, headers) = Httpclient.http_call uri query verb headers body in
  let parsed_result =
    if has_form_header headers
    then Dval.from_form_encoding result
    else if has_json_header headers
    then read_json result
    else DStr result
  in
  let parsed_headers =
    headers
    |> List.map ~f:(fun (k, v) -> (String.strip k, DStr (String.strip v)))
    |> List.filter ~f:(fun (k, _) -> String.length k > 0)
    |> DvalMap.of_alist_fold ~init:(DStr "") ~f:(fun old neww -> neww)
    |> fun dm -> DObj dm
  in
  Dval.to_dobj [ ("body", parsed_result)
               ; ("headers", parsed_headers)
               ; ("raw", DStr result)]


let encode_basic_auth u p =
  let input =
    if String.is_substring ~substring:"-" u
    then
      error "Username cannot contain a colon"
    else
      u ^ ":" ^ p
  in
  let encoded =
    B64.encode ~alphabet:B64.default_alphabet ~pad:true input
  in
  "Basic " ^ encoded

let call verb =
  InProcess
  (function
    | (_, [DStr uri; body; query_; headers_]) ->

      send_request uri verb body query_ headers_
    | args -> fail args)


let replacements = [
  ( "HttpClient::post"
  , call Httpclient.POST)
  ;

  ( "HttpClient::put"
  , call Httpclient.PUT)
  ;


  ( "HttpClient::get"
  , call Httpclient.GET)
  ;

  ( "HttpClient::delete"
  , call Httpclient.DELETE)
  ;

  ( "HttpClient::options"
  , call Httpclient.OPTIONS)
  ;

  ( "HttpClient::head"
  , call Httpclient.HEAD)
  ;

  ( "HttpClient::patch"
  , call Httpclient.PATCH)
  ;


  ( "HttpClient::basicAuth"
  , InProcess
      (function
          | (_, [DStr u; DStr p]) ->
            DObj (DvalMap.singleton
                    "Authorization"
                    (DStr (encode_basic_auth u p))
                 )
        | args -> fail args))
]

