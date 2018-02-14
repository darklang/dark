open Core
open Runtime
open Lib
open Types.RuntimeT

open Functions


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

(* TODO: integrate with dark_request *)
let call verb =
  InProcess
  (function
    | [DStr uri; body; query_; headers_] ->
        let query = Dval.to_string_pairs query_ in
        let headers = Dval.to_string_pairs headers_ in
        let body =
          match body with
          | DObj obj ->
              if has_form_header headers
              then Dval.to_form_encoding body
              else Dval.dval_to_json_string body
          | _ -> Dval.to_repr body in
        let (result, headers) = Httpclient.http_call uri query verb headers body in
        if has_form_header headers
        then Dval.from_form_encoding result
        else if has_json_header headers
        then Dval.parse result
        else DStr result
    | args -> fail args)


let fns : Lib.shortfn list = [
  { n = "HttpClient::post"
  ; o = []
  ; p = params
  ; r = TObj
  ; d = "Make blocking HTTP POST call to `uri`"
  ; f = call Httpclient.POST
  ; pr = None
  ; ps = false
  ; i = false
  }
  ;

  { n = "HttpClient::get"
  ; o = []
  ; p = params
  ; r = TObj
  ; d = "Make blocking HTTP GET call to `uri`"
  ; f = call Httpclient.GET
  ; pr = None
  ; ps = false
  ; i = false
  }

]

