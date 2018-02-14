open Core
open Runtime
open Lib
open Types.RuntimeT

open Functions

let params = [ par "uri" TStr
             ; par "body" TAny
             ; par "query" TObj
             ; par "headers" TObj]

let call verb =
  InProcess
  (function
    | [DStr uri; body; query; headers] ->
        let query = Dval.to_string_pairs query in
        let headers = Dval.to_string_pairs headers in

        let has_form_header = List.exists headers
              ~f:(fun (k,v) -> String.lowercase k = "content-type"

                               && String.lowercase v = "application/x-www-form-urlencoded") in

        let body =
          match body with
          | DObj obj ->
              if has_form_header
              then Dval.to_form_encoding body
              else Dval.dval_to_json_string body
          | _ -> Dval.to_repr body in
        DStr (Httpclient.http_call uri query verb headers body)
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

