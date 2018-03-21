open Core
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

(* TODO: integrate with dark_request *)
let call verb =
  InProcess
  (function
    | [DStr uri; body; query_; headers_] ->
        let query = Dval.dval_to_query query_ in
        let headers = Dval.to_string_pairs headers_ in
        let body =
          match body with
          | DObj obj ->
              if has_form_header headers
              then Dval.to_form_encoding body
              else Dval.dval_to_json_string body
          | _ -> Dval.to_repr body in
        let (result, headers) = Httpclient.http_call uri query verb headers body in
        let parsed_result =
          if has_form_header headers
          then Dval.from_form_encoding result
          else if has_json_header headers
          then Dval.parse result
          else DStr result
        in
        let parsed_headers =
          headers
          |> List.map ~f:(fun (k, v) -> (String.strip k, DStr (String.strip v)))
          |> List.filter ~f:(fun (k, _) -> String.length k > 0)
          |> DvalMap.of_alist_fold ~init:(DStr "") ~f:(fun old neww -> neww)
          |> fun dm -> DObj dm
        in
        Dval.to_dobj [("body", parsed_result); ("headers", parsed_headers)]
    | args -> fail args)


let fns : Lib.shortfn list = [
  { pns = ["HttpClient::post"]
  ; ins = []
  ; p = params
  ; r = TObj
  ; d = "Make blocking HTTP POST call to `uri`"
  ; f = call Httpclient.POST
  ; pr = None
  ; ps = false
  }
  ;

  { pns = ["HttpClient::get"]
  ; ins = []
  ; p = params
  ; r = TObj
  ; d = "Make blocking HTTP GET call to `uri`"
  ; f = call Httpclient.GET
  ; pr = None
  ; ps = false
  }
  ;

  { pns = ["HttpClient::delete"]
  ; ins = []
  ; p = params
  ; r = TObj
  ; d = "Make blocking HTTP DELETE call to `uri`"
  ; f = call Httpclient.DELETE
  ; pr = None
  ; ps = false
  }
  ;

  { pns = ["HttpClient::options"]
  ; ins = []
  ; p = params
  ; r = TObj
  ; d = "Make blocking HTTP OPTIONS call to `uri`"
  ; f = call Httpclient.OPTIONS
  ; pr = None
  ; ps = false
  }
  ;

  { pns = ["HttpClient::head"]
  ; ins = []
  ; p = params
  ; r = TObj
  ; d = "Make blocking HTTP HEAD call to `uri`"
  ; f = call Httpclient.HEAD
  ; pr = None
  ; ps = false
  }
  ;

  { pns = ["HttpClient::patch"]
  ; ins = []
  ; p = params
  ; r = TObj
  ; d = "Make blocking HTTP PATCH call to `uri`"
  ; f = call Httpclient.PATCH
  ; pr = None
  ; ps = false
  }
  ;

  { pns = ["HttpClient::form_content_type"]
  ; ins = []
  ; p = []
  ; r = TStr
  ; d = ""
  ; f = InProcess
        (function
        | [] -> DStr "application/x-www-form-urlencoded"
        | args -> fail args)
  ; pr = None
  ; ps = false
  }
  ;


  { pns = ["HttpClient::json_content_type"]
  ; ins = []
  ; p = []
  ; r = TStr
  ; d = ""
  ; f = InProcess
        (function
        | [] -> DStr "application/json"
        | args -> fail args)
  ; pr = None
  ; ps = false
  }
]

