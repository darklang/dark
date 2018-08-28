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
let call verb =
  InProcess
  (function
    | (_, [DStr uri; body; query_; headers_]) ->
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
  ; dep = false
  }
  ;

  { pns = ["HttpClient::put"]
  ; ins = []
  ; p = params
  ; r = TObj
  ; d = "Make blocking HTTP PUT call to `uri`"
  ; f = call Httpclient.PUT
  ; pr = None
  ; ps = false
  ; dep = false
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
  ; dep = false
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
  ; dep = false
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
  ; dep = false
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
  ; dep = false
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
  ; dep = false
  }
  ;

  { pns = ["HttpClient::formContentType"]
  ; ins = []
  ; p = []
  ; r = TObj
  ; d = ""
  ; f = InProcess
        (function
        | (_, []) ->
          DObj (DvalMap.of_alist_exn
                  [("Content-Type", DStr "application/x-www-form-urlencoded")])
        | args -> fail args)
  ; pr = None
  ; ps = true
  ; dep = false
  }
  ;


  { pns = ["HttpClient::jsonContentType"]
  ; ins = []
  ; p = []
  ; r = TObj
  ; d = ""
  ; f = InProcess
        (function
        | (_, []) ->
          DObj (DvalMap.of_alist_exn
                  [("Content-Type", DStr "application/json; charset=utf-8")])
        | args -> fail args)
  ; pr = None
  ; ps = true
  ; dep = false
  }
  ;

  { pns = ["HttpClient::plainTextContentType"]
  ; ins = []
  ; p = []
  ; r = TObj
  ; d = ""
  ; f = InProcess
        (function
        | (_, []) ->
          DObj (DvalMap.of_alist_exn
                  [("Content-Type", DStr "text/plain; charset=utf-8")])
        | args -> fail args)
  ; pr = None
  ; ps = true
  ; dep = false
  }
  ;

  { pns = ["HttpClient::htmlContentType"]
  ; ins = []
  ; p = []
  ; r = TObj
  ; d = ""
  ; f = InProcess
        (function
        | (_, []) ->
          DObj (DvalMap.of_alist_exn
                  [("Content-Type", DStr "text/html; charset=utf-8")])
        | args -> fail args)
  ; pr = None
  ; ps = true
  ; dep = false
  }
  ;




  { pns = ["HttpClient::basicAuth"]
  ; ins = []
  ; p = [par "username" TStr; par "password" TStr]
  ; r = TObj
  ; d = ""
  ; f = InProcess
        (function
          | (_, [DStr u; DStr p]) ->
            let auth_string =
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
            in
            DObj (DvalMap.of_alist_exn
                    [("Authorization", DStr auth_string)])
        | args -> fail args)
  ; pr = None
  ; ps = true
  ; dep = false
  }
  ;

  { pns = ["HttpClient::bearerToken"]
  ; ins = []
  ; p = [par "token" TStr]
  ; r = TObj
  ; d = ""
  ; f = InProcess
        (function
          | (_, [DStr token]) ->
            let auth_string = "Bearer " ^ token in
            DObj (DvalMap.of_alist_exn
                    [("Authorization", DStr auth_string)])
        | args -> fail args)
  ; pr = None
  ; ps = true
  ; dep = false
  }
]

