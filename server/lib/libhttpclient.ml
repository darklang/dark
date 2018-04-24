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
    | (_, args) -> fail args)


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
  ; r = TObj
  ; d = ""
  ; f = InProcess
        (function
        | (_, []) ->
          DObj (DvalMap.of_alist_exn
                  [("Content-Type", DStr "application/x-www-form-urlencoded")])
        | (_, args) -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["HttpClient::json_content_type"]
  ; ins = []
  ; p = []
  ; r = TObj
  ; d = ""
  ; f = InProcess
        (function
        | (_, []) ->
          DObj (DvalMap.of_alist_exn
                  [("Content-Type", DStr "application/json; charset=utf-8")])
        | (_, args) -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["HttpClient::basic_auth"]
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
                  raise_error "Username cannot contain a colon"
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
        | (_, args) -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["HttpClient::bearer_token"]
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
        | (_, args) -> fail args)
  ; pr = None
  ; ps = true
  }
]

