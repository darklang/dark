open Core_kernel
open Libexecution
open Lib
open Types
open Types.RuntimeT

let call (endpoint : string) (verb : string) (args : dval_map) : dval =
  let prefix = "https://api.twitter.com" in
  let url = prefix ^ endpoint in
  let authargs =
    args
    |> DvalMap.filter ~f:(( <> ) Types.RuntimeT.DNull)
    |> DvalMap.map ~f:(fun v -> Dval.to_url_string_exn v)
    |> DvalMap.to_list
  in
  let result =
    match verb with
    | "GET" ->
        let query = Dval.to_form_encoding (DObj args) in
        let header = Twitter.authorization_header url "GET" authargs in
        Httpclient.call (url ^ "?" ^ query) GET [header] ""
    | "POST" ->
        let body = "" in
        let header = Twitter.authorization_header url "POST" authargs in
        Httpclient.call url POST [header] body
    | _ ->
        Exception.internal ("Invalid Twitter httpMethod: " ^ verb)
  in
  Dval.of_unknown_json_v0 result


let schema = Swagger.parse "twitter.json"

let sw_type2dark tipe =
  match tipe with
  | "string" ->
      TStr
  | "int" ->
      TInt
  | _ ->
      failwith ("todo: type: " ^ tipe)


let twurl2name (url : string) : string =
  (* /1.1/{NAME}.json *)
  url
  |> String.substr_replace_first ~pattern:"/" ~with_:""
  |> String.substr_replace_first ~pattern:"1.1/" ~with_:""
  |> String.substr_replace_first ~pattern:".json" ~with_:""


let param2param (sw : Swagger.parameter) : param =
  { name = sw.name
  ; optional = not sw.required
  ; block_args = []
  ; tipe = sw.dataType |> sw_type2dark
  ; description = sw.description }


let fns =
  schema.apis
  |> List.filter_map ~f:(fun (api : Swagger.api) ->
         api.operations
         |> List.hd
         |> Option.map ~f:(fun (op : Swagger.operation) ->
                { pns = ["Twitter::" ^ twurl2name api.path]
                ; ins = []
                ; r = TAny
                ; f = API (call op.httpMethod api.path)
                ; p = List.map ~f:param2param op.parameters
                ; d = Option.value ~default:"" op.summary
                ; ps = false
                ; dep = false } ) )
