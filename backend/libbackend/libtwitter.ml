open Libexecution.Prelude
open Libexecution.Lib
open Libexecution.Types
open Libexecution.Types.RuntimeT
module Dval = Libexecution.Dval

let call (endpoint : string) (verb : string) (args : 'expr_type dval_map) :
    'expr_type dval =
  let prefix = "https://api.twitter.com" in
  let url = prefix ^ endpoint in
  let auth : Twitter.auth =
    match StrDict.get ~key:"auth" args with
    | Some (DObj v) ->
        let toStr key =
          StrDict.get ~key v
          |> Option.andThen ~f:Dval.to_string_opt
          |> Option.withDefault ~default:""
        in
        { consumer_key = toStr "consumerKey"
        ; consumer_secret = toStr "consumerSecret"
        ; access_token = toStr "accessTokenKey"
        ; access_token_secret = toStr "accessTokenSecret" }
    | _ ->
        { consumer_key = ""
        ; consumer_secret = ""
        ; access_token = ""
        ; access_token_secret = "" }
  in
  let authargs =
    args
    |> StrDict.update ~key:"auth" ~f:(fun _ -> None)
    |> DvalMap.filter ~f:(( <> ) DNull)
    |> DvalMap.map ~f:(fun v -> Dval.to_url_string_exn v)
    |> DvalMap.to_list
  in
  let args =
    args
    |> StrDict.update ~key:"auth" ~f:(fun _ -> None)
    |> DvalMap.filter ~f:(( <> ) DNull)
    |> DvalMap.to_list
  in
  if auth.consumer_key = ""
  then DError (SourceNone, "Missing string field `consumerKey`")
  else if auth.consumer_secret = ""
  then DError (SourceNone, "Missing string field `consumerSecret`")
  else if auth.access_token = ""
  then DError (SourceNone, "Missing string field `accessTokenKey`")
  else if auth.access_token_secret = ""
  then DError (SourceNone, "Missing string field `accessTokenSecret`")
  else
    let header = Twitter.authorization_header auth url verb authargs in
    let headers =
      Dval.to_dobj_exn [("Authorization", Dval.dstr_of_string_exn header)]
    in
    match verb with
    | "GET" ->
        let query = Dval.to_dobj_exn args in
        let body = Dval.dstr_of_string_exn "" in
        Legacy.LibhttpclientV0.wrapped_send_request
          url
          Httpclient.GET
          Libexecution.Dval.to_pretty_machine_json_v1
          body
          query
          headers
    | "POST" ->
        let body = Dval.to_dobj_exn args in
        Legacy.LibhttpclientV0.wrapped_send_request
          url
          Httpclient.POST
          Libexecution.Dval.to_pretty_machine_json_v1
          body
          Dval.empty_dobj
          headers
    | _ ->
        Libexecution.Exception.internal ("Invalid Twitter httpMethod: " ^ verb)


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
  |> Base.String.substr_replace_first ~pattern:"/" ~with_:""
  |> Base.String.substr_replace_first ~pattern:"1.1/" ~with_:""
  |> Base.String.substr_replace_first ~pattern:".json" ~with_:""


let param2param (sw : Swagger.parameter) : param =
  { name = sw.name
  ; optional = not sw.required
  ; block_args = []
  ; tipe = sw.dataType |> sw_type2dark
  ; description = sw.description }


let auth_param : param =
  { name = "auth"
  ; optional = false
  ; block_args = []
  ; tipe = TObj
  ; description =
      "Twitter authentication. An object containing the fields consumerKey, consumerSecret, accessTokenKey, and accessTokenSecret. The consumer fields are your app's keys. They access_token fields are your user's keys."
  }


let fns : Libexecution.Types.fluid_expr fn list =
  schema.apis
  |> List.filter ~f:(fun (api : Swagger.api) ->
         (* There are a bunch of apis that have "{id}" or "{format}" in their
          * names. We don't support filling in those paramters at the moment so
          * they can't actually be called correctly, and the presence of "{"
          * made it impossible to create objects in the autocomplete, so we
          * disabled them for now *)
         not (String.contains ~substring:"{" api.path))
  |> List.filter_map ~f:(fun (api : Swagger.api) ->
         api.operations
         |> List.head
         |> Option.map ~f:(fun (op : Swagger.operation) ->
                { prefix_names = ["Twitter::" ^ twurl2name api.path]
                ; infix_names = []
                ; return_type = TAny
                ; func = API (call api.path op.httpMethod)
                ; parameters =
                    auth_param :: List.map ~f:param2param op.parameters
                ; description = Base.Option.value ~default:"" op.summary
                ; preview_safety = Unsafe
                ; deprecated = false }))
