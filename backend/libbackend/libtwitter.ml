open Libexecution
open Prelude
open Lib
open Types
open Types.RuntimeT

let call (endpoint : string) (verb : string) (args : dval_map) : dval =
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
    |> DvalMap.filter ~f:(( <> ) Types.RuntimeT.DNull)
    |> DvalMap.map ~f:(fun v -> Dval.to_url_string_exn v)
    |> DvalMap.to_list
  in
  if auth.consumer_key = ""
  then DError "Missing string field `consumerKey`"
  else if auth.consumer_secret = ""
  then DError "Missing string field `consumerSecret`"
  else if auth.access_token = ""
  then DError "Missing string field `accessTokenKey`"
  else if auth.access_token_secret = ""
  then DError "Missing string field `accessTokenSecret`"
  else
    let result =
      match verb with
      | "GET" ->
          let query = Dval.to_form_encoding (DObj args) in
          let header = Twitter.authorization_header auth url "GET" authargs in
          Httpclient.call (url ^ "?" ^ query) GET [header] ""
      | "POST" ->
          let body = "" in
          let header = Twitter.authorization_header auth url "POST" authargs in
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


let fns =
  schema.apis
  |> List.filter_map ~f:(fun (api : Swagger.api) ->
         api.operations
         |> List.head
         |> Option.map ~f:(fun (op : Swagger.operation) ->
                { pns = ["Twitter::" ^ twurl2name api.path]
                ; ins = []
                ; r = TAny
                ; f = API (call api.path op.httpMethod)
                ; p = auth_param :: List.map ~f:param2param op.parameters
                ; d = Base.Option.value ~default:"" op.summary
                ; ps = false
                ; dep = false } ) )
