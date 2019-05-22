open Core_kernel
open Libexecution
module RTT = Types.RuntimeT
module RT = Runtime

type dval = RTT.dval

type dval_map = RTT.dval_map

module DvalMap = RTT.DvalMap

let pct_encode_key = Uri.pct_encode ~component:`Userinfo

let pct_encode = Uri.pct_encode ~component:`Userinfo

let collect_params (params : (string * string) list) : string =
  params
  (* Collecting parameters #1 - percent encoding *)
  |> List.map ~f:(fun (k, v) -> (pct_encode_key k, pct_encode v))
  (* Collecting parameters #2 - sorted*)
  (* TODO sort lexicographically? *)
  |> List.sort ~compare:(fun (a, _) (b, _) -> compare a b)
  (* Collecting parameters #3-6 - combining with a = *)
  |> List.map ~f:(fun (a, b) -> a ^ "=" ^ b)
  (* Collecting parameters #7 - combining all paramss with a & *)
  |> String.concat ~sep:"&"


(* TODO: no body? *)
let sign consumer_secret access_token_secret uri verb params =
  (* https://dev.twitter.com/oauth/overview/creating-signatures *)
  
  (* Collecting parameters *)
  let collected = collect_params params in
  (* Creating the signature base string - #1-5 *)
  let output =
    String.uppercase verb ^ "&" ^ pct_encode uri ^ "&" ^ pct_encode collected
  in
  (* Getting a signing key - #1-5*)
  let signing_key =
    pct_encode consumer_secret ^ "&" ^ pct_encode access_token_secret
  in
  output
  |> Cstruct.of_string
  |> Nocrypto.Hash.SHA1.hmac ~key:(Cstruct.of_string signing_key)
  |> Cstruct.to_string
  |> B64.encode


let nonce () = Util.random_string 42 |> Mock.get_string "nonce"

let ts () =
  Unix.gettimeofday () |> Float.to_int |> Int.to_string |> Mock.get_string "ts"


let param_to_string (key, value) : string =
  pct_encode key ^ "=\"" ^ pct_encode value ^ "\""


let oauth_params (secret : Secret.twitter_secret) url verb (args : dval_map) :
    (string * string) list =
  let initial_params =
    [ ("oauth_consumer_key", secret.consumer_key)
    ; ("oauth_nonce", nonce ())
    ; ("oauth_signature_method", "HMAC-SHA1")
    ; ("oauth_version", "1.0")
    ; ("oauth_timestamp", ts ())
    ; ("oauth_token", secret.access_token) ]
  in
  let argparams =
    args
    |> DvalMap.filter ~f:(( <> ) Types.RuntimeT.DNull)
    |> DvalMap.map ~f:(fun v -> Dval.to_url_string_exn v)
    |> DvalMap.to_list
  in
  let signature =
    sign
      secret.consumer_secret
      secret.access_token_secret
      url
      verb
      (List.append initial_params argparams)
  in
  [("oauth_signature", signature)]
  |> List.append initial_params
  |> List.sort ~compare:(fun (a, _) (b, _) -> compare a b)


let oauth_header secret url verb (args : dval_map) : string =
  oauth_params secret url verb args
  |> List.map ~f:param_to_string
  |> String.concat ~sep:", "
  |> ( ^ ) "OAuth "


let authorization_header url verb (args : dval_map) : string * string =
  ("Authorization", oauth_header Secret.twitter url verb args)


let call (endpoint : string) (verb : Httpclient.verb) (args : dval_map) : dval
    =
  let prefix = "https://api.twitter.com" in
  let url = prefix ^ endpoint in
  let result =
    match verb with
    | GET ->
        let query = Dval.to_form_encoding (DObj args) in
        let header = authorization_header url "GET" args in
        Httpclient.call (url ^ "?" ^ query) verb [header] ""
    | POST ->
        let body = "" in
        let header = authorization_header url "POST" args in
        Httpclient.call url verb [header] body
    | _ ->
        Exception.internal "not implemented yet"
  in
  Dval.of_unknown_json_v0 result


let get (url : string) (args : dval_map) : dval = call url GET args

let post (url : string) (args : dval_map) : dval = call url POST args
