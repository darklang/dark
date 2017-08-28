open Core

module RT = Runtime


(* We don't want to hit API limits, so we'll cache everything on disk
   permanently. *)
(* If we ever call it again, we'll use the on-disk version first *)

type dval = Runtime.dval
type dval_map = Runtime.dval_map
module DvalMap = Runtime.DvalMap

let pct_encode_key = Uri.pct_encode ~component:`Userinfo
let pct_encode = Uri.pct_encode ~component:`Userinfo

let collect_params (params : (string * string) list) : string =
  params
  (* Collecting parameters #1 - percent encoding *)
  |> List.map ~f:(fun (k,v) -> (pct_encode_key k, pct_encode v))

  (* Collecting parameters #2 - sorted*)
    (* TODO sort lexicographically? *)
  |> List.sort ~cmp:(fun (a,_) (b,_) -> compare a b)


  (* Collecting parameters #3-6 - combining with a = *)
  |> List.map ~f:(fun (a,b) -> a ^ "=" ^ b)

  (* Collecting parameters #7 - combining all paramss with a & *)
  |> String.concat ~sep:"&"

(* TODO: no body? *)
let sign consumer_secret access_token_secret uri verb params =
  (* https://dev.twitter.com/oauth/overview/creating-signatures *)

  (* Collecting parameters *)
  let collected = collect_params params in

  (* Creating the signature base string - #1-5 *)
  let output =
    (String.uppercase verb)
    ^ "&" ^ (pct_encode uri)
    ^ "&" ^ (pct_encode collected) in

  (* Getting a signing key - #1-5*)
  let signing_key = (pct_encode consumer_secret)
                    ^ "&"
                    ^ (pct_encode access_token_secret) in
  output
  |> Cstruct.of_string
  |> Nocrypto.Hash.SHA1.hmac ~key:(Cstruct.of_string signing_key)
  |> Cstruct.to_string
  |> B64.encode

let nonce () = Util.random_string 42 |> Mock.get_string "nonce"

let ts () = Unix.gettimeofday () |> Float.to_int |> Int.to_string |> Mock.get_string "ts"

let param_to_string (key, value) : string =
  (pct_encode key) ^ "=\"" ^ (pct_encode value) ^ "\""


let oauth_params (secret: Secret.twitter_secret) url verb (args : dval_map) : (string * string) list =
  let initial_params =
    [ "oauth_consumer_key", secret.consumer_key
    ; "oauth_nonce", nonce ()
    ; "oauth_signature_method", "HMAC-SHA1"
    ; "oauth_version", "1.0"
    ; "oauth_timestamp", ts ()
    ; "oauth_token", secret.access_token
    ] in
  let argparams = args
                  |> DvalMap.map ~f:RT.to_string
                  |> DvalMap.to_alist
  in
  let signature =
    sign secret.consumer_secret secret.access_token_secret
      url verb (List.append initial_params argparams) in

  [("oauth_signature", signature)]
  |> List.append initial_params
  |> List.sort ~cmp:(fun (a,_) (b,_) -> compare a b)

let oauth_header secret url verb (args: dval_map) : string =
  oauth_params secret url verb args
  |> List.map ~f:param_to_string
  |> String.concat ~sep:", "
  |> (^) "OAuth "

let authorization_header url verb (args: dval_map) =
  "Authorization:" ^ (oauth_header Secret.twitter url verb args)


let rec dvalmap2query (args: dval_map) : string =
  args
  |> DvalMap.fold
    ~init:[]
    ~f:(fun ~key ~data l ->
        if data = Runtime.DIncomplete
        then Exception.raise "Incomplete computation"
        else if data = Runtime.DNull then l
        else (key ^ "=" ^ (Runtime.to_string data)) :: l)
  |> String.concat ~sep:"&"

let call (endpoint: string) (verb: Http.verb) (args: dval_map) : dval =
  let prefix = "https://api.twitter.com" in
  let url = prefix ^ endpoint in
  let result =
    match verb with
    | GET ->
      let query = dvalmap2query args in
      let header = (authorization_header url "GET" args) in
      Http.call (url ^ "?" ^ query) verb [header] ""
    | POST ->
      let body = "" in
      let header = (authorization_header url "POST" args) in
      Http.call url verb [header] body
  in
  result |> Yojson.Safe.from_string |> Runtime.dval_of_yojson_

let get (url: string) (args: dval_map) : dval =
  call url GET args

let post (url: string) (args: dval_map) : dval =
  call url POST args
