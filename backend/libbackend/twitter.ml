open Core_kernel

type auth =
  { consumer_key : string
  ; consumer_secret : string
  ; access_token : string
  ; access_token_secret : string }

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


let nonce () = Libexecution.Util.random_string 42 |> Mock.get_string "nonce"

let ts () =
  Unix.gettimeofday () |> Float.to_int |> Int.to_string |> Mock.get_string "ts"


let param_to_string (key, value) : string =
  pct_encode key ^ "=\"" ^ pct_encode value ^ "\""


let oauth_params (auth : auth) url verb (args : (string * string) list) :
    (string * string) list =
  let initial_params =
    [ ("oauth_consumer_key", auth.consumer_key)
    ; ("oauth_nonce", nonce ())
    ; ("oauth_signature_method", "HMAC-SHA1")
    ; ("oauth_version", "1.0")
    ; ("oauth_timestamp", ts ())
    ; ("oauth_token", auth.access_token) ]
  in
  let signature =
    sign
      auth.consumer_secret
      auth.access_token_secret
      url
      verb
      (List.append initial_params args)
  in
  [("oauth_signature", signature)]
  |> List.append initial_params
  |> List.sort ~compare:(fun (a, _) (b, _) -> compare a b)


let oauth_header secret url verb (args : (string * string) list) : string =
  oauth_params secret url verb args
  |> List.map ~f:param_to_string
  |> String.concat ~sep:", "
  |> ( ^ ) "OAuth "


let authorization_header (auth : auth) url verb (args : (string * string) list)
    : string * string =
  ("Authorization", oauth_header auth url verb args)
