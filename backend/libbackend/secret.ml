(* Keys for: https://apps.twitter.com/app/14082455 *)
let twitter_bearer_token = "" ^ "" ^ ""

let twitter_CONSUMER_KEY = ""

let twitter_CONSUMER_SECRET = ""

(* Secrets for @paulbiggar and this app *)
let paulbiggar_ACCESS_TOKEN = ""

let paulbiggar_ACCESS_TOKEN_SECRET = ""

type twitter_secret =
  { consumer_key : string
  ; consumer_secret : string
  ; access_token : string
  ; access_token_secret : string }

let twitter =
  { consumer_key = twitter_CONSUMER_KEY
  ; consumer_secret = twitter_CONSUMER_SECRET
  ; access_token = paulbiggar_ACCESS_TOKEN
  ; access_token_secret = paulbiggar_ACCESS_TOKEN_SECRET }
