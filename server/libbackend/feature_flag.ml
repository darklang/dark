open Core

open Types
open Types.RuntimeT

module Cookie = Cohttp.Cookie

let is_browser headers : bool =
  headers
  |> fun hs -> Cohttp.Header.get hs "user-agent"
  |> Option.value ~default:""
  |> String.is_substring ~substring:"Mozilla"

let session_name = "dark_session"

let make (str : string) : feature_flag =
  RealKey str

(* note, this should be safe to put in a cookie *)
let generate () : feature_flag =
  Util.random_string 42
  |> B64.encode
  |> make

let to_sql (ff: feature_flag) : string =
  ff
  |> feature_flag_to_yojson
  |> Yojson.Safe.to_string
  |> fun sql -> "'" ^ sql ^ "'"

let from_sql (sql: string) : feature_flag =
  sql
  |> Yojson.Safe.from_string
  |> feature_flag_of_yojson
  |> Result.ok_or_failwith


let analysis : feature_flag =
  Analysis

let to_session_string (ff: feature_flag) : string =
  match ff with
  | Analysis -> ""
  | RealKey str -> str

let select (id: id) (setting : int) (l: 'a or_blank) (r: 'a or_blank) ff : 'a or_blank =
  match ff with
  | Analysis ->
    if setting > 50
    then r
    else l
  | RealKey str ->
    let sum = str
              |> Batteries.String.to_list
              |> List.map ~f:Char.to_int
              |> List.reduce ~f:(+)
              |> Option.value ~default:0
              (* We want each feature flag to be different, but keyed
               * off the user. *)
              |> fun x -> x % id
              |> fun x -> x % 100
    in
    if setting < sum
    then l
    else r


let session_headers headers (ff: feature_flag) : Cookie.cookie list =
  if is_browser headers
  then
    (session_name, to_session_string ff)
    |> Cookie.Set_cookie_hdr.make
    |> Cookie.Set_cookie_hdr.serialize
    |> fun x -> [x]
  else
    []

let fingerprint_user ip headers : feature_flag =
  (* We want to do the absolute minimal fingerprinting to allow users
   * get roughly the same set of feature flags on each request, while
   * preserving user privacy. *)

  if is_browser headers
  then
    (* If they're a browser user, just use a session, and if they dont
     * have one, create a new one. *)
    let session = headers
                |> Cookie.Cookie_hdr.extract
                |> List.find ~f:(fun (n,_) -> n = session_name)
    in
    match session with
    | Some (_, value) -> value |> make
    | None -> generate ()

  else
    (* If they're an API user, fingerprint off as many stable headers as
     * possible (ignore things with dates, urls, etc). In the future,
     * give them a header with an ID that they can opt into as well. *)
    let usable = ["user-agent"; "accept-encoding"; "accept-language";
                  "keep-alive"; "connection"; "accept"] in
    headers
    |> Cohttp.Header.to_list
    |> List.filter ~f:(fun (k,v) -> List.mem ~equal:(=) usable k)
    |> List.map ~f:Tuple.T2.get2
    |> (@) [ip]
    |> String.concat
    |> Util.hash
    |> make


