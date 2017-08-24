open Core


(* We don't want to hit API limits, so we'll cache everything on disk
   permanently. *)
(* If we ever call it again, we'll use the on-disk version first *)

type dval = Runtime.dval
type dval_map = Runtime.dval_map
module DvalMap = Runtime.DvalMap

(* See test.sh for how to get this token *)
let bearer =
  "AAAAAAAAAAAAAAAAAAAAAJfh1gAAAAAAazXXwsaMuN"
  ^ "yK2a8ZsTGVX32KdXY%3DzKh8JxqSB8tkLKzVgEY3"
  ^ "Pagi8le92ZQE5PXTqimhtVRqyjeWRz"


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
  let headers = ["Authorization: Bearer " ^ bearer] in
  let result =
    match verb with
    | GET ->
      let query = dvalmap2query args in
      let url = prefix ^ endpoint ^ "?" ^ query in
      Http.call url verb headers ""
    | POST ->
      let body = "" in
      let url = prefix ^ endpoint in
      Http.call url verb headers body
  in
  result |> Yojson.Safe.from_string |> Runtime.dval_of_yojson_

let get (url: string) (args: dval_map) : dval =
  call url GET args

let post (url: string) (args: dval_map) : dval =
  call url POST args
