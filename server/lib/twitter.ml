open Core


(* We don't want to hit API limits, so we'll cache everything on disk
   permanently. *)
(* If we ever call it again, we'll use the on-disk version first *)

type dval = Runtime.dval
type arg_map = Runtime.arg_map
module ArgMap = Runtime.ArgMap

(* See test.sh for how to get this token *)
let bearer =
  "AAAAAAAAAAAAAAAAAAAAAJfh1gAAAAAAazXXwsaMuN"
  ^ "yK2a8ZsTGVX32KdXY%3DzKh8JxqSB8tkLKzVgEY3"
  ^ "Pagi8le92ZQE5PXTqimhtVRqyjeWRz"


let rec argmap2query (args: arg_map) : string =
  args
  |> ArgMap.fold
    ~init:[]
    ~f:(fun ~key ~data l ->
        if data = Runtime.DIncomplete
        then Exception.raise "Incomplete computation"
        else
          (key ^ "=" ^ (Runtime.to_string data)) :: l)
  |> String.concat ~sep:"&"

let call (endpoint: string) (verb: Http.verb) (args: arg_map) : dval =
  let prefix = "https://api.twitter.com/1.1/" in
  let headers = ["Authorization: Bearer " ^ bearer] in
  let result =
    match verb with
    | GET ->
      let query = argmap2query args in
      let url = prefix ^ endpoint ^ "?" ^ query in
      Http.call url verb headers ""
    | POST ->
      let body = "" in
      let url = prefix ^ endpoint in
      Http.call url verb headers body
  in
  Runtime.json2dval result

let get (url: string) (args: arg_map) : dval =
  call url GET args

let post (url: string) (args: arg_map) : dval =
  call url POST args
