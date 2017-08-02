(* We don't want to hit API limits, so we'll cache everything on disk permanently. *)
(* If we ever call it again, we'll use the on-disk version first *)
open Core

type dval = Runtime.dval
module ObjMap = Runtime.ObjMap

(* See test.sh for how to get this token *)
let bearer =
  "AAAAAAAAAAAAAAAAAAAAAJfh1gAAAAAAazXXwsaMuN"
  ^ "yK2a8ZsTGVX32KdXY%3DzKh8JxqSB8tkLKzVgEY3"
  ^ "Pagi8le92ZQE5PXTqimhtVRqyjeWRz"


let rec dval2query (v: dval) : string =
  match v with
  | DObj obj -> obj
                |> ObjMap.fold
                  ~init:[]
                  ~f:(fun ~key ~data l ->
                      (key ^ "=" ^ (dval2query data)) :: l)
                |> String.concat ~sep:"&"
  | DStr str -> str
  | _ -> Runtime.to_string v

let call (endpoint: string) (verb: Http.verb) (argument: dval) : dval =
  let prefix = "https://api.twitter.com/1.1/" in
  let headers = ["Authorization: Bearer " ^ bearer] in
  let result =
    match verb with
    | GET ->
      let query_string = dval2query argument in
      let url = prefix ^ endpoint ^ "?" ^ query_string in
      Http.call url verb headers ""
    | POST ->
      let body = Runtime.dval2json argument in
      let url = prefix ^ endpoint in
      Http.call url verb headers body
  in
  Runtime.json2dval result

let get (url: string) (arg: dval) : dval =
  call url GET arg

let post (url: string) (arg: dval) : dval =
  call url POST arg
