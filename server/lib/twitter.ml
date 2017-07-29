(* We don't want to hit API limits, so we'll cache everything on disk permanently. *)
(* If we ever call it again, we'll use the on-disk version first *)

type dval = Runtime.dval

(* See test.sh for how to get this token *)
let bearer =
  "AAAAAAAAAAAAAAAAAAAAAJfh1gAAAAAAazXXwsaMuN"
  ^ "yK2a8ZsTGVX32KdXY%3DzKh8JxqSB8tkLKzVgEY3"
  ^ "Pagi8le92ZQE5PXTqimhtVRqyjeWRz"

let json2dval (json : string) = Runtime.DStr "test"
let dval2query (v: dval) : string = ""
let dval2json (v: dval) : string = "{}"

let call (endpoint: string) (verb: Http.verb) (argument: dval) =
  let prefix = "https://api.twitter.com/1.1/" in
  let headers = ["Authorization: Bearer " ^ bearer] in
  match verb with
  | GET ->
    let query_string = dval2query argument in
    let url = prefix ^ endpoint ^ query_string in
    Http.call url verb headers ""
  | POST ->
    let body = dval2json argument in
    let url = prefix ^ endpoint in
    Http.call url verb headers body

let get (url: string) (arg: dval) : dval =
  DStr (call url GET arg)

let post (url: string) (arg: dval) : dval =
  DStr (call url POST arg)

