(* We don't want to hit API limits, so we'll cache everything on disk permanently. *)
(* If we ever call it again, we'll use the on-disk version first *)

open Lwt
open Cohttp
open Cohttp_lwt_unix

type dval = Runtime.dval

type verb = GET | POST

(* See test.sh for how to get this token *)
let bearer =
  "AAAAAAAAAAAAAAAAAAAAAJfh1gAAAAAAazXXwsaMuN"
  ^ "yK2a8ZsTGVX32KdXY%3DzKh8JxqSB8tkLKzVgEY3"
  ^ "Pagi8le92ZQE5PXTqimhtVRqyjeWRz"

let headers =
  let h = Header.init () in
  Header.add h "Authorization" ("Bearer " ^ bearer)


let respond ((resp, body) : (Cohttp_lwt.Response.t * Cohttp_lwt_body.t)) =
  let code = resp |> Response.status |> Code.code_of_status in
  match code with
  | 200 ->
    body |> Cohttp_lwt_body.to_string >|=
    (fun body ->
       body
       |> Yojson.Safe.from_string
       |> Yojson.Safe.pretty_to_string ~std:true)
  | code ->
    Printf.printf "Response code: %d\n" code;
    Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
    body |> Cohttp_lwt_body.to_string >|=
    (fun body ->
       Printf.printf "Body of length: %d\n" (String.length body);
       Printf.printf "Body %s\n" body;
       failwith "bad http response code";
       body
    )


let json2dval (json : string) = Runtime.DStr "test"
let dval2query (v: dval) : string = ""
let dval2json (v: dval) : string = "{}"


let call (endpoint: string) (verb: verb) (argument: dval) : string =
  let prefix = "https://api.twitter.com/1.1/" in
  let fn =
    match verb with
    | GET ->
      let query_string = dval2query argument in
      let url = prefix ^ endpoint ^ query_string in
      Client.get ~headers (Uri.of_string url)
    | POST ->
      let body = Cohttp_lwt_body.to_string (dval2json argument) in
      let url = prefix ^ endpoint in
      Client.post ~body ~headers (Uri.of_string url)
  in
  fn >>= respond

let make_call (url: string) (arg: dval) : dval =
  let result = Lwt_main.run (call url verb arg) in
  json2dval result

let get (url: string) (arg: dval) : dval =
  make_call url GET arg

let post (url: string) (arg: dval) : dval =
  make_call url POST arg






(* GET https://api.twitter.com/1.1/users/lookup.json?screen_name=paulbiggar,twitter *)
