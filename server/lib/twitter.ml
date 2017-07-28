(* We don't want to hit API limits, so we'll cache everything on disk permanently. *)
(* If we ever call it again, we'll use the on-disk version first *)

open Lwt
open Cohttp
open Cohttp_lwt_unix

let call_api =
  let bearer = "AAAAAAAAAAAAAAAAAAAAAJfh1gAAAAAAazXXwsaMuNyK2a8ZsTGVX32KdXY%3DzKh8JxqSB8tkLKzVgEY3Pagi8le92ZQE5PXTqimhtVRqyjeWRz" in
  let headers = Header.init () in
  let headers = Header.add headers "Authorization" ("Bearer " ^ bearer) in
  let url = "https://api.twitter.com/1.1/search/tweets.json?q=%23archaeology" in
  Client.get ~headers (Uri.of_string url) >>=
    fun (resp, body) ->
       let code = resp |> Response.status |> Code.code_of_status in
       Printf.printf "Response code: %d\n" code;
       Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);

       body |> Cohttp_lwt_body.to_string >|=
       (fun body ->
          Printf.printf "Body of length: %d\n" (String.length body);
          Printf.printf "Body %s\n" body;
          body
          |> Yojson.Safe.from_string
          |> Yojson.Safe.pretty_to_string ~std:true
       )

let call : unit =
  let body = Lwt_main.run call_api in
  print_endline ("Received body\n" ^ body)


(* GET https://api.twitter.com/1.1/users/lookup.json?screen_name=paulbiggar,twitter *)
