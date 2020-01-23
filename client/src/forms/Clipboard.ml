open Prelude

(* Dark *)
module P = Pointer
module TL = Toplevel

let setData ((text, json) : clipboardContents) (e : clipboardEvent) =
  e##clipboardData##setData "text/plain" text ;
  Option.map json ~f:(fun data ->
      let data = Json.stringify data in
      e##clipboardData##setData "application/json" data)
  |> ignore ;
  e##preventDefault ()


let getData (e : clipboardEvent) : clipboardContents =
  let json =
    let json = e##clipboardData##getData "application/json" in
    if json = ""
    then None
    else
      try Some (Json.parseOrRaise json)
      with _ ->
        reportError "could not parse clipboard data" json ;
        None
  in
  (e##clipboardData##getData "text/plain", json)
