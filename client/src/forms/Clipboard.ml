open Prelude

(* Dark *)
module P = Pointer
module TL = Toplevel

let setData (data : clipboardContents) (e : clipboardEvent) =
  match data with
  | `Text text ->
      e##clipboardData##setData "text/plain" text ;
      e##preventDefault ()
  | `Json json ->
      let data = Json.stringify json in
      e##clipboardData##setData "application/json" data ;
      e##preventDefault ()
  | `None ->
      ()


let getData (e : clipboardEvent) : clipboardContents =
  let json = e##clipboardData##getData "application/json" in
  if json <> ""
  then (
    try `Json (Json.parseOrRaise json)
    with _ ->
      reportError "could not parse clipboard data" json ;
      `None )
  else
    let text = e##clipboardData##getData "text/plain" in
    if text <> "" then `Text text else `None
