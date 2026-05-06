/// Translates a Dark route literal (e.g. `/user/:id`) into a SQL LIKE pattern
/// used by the toplevel index when looking up handlers from the DB.
module LibCloud.Routing

open FSharpx
open Prelude

let splitUriPath (path : string) : string array =
  let subs = String.splitChar [| '/' |] path
  Array.filter (fun x -> String.length x > 0) subs

let routeToPostgresPattern (route : string) : string =
  // https://www.postgresql.org/docs/9.6/functions-matching.html
  route
  |> String.collect (fun str ->
    match str with
    | '%' -> "\\%"
    | '_' -> "\\_"
    | other -> string other)
  |> splitUriPath
  |> Array.map (fun segment ->
    if String.startsWith ":" segment then "%" else segment)
  |> String.concat "/"
  |> (+) "/"
