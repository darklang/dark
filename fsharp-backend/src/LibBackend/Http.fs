module LibBackend.Http

open FSharpx

// Functions related to the HTTP server

// Names are confusing, so to be clear:
//  - `route`: refers to the name of the handler. It may have variable names in it. Mnemonic: routing table.
//  - `request_path`: the actual string from the request. It is matched against the route
//  - `path`: refers to both of the above.


let routeToPostgresPattern (route : string) : string =
  let splitUriPath (path : string) : string array =
    let subs = String.splitChar [| '/' |] path
    Array.filter (fun x -> String.length x > 0) subs

  // https://www.postgresql.org/docs/9.6/functions-matching.html
  route
  |> String.collect (function
       | '%' -> "\\%"
       | '_' -> "\\_"
       | other -> other.ToString())
  |> splitUriPath
  |> Array.map (fun segment -> if String.startsWith ":" segment then "%" else segment)
  |> String.concat "/"
  |> (+) "/"
