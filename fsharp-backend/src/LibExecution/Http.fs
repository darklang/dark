module LibExecution.Http

open FSharpx

module RT = LibExecution.RuntimeTypes

// Functions related to the HTTP server

// Names are confusing, so to be clear:
//  - `route`: refers to the name of the handler. It may have variable names in it. Mnemonic: routing table.
//  - `requestPath`: the actual string from the request. It is matched against the route
//  - `path`: refers to both of the above.

let splitUriPath (path : string) : string array =
  let subs = String.splitChar [| '/' |] path
  Array.filter (fun x -> String.length x > 0) subs

let routeToPostgresPattern (route : string) : string =

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

let routeVariable (routeSegment : string) : string option =
  if String.startsWith ":" routeSegment then
    Some(String.remove' 0 1 routeSegment)
  else
    None

let routeVariables (route : string) : string list =
  route |> splitUriPath |> Array.toList |> List.choose routeVariable



let routeInputVars (route : string)
                   (requestPath : string)
                   : Option<List<string * RT.Dval>> =
  let doBinding route path =
    // We know route length = requestPath length
    List.zip route path
    |> List.fold (fun acc (r, p) ->
         Option.bind (fun acc ->
           match routeVariable r with
           | Some rv -> Some((rv, RT.DStr p) :: acc)
           | None ->
               // Concretized match, or we were passed a Postgres wildcard
               // and should treat this as a match.
               // Otherwise, this route/path do not match and should fail
               if r = p || r = "%" then Some acc else None) acc) (Some [])

  let splitRoute = splitUriPath route |> Array.toList
  let splitRequestPath = splitUriPath requestPath |> Array.toList
  if splitRoute.Length > splitRequestPath.Length then
    // Can't match. Route *must* be the <= the length of path
    None
  else if splitRequestPath.Length = splitRoute.Length then
    // If the route/path are the same length we can zip a binding down
    doBinding splitRoute splitRequestPath
  else
    // If the route is shorter than the path, AND the last segment of the route is
    // wild then we'll munge the path's extra segments into a single string such that
    // the lengths match and we can do a zip binding
    let lastRouteSegment = List.tryLast splitRoute
    if Option.isSome (lastRouteSegment |> Option.bind routeVariable)
       || Option.defaultValue "" lastRouteSegment = "%" then
      let mungedPath =
        let before, after =
          List.splitAt (List.length splitRoute - 1) splitRequestPath

        let afterStr = String.concat "/" after
        List.append before [ afterStr ]

      doBinding splitRoute mungedPath
    else
      None
