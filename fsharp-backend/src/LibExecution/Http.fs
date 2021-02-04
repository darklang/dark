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
  |> String.collect
       (function
       | '%' -> "\\%"
       | '_' -> "\\_"
       | other -> other.ToString())
  |> splitUriPath
  |> Array.map
       (fun segment -> if String.startsWith ":" segment then "%" else segment)
  |> String.concat "/"
  |> (+) "/"

let routeVariable (routeSegment : string) : string option =
  if String.startsWith ":" routeSegment then
    Some(String.remove' 0 1 routeSegment)
  else
    None

let routeVariables (route : string) : string list =
  route |> splitUriPath |> Array.toList |> List.choose routeVariable



let routeInputVars
  (route : string)
  (requestPath : string)
  : Option<List<string * RT.Dval>> =
  let doBinding route path =
    // We know route length = requestPath length
    List.zip route path
    |> List.fold
         (fun acc (r, p) ->
           Option.bind
             (fun acc ->
               match routeVariable r with
               | Some rv -> Some((rv, RT.DStr p) :: acc)
               | None ->
                   // Concretized match, or we were passed a Postgres wildcard
                   // and should treat this as a match.
                   // Otherwise, this route/path do not match and should fail
                   if r = p || r = "%" then Some acc else None)
             acc)
         (Some [])

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

// We say that a `path` matches a `route` iff. we could successfully run the
// binding algorithm across it
let requestPathMatchesRoute (route : string) (requestPath : string) : bool =
  Option.isSome (routeInputVars route requestPath)


// (* Postgres matches the provided path `/` with handler `/:a` due to
//   * `/` matching `/%%` via LIKE logic`. This cleans this edge case from the
//   * set.
//   * *)
// let filter_invalid_handler_matches
//     ~(path : string) (handlers : RT.HandlerT.handler list) :
//     RT.HandlerT.handler list =
//   List.filter
//     ~f:(fun h ->
//       let route = Handler.event_name_for_exn h in
//       request_path_matches_route ~route path)
//     handlers
//
//
// (* From left-to-right segment-wise, we say that concrete is more specific
//  * than wild is more specific than empty *)
// let rec compare_route_specificity (left : string list) (right : string list) :
//     int =
//   let is_wild s = String.is_prefix ~prefix:":" s in
//   let is_concrete s = not (is_wild s) in
//   match (left, right) with
//   | [], [] ->
//       0
//   | _l, [] ->
//       1
//   | [], _r ->
//       -1
//   | l :: _, r :: _ when is_concrete l && is_wild r ->
//       1
//   | l :: _, r :: _ when is_wild l && is_concrete r ->
//       -1
//   | _ :: ls, _ :: rs ->
//       compare_route_specificity ls rs
//
//
// let compare_page_route_specificity
//     (left : RT.HandlerT.handler) (right : RT.HandlerT.handler) : int =
//   compare_route_specificity
//     (left |> Handler.event_name_for_exn |> split_uri_path)
//     (right |> Handler.event_name_for_exn |> split_uri_path)
//
//
// (* Takes a list of handlers that match a request's path, and filters the list
//  * down to the list of handlers that match the request most specifically
//  *
//  * It looks purely at the handler's definition for its specificity relation.
//  *
//  * *)
// let filter_matching_handlers_by_specificity (pages : RT.HandlerT.handler list) :
//     RT.HandlerT.handler list =
//   let ordered_pages =
//     pages
//     |> List.sort ~compare:(fun left right ->
//            compare_page_route_specificity left right)
//     (* we intentionally sort in least specific to most specific order
//      * because it's much easier to define orderings from 'least-to-most'.
//      *
//      * as we want the most specific, we then reverse this list.
//      *
//      * we could do invert the relationship (ie. change the 1's to -1's and vice versa
//      * in our comparison function) but my brain really didn't like that and found
//      * it confusing.
//      *
//      *)
//     |> List.rev
//   in
//   (* ordered_pages is ordered most-specific to least-specific, so pluck the
//      * most specific and return it along with all others of its specificity *)
//   match ordered_pages with
//   | [] ->
//       []
//   | [a] ->
//       [a]
//   | a :: rest ->
//       let same_specificity =
//         List.filter
//           ~f:(fun b ->
//             let comparison = compare_page_route_specificity a b in
//             comparison = 0)
//           rest
//       in
//       a :: same_specificity
//
//
// let filter_matching_handlers ~(path : string) (pages : RT.HandlerT.handler list)
//     : RT.HandlerT.handler list =
//   pages
//   |> List.filter ~f:Handler.is_complete
//   |> filter_invalid_handler_matches ~path
//   |> filter_matching_handlers_by_specificity
//
