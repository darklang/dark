/// Maps user-defined routes (in their Http Handlers) to Sqlite patterns
/// for filtering when BWD gets a request, to choose the 'right',
/// most appropriate handler.
///
/// In charge of extracting out any URL parameters along the way
/// (i.e. if `/hello/:name` is defined, and you go to `/hello/john`)
module LibCloud.Routing

open FSharp.Control.Tasks
open System.Threading.Tasks

open FSharpx
open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser

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

let routeVariable (routeSegment : string) : string option =
  if String.startsWith ":" routeSegment then
    Some(String.remove' 0 1 routeSegment)
  else
    None

/// Extracts variables from a route
///
/// e.g. from "/user/:userid/card/:cardid", it returns ["userid"; "cardid"]
let routeVariables (route : string) : string list =
  route |> splitUriPath |> Array.toList |> List.filterMap routeVariable


let routeInputVars
  (route : string)
  (requestPath : string)
  : Option<List<string * RT.Dval>> =
  let doBinding routeParts pathParts =
    // We assume (handled elsewhere) that route length = requestPath length
    List.zip routeParts pathParts
    |> List.rev
    |> List.fold
      (fun acc (r, p) ->
        Option.bind
          (fun acc ->
            match routeVariable r with
            | Some rv -> Some((rv, RT.DString p) :: acc)
            | None ->
              // Concretized match, or we were passed a Sqlite wildcard
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
  elif splitRequestPath.Length = splitRoute.Length then
    // If the route/path are the same length we can zip a binding down
    doBinding splitRoute splitRequestPath
  else
    // If the route is shorter than the path, AND the last segment of the route is
    // wild then we'll munge the path's extra segments into a single string such that
    // the lengths match and we can do a zip binding
    let lastRouteSegment = List.tryLast splitRoute

    if
      Option.isSome (lastRouteSegment |> Option.bind routeVariable)
      || Option.defaultValue "" lastRouteSegment = "%"
    then
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


// Postgres matches the provided path `/` with handler `/:a` due to
// `/` matching `/%%` via LIKE logic`. This cleans this edge case from the
// set. TODO see if this is true with Sqlite
let filterInvalidHandlerMatches
  (path : string)
  (handlers : List<PT.Handler.T>)
  : List<PT.Handler.T> =
  List.filter
    (fun h ->
      let route = PTParser.Handler.Spec.toName h.spec
      requestPathMatchesRoute route path)
    handlers


/// From left-to-right segment-wise, we say:
/// - concrete is more specific than wild
/// - wild is more specific than empty
let rec compareRouteSpecificity (left : string list) (right : string list) : int =
  let isWild s = String.startsWith ":" s
  let isConcrete s = not (isWild s)

  match (left, right) with
  | [], [] -> 0
  | _l, [] -> 1
  | [], _r -> -1
  | l :: _, r :: _ when isConcrete l && isWild r -> 1
  | l :: _, r :: _ when isWild l && isConcrete r -> -1
  | _ :: ls, _ :: rs -> compareRouteSpecificity ls rs

let comparePageRouteSpecificity (left : PT.Handler.T) (right : PT.Handler.T) : int =
  compareRouteSpecificity
    (PTParser.Handler.Spec.toName left.spec |> splitUriPath |> Array.toList)
    (PTParser.Handler.Spec.toName right.spec |> splitUriPath |> Array.toList)


/// Takes a list of handlers that match a request's path, and filters the list
/// down to the list of handlers that match the request most specifically.
/// It looks purely at the handler's definition for its specificity relation.
let filterMatchingHandlersBySpecificity
  (pages : List<PT.Handler.T>)
  : List<PT.Handler.T> =
  let orderedPages =
    pages
    |> List.sortWith (fun left right -> comparePageRouteSpecificity left right)
    // we intentionally sort in least specific to most specific order
    // because it's much easier to define orderings from 'least-to-most'.
    //
    // as we want the most specific, we then reverse this list.
    //
    // we could do invert the relationship (ie. change the 1's to -1's and vice versa
    // in our comparison function) but my brain really didn't like that and found
    // it confusing.
    |> List.rev

  // orderedPages is ordered most-specific to least-specific, so pluck the
  // most specific and return it along with all others of its specificity
  match orderedPages with
  | [] -> []
  | [ a ] -> [ a ]
  | a :: rest ->
    let sameSpecificity =
      List.filter (fun b -> comparePageRouteSpecificity a b = 0) rest

    a :: sameSpecificity


let filterMatchingHandlers
  (path : string)
  (pages : List<PT.Handler.T>)
  : List<PT.Handler.T> =
  pages
  |> List.filter (fun h -> PTParser.Handler.Spec.isComplete h.spec)
  |> filterInvalidHandlerMatches path
  |> filterMatchingHandlersBySpecificity


let sanitizeUrlPath (path : string) : string =
  path
  |> FsRegEx.replace "//+" "/"
  |> String.trimEnd [| '/' |]
  |> fun str -> if str = "" then "/" else str
