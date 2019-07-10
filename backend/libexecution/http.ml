open Core_kernel
open Libcommon
module RT = Types.RuntimeT

(* Names are confusing, so to be clear:
 - `route`: refers to the name of the handler. It may have variable names in it. Mnemonic: routing table.
 - `request_path`: the actual string from the request. It is matched against the route
 - `path`: refers to both of the above.
 *)

let split_uri_path (path : string) : string list =
  let subs = String.split ~on:'/' path in
  List.filter ~f:(fun x -> String.length x > 0) subs


let route_to_postgres_pattern (route : string) : string =
  (* https://www.postgresql.org/docs/9.6/functions-matching.html *)
  route
  |> Util.string_replace "%" "\\%"
  |> Util.string_replace "_" "\\_"
  |> split_uri_path
  |> List.map ~f:(fun segment ->
         if String.is_prefix ~prefix:":" segment then "%" else segment )
  |> String.concat ~sep:"/"
  |> ( ^ ) "/"


let route_variable (route_segment : string) : string option =
  let prefix = ":" in
  if String.is_prefix ~prefix route_segment
  then Some (String.chop_prefix_exn ~prefix route_segment)
  else None


let route_variables (route : string) : string list =
  route |> split_uri_path |> List.filter_map ~f:route_variable


let bind_route_variables ~(route : string) (request_path : string) :
    (string * RT.dval) list option =
  let do_binding route path =
    (* assumes route length = path length *)
    List.zip_exn route path
    |> List.fold ~init:(Some []) ~f:(fun acc (r, p) ->
           Option.bind acc ~f:(fun acc ->
               match route_variable r with
               | Some rv ->
                   Some ((rv, Dval.dstr_of_string_exn p) :: acc)
               | None ->
                   (* Concretized match, or we were passed a Postgres wildcard
                    * and should treat this as a match
                    *
                    * Otherwise, this route/path do not match and should fail
                    * *)
                   if r = p || r = "%" then Some acc else None ) )
  in
  let split_route = split_uri_path route in
  let split_path = split_uri_path request_path in
  match Int.compare (List.length split_path) (List.length split_route) with
  | -1 ->
      (* Route *must* be the <= the length of path *)
      None
  | 0 ->
      (* If the route/path are the same length we can zip a binding down *)
      do_binding split_route split_path
  | 1 ->
      (* If the route is shorter than the path, AND the last segment of the route is
       * wild then we'll munge the path's extra segments into a single string such that
       * the lengths match and we can do a zip binding *)
      let last_route_segment = List.last split_route in
      if Option.is_some (last_route_segment |> Option.bind ~f:route_variable)
         || Option.value ~default:"" last_route_segment = "%"
      then
        let munged_path =
          let before, after =
            List.split_n split_path (List.length split_route - 1)
          in
          let after_str = String.concat ~sep:"/" after in
          List.append before [after_str]
        in
        do_binding split_route munged_path
      else None
  | _ ->
      Exception.internal
        "Built-in OCaml `Int.compare` returned something that's not {-1, 0, 1} -- can we page someone in France?"


let bind_route_variables_exn ~(route : string) (request_path : string) :
    (string * RT.dval) list =
  Option.value_exn
    ~message:"request/route mismatch"
    (bind_route_variables ~route request_path)


(* We say that a `path` matches a `route` iff. we could successfully run the
 * binding algorithm across it *)
let request_path_matches_route ~(route : string) (request_path : string) : bool
    =
  Option.is_some (bind_route_variables ~route request_path)


(* Postgres matches the provided path `/` with handler `/:a` due to
  * `/` matching `/%%` via LIKE logic`. This cleans this edge case from the
  * set.
  * *)
let filter_invalid_handler_matches
    ~(path : string) (handlers : RT.HandlerT.handler list) :
    RT.HandlerT.handler list =
  List.filter
    ~f:(fun h ->
      let route = Handler.event_name_for_exn h in
      request_path_matches_route ~route path )
    handlers


(* From left-to-right segment-wise, we say that concrete is more specific
 * than wild is more specific than empty *)
let rec compare_route_specificity (left : string list) (right : string list) :
    int =
  let is_wild s = String.is_prefix ~prefix:":" s in
  let is_concrete s = not (is_wild s) in
  match (left, right) with
  | [], [] ->
      0
  | _l, [] ->
      1
  | [], _r ->
      -1
  | l :: _, r :: _ when is_concrete l && is_wild r ->
      1
  | l :: _, r :: _ when is_wild l && is_concrete r ->
      -1
  | _ :: ls, _ :: rs ->
      compare_route_specificity ls rs


let compare_page_route_specificity
    (left : RT.HandlerT.handler) (right : RT.HandlerT.handler) : int =
  compare_route_specificity
    (left |> Handler.event_name_for_exn |> split_uri_path)
    (right |> Handler.event_name_for_exn |> split_uri_path)


(* Takes a list of handlers that match a request's path, and filters the list
 * down to the list of handlers that match the request most specifically
 *
 * It looks purely at the handler's definition for its specificity relation.
 *
 * *)
let filter_matching_handlers_by_specificity (pages : RT.HandlerT.handler list)
    : RT.HandlerT.handler list =
  let ordered_pages =
    pages
    |> List.sort ~compare:(fun left right ->
           compare_page_route_specificity left right )
    (* we intentionally sort in least specific to most specific order
     * because it's much easier to define orderings from 'least-to-most'.
     *
     * as we want the most specific, we then reverse this list.
     *
     * we could do invert the relationship (ie. change the 1's to -1's and vice versa
     * in our comparison function) but my brain really didn't like that and found
     * it confusing.
     *
     *)
    |> List.rev
  in
  (* ordered_pages is ordered most-specific to least-specific, so pluck the
     * most specific and return it along with all others of its specificity *)
  match ordered_pages with
  | [] ->
      []
  | [a] ->
      [a]
  | a :: rest ->
      let same_specificity =
        List.filter
          ~f:(fun b ->
            let comparison = compare_page_route_specificity a b in
            comparison = 0 )
          rest
      in
      a :: same_specificity


let filter_matching_handlers
    ~(path : string) (pages : RT.HandlerT.handler list) :
    RT.HandlerT.handler list =
  pages
  |> List.filter ~f:Handler.is_complete
  |> filter_invalid_handler_matches ~path
  |> filter_matching_handlers_by_specificity
