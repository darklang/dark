open Core_kernel
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


let request_path_matches_route ~(route : string) (request_path : string) : bool
    =
  (* Routes are stored in two ways, either in postgres using %% for variables,
   * or in handlers, which use :varname for variables. This needs to handle
   * both. *)
  let split_request_path = split_uri_path request_path in
  let split_route = split_uri_path route in
  let same_length = List.length split_request_path = List.length split_route in
  same_length
  && List.for_all2_exn split_request_path split_route ~f:(fun a r ->
         r = "%" || a = r || route_variable r <> None )


let route_variables (route : string) : string list =
  route
  |> split_uri_path
  |> List.filter ~f:(fun x -> route_variable x <> None)
  |> List.map ~f:(String.chop_prefix_exn ~prefix:":")


let has_route_variables (route : string) : bool =
  List.length (route_variables route) > 0


(* assumes route and request match *)
let bind_route_variables_exn ~(route : string) (request_path : string) :
    (string * RT.dval) list =
  if not (request_path_matches_route ~route request_path)
  then Exception.internal "request/route mismatch" ;
  let split_request_path = split_uri_path request_path in
  let split_route = split_uri_path route in
  List.map2_exn split_request_path split_route ~f:(fun a r ->
      match route_variable r with
      | None ->
          None
      | Some var ->
          Some (var, Dval.dstr_of_string_exn a) )
  |> List.filter_map ~f:(fun x -> x)


(* The specificity ordering is defined as the ordering between the
 * number of `/`-delimited * segments in the route.
 *
 * ie. `/` < `/:a` = `/:b` < `/:a/:b` = `/:aa/:bb` < `/:a/:b/:c` etc.
 *
 *)
let compare_route_specificity (left : string) (right : string) : int =
  let left_count = List.length (split_uri_path left) in
  let right_count = List.length (split_uri_path right) in
  Int.compare left_count right_count


let order_and_filter_wildcards (pages : RT.HandlerT.handler list) :
    RT.HandlerT.handler list =
  if List.length pages <= 1
  then (* do nothing if we have at-most 1 match *)
    pages
  else
    (* partition out wildcards *)
    let wildcards, concrete =
      List.partition_tf
        ~f:(fun h -> has_route_variables (Handler.event_name_for_exn h))
        pages
    in
    if List.length concrete > 0
    then (* if we have concrete route matches, just return them *)
      concrete
    else
      (* else, we _only_ have wildcard matches, and we should return at-most one of them.
       * we should choose which one to match based on a specificity algorithm
       * *)
      let ordered_wildcards =
        wildcards
        |> List.sort ~compare:(fun left right ->
               compare_route_specificity
                 (Handler.event_name_for_exn left)
                 (Handler.event_name_for_exn right) )
        (* we intentionally sort in least specific to most specific order
         * then reverse because the lists are small. we could do it in
         * a single pass by negating the comparison function, but that might
         * obfuscate what we're trying to do here
         *)
        |> List.rev
      in
      (* ordered_wildcards is ordered most-specific to least-specific, so pluck the
       * most specific and return it along with all others of its specificity *)
      match ordered_wildcards with
      | [] ->
          []
      | [a] ->
          [a]
      | a :: rest ->
          let same_specificity =
            List.filter
              ~f:(fun b ->
                let comparison =
                  compare_route_specificity
                    (Handler.event_name_for_exn a)
                    (Handler.event_name_for_exn b)
                in
                comparison = 0 )
              rest
          in
          a :: same_specificity
