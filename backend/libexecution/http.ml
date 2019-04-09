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


let order_and_filter_wildcards (pages : RT.HandlerT.handler list) :
    RT.HandlerT.handler list =
  if List.length pages <= 1
  then (* do nothing if we have at-most 1 match *)
    pages
  else
    let ordered_pages =
      pages
      |> List.sort ~compare:(fun left right ->
             compare_page_route_specificity left right )
      (* we intentionally sort in least specific to most specific order
        * then reverse because the lists are small. we could do it in
        * a single pass by negating the comparison function, but that might
        * obfuscate what we're trying to do here
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
