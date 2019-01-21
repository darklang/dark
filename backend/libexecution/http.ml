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
  route
  |> split_uri_path
  |> List.map ~f:(fun segment ->
         if String.is_prefix ~prefix:":" segment then "%%" else segment )
  |> String.concat ~sep:"/"
  |> ( ^ ) "/"


let route_variable (route_segment : string) : string option =
  let prefix = ":" in
  if String.is_prefix ~prefix route_segment
  then Some (String.chop_prefix_exn ~prefix route_segment)
  else None


let request_path_matches_route ~(route : string) (request_path : string) : bool
    =
  let split_request_path = split_uri_path request_path in
  let split_route = split_uri_path route in
  let same_length = List.length split_request_path = List.length split_route in
  same_length
  && List.for_all2_exn split_request_path split_route ~f:(fun a r ->
         r = "%%" || a = r || route_variable r <> None )


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
