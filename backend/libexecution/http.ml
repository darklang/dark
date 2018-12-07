open Core_kernel
module RT = Types.RuntimeT

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


let path_matches_route ~(path : string) (route : string) : bool =
  let split_path = split_uri_path path in
  let split_route = split_uri_path route in
  let same_length = List.length split_path = List.length split_route in
  same_length
  && List.for_all2_exn split_path split_route ~f:(fun p r ->
         p = r || String.is_prefix ~prefix:":" r )


let route_variable_pairs (route : string) : (int * string) list =
  route
  |> split_uri_path
  |> List.mapi ~f:(fun i x -> (i, x))
  |> List.filter ~f:(fun (_, x) -> String.is_prefix ~prefix:":" x)
  |> List.map ~f:(fun (i, x) -> (i, String.chop_prefix_exn ~prefix:":" x))


let route_variables (route : string) : string list =
  route |> route_variable_pairs |> List.map ~f:Tuple.T2.get2


let has_route_variables (route : string) : bool =
  List.length (route_variables route) > 0


(* assumes route and path match *)
let bind_route_params_exn ~(path : string) ~(route : string) :
    (string * RT.dval) list =
  if path_matches_route ~path route
  then
    let split_path = split_uri_path path in
    route
    |> route_variable_pairs
    |> List.map ~f:(fun (i, r) -> (r, RT.DStr (List.nth_exn split_path i)))
  else Exception.internal "path/route mismatch"
