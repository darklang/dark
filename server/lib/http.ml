open Core

module RT = Runtime

module RouteParamMap = String.Map
type route_param_map = RT.dval RouteParamMap.t

let split_uri_path (path: string) : string list =
  let subs = String.split ~on:'/' path in
  List.filter ~f:(fun x -> String.length x > 0) subs

let path_matches_route ~(path: string) (route: string) : bool =
  path = route

let route_variables (route: string) : string list =
  let suffix = List.drop (split_uri_path route) 1 in
  suffix
  |> List.filter ~f:(fun x -> String.is_prefix ~prefix:":" x)
  |> List.map ~f:(fun x -> String.chop_prefix_exn ~prefix:":" x)

let has_route_variables (route: string) : bool =
  List.length (route_variables route) > 0

let unbound_path_variables (path: string) : string list =
  List.drop (split_uri_path path) 1

let sample_bound_route_params ~(route: string) : route_param_map =
  let rpm = RouteParamMap.empty in
  let vars = route_variables route in
  List.fold_left
    ~init:rpm
    ~f:(fun rpm1 v -> RouteParamMap.add rpm1 ~key:v ~data:(RT.DStr ""))
    vars

(* assumes route and path match *)
let bind_route_params_exn ~(uri: Uri.t) ~(route: string) : route_param_map =
  let path = Uri.path uri in
  if path_matches_route ~path:path route
  then
    let rpm = RouteParamMap.empty in
    let rvars = route_variables route in
    let pvars = unbound_path_variables path in
    List.fold_left
      ~init:rpm
      ~f:(fun rpm1 (r,p) -> RouteParamMap.add rpm1 ~key:r ~data:(RT.DStr p))
      (List.zip_exn rvars pvars)
  else
    Exception.internal "path/route mismatch"

