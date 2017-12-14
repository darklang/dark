open Core

module RT = Types.RuntimeT

module RouteParamMap = String.Map
type route_param_map = RT.dval RouteParamMap.t

let split_uri_path (path: string) : string list =
  let subs = String.split ~on:'/' path in
  List.filter ~f:(fun x -> String.length x > 0) subs

let path_matches_route ~(path: string) (route: string) : bool =
  let split_path = split_uri_path path in
  let split_route = split_uri_path route in
  let same_length = List.length split_path = List.length split_route in
    same_length &&
    List.for_all2_exn
      split_path
      split_route
      ~f:(fun p r -> p = r || String.is_prefix ~prefix:":" r)

let route_variable_pairs (route: string) : (int * string) list =
  route
  |> split_uri_path
  |> List.mapi ~f:(fun i x -> (i, x))
  |> List.filter ~f:(fun (_, x) -> String.is_prefix ~prefix:":" x)
  |> List.map ~f:(fun (i, x) -> (i, String.chop_prefix_exn ~prefix:":" x))

let route_variables (route: string) : string list =
  route
  |> route_variable_pairs
  |> List.map ~f:Tuple.T2.get2

let has_route_variables (route: string) : bool =
  List.length (route_variables route) > 0

let sample_bound_route_params ~(route: string) : route_param_map =
  let rpm = RouteParamMap.empty in
  let vars = route_variables route in
  List.fold_left
    ~init:rpm
    ~f:(fun rpm1 v -> Map.add rpm1 ~key:v ~data:(RT.DStr ""))
    vars

(* assumes route and path match *)
let bind_route_params_exn ~(uri: Uri.t) ~(route: string) : route_param_map =
  let path = Uri.path uri in
  if path_matches_route ~path:path route
  then
    let split_path = split_uri_path path in
    let pairs =
      route
      |> route_variable_pairs
      |> List.map
           ~f:(fun (i, r) -> (r, List.nth_exn split_path i))
    in
    let rpm = RouteParamMap.empty in
    List.fold_left
      ~init:rpm
      ~f:(fun rpm1 (r,p) -> Map.add rpm1 ~key:r ~data:(RT.DStr p))
      pairs
  else
    Exception.internal "path/route mismatch"

