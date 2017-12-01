open Core

module C = Canvas
module TL = Toplevel

module RouteParamMap = String.Map
type route_param_map = string RouteParamMap.t

let url_for (eh: Handler.handler) : string option =
  match eh.spec.module_, eh.spec.name with
  | Full module_, Full name when String.lowercase module_ = "http" ->
    Some name
  | _ -> None

let url_for_exn (eh: Handler.handler) : string =
  match (url_for eh) with
  | Some s -> s
  | None -> Exception.internal "Called url_for_exn on a toplevel without a `url` param"

let split_uri_path (path: string) : string list =
  let subs = String.split ~on:'/' path in
  List.filter ~f:(fun x -> String.length x > 0) subs

let path_matches_route ~(path: string) (route: string) : bool =
  path = route

let matching_routes ~(uri: Uri.t) (c: C.canvas) : Handler.handler list =
  let path = Uri.path uri in
  c.toplevels
  |> TL.handlers
  |> List.filter
    ~f:(fun tl -> url_for tl <> None)
  |> List.filter
    ~f:(fun tl -> path_matches_route ~path:path (url_for_exn tl))

let pages_matching_route ~(uri: Uri.t) (c: C.canvas) : Handler.handler list =
  matching_routes ~uri:uri c

let route_variables (route: string) : string list =
  let suffix = List.drop (split_uri_path route) 1 in
  suffix
  |> List.filter ~f:(fun x -> String.is_prefix ~prefix:":" x)
  |> List.map ~f:(fun x -> String.chop_prefix_exn ~prefix:":" x)

let has_route_variables (route: string) : bool =
  List.length (route_variables route) > 0

let unbound_path_variables (path: string) : string list =
  List.drop (split_uri_path path) 1

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
      ~f:(fun rpm1 (r,p) -> RouteParamMap.add rpm1 ~key:r ~data:p)
      (List.zip_exn rvars pvars)
  else Exception.internal "Attempted to parse path into route that does not match"

