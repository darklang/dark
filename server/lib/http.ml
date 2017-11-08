open Core
module G = Graph

let routes (g: G.graph) : (string * G.node) list =
  G.page_routes g

let path_matches_route ~(path: string) (route: string) : bool =
  path = route

let matching_routes ~(uri: Uri.t) (g: G.graph) : (string * G.node) list =
  let path = Uri.path uri in
  (routes g)
  |> List.filter ~f:(fun (route, _) -> path_matches_route ~path:path route)


let pages_matching_route ~(uri: Uri.t) (g: G.graph) : G.node list =
  let rs = matching_routes ~uri:uri g in
  List.map ~f:Tuple.T2.get2 rs

